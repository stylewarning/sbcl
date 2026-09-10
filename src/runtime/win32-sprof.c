/* Windows statistical profiling. The Lisp sampler owns the state and protects
 * each target with its TLS lock. Capture runs WITHOUT-GCING; storage does not.
 * No allocation, Lisp call, or user-space lock is allowed while suspended. */
#include "thread.h"
#include "arch.h"
#include "gc.h"
#include "lispregs.h"
#include "sprof.h"
#include <winternl.h>
#ifdef LISP_FEATURE_X86_64
#include <dbghelp.h>
#include "core.h"
#endif

#ifdef LISP_FEATURE_X86_64
/* DbgHelp is not reentrant. Never wait: an allocation sampler can be in
 * pseudo-atomic, and a timer sampler is inhibiting collection. */
static int unwind_busy;
struct stack_reader {
    struct thread* thread;
    struct sprof_snapshot* snapshot;
};

static BOOL CALLBACK read_stack(HANDLE token, DWORD64 address, PVOID buffer,
                                DWORD size, LPDWORD bytes_read)
{
    struct stack_reader* reader = (void*)token;
    struct sprof_snapshot* stack = reader->snapshot;
    *bytes_read = 0;
    if (stack && address >= (uword_t)reader->thread->control_stack_start
        && address < (uword_t)reader->thread->control_stack_end) {
        if (address < stack->start || size > stack->size
            || address-stack->start > stack->size-size) return FALSE;
        memcpy(buffer, stack->data + (address-stack->start), size);
        *bytes_read = size;
        return TRUE;
    }
    SIZE_T count;
    BOOL ok = ReadProcessMemory(GetCurrentProcess(), (void*)address, buffer, size, &count);
    *bytes_read = count;
    return ok;
}

static PVOID CALLBACK function_table(HANDLE token, DWORD64 pc)
{
    (void)token;
    DWORD64 base;
    return RtlLookupFunctionEntry(pc, &base, NULL);
}

static DWORD64 CALLBACK module_base(HANDLE token, DWORD64 pc)
{
    (void)token;
    MEMORY_BASIC_INFORMATION info;
    return VirtualQuery((void*)pc, &info, sizeof info) && info.Type == MEM_IMAGE
        ? (DWORD64)info.AllocationBase : 0;
}

/* Native frames use PE unwind information, including frame-pointer omission.
 * Lisp frames use SBCL's frame chain. All target stack reads go through the
 * snapshot callback; StackWalk64 runs only AFTER the target has resumed. */
int win32_sprof_unwind(struct thread* thread, os_context_t* context,
                       struct sprof_snapshot* snapshot, uword_t* pcs,
                       int limit, int allocation)
{
    if (__sync_lock_test_and_set(&unwind_busy, 1)) return 0;
    struct stack_reader reader = {thread, snapshot};
    CONTEXT registers = *context->win32_context;
    int len = 0, reached_lisp = 0;
    uword_t seh = (uword_t)get_asm_routine_by_name("SEH-TRAMPOLINE", NULL);
    for (int depth = 0; depth < TRACE_BUFFER_LEN && len < limit; ++depth) {
        uword_t pc = registers.Rip;
        if (!pc || (pc >= (uword_t)thread->control_stack_start
                    && pc < (uword_t)thread->control_stack_end)) break;
        int lispp = component_ptr_from_pc((void*)pc) != 0;
        if (reached_lisp && !lispp) break;
        if (!allocation || lispp) pcs[len++] = pc;
        if (len == limit || !pc) break;
        if (lispp) {
            reached_lisp = 1;
            allocation = 0;
            /* The SEH thunk keeps the actual Lisp return PC in nonvolatile R15. */
            if (pc >= seh && pc < seh+8 && registers.R15
                && registers.R15 != pc) {
                pcs[len++] = registers.R15;
                if (len == limit) break;
            }
            uword_t fp = registers.Rbp, words[2];
            DWORD count;
            if ((fp & 7) || fp < registers.Rsp
                || fp < (uword_t)thread->control_stack_start
                || fp > (uword_t)thread->control_stack_end - sizeof words
                || !read_stack((HANDLE)&reader, fp, words, sizeof words, &count)) break;
            registers.Rsp = fp + sizeof words;
            registers.Rbp = words[0];
            registers.Rip = words[1];
            if (registers.Rbp <= fp) break;
        } else {
            STACKFRAME64 frame;
            memset(&frame, 0, sizeof frame);
            frame.AddrPC.Offset = pc;
            frame.AddrStack.Offset = registers.Rsp;
            frame.AddrFrame.Offset = registers.Rbp;
            frame.AddrPC.Mode = frame.AddrStack.Mode = frame.AddrFrame.Mode = AddrModeFlat;
            int advanced = 0;
            for (int attempt = 0; attempt < 2; ++attempt) {
                if (!StackWalk64(IMAGE_FILE_MACHINE_AMD64, (HANDLE)&reader,
                                 (HANDLE)thread->os_thread, &frame, &registers,
                                 read_stack, function_table, module_base, NULL)) break;
                if (frame.AddrPC.Offset != pc) { advanced = 1; break; }
            }
            if (!advanced) {
                if (len && len < limit) pcs[len++] = (uword_t)-1;
                break;
            }
            registers.Rip = frame.AddrPC.Offset;
            registers.Rsp = frame.AddrStack.Offset;
        }
    }
    __sync_lock_release(&unwind_busy);
    return len;
}
#endif

#define SNAPSHOT_BYTES (256*1024)
enum { ATTEMPTS, CAPTURE_FAILURES, SHORT_STACKS, MISSED_TICKS,
       STORE_FAILURES, IDLE_SAMPLES, MISSED_CPU_SAMPLES, UNWIND_FAILURES, N_COUNTERS };

typedef NTSTATUS (NTAPI *query_system_t)(SYSTEM_INFORMATION_CLASS, PVOID, ULONG, PULONG);
typedef NTSTATUS (NTAPI *query_thread_t)(HANDLE, ULONG, PVOID, ULONG, PULONG);
#define PROCESS_INFO_BYTES (8*1024*1024)

struct win32_sprof {
    HANDLE timer, stop;
    uword_t random;
    uword_t epoch;
    uint64_t interval;
    LARGE_INTEGER frequency, deadline;
    query_system_t query_system;
    query_thread_t query_thread;
    char* process_info;
    int cpu;
    uint64_t counters[N_COUNTERS];
    CONTEXT context;
    struct trace trace;
    struct sprof_snapshot snapshot;
    char stack[SNAPSHOT_BYTES];
};

static uword_t next_epoch;

/* GetThreadTimes is accounted in system clock ticks, even though its unit is
 * 100ns. Expose the actual resolution instead of manufacturing extra samples. */
double win32_sprof_interval(double requested, int cpu)
{
    DWORD adjustment, increment;
    BOOL disabled;
    if (cpu && GetSystemTimeAdjustment(&adjustment, &increment, &disabled)
        && requested < increment / 10000000.0)
        return increment / 10000000.0;
    return requested;
}

void win32_sprof_destroy(struct win32_sprof* state)
{
    if (state->timer) CloseHandle(state->timer);
    if (state->stop) CloseHandle(state->stop);
    if (state->process_info) VirtualFree(state->process_info, 0, MEM_RELEASE);
    VirtualFree(state, 0, MEM_RELEASE);
}

void* win32_sprof_create(uint64_t interval, int cpu)
{
    if (!interval) return NULL;
    struct win32_sprof* state = VirtualAlloc(NULL, sizeof *state,
                                            MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
    if (!state) return NULL;
    typedef HANDLE (WINAPI *create_timer_t)(LPSECURITY_ATTRIBUTES, LPCWSTR, DWORD, DWORD);
    create_timer_t create_timer = (create_timer_t)GetProcAddress(
        GetModuleHandleW(L"kernel32.dll"), "CreateWaitableTimerExW");
    if (create_timer)
        state->timer = create_timer(NULL, NULL, 2 /* HIGH_RESOLUTION */, TIMER_ALL_ACCESS);
    if (!state->timer) state->timer = CreateWaitableTimer(NULL, FALSE, NULL);
    state->stop = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (!state->timer || !state->stop) {
        win32_sprof_destroy(state);
        return NULL;
    }
    state->query_system = (query_system_t)GetProcAddress(
        GetModuleHandleW(L"ntdll.dll"), "NtQuerySystemInformation");
    state->query_thread = (query_thread_t)GetProcAddress(
        GetModuleHandleW(L"ntdll.dll"), "NtQueryInformationThread");
    /* ThreadSystemThreadInformation (40), available since Windows 10, avoids
     * enumerating every process and thread for each accounting deadline.
     * Probe the information class rather than assuming an OS version. */
    SYSTEM_THREAD_INFORMATION info;
    if (state->query_thread
        && state->query_thread(GetCurrentThread(), 40, &info, sizeof info, NULL) < 0)
        state->query_thread = NULL;
    if (cpu && !state->query_thread)
        state->process_info = VirtualAlloc(NULL, PROCESS_INFO_BYTES,
                                               MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
    if (cpu && !state->query_thread && (!state->query_system || !state->process_info)) {
        win32_sprof_destroy(state);
        return NULL;
    }
    QueryPerformanceFrequency(&state->frequency);
    if (interval * (double)state->frequency.QuadPart / 10000000.0 > (double)INT64_MAX/4) {
        win32_sprof_destroy(state);
        return NULL;
    }
    LARGE_INTEGER now;
    QueryPerformanceCounter(&now);
    state->random = (uword_t)now.QuadPart ^ (uword_t)state;
    if (!state->random) state->random = 1;
    state->epoch = __sync_add_and_fetch(&next_epoch, 1);
    state->interval = interval;
    state->cpu = cpu;
    state->snapshot.data = state->stack;
    return state;
}

/* Return 1 for a tick, 0 for shutdown, -1 for an OS error. One-shot timers
 * allow sub-millisecond intervals and do not accumulate a backlog of ticks. */
int win32_sprof_wait(struct win32_sprof* state)
{
    /* A normal-priority sampler can be starved by the very CPU-bound threads
     * it is measuring. Only this private worker gets a priority boost. It
     * blocks on the timer between scans; no realtime priority is required. */
    if (!state->deadline.QuadPart
        && !SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_ABOVE_NORMAL)) return -1;
    /* CPU deadlines advance in CPU time. Poll with jitter so that short,
     * periodic bursts are not persistently missed by a fixed polling phase. */
    double interval = state->interval;
    if (state->cpu) {
        if (interval > 10000) interval = 10000;
        state->random ^= state->random << 13;
        state->random ^= state->random >> 17;
        state->random ^= state->random << 5;
        interval *= 0.5 + (state->random & 0xffff) / 65536.0;
    }
    LARGE_INTEGER now;
    QueryPerformanceCounter(&now);
    int64_t period = interval * state->frequency.QuadPart / 10000000.0;
    if (period < 1) period = 1;
    if (!state->deadline.QuadPart) state->deadline.QuadPart = now.QuadPart;
    state->deadline.QuadPart += period;
    if (state->deadline.QuadPart <= now.QuadPart) {
        ++state->counters[MISSED_TICKS];
        state->deadline.QuadPart = now.QuadPart + period;
    }
    LARGE_INTEGER due;
    due.QuadPart = -(int64_t)((state->deadline.QuadPart-now.QuadPart)
                              * (10000000.0/state->frequency.QuadPart));
    if (!due.QuadPart) due.QuadPart = -1;
    if (!SetWaitableTimer(state->timer, &due, 0, NULL, NULL, FALSE)) return -1;
    HANDLE handles[2] = {state->stop, state->timer};
    DWORD result = WaitForMultipleObjects(2, handles, FALSE, INFINITE);
    return result == WAIT_OBJECT_0 ? 0 : result == WAIT_OBJECT_0+1 ? 1 : -1;
}

void win32_sprof_stop(struct win32_sprof* state)
{
    SetEvent(state->stop);
}

uint64_t win32_sprof_counter(struct win32_sprof* state, int index)
{
    return index >= 0 && index < N_COUNTERS ? state->counters[index] : 0;
}

void win32_sprof_set_sampling(struct thread* thread, int enable)
{
    thread_extra_data(thread)->sprof_epoch = 0;
    thread->sprof_enable = make_fixnum(enable);
}

static int thread_state(struct win32_sprof* state, struct thread* thread)
{
    if (state->query_thread) {
        SYSTEM_THREAD_INFORMATION info;
        if (state->query_thread((HANDLE)thread->os_thread, 40, &info, sizeof info, NULL) < 0)
            return -1;
        return info.ThreadState;
    }
    ULONG length;
    if (state->query_system(SystemProcessInformation, state->process_info,
                             PROCESS_INFO_BYTES, &length) < 0) return -1;
    ULONG offset = 0;
    struct thread_instance* instance = (void*)native_pointer(thread->lisp_thread);
#ifdef LISP_FEATURE_64_BIT
    uword_t tid = fixnum_value(instance->os_tid);
#else
    uword_t tid = instance->uw_os_tid;
#endif
    while (offset <= length && length-offset >= sizeof(SYSTEM_PROCESS_INFORMATION)) {
        SYSTEM_PROCESS_INFORMATION* process = (void*)(state->process_info+offset);
        if ((uword_t)process->UniqueProcessId == GetCurrentProcessId()) {
            SYSTEM_THREAD_INFORMATION* threads = (void*)(process+1);
            if (process->NumberOfThreads > (length-offset-sizeof *process)/sizeof *threads)
                return -1;
            for (ULONG i = 0; i < process->NumberOfThreads; ++i) {
                if ((uword_t)threads[i].ClientId.UniqueThread == tid) {
                    return threads[i].ThreadState;
                }
            }
            break;
        }
        if (!process->NextEntryOffset || process->NextEntryOffset > length-offset) break;
        offset += process->NextEntryOffset;
    }
    return 4; // terminated or absent
}

static int cpu_sample_due(struct win32_sprof* state, struct thread* thread)
{
    FILETIME creation, exit, kernel, user;
    if (!GetThreadTimes((HANDLE)thread->os_thread, &creation, &exit, &kernel, &user)) return -1;
    uint64_t now = (((uint64_t)kernel.dwHighDateTime << 32) | kernel.dwLowDateTime)
                 + (((uint64_t)user.dwHighDateTime << 32) | user.dwLowDateTime);
    struct extra_thread_data* extra = thread_extra_data(thread);
    if (extra->sprof_epoch != state->epoch) {
        extra->sprof_epoch = state->epoch;
        extra->sprof_cpu_deadline = now + 1 + state->random % state->interval;
        return 0;
    }
    if (now < extra->sprof_cpu_deadline) return 0;

    /* CPU time can become visible on a context switch into a wait. Keep the
     * deadline pending until the thread is runnable, to avoid charging that
     * time to its blocking call. Query before suspension (which changes state).
     * The fallback buffer is reserved at startup, never while a target is stopped. */
    int status = thread_state(state, thread);
    if (status < 0) return -1;
    int runnable = status == 1 || status == 2 || status == 3 || status == 7;
    if (!runnable) { ++state->counters[IDLE_SAMPLES]; return 0; }
    uint64_t missed = (now-extra->sprof_cpu_deadline) / state->interval;
    state->counters[MISSED_CPU_SAMPLES] += missed;
    extra->sprof_cpu_deadline += (missed+1) * state->interval;
    return 1;
}

/* Return a prepared trace, no sample, or -1 if resumption failed. The caller
 * reports errors only after leaving WITHOUT-GCING. */
int win32_sprof_capture(struct win32_sprof* state, struct thread* thread)
{
    if (!thread->sprof_enable || thread == get_sb_vm_thread()) return 0;
    HANDLE handle = (HANDLE)thread->os_thread;
    if (state->cpu) {
        int due = cpu_sample_due(state, thread);
        if (!due) return 0;
        if (due < 0) return -2;
    }
    ++state->counters[ATTEMPTS];
    memset(&state->context, 0, sizeof state->context);
    state->context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
    state->snapshot.size = 0;
    int priority = GetThreadPriority(handle);
    DWORD suspended = SuspendThread(handle);
    if (suspended == (DWORD)-1) goto failed;
    /* GetThreadContext needs the target to service a kernel APC. A ready
     * thread can otherwise wait an entire scheduling quantum per competitor.
     * Raise its priority only while suspended, and restore it BEFORE resuming
     * user code. Never lower an already higher priority or change an external
     * suspension. Failure to obtain the boost still permits ordinary capture. */
    int boosted = suspended == 0 && priority != THREAD_PRIORITY_ERROR_RETURN
        && priority < THREAD_PRIORITY_HIGHEST
        && SetThreadPriority(handle, THREAD_PRIORITY_HIGHEST);
    int captured = 0;
    if (suspended == 0 && GetThreadContext(handle, &state->context)) {
        os_context_t context = { .win32_context = &state->context };
        uword_t low = (uword_t)thread->control_stack_start;
        uword_t high = (uword_t)thread->control_stack_end;
        uword_t start, end;
#ifdef LISP_FEATURE_ARM64
        state->snapshot.foreignp = foreign_function_call_active_p(thread) != 0;
        state->snapshot.frame_pointer = state->snapshot.foreignp
            ? (uword_t)access_control_frame_pointer(thread)
            : *os_context_register_addr(&context, reg_CFP);
        end = state->snapshot.foreignp ? (uword_t)access_control_stack_pointer(thread)
            : *os_context_register_addr(&context, reg_CSP);
        start = end >= low && end-low > SNAPSHOT_BYTES ? end-SNAPSHOT_BYTES : low;
#else
        start = *os_context_sp_addr(&context);
        end = high;
        if (start <= end && end-start > SNAPSHOT_BYTES) end = start+SNAPSHOT_BYTES;
#endif
        if (start >= low && end <= high && start < end) {
            SIZE_T copied = 0;
            state->snapshot.start = start;
            if (ReadProcessMemory(GetCurrentProcess(), (void*)start, state->stack,
                                  end-start, &copied) && copied == end-start) {
                state->snapshot.size = copied;
                captured = 1;
                if (copied == SNAPSHOT_BYTES) ++state->counters[SHORT_STACKS];
            }
        }
    }
    int restored = !boosted || SetThreadPriority(handle, priority);
    /* Balance exactly the suspension we acquired, including skipped targets. */
    if (ResumeThread(handle) == (DWORD)-1) return -1;
    if (!restored) return -3;
    if (captured == 1) {
        os_context_t context = { .win32_context = &state->context };
        int prepared = sprof_prepare_trace(thread, &context, &state->snapshot, &state->trace);
        if (!prepared) ++state->counters[UNWIND_FAILURES];
        return prepared;
    }
failed:
    ++state->counters[CAPTURE_FAILURES];
    return 0;
}

void win32_sprof_record(struct win32_sprof* state, struct thread* thread)
{
    if (sprof_store_trace(thread, &state->trace, 1) != 1)
        ++state->counters[STORE_FAILURES];
}
