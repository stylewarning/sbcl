#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include "thread.h"
#include "murmur_hash.h"
#include "gc-assert.h"
#include "arch.h" // why is component_ptr_from_pc declared here???
#include "code.h"
#include "gc.h"
#include "lispregs.h"
#include "sprof.h"
#if !defined LISP_FEATURE_X86 && !defined LISP_FEATURE_X86_64
#include "callframe.inc"
#endif
#include "genesis/compiled-debug-info.h"

#ifdef MEMORY_SANITIZER
#include <sanitizer/msan_interface.h>
#endif

#include <limits.h>
#include <fcntl.h>
#include <unistd.h>
/* Basic approach:
 * each thread allocates a storage for samples (traces) and a hash-table
 * to groups matching samples together. Collisions in the table are resolved
 * by chaining.
 *
 * At each sample collection:
 * 1. gather up to TRACE_BUFFER_LEN program-counter locations as the stack trace
 * 2. if it exceeds MAX_RECORDED_TRACE_LEN locations, condense it to
      the first and last frames with an elision marker in between.
 * 3. compute a hash of the trace
 * 4. if trace is present in hash-table
 *     then increment the count
 *     else
 *       ensure trace has stable PC locations
 *       add or update
 * Each PC location is tentatively recorded as raw PC location.
 * To ensure location stability:
 *    Check whether each raw PC loc is within lisp code and not pseudostatic.
 *    For each which is movable, express the PC as a code serial# and offset.
 */

#define N_BUCKETS 0x10000
#define HASH_MASK (N_BUCKETS-1)
#ifdef LISP_FEATURE_64_BIT
  // Disable sampling if buffer has grown to 8 million elements, which is 64MiB.
  // This is enough to store 128k traces if each trace has its maximum of 62 frames
  // which most of them won't. ((62+2) * 8 * 128 * 1024) = 64MiB
#  define CAPACITY_MAX 8*1024*1024
#else
  // Disable sampling if buffer has grown to 1 million elements, which is 8MiB.
#  define CAPACITY_MAX 1024*1024
#endif
#define INITIAL_FREE_POINTER 2

// Lisp and C both accesses this.
// The structure is 2 elements long, each element being 8 bytes.
struct sprof_data {
    // Element 0
    uint32_t *buckets; // power-of-2-sized bucket array
#ifndef LISP_FEATURE_64_BIT
    uint32_t padding0;
#endif
    // Element 1
    // index into next available element of trace_buffer.
    // elements are always 8 bytes regardless of machine word size.
    uint32_t free_pointer;
    uint32_t capacity;
};

static inline struct trace* sprof_data_trace(struct sprof_data* data, uint32_t index) {
    return (void*)((char*)data + index*ELEMENT_SIZE);
}

static int in_stack_range(uword_t pc, struct thread* thread)
{
    return pc >= (uword_t)thread->control_stack_start
        && pc < (uword_t)thread->control_stack_end;
}

// Use the WORD-MIX algorithm from src/code/string-hash
static inline uword_t word_mix(uword_t x, uword_t y)
{
    uword_t mul = 3622009729038463111LL & UINT_MAX;
    uword_t xor = 608948948376289905LL & UINT_MAX;
    sword_t xy = x * mul + y;
    return xor ^ xy ^ (xy >> 5);
}

#ifdef LISP_FEATURE_64_BIT
static uint32_t compute_hash(uword_t* elements, int len) {
    int i;
    uword_t hash = len;
    for (i = 0; i < len; i++) {
        uword_t pc = elements[i];
        hash = word_mix(hash, pc);
    }
    return (uint32_t)murmur3_fmix64(hash);
}
#else
static uint32_t compute_hash(struct loc* elements, int len) {
    int i;
    uword_t hash = len;
    for (i = 0; i < len; i++) {
        uword_t pc = elements[i].word0; // this is either a PC or the code serial#
        hash = word_mix(hash, pc);
    }
    return murmur3_fmix32(hash);
}
#endif

static inline void store_trace_header(struct trace* trace, uint32_t hash, uint32_t len)
{
#ifdef LISP_FEATURE_64_BIT
    trace->header = ((uword_t)hash<<32) | len;
#else
    trace->hash = hash;
    trace->len = len;
#endif
}

static int trace_equal(struct trace* a, struct trace* b)
{
    int i;
#ifdef LISP_FEATURE_64_BIT
    if (a->header != b->header) return 0;
    for (i=0; i<trace_len(a); ++i) if (a->locs[i] != b->locs[i]) return 0;
#else
    if (a->hash != b->hash || a->len != b->len) return 0;
    for (i=0; i<trace_len(a); ++i)
        if (a->locs[i].word0 != b->locs[i].word0 ||
            a->locs[i].word1 != b->locs[i].word1) return 0;
#endif
    return 1;
}

static uint32_t* hash_get(struct sprof_data* data, struct trace* trace, uint32_t hash)
{
    int index = hash & HASH_MASK;
    uint32_t entry = data->buckets[index];
    while (entry) {
        struct trace* key = sprof_data_trace(data, entry);
        if (trace_equal(key, trace)) return &key->multiplicity;
        entry = key->next;
    }
    return 0;
}

static uint32_t* hash_insert(struct sprof_data* data, struct trace* trace, uint32_t hash)
{
    // allocate a permanent copy. +2 is for the fixed overhead elements
    int n_elements = TRACE_PREFIX_ELEMENTS + trace_len(trace);
    struct trace* copy = sprof_data_trace(data, data->free_pointer);
    memcpy(copy, trace, n_elements * ELEMENT_SIZE);
    copy->multiplicity = 0;
    // insert into chain for this hash
    int index = hash & HASH_MASK;
    copy->next = data->buckets[index];
    data->buckets[index] = data->free_pointer;
    // consume the buffer elements
    data->free_pointer += n_elements;
    return &copy->multiplicity;
}

static int unstable_program_counter_p(uword_t addr)
{
#ifdef LISP_FEATURE_CHENEYGC
    return (DYNAMIC_0_SPACE_START <= addr &&
            addr < DYNAMIC_0_SPACE_START + dynamic_space_size)
        || (DYNAMIC_1_SPACE_START <= addr &&
            addr < DYNAMIC_1_SPACE_START + dynamic_space_size);
#else
    /* I think that it's a reasonable assumption that code in immobile
     * space will not be garbage-collected during the profiling run.
     * Similar issue for fdefns which contain an executable instruction
     * as well as closure-calling trampolines and builtin-trampoline GFs.
     * It might be neat to mark some objects with a bit saying
     * never to move them if they appeared in a trace.
     * When would the bit get cleared though? I don't know */
    page_index_t page = find_page_index((void*)addr);
    return page >= 0 && page_table[page].gen != PSEUDO_STATIC_GENERATION;
#endif
}

#ifdef LISP_FEATURE_64_BIT
#define STORE_PC(tr, indx, val) (tr).locs[indx] = val
#define STORE_REL_PC(tr, indx, ser, offs) \
  (tr).locs[indx] = ((uword_t)1 << 63) | ((offs) << 32) | (ser)
#else
#define STORE_PC(tr, indx, val) (tr).locs[indx].word0 = val; (tr).locs[indx].word1 = 0
#define STORE_REL_PC(tr, indx, ser, offs) \
  (tr).locs[indx].word0 = ser; (tr).locs[indx].word1 = offs
#endif

/* Represent 'trace' using code_serialno + offset for some PC locations.
 * Locations in foreign and pseudo-static code may remain as-is.
 * Return 1 if the trace was affected by stabilizing it. */
static int NO_SANITIZE_MEMORY stabilize(struct trace* trace)
{
    int len = trace_len(trace);
    int changedp = 0;
    int i;
    uword_t pc;
    for(i=0; i<len; ++i) {
#ifdef LISP_FEATURE_64_BIT
        pc = trace->locs[i];
#else
        pc = trace->locs[i].word0;
#endif
        if (unstable_program_counter_p(pc)) {
            struct code *code = (void*)component_ptr_from_pc((char*)pc);
            if (code) {
                STORE_REL_PC(*trace, i, code_serialno(code), (pc - (uword_t)code));
                changedp = 1;
            } else {
                // Can't have any unstable locations in the result
                STORE_PC(*trace, i, (uword_t)-1);
                changedp = 1;
            }
        }
    }
    return changedp;
}

static int NO_SANITIZE_MEMORY
read_frame(struct thread* thread, struct sprof_snapshot* snapshot,
           uword_t address, uword_t* words)
{
    if ((address & (N_WORD_BYTES-1))
        || address < (uword_t)thread->control_stack_start
        || address > (uword_t)thread->control_stack_end - 2*N_WORD_BYTES)
        return 0;
    if (snapshot) {
        if (address < snapshot->start || snapshot->size < 2*N_WORD_BYTES
            || address - snapshot->start > snapshot->size - 2*N_WORD_BYTES)
            return 0;
        memcpy(words, snapshot->data + (address - snapshot->start), 2*N_WORD_BYTES);
    } else {
        memcpy(words, (void*)address, 2*N_WORD_BYTES);
    }
    return 1;
}

#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_X86
/* x86 allocation trampolines spill two or three registers BEFORE establishing
 * EBP. Their return PC is above those spills, not at EBP+4. Recognize the
 * epilogue at the return from C: MOV ESP,EBP; POP EBP; [MOV reg,EAX]; POPs; RET.
 * This is the sequence emitted by src/assembly/x86/alloc.lisp. */
static void x86_allocation_frame(struct thread* thread, struct sprof_snapshot* snapshot,
                                 uword_t pc, uword_t fp, uword_t* words)
{
    if (!points_to_asm_code_p(pc)) return;
    unsigned char* p = (void*)pc;
    if (p[0] != 0x8b || p[1] != 0xe5 || p[2] != 0x5d) return;
    p += 3;
    if (p[0] == 0x8b && (p[1] & 0xc7) == 0xc0) p += 2;
    int spills = 0;
    while (spills < 3 && *p >= 0x58 && *p <= 0x5f) { ++spills; ++p; }
    if ((spills == 2 || spills == 3) && *p == 0xc3) {
        uword_t saved[2];
        if (read_frame(thread, snapshot, fp + spills*N_WORD_BYTES, saved))
            words[1] = saved[1];
    }
}
#endif

static int NO_SANITIZE_MEMORY
gather_trace_from_context(struct thread* thread, os_context_t* context,
                          struct trace* trace, int limit,
                          struct sprof_snapshot* snapshot)
{
#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_X86_64
    return win32_sprof_unwind(thread, context, snapshot, trace->locs, limit, 0);
#endif
    uword_t pc = os_context_pc(context);
    int len = 1;

#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
    STORE_PC(*trace, 0, pc);
    uword_t* fp = (uword_t*)os_context_frame_pointer(context);
    uword_t* sp = (uword_t*)*os_context_sp_addr(context);
    if (fp >= sp && fp < thread->control_stack_end) { // plausible frame-pointer
        // TODO: This should be replaced with more sophisticated backtrace routine
        // that understands foreign code compiled without frame pointers.
        // It's no different from what we have now though.
        for(;;) {
            uword_t words[2];
            if (!read_frame(thread, snapshot, (uword_t)fp, words)) break;
#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_X86
            x86_allocation_frame(thread, snapshot, pc, (uword_t)fp, words);
#endif
            uword_t prev_fp = words[0];
            uword_t prev_pc = words[1];
#ifdef LISP_FEATURE_64_BIT
            // If this can't possibly be a valid program counter,
            // change it to the "unknown" value.
            if ((sword_t)prev_pc < 0) prev_pc = (sword_t)-1;
#endif
            STORE_PC(*trace, len, prev_pc);
            if (++len == limit) break;
            // Ensure that the next FP and PC are reasonable.
            if (prev_fp <= (uword_t)fp || prev_fp >= (uword_t)thread->control_stack_end
                || in_stack_range(prev_pc, thread)) break;
            fp = (uword_t*)prev_fp;
            pc = prev_pc;
        }
    }
#else
    if (gc_managed_heap_space_p(pc) && component_ptr_from_pc((void*)pc)) {
        // If the PC was in lisp code, then the frame register is
        // probably correct, and so it's probably the case that
        // 'frame->saved_lra' is a tagged PC in the
        // caller. Unfortunately it is not 100% reliable in a signal
        // handler, so we don't try to walk back more than one frame,
        // unless we are on ARM64 where precise backtraces are
        // available.
        struct call_frame* frame = (void*)(*os_context_register_addr(context, reg_CFP));
#ifdef LISP_FEATURE_ARM64

        lispobj lr;
        uword_t frame_words[2];
        unsigned inst = ((unsigned *) pc)[0];

        /* The first frame needs to be found */
        if (points_to_asm_code_p(pc) ||
            (inst == 0xD65F03C0 && // RET
             ((unsigned *) pc)[-1] == 0xA9407B5A)) { // LDP CFP, LR, [CFP]
            STORE_PC(*trace, 0, pc);
            lr = (lispobj)*os_context_register_addr(context, reg_LR);
            STORE_PC(*trace, len, lr);
            if (++len == limit) return len;
        }
        else if (inst == 0xD63F03C0) { // BLR LR
            /* Use the destination address as the current PC, because
               the new frame is not yet set up */
            lr = (lispobj)*os_context_register_addr(context, reg_LR);
            STORE_PC(*trace, 0, lr);
            /* And it's called by the current function */
            STORE_PC(*trace, len, pc+4);
            if (++len == limit) return len;

            /* Unless an asm routine is called within the same frame. */
            if ((((unsigned *) pc)[-1] | 0x1F0000) == 0xAA1F03FA) { // MOV CFP, Rx
                if (!read_frame(thread, snapshot, (uword_t)frame, frame_words))
                    return len;
                frame = (void*)frame_words[0];
            }
        }
        else if (inst == 0xF900075E) {  // STR LR, [CFP, #8]
            STORE_PC(*trace, 0, pc);
            lr = (lispobj)*os_context_register_addr(context, reg_LR);
            STORE_PC(*trace, len, lr);
            if (++len == limit) return len;
            if (!read_frame(thread, snapshot, (uword_t)frame, frame_words))
                return len;
            frame = (void*)frame_words[0];
        } else if ((inst >> 25) == 0x4A && // BL Lx
                   ((((unsigned *) pc)[-1] | 0x1F0000) == 0xAA1F03FA)) { // MOV CFP, Rx
            /* A local call */
            unsigned imm = inst & 0x3FFFFFF;
            // sign extend
            int offset = ((int)(imm << 6) >> 6) * 4;
            STORE_PC(*trace, 0, pc+offset);
            STORE_PC(*trace, len, pc+4);
            if (++len == limit) return len;
            if (!read_frame(thread, snapshot, (uword_t)frame, frame_words))
                return len;
            frame = (void*)frame_words[0];
        }
        else {
            STORE_PC(*trace, 0, pc);
        }

        for (;;) {
            if (!read_frame(thread, snapshot, (uword_t)frame, frame_words))
                break;
            lr = frame_words[1];

            if (!component_ptr_from_pc((char*)lr))
                break;
            STORE_PC(*trace, len, lr);
            if (++len == limit) break;
            if (frame_words[0] >= (uword_t)frame) break;
            frame = (void*)frame_words[0];
        }
#else
        STORE_PC(*trace, 0, pc);
        if (in_stack_range((uword_t)frame, thread) &&
#ifdef reg_LRA
            lowtag_of(frame->saved_lra) == OTHER_POINTER_LOWTAG &&
#endif
            component_ptr_from_pc((void*)frame->saved_lra)) {
            STORE_PC(*trace, len, frame->saved_lra);
            ++len;
        }

#endif
    } else {
        /* Probably foreign code */
         STORE_PC(*trace, 0, pc);

#ifdef LISP_FEATURE_ARM64
         if (snapshot ? snapshot->foreignp : foreign_function_call_active_p(thread) != 0) {
             struct call_frame* frame = (void*)(snapshot ? snapshot->frame_pointer
                                                        : (uword_t)access_control_frame_pointer(thread));
             lispobj lr;
             uword_t frame_words[2];

             for (;;) {
                 if (!read_frame(thread, snapshot, (uword_t)frame, frame_words))
                     break;
                 lr = frame_words[1];

                 if (!component_ptr_from_pc((char*)lr))
                     break;
                 STORE_PC(*trace, len, lr);
                 if (++len == limit) break;
                 if (frame_words[0] >= (uword_t)frame) break;
                 frame = (void*)frame_words[0];
             }
         }
#endif

    }
#endif
    return len;
}

static int gather_trace_from_frame(struct thread* thread, uword_t* fp,
                                   struct trace* trace, int limit)
{
#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_X86_64
    CONTEXT registers;
    RtlCaptureContext(&registers);
    os_context_t context = { .win32_context = &registers };
    return win32_sprof_unwind(thread, &context, NULL, trace->locs, limit, 1);
#endif
    int len = 0;

#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_X86
    uword_t pc = 0;
#endif
    if (fp >= thread->control_stack_start && fp < thread->control_stack_end) {
        for(;;) {
            uword_t words[2];
            if (!read_frame(thread, NULL, (uword_t)fp, words)) break;
#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_X86
            x86_allocation_frame(thread, NULL, pc, (uword_t)fp, words);
#endif
            uword_t prev_fp = words[0];
            uword_t prev_pc = words[1];
#ifdef LISP_FEATURE_64_BIT
            // If this can't possibly be a valid program counter,
            // change it to the "unknown" value.
            if ((sword_t)prev_pc < 0) prev_pc = (sword_t)-1;
#endif
            STORE_PC(*trace, len, prev_pc);
            if (++len == limit) break;
            // Ensure that the next FP and PC are reasonable.
            if (prev_fp <= (uword_t)fp || prev_fp >= (uword_t)thread->control_stack_end
                || in_stack_range(prev_pc, thread)) break;
            fp = (uword_t*)prev_fp;
#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_X86
            pc = prev_pc;
#endif
        }
    }
#elif defined LISP_FEATURE_ARM64
    /* The allocation trampolines publish the Lisp CFP before entering C. */
    uword_t frame = (uword_t)access_control_frame_pointer(thread), words[2];
    while (read_frame(thread, NULL, frame, words)) {
        if (!component_ptr_from_pc((void*)words[1])) break;
        STORE_PC(*trace, len, words[1]);
        if (++len == limit || words[0] >= frame) break;
        frame = words[0];
    }
#else
    struct call_info info;
    memset(&info, 0, sizeof info);
    info.frame = (struct call_frame *)access_control_frame_pointer(thread);
    // Only try to step 2 frames because anything more and we may randomly crash.
    // If you don't believe this claim, then just try inserting a lisp_backtrace(20)
    // into the start of lisp_alloc() and watch that about half the time
    // you get a nice backtrace, and half the time you get a crash.
    if (lisp_frame_previous(thread, &info) && info.code) {
        lispobj name;
        name = debug_function_name_from_pc((struct code *)info.code,
                                           (void*)((uword_t)info.code + info.pc));
        if (name) {
            STORE_PC(*trace, 0, (uword_t)info.code + info.pc);
            ++len;
            if (lisp_frame_previous(thread, &info) && info.code) {
                STORE_PC(*trace, 0, (uword_t)info.code + info.pc);
                ++len;
            }
        }
    }
#endif
    return len;
}

#define LOCKED_BY_SELF  1
#define LOCKED_BY_OTHER 2
#define LOCK_CONTENDED  (LOCKED_BY_SELF|LOCKED_BY_OTHER)

static void* initialize_sprof_data(struct thread* thread)
{
    void* buckets = (void*)os_allocate(N_BUCKETS * sizeof (uint32_t));
    if (!buckets) return 0;
    int capacity = 128*1024; // arbitrary starting size for traces, 128K elements = 1MiB
    struct sprof_data *data = (void*)os_allocate(capacity * ELEMENT_SIZE);
    if (!data) { os_deallocate(buckets, N_BUCKETS * sizeof (uint32_t)); return 0; }
    data->capacity = capacity;
    data->free_pointer = INITIAL_FREE_POINTER; // next available element
    data->buckets = buckets;
    thread->sprof_data = (lispobj)data;
    return data;
}

static struct sprof_data* enlarge_buffer(struct sprof_data* current,
                                         uint32_t new_capacity)
{
    char * new_buffer = os_allocate(new_capacity * ELEMENT_SIZE);
    if (!new_buffer) return 0;
    memcpy(new_buffer, current, current->free_pointer * ELEMENT_SIZE);
    os_deallocate((void*)current, current->capacity * ELEMENT_SIZE);
    current = (struct sprof_data*)new_buffer;
    current->capacity = new_capacity;
    return current;
}

#define SPROF_LOCK(th) thread_extra_data(th)->sprof_lock

#ifdef LISP_FEATURE_SB_THREAD
/* If this thread acquired an uncontended lock (old == LOCKED_BY_SELF), release it.
 * If this thread didn't acquire the lock (old == 0 or old == 2), do nothing.
 * The only interesting case is LOCK_CONTENDED */
#define RELEASE_LOCK(th) \
  int oldval = __sync_val_compare_and_swap(&SPROF_LOCK(th), LOCKED_BY_SELF, 0); \
  if (oldval == LOCK_CONTENDED) { \
        oldval = __sync_val_compare_and_swap(&SPROF_LOCK(th), LOCK_CONTENDED, LOCKED_BY_OTHER); \
        gc_assert(oldval == LOCK_CONTENDED); \
        os_sem_post(&thread_extra_data(th)->sprof_sem); \
    }
#else
#define RELEASE_LOCK(th) SPROF_LOCK(th) = 0
#endif

int sb_sprof_trace_ct;
int sb_sprof_trace_ct_max;
#ifdef LISP_FEATURE_WIN32
/* Distinct from sb_sprof_enabled, which keeps code alive until conversion.
 * STOP-PROFILING closes this gate and drains the per-thread writers. */
int sb_sprof_recording;
#endif

static void finish_trace(struct trace* trace, int len)
{
    if (len > MAX_RECORDED_TRACE_LEN) {
        int midpoint = MAX_RECORDED_TRACE_LEN/2;
        int suffix = midpoint-1;
        STORE_PC(*trace, midpoint, (uword_t)-1);
        memmove(&trace->locs[midpoint+1], &trace->locs[len-suffix],
                sizeof trace->locs[0] * suffix);
        len = MAX_RECORDED_TRACE_LEN;
    }
    store_trace_header(trace, compute_hash(trace->locs, len), len);
}

/* Called with GC inhibited, after the target has resumed. */
int sprof_prepare_trace(struct thread* th, os_context_t* context,
                        struct sprof_snapshot* snapshot, struct trace* trace)
{
    int len = gather_trace_from_context(th, context, trace, TRACE_BUFFER_LEN, snapshot);
    if (len < 1) return 0;
    finish_trace(trace, len);
    if (stabilize(trace))
        store_trace_header(trace, compute_hash(trace->locs, trace_len(trace)), trace_len(trace));
    return 1;
}

/* this could get false msan positives because Lisp don't mark stack words as clean
   so anything may appear as unwritten from C depending on whether any C code
   ever marked them. So it was basically down to luck whether this worked or not */
int NO_SANITIZE_MEMORY
sprof_store_trace(struct thread* th, struct trace* trace, int stable)
{
    int len = trace_len(trace);
    // Hash before trying to insert so that potentially the conversion of unstable
    // PCs to stable PCs can be skipped, if there is a hash match.
    uword_t hash = compute_hash(trace->locs, len);
    store_trace_header(trace, hash, len);

    // Try to acquire the lock
    if (__sync_val_compare_and_swap(&SPROF_LOCK(th), 0, LOCKED_BY_SELF)!=0) {
        return -2; // already locked
    }

    int result = 0, reserved = 0;
#ifdef LISP_FEATURE_WIN32
    if (!__sync_val_compare_and_swap(&sb_sprof_recording, 0, 0)) goto done;
#endif
    for (;;) {
        int count = __sync_val_compare_and_swap(&sb_sprof_trace_ct, 0, 0);
        if (count >= sb_sprof_trace_ct_max) { result = -1; goto done; }
        if (__sync_val_compare_and_swap(&sb_sprof_trace_ct, count, count+1) == count) break;
    }
    reserved = 1;
    struct sprof_data* data = (void*)th->sprof_data;
    if (!data) data = initialize_sprof_data(th);
    if (!data) goto done;
    uint32_t* pcount;
    if ((pcount = hash_get(data, trace, hash)) == NULL) {
        if (!stable && stabilize(trace)) { // changed ?
            hash = compute_hash(trace->locs, len); // revise the hash
            store_trace_header(trace, hash, len);
            pcount = hash_get(data, trace, hash);
        }
        if (!pcount) { // still not found, insert it
            uint32_t n_elements = TRACE_PREFIX_ELEMENTS + len;
            uint32_t capacity = data->capacity;
            if (data->free_pointer + n_elements > capacity) {
                // If we're at maximum capacity, bail out
                if (capacity == CAPACITY_MAX) {
                    th->sprof_enable = 0;
                    goto done;
                }
                // Before enlarging the buffer, check whether anyone is trying
                // to read it; if so, just bail out.
                // This is not to avoid a race - that's taken care of by the
                // cmpxchg - but it's preferable to drop the current sample
                // versus make a bunch more system call while there is a waiter.
                if (SPROF_LOCK(th) & LOCKED_BY_OTHER) goto done;
                data = enlarge_buffer(data, 2*capacity);
                if (!data) goto done;
                th->sprof_data = (lispobj)data;
            }
            pcount = hash_insert(data, trace, hash);
        }
    }
    ++*pcount;
    result = 1;
done:
    if (reserved && result != 1) __sync_fetch_and_sub(&sb_sprof_trace_ct, 1);
    RELEASE_LOCK(th);
    return result;
}

static int NO_SANITIZE_MEMORY
collect_backtrace(struct thread* th, int contextp, void* context_or_fp)
{
    if (sb_sprof_trace_ct >= sb_sprof_trace_ct_max) return -1;
    struct trace trace;
    int len = contextp
        ? gather_trace_from_context(th, context_or_fp, &trace, TRACE_BUFFER_LEN, NULL)
        : gather_trace_from_frame(th, context_or_fp, &trace, TRACE_BUFFER_LEN);
    if (len < 1) return 0;
    finish_trace(&trace, len);
    return sprof_store_trace(th, &trace, 0);
}

static void diagnose_failure(struct thread* thread) {
#ifndef LISP_FEATURE_WIN32
    // MAX-SAMPLES bounds the memory growth within a constant factor for one thread,
    // but if multithreaded, each thread could allocate a buffer and grow it an
    // arbitrary number of times. The automatic disable tries to avoid an explosion
    // in memory consumption.
    struct sprof_data* data = (void*)thread->sprof_data;
    if (data && data->capacity == CAPACITY_MAX) {
        // disable the profiler in this thread
        thread->sprof_enable = 0;
#ifdef LISP_FEATURE_SB_THREAD
        char msg[100];
        int msglen = sprintf(msg,
                             "WARNING: thread %p disabled sprof sampler to limit memory use\n",
                             (void*)thread->os_thread);
        ignore_value(write(2, msg, msglen));
#endif
    }
#else
    /* On Windows the buffer can be detached as soon as the writer releases
     * its lock. Do not inspect it here; the writer disables a full buffer. */
    (void)thread;
#endif
}

void record_backtrace_from_context(void *context, struct thread* thread) {
    int success = collect_backtrace(thread, 1, context) == 1;
    if (!success) diagnose_failure(thread);
}

/* The SIGPROF handler. This used to be deferrable via the can_handle_now_test()
 * check in interrupt.c, which would return false during GC, because Lisp binds
 * *INTERRUPTS-ENABLED* to NIL in the thread which performs the GC; and all other
 * threads are in their stop_for_gc handler which blocks async signals including
 * SIGPROF, as per the sa_mask in the sigaction() call that assigned the handler.
 * But now that SIGPROF is never deferred, we have to be careful around GC.
 * There's a complicated solution and an easy solution. The complicated is to have
 * component_ptr_from_pc() fail safely if called, so that taking a sample is fine
 * provided that all PC locations are pseudo-static - in that case we do not use
 * component_ptr_from_pc() in the signal handler.
 * The easy out is just to drop the sample; so that's what we do, and versus
 * blocking/unblocking SIGPROF in collect_garbage(), it avoids 2 system calls. */
#ifndef LISP_FEATURE_WIN32
void sigprof_handler(int sig, __attribute__((unused)) siginfo_t* info,
                     void *context)
{
    if (gc_active_p) return; // no mem barrier needed to read this
    int _saved_errno = errno;
    struct thread* thread = get_sb_vm_thread();
    // We can only profile Lisp threads.
    if (thread) {
        if (thread->sprof_enable)
            record_backtrace_from_context(context, thread);
        else
            // Block further signals it on return from the handler.
            // This won't actually work if there are nested handlers on the stack,
            // but that's OK, we'll just try again to block it if it occurs.
            sigaddset(os_context_sigmask_addr(context), sig);
    }
    errno = _saved_errno;
}
#endif

#if !(defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 || defined LISP_FEATURE_SPARC)
void allocator_record_backtrace(void* frame_ptr, struct thread* thread)
{
    int success = collect_backtrace(thread, 0, frame_ptr) == 1;
    if (!success) diagnose_failure(thread);
}
#endif

/// Ensuring mutual exclusivity with the SIGPROF handler,
/// return the profiling data for 'thread', or 0 if none.
static void acquire_sprof_lock(struct thread* thread)
{
    int old = __sync_fetch_and_or(&SPROF_LOCK(thread), LOCKED_BY_OTHER);
#ifdef LISP_FEATURE_SB_THREAD
    if (old != 0) {
        gc_assert(old == LOCKED_BY_SELF); // could not be LOCKED_BY_OTHER
        // A profiled thread will sem_post() on return from its profiling signal handler
        // if it observes that both lock bits were on.
        // This should be called with the thread's TLS lock held so that the thread
        // whose data are being acquired can't exit and delete its sprof_sem.
        os_sem_wait(&thread_extra_data(thread)->sprof_sem);
        gc_assert(SPROF_LOCK(thread) == LOCKED_BY_OTHER);
    }
#else
    gc_assert(old == 0);
#endif
}

#ifdef LISP_FEATURE_WIN32
void sprof_synchronize(struct thread* thread)
{
    acquire_sprof_lock(thread);
    __sync_fetch_and_and(&SPROF_LOCK(thread), 0);
}
#endif

uword_t acquire_sprof_data(struct thread* thread)
{
    acquire_sprof_lock(thread);
    // sync cas prevents reading before setting the lock
    uword_t retval = __sync_val_compare_and_swap(&thread->sprof_data, 0, 0);
    // if data were allocated, then set the field to 0
    if (retval) {
        __sync_val_compare_and_swap(&thread->sprof_data, retval, 0);
#ifdef MEMORY_SANITIZER
        // Traces were recorded with the sanitizer disabled, so we either need to
        // read the memory from lisp in SAFETY 0 which disables UNINITIALIZED-LOAD-TRAP,
        // or simply mark the memory as clean.
        int freeptr = ((struct sprof_data*)retval)->free_pointer;
        __msan_unpoison((void*)retval, freeptr * ELEMENT_SIZE);
#endif
    }
    __sync_fetch_and_and(&SPROF_LOCK(thread), 0);
    // This this thread owns that thread's data. ('This' and 'that' could be the same)
    return retval;
}
