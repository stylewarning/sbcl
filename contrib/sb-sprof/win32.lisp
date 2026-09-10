;;;; Native Windows sampling backend.
;;;;
;;;; CPU and wallclock profiling share a Lisp sampler thread and a native
;;;; waitable timer in src/runtime/win32-sprof.c. Allocation profiling uses
;;;; the existing allocation-region overflow hooks. All three modes use the
;;;; recorder in src/runtime/sprof.c, including its trace representation,
;;;; deduplication, sample limits, and transfer to the Lisp reporting code.
;;;; Consequently call graphs and disassembly annotations need no separate
;;;; Windows implementation.
;;;;
;;;; CPU sampling is driven by each thread's accumulated user and kernel
;;;; time, with an interval no smaller than the Windows accounting tick.
;;;; The sampler polls accounting with jitter and gives each thread an initial
;;;; random phase. Scheduler state is queried before suspension: waiting
;;;; threads retain their pending CPU deadline until they become runnable.
;;;; Missed deadlines are counted, never replaced by copies of a later stack.
;;;; Wallclock sampling visits the selected threads regardless of their state.
;;;;
;;;; Scheduler queries use NtQueryInformationThread's
;;;; ThreadSystemThreadInformation class when a startup probe succeeds, or
;;;; NtQuerySystemInformation with a buffer reserved at startup. The high
;;;; resolution waitable timer also has a fallback to an ordinary timer.
;;;; These APIs are resolved dynamically; no service or elevation is needed.
;;;;
;;;; For each target, the sampler first tries its THREAD-STORAGE-LOCK to
;;;; protect the native thread's lifetime. It then inhibits GC, suspends the
;;;; target, obtains its registers, and copies at most 256 KiB of stack.
;;;; While the target is suspended, capture must not allocate, call Lisp, or
;;;; acquire user-space locks: the target might hold those locks. Every
;;;; successful suspension is balanced, including an existing suspension
;;;; which causes capture to be skipped. A temporary priority boost helps
;;;; GetThreadContext under contention and is undone before resuming the
;;;; target. The sampler itself runs at above-normal priority.
;;;;
;;;; Only after resumption does the sampler unwind the copied stack and
;;;; stabilize Lisp PCs as code serial numbers and offsets, still with GC
;;;; inhibited. Trace storage follows outside WITHOUT-GCING. On x86-64,
;;;; native unwind information leads back to Lisp frame chains, including
;;;; the SEH allocation trampoline. DbgHelp calls use a nonblocking lock.
;;;; x86 allocation traces account for registers spilled by the allocation
;;;; trampoline before it establishes EBP. ARM64 uses the Lisp frame chain
;;;; and the published stack state when a foreign call is active.
;;;;
;;;; A recursive control mutex serializes start, stop, reset, and report.
;;;; The worker never acquires that mutex and only tries target TLS locks,
;;;; so shutdown can join it while holding the control mutex. Shutdown also
;;;; closes the recording gate and drains writers before freezing counts.
;;;; Exiting threads publish detached buffers under their TLS lock. The save
;;;; hook stops profiling and releases native state and buffers; a restored
;;;; core starts with the profiler idle.
;;;;
;;;; Accounting and capture are separate observations. Short CPU bursts and
;;;; scheduling transitions can bias attribution, and CPU contention can
;;;; cause missed deadlines. Longer runs and larger intervals help; the
;;;; report exposes capture failures, missed samples, and snapshot limits.
;;;; Bounded snapshots and missing native unwind information can truncate
;;;; traces; this is not a complete PDB-based foreign stack unwinder.

(in-package #:sb-sprof)

(define-alien-routine "win32_sprof_create" system-area-pointer
  (interval (unsigned 64)) (cpu int))
(define-alien-routine "win32_sprof_interval" double-float
  (requested double-float) (cpu int))
(define-alien-routine "win32_sprof_destroy" void (state system-area-pointer))
(define-alien-routine "win32_sprof_wait" int (state system-area-pointer))
(define-alien-routine "win32_sprof_stop" void (state system-area-pointer))
(define-alien-routine "win32_sprof_capture" int
  (state system-area-pointer) (thread unsigned))
(define-alien-routine "win32_sprof_record" void
  (state system-area-pointer) (thread unsigned))
(define-alien-routine "win32_sprof_counter" (unsigned 64)
  (state system-area-pointer) (index int))
(define-alien-variable ("sb_sprof_recording" windows-recording) int)
(define-alien-routine "sprof_synchronize" void (thread unsigned))

(defstruct windows-profiler
  state thread failure)
(defvar *windows-profiler* nil)

(defun windows-drain-profiler ()
  (setf windows-recording 0)
  (sb-thread::avltree-filter
   (lambda (node)
     (let ((thread (sb-thread::avlnode-data node)))
       (sb-thread:with-tls-lock (thread c-thread)
         (unless (zerop c-thread) (sprof-synchronize c-thread))))
     nil)
   sb-thread::*all-threads*))

(defun windows-sample-thread (state thread)
  ;; Acquire before WITHOUT-GCING. A contended target may be skipped, but may
  ;; never make the sampler wait for a thread which is stopped for collection.
  (sb-thread:with-mutex ((sb-thread::thread-storage-lock thread) :wait-p nil)
    (let ((c-thread (sb-thread::thread-primitive-thread thread)))
      (unless (zerop c-thread)
        (let ((captured (sb-sys:without-gcing
                          (win32-sprof-capture state c-thread))))
          (cond ((plusp captured) (win32-sprof-record state c-thread))
                ((= captured -1)
                 (error "SPROF could not resume sampled thread ~S" thread))
                ((= captured -2)
                 (error "SPROF could not query Windows CPU accounting or scheduler state"))
                ((= captured -3)
                 (error "SPROF could not restore sampled thread priority for ~S" thread))))))))

(defun windows-profiler-loop (profiler threads)
  (handler-case
      (loop with state = (windows-profiler-state profiler)
            for tick = (win32-sprof-wait state)
            until (or (zerop tick) (>= trace-count trace-limit))
            do (when (minusp tick) (error "SPROF timer wait failed"))
               (let ((targets (if (eq threads :all)
                                  (sb-thread:list-all-threads)
                                  threads)))
                 ;; Alternate traversal direction to spread scan latency.
                 (when (oddp (get-internal-real-time))
                   (setf targets (reverse targets)))
                 (dolist (thread targets)
                   (when (>= trace-count trace-limit) (return))
                   (unless (eq thread sb-thread:*current-thread*)
                     (windows-sample-thread state thread)))))
    (error (condition)
      (setf (windows-profiler-failure profiler) condition))))

(defun windows-start-profiler (mode interval threads)
  (let* ((ticks (ceiling (* interval 10000000)))
         (state (progn
                  (unless (< 0 ticks (ash 1 63))
                    (error "Profiling interval is outside the Windows timer range"))
                  (win32-sprof-create ticks (if (eq mode :cpu) 1 0)))))
    (when (zerop (sap-int state)) (error "Could not create Windows profiler"))
    (let ((profiler (make-windows-profiler :state state))
          (started nil))
      (unwind-protect
           (progn
             (setf (windows-profiler-thread profiler)
                   (sb-thread:make-thread
                    (lambda () (windows-profiler-loop profiler threads))
                    :name "SPROF timer"))
             (setf *windows-profiler* profiler started t))
        (unless started (win32-sprof-destroy state))))))

(defun windows-stop-profiler ()
  (let ((profiler *windows-profiler*))
    (when profiler
      (let ((state (windows-profiler-state profiler)))
        (sb-sys:without-interrupts
          (win32-sprof-stop state)
          ;; The worker does not acquire *PROFILER-LOCK* and only tries TLS locks.
          (sb-thread:join-thread (windows-profiler-thread profiler))
          (setf (samples-diagnostics *samples*)
                (loop for name in '(:attempts :capture-failures :stack-snapshot-limits
                                   :missed-ticks :store-failures :idle-observations
                                   :missed-cpu-samples :unwind-failures)
                      for index from 0
                      append (list name (win32-sprof-counter state index))))
          (win32-sprof-destroy state)
          (setf *windows-profiler* nil))
        (when (windows-profiler-failure profiler)
          (warn "Windows profiler stopped: ~A" (windows-profiler-failure profiler)))))))
