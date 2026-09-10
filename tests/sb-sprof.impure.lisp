
;;; This exists outside of the unit test in sb-sprof so that you can execute
;;; it with parallel-exec specifying an arbitrarily huge --runs_per_test.
;;; It is uncharacteristically verbose in its output for my liking,
;;; but I need to try to see it behaving badly (if it does),
;;; and there's really no other way than to watch for bad output.

#+sparc (invoke-restart 'run-tests::skip-file)

(require :sb-sprof)

;;; silly examples

(defun test-0 (n &optional (depth 0))
  (declare (optimize (debug 3)))
  (when (< depth n)
    (dotimes (i n)
      (test-0 n (1+ depth))
      (test-0 n (1+ depth))))
  (values 'a 'b 'c))

(defun test ()
  (sb-sprof:with-profiling (:reset t :max-samples 1000 :report :graph)
    (test-0 6)))
(compile 'test-0)
(compile 'test)
(with-test (:name :with-profiling-return-value)
  (let ((answer
         ;; don't want to actually see the report
         (let ((*standard-output* (make-broadcast-stream)))
           (multiple-value-list (test)))))
    (assert (equal answer '(a b c)))  ))

(defun consalot ()
  (let ((junk '()))
    (loop repeat 10000 do
         (push (make-array 10) junk))
    junk))
(compile 'consalot)
(defun consing-test ()
  ;; This used to test that rapid consing didn't improperly interrupt pseudo-atomic.
  ;; But now that the profiling signal isn't deferrable, I don't really think
  ;; this tests anything.
  (sb-sprof:with-profiling (:reset t
                          ;; setitimer with small intervals
                          ;; is broken on FreeBSD 10.0
                          ;; And ARM targets are not fast in
                          ;; general, causing the profiling signal
                          ;; to be constantly delivered without
                          ;; making any progress.
                          #-(or freebsd arm) :sample-interval
                          #-(or freebsd arm) 0.0001
                          #+arm :sample-interval #+arm 0.1
                          :report :graph)
    (loop with target = (+ (get-universal-time) 2)
          while (< (get-universal-time) target)
          do (consalot))))
(compile 'consing-test)
(with-test (:name :sprof-consing-test)
  ;; again don't want to actually see the report
  (let ((*standard-output* (make-broadcast-stream)))
    (consing-test))
  ;; For debugging purposes, print output for visual inspection to see where
  ;; the allocation sequence gets hit.
  ;; It can be interrupted even inside pseudo-atomic now.
  (disassemble #'consalot :stream *error-output*))

(load "../contrib/sb-sprof/test.lisp")

(with-test (:name :sprof)
  (with-scratch-file (f "fasl")
    (setq sb-sprof-test::*compiler-input* "../contrib/sb-sprof/graph.lisp"
          sb-sprof-test::*compiler-output* f
          ;; It was supposed to be 100 before I decreased it.
          ;; surely more samples is better, right?
          sb-sprof-test::*sprof-loop-test-max-samples* 100)
    (sb-sprof-test:run-tests)))

;;; Windows sampling and lifetime regressions.
#+win32
(progn
  (sb-alien:define-alien-routine ("GetThreadPriority" sprof-thread-priority) sb-alien:int
    (handle sb-alien:unsigned))
  (sb-alien:define-alien-routine ("GetProcessHandleCount" sprof-handle-count) sb-alien:int
    (handle sb-alien:unsigned) (count (* (sb-alien:unsigned 32))))
  (defun sprof-handles ()
    (sb-alien:with-alien ((count (sb-alien:unsigned 32)))
      (assert (plusp (sprof-handle-count sb-ext:most-positive-word (sb-alien:addr count))))
      count))

  (declaim (notinline sprof-work sprof-busy sprof-allocate sprof-wait))
  (defun sprof-work (n)
    (declare (fixnum n) (optimize (speed 3) (safety 0) (debug 2)))
    (loop with x of-type fixnum = 17
          for i of-type fixnum below n
          do (setf x (logand most-positive-fixnum (+ x (logxor x i))))
          finally (return x)))

  (defun sprof-busy (seconds)
    (loop with end = (+ (get-internal-real-time)
                       (round (* seconds internal-time-units-per-second)))
          while (< (get-internal-real-time) end)
          do (sprof-work 1000000)))

  (defun sprof-wait (seconds) (sleep seconds) (values))
  (defun sprof-allocate () (loop repeat 10000 collect (make-array 100)))

  (declaim (notinline sprof-deep))
  (defun sprof-deep (depth mode)
    (if (zerop depth)
        (progn (if (eq mode :alloc) (sprof-allocate) (sprof-busy .3)) 0)
        (1+ (sprof-deep (1- depth) mode))))

  (defun sprof-result ()
    (let ((graph (sb-sprof:report :type nil)) (count 0))
      (sb-sprof::map-traces (lambda (thread trace)
                             (declare (ignore thread trace)) (incf count))
                           sb-sprof::*samples*)
      (assert (= count (sb-sprof::call-graph-nsamples graph)
                      sb-sprof::trace-count))
      (assert (null sb-sprof::*windows-profiler*))
      graph))

  (defun sprof-node (graph name)
    (find name (sb-sprof::graph-vertices graph) :key #'sb-sprof::node-name :test #'equal))

  (with-test (:name :windows-sprof-all-modes)
    (dolist (mode '(:cpu :time :alloc))
      (sb-sprof:reset)
      (unwind-protect
           (progn
             (sb-sprof:start-profiling :mode mode :max-samples 40
                                       :sample-interval .002
                                       :threads (list sb-thread:*current-thread*))
             (if (eq mode :alloc) (sprof-allocate) (sprof-busy .8))
             (sb-sprof:stop-profiling)
             (let* ((count sb-sprof::trace-count) (graph (sprof-result)))
               (assert (< 0 count 41))
               (assert (sprof-node graph (if (eq mode :alloc) 'sprof-allocate 'sprof-busy)))
               (sb-sprof::map-all-pc-locs
                (lambda (info offset)
                  (assert (integerp (sb-sprof:sample-pc info offset)))))
               (sprof-allocate)
               (sleep .03)
               (assert (= count sb-sprof::trace-count))
               (dolist (type '(:flat :graph))
                 (assert (search "samples"
                                 (with-output-to-string (stream)
                                   (sb-sprof:report :call-graph graph :type type :stream stream)))))
               (assert (search " samples"
                               (with-output-to-string (stream)
                                 (disassemble (if (eq mode :alloc)
                                                  #'sprof-allocate #'sprof-work)
                                              :stream stream))))))
        (sb-sprof:reset))))

  (with-test (:name :windows-sprof-without-interrupts)
    (dolist (mode '(:cpu :time))
      (sb-sprof:reset)
      (unwind-protect
           (progn
             (sb-sprof:start-profiling :mode mode :sample-interval .002
                                       :threads (list sb-thread:*current-thread*))
             (sb-sys:without-interrupts (sprof-busy .4))
             (sb-sprof:stop-profiling)
             (assert (sprof-node (sprof-result) 'sprof-work)))
        (sb-sprof:reset))))

  (with-test (:name :windows-sprof-deep-traces)
    (dolist (mode '(:cpu :time :alloc))
      (unwind-protect
           (progn
             (sb-sprof:start-profiling :mode mode :max-samples 20 :sample-interval .002
                                       :threads (list sb-thread:*current-thread*))
             (sprof-deep 400 mode)
             (sb-sprof:stop-profiling)
             (sb-ext:gc :full t)
             (let ((graph (sprof-result)))
               (assert (sprof-node graph 'sprof-deep))
               (assert (sprof-node graph 'sb-sprof::unavailable-frames))))
        (sb-sprof:reset))))

  (with-test (:name :windows-sprof-empty-and-invalid-inputs)
    (dolist (interval '(0 -1))
      (assert (handler-case (progn (sb-sprof:start-profiling :sample-interval interval) nil)
                (type-error () t))))
    (dolist (mode '(:cpu :time :alloc))
      (unwind-protect
           (progn
             (sb-sprof:start-profiling :mode mode :threads nil)
             (sprof-allocate)
             (sprof-busy .04)
             (sb-sprof:stop-profiling)
             (assert (zerop (sb-sprof::call-graph-nsamples (sprof-result)))))
        (sb-sprof:reset))))

  (with-test (:name :windows-sprof-cpu-versus-time)
    (let ((counts nil))
      (dolist (mode '(:cpu :time))
        (sb-sprof:reset)
        (unwind-protect
             (progn
               (sb-sprof:start-profiling :mode mode :sample-interval .005
                                         :threads (list sb-thread:*current-thread*))
               (sprof-wait .4)
               (sb-sprof:stop-profiling)
               (push (sb-sprof::call-graph-nsamples (sprof-result)) counts))
          (sb-sprof:reset)))
      (destructuring-bind (wall cpu) counts
        (assert (> wall 20))
        (assert (< cpu (max 2 (/ wall 10)))))))

  (with-test (:name :windows-sprof-sampling-controls)
    (dolist (mode '(:cpu :time :alloc))
      (sb-sprof:reset)
      (unwind-protect
           (progn
             (sb-sprof:start-profiling :mode mode :sample-interval .002
                                       :threads (list sb-thread:*current-thread*))
             (sb-sprof:with-sampling (nil)
               (let ((before sb-sprof::trace-count))
                 (if (eq mode :alloc) (sprof-allocate) (sprof-busy .1))
                 (assert (= before sb-sprof::trace-count))))
             (if (eq mode :alloc) (sprof-allocate) (sprof-busy .2))
             (sb-sprof:stop-profiling)
             (assert (plusp (sb-sprof::call-graph-nsamples (sprof-result)))))
        (sb-sprof:reset))))

  (with-test (:name :windows-sprof-new-and-exited-threads)
    (dolist (mode '(:cpu :time :alloc))
      (sb-sprof:reset)
      (unwind-protect
           (progn
             (sb-sprof:start-profiling :mode mode :sample-interval .002 :threads :all)
             (let ((thread (sb-thread:make-thread
                            (lambda ()
                              (if (eq mode :alloc) (sprof-allocate) (sprof-busy .25)))
                            :name "profiled worker")))
               (sb-thread:join-thread thread)
               (sb-sprof:stop-profiling)
               (assert (assoc thread (sb-sprof::call-graph-sampled-threads (sprof-result))))))
        (sb-sprof:reset))))

  (with-test (:name :windows-sprof-explicit-thread-selection)
    (let* ((start (sb-thread:make-semaphore))
           (thread (sb-thread:make-thread (lambda ()
                                            (sb-thread:wait-on-semaphore start)
                                            (sprof-busy .25)))))
      (unwind-protect
           (progn
             (sb-sprof:reset)
             (sb-sprof:start-profiling :mode :time :sample-interval .002
                                       :threads (list thread))
             (sb-thread:signal-semaphore start)
             (sprof-busy .3)
             (sb-thread:join-thread thread)
             (sb-sprof:stop-profiling)
             (assert (equal (mapcar #'car (sb-sprof::call-graph-sampled-threads (sprof-result)))
                            (list thread))))
        (sb-thread:signal-semaphore start)
        (sb-thread:join-thread thread)
        (sb-sprof:reset))))

  (with-test (:name :windows-sprof-gc-and-concurrent-stop)
    (dolist (mode '(:cpu :time :alloc))
      (sb-sprof:reset)
      (unwind-protect
           (progn
             (sb-sprof:start-profiling :mode mode :sample-interval .001 :max-samples 100)
             (let ((workers (loop repeat 3 collect
                                  (sb-thread:make-thread
                                   (lambda () (dotimes (i 5) (sprof-allocate) (sb-ext:gc)))))))
               (sleep .03)
               (sb-sprof:stop-profiling)
               (let ((count sb-sprof::trace-count))
                 (mapc #'sb-thread:join-thread workers)
                 (assert (= count sb-sprof::trace-count))
                 (assert (<= count 100))
                 (sprof-result))))
        (sb-sprof:reset))))

  (with-test (:name :windows-sprof-call-counting-and-errors)
    (sb-sprof:profile-call-counts 'sprof-work)
    (unwind-protect
         (progn
           (assert (handler-case
                       (sb-sprof:with-profiling (:mode :time :reset t)
                         (dotimes (i 7) (sprof-work 100))
                         (error "unwind profiling"))
                     (error () t)))
           (assert (null sb-sprof::*profiling*))
           (assert (null sb-sprof::*windows-profiler*))
           (assert (= 7 (car (gethash 'sprof-work sb-sprof::*encapsulations*)))))
      (sb-sprof:unprofile-call-counts)
      (sb-sprof:reset)))

  (with-test (:name :windows-sprof-repeated-start-stop)
    (dotimes (i 20)
      (sb-sprof:start-profiling :mode :time :max-samples (mod i 2) :sample-interval .001)
      (sleep .003)
      (sb-sprof:stop-profiling)
      (sprof-result)
      (sb-sprof:reset))
    (assert (null sb-sprof::*samples*))
    (assert (zerop sb-sprof::trace-count))
    (assert (zerop (sb-alien:extern-alien "sb_sprof_enabled" sb-alien:int)))
    (assert (notany (lambda (thread) (equal "SPROF timer" (sb-thread:thread-name thread)))
                    (sb-thread:list-all-threads))))

  (with-test (:name :windows-sprof-handles-and-priority)
    (sb-ext:gc :full t)
    (let ((handles (sprof-handles))
          (priority (sprof-thread-priority (- sb-ext:most-positive-word 1))))
      (dotimes (i 30)
        (sb-sprof:start-profiling :mode :time :threads (list sb-thread:*current-thread*)
                                  :sample-interval .001)
        (sprof-busy .01)
        (sb-sprof:reset))
      (sb-ext:gc :full t)
      (assert (= priority (sprof-thread-priority (- sb-ext:most-positive-word 1))))
      ;; Allow unrelated runtime handles, but detect leaking a timer/event or
      ;; profiling semaphore on every start/stop cycle.
      (assert (<= (sprof-handles) (+ handles 6)))))

  (with-test (:name :windows-sprof-startup-failure)
    (let ((create (symbol-function 'sb-sprof::win32-sprof-create)))
      (unwind-protect
           (progn
             (setf (symbol-function 'sb-sprof::win32-sprof-create)
                   (lambda (interval cpu)
                     (declare (ignore interval cpu))
                     (sb-sys:int-sap 0)))
             (assert (handler-case (progn (sb-sprof:start-profiling :mode :cpu) nil)
                       (error () t)))
             (assert (null sb-sprof::*profiling*))
             (assert (null sb-sprof::*windows-profiler*))
             (assert (null sb-sprof::*samples*))
             (assert (zerop (sb-alien:extern-alien "sb_sprof_enabled" sb-alien:int))))
        (setf (symbol-function 'sb-sprof::win32-sprof-create) create)
        (sb-sprof:reset))))

  ;; Reuse the foreign-thread fixture to exercise attachment, sampling, and
  ;; retirement of a fresh Lisp thread object on each callback.
  (compile-so "fcb-threads.c" "sprof-fcb-threads.so")
  (sb-alien:define-alien-callable sprof-callback sb-alien:int ()
    (sprof-allocate)
    (sprof-busy .05)
    0)

  (with-test (:name :windows-sprof-foreign-callbacks)
    (dolist (mode '(:cpu :time :alloc))
      (sb-sprof:reset)
      (unwind-protect
           (progn
             (sb-sprof:start-profiling :mode mode :sample-interval .002 :threads :all)
             (sb-alien:with-alien ((call-foreign-thread
                                   (function sb-alien:int sb-alien:system-area-pointer sb-alien:int)
                                   :extern "minimal_perftest"))
               (sb-alien:alien-funcall call-foreign-thread
                                      (sb-alien:alien-sap
                                       (sb-alien:alien-callable-function 'sprof-callback))
                                      5))
             (sb-sprof:stop-profiling)
             (let ((graph (sprof-result)))
               (assert (some (lambda (entry) (typep (car entry) 'sb-thread:foreign-thread))
                             (sb-sprof::call-graph-sampled-threads graph)))
               (assert (sprof-node graph (if (eq mode :alloc) 'sprof-allocate 'sprof-busy)))))
        (sb-sprof:reset)))))
