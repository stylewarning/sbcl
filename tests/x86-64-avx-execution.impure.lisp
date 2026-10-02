;;;; AVX-512 execution tests.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

#-(and x86-64 sb-simd-pack-512) (invoke-restart 'run-tests::skip-file)
(when (zerop (sb-alien:extern-alien "avx512_supported" int))
  (invoke-restart 'run-tests::skip-file))

(in-package "SB-VM")

(sb-c:defknown %avx-encoding-execution (system-area-pointer system-area-pointer fixnum)
    (values) ())

(define-vop (%avx-encoding-execution)
  (:translate %avx-encoding-execution)
  (:policy :fast-safe)
  (:args (input :scs (sap-reg)) (output :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer (:constant t))
  (:info operation)
  (:temporary (:sc double-avx512-reg :offset 17) a)
  (:temporary (:sc double-avx512-reg :offset 26) b)
  (:temporary (:sc double-avx512-reg :offset 31) result)
  (:generator 1
    (inst vmovupd a (ea input))
    (inst vmovupd b (ea 64 input))
    (ecase operation
      (0 (inst vpermilpd result a #x55))
      (1 (inst vmovdqu64 result (ea 128 input))
         (inst vpermilpd result a result))
      (2 (inst vshufpd result a b #x55))
      (3 (inst vmovddup result a))
      (4 (inst vpunpcklqdq result a b))
      (5 (inst vpunpckhqdq result a b))
      (6 (inst vcvtpd2ps (sb-x86-64-asm::get-fpr :ymm 26) a)
         (inst vcvtps2pd result (sb-x86-64-asm::get-fpr :ymm 26))))
    (inst vmovupd (ea output) result)))

(macrolet ((def (name sc type)
             `(define-vop (,name)
                (:args (value :scs (,sc)) (sap :scs (sap-reg)))
                (:arg-types ,type system-area-pointer)
                (:generator 1
                  (inst vmovdqu64 (ea sap) value)))))
  (def avx-store-int-constant int-avx512-reg simd-pack-512-ub64)
  (def avx-store-single-constant single-avx512-reg simd-pack-512-single)
  (def avx-store-double-constant double-avx512-reg simd-pack-512-double))

(in-package "CL-USER")

(with-test (:name :avx512-permute-execution)
  (let ((input (make-array 24 :element-type 'double-float
                          :initial-contents (loop for i from 1 to 24 collect (float i 1d0))))
        (output (make-array 8 :element-type 'double-float :initial-element 0d0)))
    (sb-sys:with-pinned-objects (input output)
      ;; VPERMILPD's variable control selects using bit 1 of each qword.
      (dotimes (i 8)
        (setf (sb-sys:sap-ref-64 (sb-sys:vector-sap input) (+ 128 (* i 8)))
              (if (evenp i) 2 0)))
      (loop for expected in '((2 1 4 3 6 5 8 7)
                              (2 1 4 3 6 5 8 7)
                              (2 9 4 11 6 13 8 15)
                              (1 1 3 3 5 5 7 7)
                              (1 9 3 11 5 13 7 15)
                              (2 10 4 12 6 14 8 16)
                              (1 2 3 4 5 6 7 8))
            for op from 0
            for fun = (compile nil `(lambda (src dst)
                                      (declare (type sb-sys:system-area-pointer src dst))
                                      (sb-vm::%avx-encoding-execution src dst ,op)))
            do (fill output 0d0)
               (funcall fun (sb-sys:vector-sap input) (sb-sys:vector-sap output))
               (assert (every #'= expected output))))))


(with-test (:name :avx512-all-ones-constant)
  (let* ((ones (ldb (byte 64 0) -1))
         (packs (list (apply #'sb-ext:%make-simd-pack-512-ub64
                             (make-list 8 :initial-element ones))
                      (apply #'sb-ext:%make-simd-pack-512-single
                             (make-list 16 :initial-element (sb-kernel:make-single-float -1)))
                      (apply #'sb-ext:%make-simd-pack-512-double
                             (make-list 8 :initial-element
                                        (sb-kernel:make-double-float -1 #xffffffff)))))
         (output (make-array 8 :element-type '(unsigned-byte 64))))
    (loop for pack in packs
          for vop in '(sb-vm::avx-store-int-constant
                       sb-vm::avx-store-single-constant
                       sb-vm::avx-store-double-constant)
          for fun = (compile nil `(lambda (sap)
                                    (declare (type sb-sys:system-area-pointer sap))
                                    (sb-sys:%primitive ,vop ,pack sap)
                                    (values)))
          do (fill output 0)
             (sb-sys:with-pinned-objects (output)
               (funcall fun (sb-sys:vector-sap output)))
             (assert (every (lambda (x) (= x ones)) output)))))
