. ./subr.sh

run_sbcl <<EOF
#+x86-64
(when (member :immobile-code sb-impl:+internal-features+) (exit :code 0))
(exit :code 2)
EOF
status=$?
if [ "$status" = 2 ]; then
    exit $EXIT_TEST_WIN
fi
check_status_maybe_lose "immobile-code feature check" "$status" 0 "supported"

use_test_subdirectory
tmpcore=$TEST_FILESTEM.core

# Unreachable bytes simulate a user VOP unknown to the disassembler.
# Such code must survive saving without speculative relocation fixups.
run_sbcl <<EOF
(setf (sb-alien:extern-alien "immobile_space_defrag_p" sb-alien:int) 0)
(sb-vm::collect-immobile-code-relocs)
(assert (= (sb-alien:extern-alien "immobile_space_defrag_p" sb-alien:int) 1))
(in-package "SB-VM")
(define-vop (undecodable-code)
  (:generator 1
    (let ((end (gen-label)))
      (inst jmp end)
      (dolist (byte '(#x62 0 0 0 0 0)) (inst byte byte))
      (emit-label end))))
(in-package "CL-USER")
(let ((sb-c::*compile-to-memory-space* :immobile))
  (compile 'saved-function
           '(lambda (x)
              (sb-sys:%primitive sb-vm::undecodable-code)
              (identity x))))
(assert (= (saved-function 42) 42))
(sb-vm::collect-immobile-code-relocs)
(assert (zerop (sb-alien:extern-alien "immobile_space_defrag_p" sb-alien:int)))
(save-lisp-and-die "$tmpcore")
EOF
check_status_maybe_lose "save undecodable code" $? 0 "saved"

run_sbcl_with_core "$tmpcore" <<EOF
(assert (= (saved-function 42) 42))
(exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "reload undecodable code" $?

exit $EXIT_TEST_WIN
