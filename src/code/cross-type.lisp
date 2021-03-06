;;;; cross-compiler-only versions of TYPEP, TYPE-OF, and related functions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(define-condition cross-type-warning (warning)
  ((call :initarg :call :reader cross-type-warning-call)
   (message :reader cross-type-warning-message
            #+host-quirks-cmu :initarg #+host-quirks-cmu :message ; (to stop bogus non-STYLE WARNING)
            ))
  (:report (lambda (c s)
             (format
              s
              "cross-compiler type ambiguity in ~S:~%~A"
              (cross-type-warning-call c)
              (cross-type-warning-message c)))))

;;; This warning is signaled when giving up on a type calculation
;;; during cross-compilation.
(define-condition cross-type-giving-up (cross-type-warning)
  ((message :initform "giving up conservatively"
            #+host-quirks-cmu :reader #+host-quirks-cmu #.(gensym) ; (to stop bogus non-STYLE WARNING)
            )))

;;; This warning refers to the flexibility in the ANSI spec with
;;; regard to run-time distinctions between floating point types.
;;; (E.g. the cross-compilation host might not even distinguish
;;; between SINGLE-FLOAT and DOUBLE-FLOAT, so a DOUBLE-FLOAT number
;;; would test positive as SINGLE-FLOAT.) If the target SBCL does make
;;; this distinction, then information is lost. It's not too hard to
;;; contrive situations where this would be a problem. In practice we
;;; don't tend to run into them because all widely used Common Lisp
;;; environments do recognize the distinction between SINGLE-FLOAT and
;;; DOUBLE-FLOAT, and we don't really need the other distinctions
;;; (e.g. between SHORT-FLOAT and SINGLE-FLOAT), so we call
;;; WARN-POSSIBLE-CROSS-TYPE-FLOAT-INFO-LOSS to test at runtime
;;; whether we need to worry about this at all, and not warn unless we
;;; do. If we *do* have to worry about this at runtime, my (WHN
;;; 19990808) guess is that the system will break in multiple places,
;;; so this is a real WARNING, not just a STYLE-WARNING.
;;;
;;; KLUDGE: If we ever try to support LONG-FLOAT or SHORT-FLOAT, this
;;; situation will get a lot more complicated.
(defun warn-possible-cross-type-float-info-loss (caller host-object ctype)
  (when (or (subtypep 'single-float 'double-float)
            (subtypep 'double-float 'single-float))
    (warn "possible floating point information loss in ~S"
          (list caller host-object ctype))))

;; Return T if SYMBOL is a predicate acceptable for use in a SATISFIES type
;; specifier. We assume that anything in CL: is allowed.
(defvar *seen-xtypep-preds* nil)
(defun acceptable-cross-typep-pred (symbol usage)
  #+nil
  (cond ((not (fboundp symbol))
         (format t "~&XTYPEP: ~S is not fboundp~%" symbol))
        ((not (member symbol *seen-xtypep-preds*))
         (format t "~&XTYPEP: checking applicability of ~S for ~S~%"
                 symbol usage)
         (push symbol *seen-xtypep-preds*)))
  ;; For USAGE = TYPEP, always call the predicate.
  ;; For USAGE = CTYPEP, call it only if it is a foldable function.
  ;; The major case in which we don't call something for CTYPEP is KEYWORDP
  ;; which we consider not to be foldable.
  (and (eq (sb-xc:symbol-package symbol) *cl-package*)
       (or (eq usage 'sb-xc:typep)
           (awhen (info :function :info symbol)
             (sb-c::ir1-attributep (sb-c::fun-info-attributes it) sb-c:foldable)))))

(defun complex-num-fmt-match-p (num fmt)
  ;; compiled TYPEP on ECL is wrong. See example at bottom of this file.
  ;; Of course, it's abstraction-breaking to suppose that TYPECASE
  ;; directly utilizes TYPEP, but naturally it does.
  #+host-quirks-ecl (declare (notinline typep))
  (aver (memq fmt '(single-float double-float rational)))
  (and (complexp num)
       (let ((yesp (eq (etypecase (realpart num)
                         (single-float 'single-float)
                         (double-float 'double-float)
                         (rational 'rational))
                       fmt)))
         (when yesp
           (aver (typep (imagpart num) fmt)))
         yesp)))

(defun flonum-fmt-match-p (num fmt)
  (aver (memq fmt '(single-float double-float)))
  (and (floatp num)
       (eq (etypecase num
            (single-float 'single-float)
            (double-float 'double-float))
           fmt)))

;;; This is like TYPEP, except that it asks whether HOST-OBJECT would
;;; be of type TYPE when instantiated on the target SBCL. Since this
;;; is hard to determine in some cases, and since in other cases we
;;; just haven't bothered to try, it needs to return two values, just
;;; like SUBTYPEP: the first value for its conservative opinion (never
;;; T unless it's certain) and the second value to tell whether it's
;;; certain.
;;; The logic is a mixture of the code for CTYPEP as defined in src/code/late-type
;;; and %%TYPEP as defined in src/code/typep.
;;; The order of clauses is fairly symmetrical with that of %%TYPEP.
(defun cross-typep (caller host-object type)
  (declare (type (member sb-xc:typep ctypep) caller))
  (named-let recurse ((obj host-object) (type type))
    (flet ((unimplemented ()
             (bug "Incomplete implementation of ~S ~S ~S" caller obj type))
           (uncertain ()
             (values nil nil)))
      (etypecase type
       (named-type
        (ecase (named-type-name type)
          ((t) (values t t)) ; universal supertype
          ((instance) (values (%instancep obj) t))
          ((nil extended-sequence funcallable-instance)
           (values nil t)))) ; nothing could be these
       (numeric-type
        (flet ((bound-test (val)
                 (let ((low (numeric-type-low type))
                       (high (numeric-type-high type)))
                   (and (cond ((null low) t)
                              ((listp low) (> val (car low)))
                              (t (>= val low)))
                        (cond ((null high) t)
                              ((listp high) (< val (car high)))
                              (t (<= val high)))))))
          (if (not (numberp obj))
              (values nil t) ; false certainly
              (let ((fmt (numeric-type-format type)))
                (case (numeric-type-class type)
                 ((nil)
                  (if (and (null (numeric-type-complexp type)) (null fmt))
                      (values (numberp obj) t)
                      (unimplemented)))
                 (float
                  (warn-possible-cross-type-float-info-loss caller host-object type)
                  (values (ecase (numeric-type-complexp type)
                           (:complex (complex-num-fmt-match-p obj fmt))
                           (:real (and (flonum-fmt-match-p obj fmt) (bound-test obj))))
                          t))
                 (t ; integer or rational
                  (values (ecase (numeric-type-complexp type)
                           (:complex (complex-num-fmt-match-p obj 'rational))
                           (:real (and (if (eq (numeric-type-class type) 'integer)
                                           (integerp obj)
                                           (rationalp obj))
                                       (bound-test obj))))
                          t)))))))
       (array-type
        ;; Array types correspond fairly closely between host and target, but
        ;; asking whether an array is definitely non-simple is a nonsensical
        ;; operation for the cross-compiler. The host can disagree with us,
        ;; reporting that all arrays are simple. So ensure we don't ask.
        ;; Otherwise the specialized array registry would need to track
        ;; our idea of whether the array is non-simple.
        (when (and (arrayp obj) (eq (array-type-complexp type) t))
          (bug "Should not call cross-typep with definitely-non-simple array type"))
        ;; This is essentially just the ARRAY-TYPE case of %%TYPEP
        ;; using !SPECIALIZED-ARRAY-ELEMENT-TYPE, not ARRAY-ELEMENT-TYPE,
        ;; and disregarding simple-ness.
        (values (and (arrayp obj)
                     (or (eq (array-type-element-type type) *wild-type*)
                         (type= (specifier-type
                                 (!specialized-array-element-type obj))
                                (array-type-specialized-element-type type)))
                     (or (eq (array-type-dimensions type) '*)
                         (and (= (length (array-type-dimensions type))
                                 (array-rank obj))
                              (every (lambda (required actual)
                                       (or (eq required '*) (eql required actual)))
                                     (array-type-dimensions type)
                                     (array-dimensions obj)))))
                t))
       (member-type
        (values (if (member-type-member-p obj type) t) t))
       (compound-type
        (funcall (etypecase type
                  (intersection-type #'every/type)
                  (union-type #'any/type))
                 #'recurse
                 obj
                 (compound-type-types type)))
       (cons-type
        (if (not (consp obj))
            (values nil t)
            (multiple-value-bind (result certain)
                (recurse (car obj) (cons-type-car-type type))
              (if result
                  (recurse (cdr obj) (cons-type-cdr-type type))
                  (values nil certain)))))
       ((or #+sb-simd-pack simd-pack-type
            #+sb-simd-pack-256 simd-pack-256-type)
        (values nil t))
       (character-set-type
        (cond ((not (characterp obj)) (values nil t)) ; certainly no
              ;; Return yes if the type is:
              ;;  - BASE-CHAR, and SB-XC:CHAR-CODE is within BASE-CHAR-CODE-LIMIT
              ;;  - STANDARD-CHAR and the host says that the char is standard
              ;;  - the full set of characters
              ((or (and (type= type (specifier-type 'base-char))
                        (< (sb-xc:char-code obj) base-char-code-limit))
                   (and (type= type (specifier-type 'standard-char))
                        (cl:standard-char-p obj))
                   (type= type (specifier-type 'character)))
               (values t t)) ; certainly yes
              (t (uncertain))))
       (negation-type
        (multiple-value-bind (res win) (recurse obj (negation-type-type type))
          (if win
              (values (not res) t)
              (uncertain))))
       ;; Test BUILT-IN before falling into the CLASSOID case
       (built-in-classoid
        (ecase (classoid-name type)
          (symbol (values (symbolp obj) t)) ; 1:1 correspondence with host
          (function
           (if (functionp obj)
               (uncertain)
               ;; probably not a function. What about FMT-CONTROL instances?
               (values nil t)))
          ((system-area-pointer stream fdefn weak-pointer file-stream
            code-component lra)
           (values nil t)))) ; nothing could be this type
       (classoid ; = {structure,condition,standard,static}-classoid
        (if (not (%instancep obj))
            (values nil t) ; false certainly
            (let ((name (classoid-name type)))
              (if (and (cl:find-class name nil) ; see if the host knows the type
                       ;; and it's in our object hierarchy
                       (cl:subtypep name 'structure!object))
                  (values (cl:typep obj name) t)
                  (unimplemented)))))
       ;; Test FUN-DESIGNATOR before falling into the FUN-TYPE case
       (fun-designator-type (values (typep obj '(or symbol function)) t))
       (fun-type
        ;; FUNCTION is not a specifier suitable for discrimination,
        ;; thus TYPEP is not allowed to determine whether an object
        ;; is in a particular subtype of function.
        ;; But be lenient when the object is not a function.
        (if (and (functionp obj) (eq caller 'sb-xc:typep))
            (error "TYPEP called with function type")
            (values (functionp obj) t)))
       (alien-type-type (if (null obj) (values nil t) (unimplemented)))
       ;; Test UNKNOWN before falling into the HAIRY case
       (unknown-type
        (let ((spec (unknown-type-specifier type)))
          ;; Apparently we ask whether some objects are CLASSOIDS and other things
          ;; before we've made the respective type known to our type system,
          ;; but after the type is known by the host as one of our defstructs.
          ;; It happens during initialization of the type system, but later too.
          ;; Ideally we'd restrict this case to initialization only.
          ;; The post-initialization occurrences are for computing DD-BITMAP
          ;; of structures, which requires testing whether (TYPEP NIL X) is
          ;; true for various X that are not known yet.
          (cond ((and (symbolp spec)
                      (cl:find-class spec nil)
                      ;; See if the host knows our DEF!STRUCT yet
                      (cl:subtypep spec 'structure!object))
                 (values (cl:typep obj spec) t))
                ;; Special-case a few forward-referenced instance types.
                ;; Some are picked off by the quick exit case in (DEFUN SB-XC:TYPEP)
                ;; but a bunch of other cases arise within the type methods per se
                ;; which tend not to call via SB-XC:TYPEP but rather CTYPEP.
                ;; So you might think to pick off (%CROSS-TYPEP NIL AN-UNKNOWN-TYPE)
                ;; in this function, but then you have a new problem: bootstrap falls down
                ;; an infinite hole because simplifying unions and intersections often involves
                ;; asking whether NIL is a member of a type. The stub code we formerly had
                ;; for SB-XC:TYPEP tried to avoid parsing, and CROSS-TYPEP was almost entirely
                ;; implemented in terms of unparsed specifiers, not parsed specifiers,
                ;; which sort of accidentally worked for things like
                ;; (typep nil '(or (member null ...) blah)). That was a solution, but unprincipled
                ;; and not very testable, which was a problem considering how much it diverged
                ;; from the real implementations of CTYPEP and %%TYPEP.
                ;; So unfortunately we still munst handle a few cases without parsing,
                ;; but fewer than before, and then some after parsing.
                ((and (symbolp obj)
                      (member spec '(;; these occur during make-host-1
                                     hash-table lexenv sb-c::abstract-lexenv
                                     condition restart
                                     sb-assem::label
                                     ;; in addition to the above, these occur in make-host-2
                                     #+sb-fasteval sb-interpreter:interpreted-function
                                     #+sb-eval sb-eval:interpreted-function
                                     synonym-stream
                                     )))
                 (values nil t))
                (t
                 (uncertain)))))
       (hairy-type
        (let ((spec (hairy-type-specifier type)))
          (if (cl:typep spec '(cons (eql satisfies) (cons symbol null)))
              (let ((predicate (cadr spec)))
                (if (acceptable-cross-typep-pred predicate caller)
                    (values (funcall predicate obj) t)
                    (uncertain)))
              (unimplemented))))))))

;;; This is an incomplete TYPEP which runs at cross-compile time to
;;; tell whether OBJECT is the host Lisp representation of a target
;;; SBCL type specified by TARGET-TYPE-SPEC. It need make no pretense
;;; to completeness, since it need only handle the cases which arise
;;; when building SBCL itself, e.g. testing that range limits FOO and
;;; BAR in (INTEGER FOO BAR) are INTEGERs.
(defun sb-xc:typep (host-object target-type-spec &optional (env nil env-p))
  (declare (ignore env))
  (aver (null env-p)) ; 'cause we're too lazy to think about it

  ;; Special case which avoids having to add more more hardcoded types
  ;; to the list of allowed unknown in cross-typep.
  ;; This is often used for deciding whether a structure slot can be raw.
  ;; If NIL is in the type, then it isn't raw.
  ;; The problem this solves is that even so much as parsing a type
  ;; can involve asking whether NIL is a member of the type,
  ;; and thus we need a way to break the endless cycle of not knowing
  ;; which is problem during self-build. The target system will make
  ;; any slot non-raw if its type is unknown, but during bootstrap
  ;; we're trying to be extremeley pedantic by disallowing unknowns.
  (if (and (null host-object)
           (typep target-type-spec '(cons (eql or)))
           (member 'null (cdr target-type-spec)))
      t
      (multiple-value-bind (opinion certain-p)
          (cross-typep 'sb-xc:typep host-object (specifier-type target-type-spec))
    ;; A program that calls TYPEP doesn't want uncertainty and
    ;; probably can't handle it.
        (if certain-p
            opinion
            (error "uncertain in SB-XC:TYPEP ~S ~S" host-object target-type-spec)))))

;;; This is an incomplete, portable implementation for use at
;;; cross-compile time only.
(defun ctypep (obj ctype)
  (declare (type ctype ctype))
  (multiple-value-bind (answer certain) (cross-typep 'ctypep obj ctype)
    (if (or certain
            ;; Allow uncertainty only if the type contains a SATISFIES
            (block contains-satisfies
              (map-type (lambda (type)
                          (when (and (hairy-type-p type)
                                     (typep (hairy-type-specifier type)
                                            '(cons (eql satisfies))))
                            (return-from contains-satisfies t)))
                        ctype)))
        (values answer certain)
        (warn 'cross-type-giving-up :call `(ctypep ,obj ,ctype)))))

(defun ctype-of (x)
  (typecase x
    (function
     (if (typep x 'generic-function)
         ;; Since at cross-compile time we build a CLOS-free bootstrap
         ;; version of SBCL, it's unclear how to explain to it what a
         ;; generic function is.
         (error "not implemented: cross CTYPE-OF generic function")
         ;; There's no ANSI way to find out what the function is
         ;; declared to be, so we just return the CTYPE for the
         ;; most-general function.
         *universal-fun-type*))
    (symbol
     (make-eql-type x))
    (number
     (ctype-of-number x))
    (array
     ;; It is critical not to inquire of the host for the array's element type.
     (let ((etype (specifier-type (!specialized-array-element-type x))))
       (make-array-type (array-dimensions x)
                        ;; complexp relies on the host implementation,
                        ;; but in practice any array for which we need to
                        ;; call ctype-of will be a simple-array.
                        :complexp (not (typep x 'simple-array))
                        :element-type etype
                        :specialized-element-type etype)))
    (cons (specifier-type 'cons))
    (character
     (cond ((typep x 'standard-char)
            (specifier-type 'base-char))
           ((not (characterp x))
            nil)
           (t
            ;; Beyond this, there seems to be no portable correspondence.
            (error "can't map host Lisp CHARACTER ~S to target Lisp" x))))
    (structure!object
     (find-classoid (uncross (class-name (class-of x))))) ; FIXME: TYPE-OF?
    ;; host packages obviously don't inherit from STRUCTURE!OBJECT
    ;; but we can use them as if part of our type system for fasl dumping.
    (package (find-classoid 'package))
    (t
     ;; There might be more cases which we could handle with
     ;; sufficient effort; since all we *need* to handle are enough
     ;; cases for bootstrapping, we don't try to be complete here,. If
     ;; future maintainers make the bootstrap code more complicated,
     ;; they can also add new cases here to handle it. -- WHN 2000-11-11
     (error "can't handle ~S in cross CTYPE-OF" x))))

(defun sb-pcl::class-has-a-forward-referenced-superclass-p (x)
  (bug "CLASS-HAS-A-FORWARD-REFERENCED-SUPERCLASS-P reached: ~S" x))

#|
ECL compiler strangeness:

(defun foo-sd (x) ; test for single- then double-float
  (typecase x
    ((complex single-float) 'csf)
    ((complex double-float) 'cdf)))

(defun foo-ds (x) ; test for double- then single-float
  (typecase x
    ((complex double-float) 'cdf)
    ((complex single-float) 'csf)))

(defun try-both ()
  ;; This test proves that whatever complex type is tried first wins when compiled,
  ;; but when interpreted it gives what we would call the right answer.
  (list (foo-sd #c(0.0s0 0.0s0)) (foo-sd #c(0.0d0 0.0d0))
        (foo-ds #c(0.0s0 0.0s0)) (foo-ds #c(0.0d0 0.0d0))))

(try-both) => (CSF CDF CSF CDF)
(progn (compile 'foo-sd) (compile 'foo-ds))
(try-both) => (CSF CSF CDF CDF)
|#
