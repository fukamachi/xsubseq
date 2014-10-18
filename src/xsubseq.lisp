(in-package :cl-user)
(defpackage xsubseq
  (:use :cl)
  (:import-from #+sbcl :sb-cltl2
                #+openmcl :ccl
                #+cmu :ext
                #+allegro :sys
                #+ecl :si
                #+abcl :lisp
                :variable-information)
  (:export :xsubseq
           :concatenated-xsubseqs
           :xlength
           :xnconc
           :xnconcf
           :coerce-to-sequence
           :with-xsubseqs))
(in-package :xsubseq)

(deftype octets (&optional (len '*))
  `(simple-array (unsigned-byte 8) (,len)))

(defstruct (xsubseq (:constructor make-xsubseq (data start &optional (end (length data))
                                                &aux (len (- end start)))))
  (data nil)
  (start 0 :type integer)
  (end 0 :type integer)
  (len 0 :type integer))

(defstruct (octets-xsubseq (:include xsubseq)
                           (:constructor make-octets-xsubseq (data start &optional (end (length data))
                                                              &aux (len (- end start))))))

(defstruct (string-xsubseq (:include xsubseq)
                           (:constructor make-string-xsubseq (data start &optional (end (length data))
                                                              &aux (len (- end start))))))

(defstruct concatenated-xsubseqs
  (len 0 :type integer)
  (last nil :type list)
  (children nil :type list))

(defstruct (octets-concatenated-xsubseqs (:include concatenated-xsubseqs)))

(defstruct (string-concatenated-xsubseqs (:include concatenated-xsubseqs)))

(defun xsubseq (data start &optional (end (length data)))
  (typecase data
    (octets (make-octets-xsubseq data start end))
    (string (make-string-xsubseq data start end))
    (T (make-xsubseq data start end))))

#+(or sbcl openmcl cmu allegro ecl abcl)
(define-compiler-macro xsubseq (&whole form &environment env data start &optional end)
  (let ((type (cond
                ((constantp data) (type-of data))
                ((and (symbolp data)
                      (assoc 'type (nth-value 2 (variable-information data env)))))
                ((and (listp data)
                      (eq (car data) 'make-string))
                 'string)
                ((and (listp data)
                      (eq (car data) 'the)
                      (cadr data)
                      (subtypep (cadr data) 'octets))
                 'octets)
                ((and (listp data)
                      (eq (car data) 'make-array)
                      (null (cadr (member :adjustable data)))
                      (null (cadr (member :fill-pointer data)))
                      (eq (cadr (member :element-type data))
                          '(unsigned-byte 8)))
                 'octets)))
        (g-data (gensym "DATA")))
    (if (null type)
        form
        (cond
          ((subtypep type 'octets) `(let ((,g-data ,data))
                                      (make-octets-xsubseq ,g-data ,start ,(or end `(length ,g-data)))))
          ((subtypep type 'string) `(let ((,g-data ,data))
                                      (make-string-xsubseq ,g-data ,start ,(or end `(length ,g-data)))))
          (T form)))))

(defun %xnconc2 (seq1 seq2)
  (flet ((seq-values (seq)
           (if (concatenated-xsubseqs-p seq)
               (values (concatenated-xsubseqs-children seq)
                       (concatenated-xsubseqs-last seq)
                       (concatenated-xsubseqs-len seq))
               (let ((children (list seq)))
                 (values children children
                         (xsubseq-len seq))))))
    (macrolet ((make-concatenated (type seq1 seq2)
                  `(multiple-value-bind (children last len)
                       (seq-values ,seq2)
                     (,(cond
                         ((eq type 'octets) 'make-octets-concatenated-xsubseqs)
                         ((eq type 'string) 'make-string-concatenated-xsubseqs)
                         (T 'make-concatenated-xsubseqs))
                      :len (+ (xsubseq-len ,seq1) len)
                      :children (cons ,seq1 children)
                      :last last))))
      (etypecase seq1
        (concatenated-xsubseqs
         (multiple-value-bind (children last len)
             (seq-values seq2)
           (if (concatenated-xsubseqs-last seq1)
               (progn
                 (rplacd (concatenated-xsubseqs-last seq1)
                         children)
                 (setf (concatenated-xsubseqs-last seq1) last)
                 (incf (concatenated-xsubseqs-len seq1) len))
               ;; empty concatenated-xsubseqs
               (progn
                 (setf (concatenated-xsubseqs-children seq1) children
                       (concatenated-xsubseqs-len seq1) len
                       (concatenated-xsubseqs-last seq1) last)))
           seq1))
        (octets-xsubseq
         (make-concatenated octets seq1 seq2))
        (string-xsubseq
         (make-concatenated string seq1 seq2))
        (xsubseq (make-concatenated t seq1 seq2))))))

(defun xnconc (subseq &rest more-subseqs)
  (reduce #'%xnconc2 more-subseqs :initial-value subseq))

(define-modify-macro xnconcf (subseq &rest more-subseqs) xnconc)

(defun xlength (seq)
  (etypecase seq
    (xsubseq (xsubseq-len seq))
    (concatenated-xsubseqs (concatenated-xsubseqs-len seq))))

(defun coerce-to-sequence (seq)
  (etypecase seq
    (octets-concatenated-xsubseqs (octets-concatenated-xsubseqs-to-sequence seq))
    (string-concatenated-xsubseqs (string-concatenated-xsubseqs-to-sequence seq))
    (concatenated-xsubseqs (concatenated-xsubseqs-to-sequence seq))
    (xsubseq (xsubseq-to-sequence seq))))

(defun xsubseq-to-sequence (seq)
  (let ((result (make-array (xsubseq-len seq)
                            :element-type
                            (array-element-type (xsubseq-data seq)))))
    (replace result (xsubseq-data seq)
             :start2 (xsubseq-start seq)
             :end2 (xsubseq-end seq))
    result))

(defun concatenated-xsubseqs-to-sequence (seq)
  (let ((result (make-array (concatenated-xsubseqs-len seq)
                            :element-type
                            (array-element-type (xsubseq-data (car (concatenated-xsubseqs-children seq)))))))
    (loop with current-pos = 0
          for seq in (concatenated-xsubseqs-children seq)
          do (replace result (xsubseq-data seq)
                      :start1 current-pos
                      :start2 (xsubseq-start seq)
                      :end2 (xsubseq-end seq))
             (incf current-pos
                   (xsubseq-len seq)))
    result))

(defun octets-concatenated-xsubseqs-to-sequence (seq)
  (let ((result (make-array (concatenated-xsubseqs-len seq)
                            :element-type '(unsigned-byte 8))))
    (declare (type octets result))
    (loop with current-pos of-type integer = 0
          for seq in (concatenated-xsubseqs-children seq)
          do (replace result (the octets (xsubseq-data seq))
                      :start1 current-pos
                      :start2 (xsubseq-start seq)
                      :end2 (xsubseq-end seq))
             (incf current-pos
                   (xsubseq-len seq)))
    result))

(defun string-concatenated-xsubseqs-to-sequence (seq)
  (let ((result (make-string (concatenated-xsubseqs-len seq))))
    (declare (type simple-string result))
    (loop with current-pos of-type integer = 0
          for seq in (concatenated-xsubseqs-children seq)
          do (replace result (the simple-string (xsubseq-data seq))
                      :start1 current-pos
                      :start2 (xsubseq-start seq)
                      :end2 (xsubseq-end seq))
             (incf current-pos
                   (xsubseq-len seq)))
    result))

(defmacro with-xsubseqs ((xsubseqs &key initial-value) &body body)
  `(let ((,xsubseqs ,(or initial-value
                         `(make-concatenated-xsubseqs))))
     ,@body
     (concatenated-xsubseqs-to-sequence ,xsubseqs)))
