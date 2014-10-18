(in-package :cl-user)
(defpackage xsubseq
  (:use :cl)
  (:export :xsubseq
           :concatenated-xsubseqs
           :xlength
           :xnconc
           :xnconcf
           :coerce-to-sequence
           :with-xsubseqs))
(in-package :xsubseq)

(defstruct (xsubseq (:constructor xsubseq (data start &optional (end (length data))
                                                        &aux
                                                          (len (- end start)))))
  (data nil)
  (start 0 :type integer)
  (end 0 :type integer)
  (len 0 :type integer))

(defstruct concatenated-xsubseqs
  (len 0 :type integer)
  (last nil :type list)
  (children nil :type list))

(defun %xnconc2 (seq1 seq2)
  (flet ((seq-values (seq)
           (if (concatenated-xsubseqs-p seq)
               (values (concatenated-xsubseqs-children seq)
                       (concatenated-xsubseqs-last seq)
                       (concatenated-xsubseqs-len seq))
               (let ((children (list seq)))
                 (values children children
                         (xsubseq-len seq))))))
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
      (xsubseq (multiple-value-bind (children last len)
                   (seq-values seq2)
                 (make-concatenated-xsubseqs
                  :len (+ (xsubseq-len seq1) len)
                  :children (cons seq1 children)
                  :last last))))))

(defun xnconc (subseq &rest more-subseqs)
  (reduce #'%xnconc2 more-subseqs :initial-value subseq))

(define-modify-macro xnconcf (subseq &rest more-subseqs) xnconc)

(defun xlength (seq)
  (etypecase seq
    (xsubseq (xsubseq-len seq))
    (concatenated-xsubseqs (concatenated-xsubseqs-len seq))))

(defun coerce-to-sequence (seq)
  (etypecase seq
    (concatenated-xsubseqs (concatenated-xsubseqs-to-sequence seq))
    (xsubseq (xsubseq-to-sequence seq))))

(defun xsubseq-to-sequence (seq)
  (subseq (xsubseq-data seq) (xsubseq-start seq) (xsubseq-end seq)))

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

(defmacro with-xsubseqs ((xsubseqs &key initial-value) &body body)
  `(let ((,xsubseqs ,(or initial-value
                         `(make-concatenated-xsubseqs))))
     ,@body
     (concatenated-xsubseqs-to-sequence ,xsubseqs)))
