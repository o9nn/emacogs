;;; opencog-tensor-logic.el --- Tensor Logic for OpenCog -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Keywords: ai, tensor-logic, probabilistic-reasoning
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (opencog-atomspace "1.0.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tensor logic implementation for probabilistic reasoning in OpenCog.
;; Implements tensor operations, probabilistic logic networks (PLN),
;; and fuzzy logic inference.
;;
;; Features:
;; - Tensor operations for multi-dimensional reasoning
;; - Probabilistic logic inference rules
;; - Fuzzy logic operations
;; - Truth value propagation
;; - Uncertain reasoning with confidence

;;; Code:

(require 'cl-lib)
(require 'opencog-atomspace)

;;; Tensor Structures

(cl-defstruct (opencog-tensor
               (:constructor opencog-tensor-create)
               (:copier nil))
  "Multi-dimensional tensor for cognitive reasoning."
  (shape nil :type list
         :documentation "Dimensions of the tensor")
  (data nil :type (or vector list)
        :documentation "Tensor data")
  (dtype 'float :type symbol
         :documentation "Data type"))

(defun opencog-tensor-make (shape &optional init-value)
  "Create a tensor with SHAPE, optionally initialized with INIT-VALUE."
  (let* ((size (apply #'* shape))
         (data (make-vector size (or init-value 0.0))))
    (opencog-tensor-create :shape shape :data data :dtype 'float)))

(defun opencog-tensor-get (tensor &rest indices)
  "Get value from TENSOR at INDICES."
  (let ((flat-index (opencog-tensor--flatten-index
                     indices (opencog-tensor-shape tensor))))
    (aref (opencog-tensor-data tensor) flat-index)))

(defun opencog-tensor-set (tensor value &rest indices)
  "Set VALUE in TENSOR at INDICES."
  (let ((flat-index (opencog-tensor--flatten-index
                     indices (opencog-tensor-shape tensor))))
    (aset (opencog-tensor-data tensor) flat-index value)))

(defun opencog-tensor--flatten-index (indices shape)
  "Convert multi-dimensional INDICES to flat index using SHAPE."
  (let ((result 0)
        (multiplier 1))
    (cl-loop for i from (1- (length indices)) downto 0
             for idx in (reverse indices)
             for dim in (reverse shape)
             do (setq result (+ result (* idx multiplier)))
             do (setq multiplier (* multiplier dim)))
    result))

;;; Tensor Operations

(defun opencog-tensor-add (tensor1 tensor2)
  "Element-wise addition of TENSOR1 and TENSOR2."
  (unless (equal (opencog-tensor-shape tensor1)
                 (opencog-tensor-shape tensor2))
    (error "Tensor shapes must match for addition"))
  (let ((result (opencog-tensor-make (opencog-tensor-shape tensor1))))
    (dotimes (i (length (opencog-tensor-data tensor1)))
      (aset (opencog-tensor-data result) i
            (+ (aref (opencog-tensor-data tensor1) i)
               (aref (opencog-tensor-data tensor2) i))))
    result))

(defun opencog-tensor-multiply (tensor1 tensor2)
  "Element-wise multiplication of TENSOR1 and TENSOR2."
  (unless (equal (opencog-tensor-shape tensor1)
                 (opencog-tensor-shape tensor2))
    (error "Tensor shapes must match for multiplication"))
  (let ((result (opencog-tensor-make (opencog-tensor-shape tensor1))))
    (dotimes (i (length (opencog-tensor-data tensor1)))
      (aset (opencog-tensor-data result) i
            (* (aref (opencog-tensor-data tensor1) i)
               (aref (opencog-tensor-data tensor2) i))))
    result))

(defun opencog-tensor-scalar-multiply (tensor scalar)
  "Multiply TENSOR by SCALAR."
  (let ((result (opencog-tensor-make (opencog-tensor-shape tensor))))
    (dotimes (i (length (opencog-tensor-data tensor)))
      (aset (opencog-tensor-data result) i
            (* (aref (opencog-tensor-data tensor) i) scalar)))
    result))

;;; Probabilistic Logic Network (PLN)

(defun opencog-pln-deduction (tv-ab tv-bc)
  "PLN deduction rule: from A->B and B->C, infer A->C.
TV-AB and TV-BC are truth values."
  (let ((s-ab (opencog-truth-value-strength tv-ab))
        (c-ab (opencog-truth-value-confidence tv-ab))
        (s-bc (opencog-truth-value-strength tv-bc))
        (c-bc (opencog-truth-value-confidence tv-bc)))
    ;; Simplified deduction formula
    (opencog-truth-value-simple
     (* s-ab s-bc)
     (* c-ab c-bc 0.9)))) ; Confidence discount

(defun opencog-pln-inversion (tv-ab)
  "PLN inversion rule: from A->B, estimate B->A.
TV-AB is the truth value of A->B."
  (let ((s (opencog-truth-value-strength tv-ab))
        (c (opencog-truth-value-confidence tv-ab)))
    ;; Simplified inversion
    (opencog-truth-value-simple
     s
     (* c 0.5)))) ; Higher uncertainty

(defun opencog-pln-abduction (tv-ab tv-cb)
  "PLN abduction rule: from A->B and C->B, infer A->C.
TV-AB and TV-CB are truth values."
  (let ((s-ab (opencog-truth-value-strength tv-ab))
        (c-ab (opencog-truth-value-confidence tv-ab))
        (s-cb (opencog-truth-value-strength tv-cb))
        (c-cb (opencog-truth-value-confidence tv-cb)))
    (opencog-truth-value-simple
     (* s-ab s-cb 0.8) ; Discount for abductive reasoning
     (* c-ab c-cb 0.6))))

(defun opencog-pln-and (tv1 tv2)
  "PLN conjunction: compute truth value of (AND A B).
TV1 and TV2 are truth values of A and B."
  (let ((s1 (opencog-truth-value-strength tv1))
        (c1 (opencog-truth-value-confidence tv1))
        (s2 (opencog-truth-value-strength tv2))
        (c2 (opencog-truth-value-confidence tv2)))
    (opencog-truth-value-simple
     (* s1 s2)
     (* c1 c2))))

(defun opencog-pln-or (tv1 tv2)
  "PLN disjunction: compute truth value of (OR A B).
TV1 and TV2 are truth values of A and B."
  (let ((s1 (opencog-truth-value-strength tv1))
        (c1 (opencog-truth-value-confidence tv1))
        (s2 (opencog-truth-value-strength tv2))
        (c2 (opencog-truth-value-confidence tv2)))
    (opencog-truth-value-simple
     (- (+ s1 s2) (* s1 s2))
     (* c1 c2))))

(defun opencog-pln-not (tv)
  "PLN negation: compute truth value of (NOT A).
TV is the truth value of A."
  (let ((s (opencog-truth-value-strength tv))
        (c (opencog-truth-value-confidence tv)))
    (opencog-truth-value-simple
     (- 1.0 s)
     c)))

;;; Fuzzy Logic Operations

(defun opencog-fuzzy-min (a b)
  "Fuzzy minimum of A and B."
  (min a b))

(defun opencog-fuzzy-max (a b)
  "Fuzzy maximum of A and B."
  (max a b))

(defun opencog-fuzzy-product (a b)
  "Fuzzy product of A and B."
  (* a b))

(defun opencog-fuzzy-complement (a)
  "Fuzzy complement of A."
  (- 1.0 a))

(defun opencog-fuzzy-lukasiewicz-and (a b)
  "Lukasiewicz t-norm (fuzzy AND)."
  (max 0.0 (- (+ a b) 1.0)))

(defun opencog-fuzzy-lukasiewicz-or (a b)
  "Lukasiewicz t-conorm (fuzzy OR)."
  (min 1.0 (+ a b)))

;;; Inference Engine

(defvar opencog-inference-rules
  '((deduction . opencog-pln-deduction)
    (inversion . opencog-pln-inversion)
    (abduction . opencog-pln-abduction)
    (conjunction . opencog-pln-and)
    (disjunction . opencog-pln-or)
    (negation . opencog-pln-not))
  "Available inference rules.")

(defun opencog-infer (rule &rest args)
  "Apply inference RULE with ARGS."
  (let ((rule-fn (cdr (assq rule opencog-inference-rules))))
    (unless rule-fn
      (error "Unknown inference rule: %s" rule))
    (apply rule-fn args)))

(defun opencog-forward-chain (atom max-steps)
  "Perform forward chaining inference from ATOM for MAX-STEPS."
  (let ((inferred nil)
        (queue (list atom))
        (visited (make-hash-table :test 'equal))
        (steps 0))
    (while (and queue (< steps max-steps))
      (let ((current (pop queue)))
        (unless (gethash (opencog-atom-key current) visited)
          (puthash (opencog-atom-key current) t visited)
          (cl-incf steps)
          ;; Apply inference rules
          (dolist (other (opencog-atomspace-get-all-atoms))
            (when (and (eq (opencog-atom-type other) 'InheritanceLink)
                       (not (eq current other)))
              ;; Try deduction
              (let ((new-tv (opencog-pln-deduction
                             (opencog-atom-truth-value current)
                             (opencog-atom-truth-value other))))
                (when (> (opencog-truth-value-strength new-tv) 0.5)
                  (let ((new-atom (opencog-atom-create-link
                                   'InheritanceLink
                                   (list (car (opencog-atom-outgoing current))
                                         (cadr (opencog-atom-outgoing other)))
                                   new-tv)))
                    (push new-atom inferred)
                    (push new-atom queue)))))))))
    inferred))

;;; Attention Allocation with Tensor Weighting

(defun opencog-tensor-attention-weights (atoms)
  "Compute attention weights for ATOMS using tensor operations."
  (let ((n (length atoms))
        (weights (opencog-tensor-make (list n))))
    (cl-loop for i from 0 below n
             for atom in atoms
             do (let* ((tv (opencog-atom-truth-value atom))
                       (av (opencog-atom-attention-value atom))
                       (weight (+ (* (opencog-truth-value-strength tv) 0.5)
                                  (* (/ (float (opencog-attention-value-sti av))
                                        100.0)
                                     0.3)
                                  (* (/ (float (opencog-attention-value-lti av))
                                        100.0)
                                     0.2))))
                  (opencog-tensor-set weights weight i)))
    weights))

;;; Utilities

(defun opencog-tensor-logic-info ()
  "Display information about tensor logic system."
  (interactive)
  (message "OpenCog Tensor Logic System
Available inference rules: %s
Tensor operations: add, multiply, scalar-multiply
PLN rules: deduction, inversion, abduction, and, or, not"
           (mapcar #'car opencog-inference-rules)))

(provide 'opencog-tensor-logic)
;;; opencog-tensor-logic.el ends here
