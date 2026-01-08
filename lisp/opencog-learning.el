;;; opencog-learning.el --- Learning capabilities for OpenCog -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Maintainer: emacogs@gnu.org
;; Keywords: ai, cognitive-architecture, learning, evolution
;; Version: 1.1.0
;; Package-Requires: ((emacs "29.1"))

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

;; Learning system for OpenCog Atomspace.
;;
;; Features:
;; - Attention spreading and allocation
;; - Pattern mining from atomspace
;; - Importance decay and forgetting
;; - Hebbian learning for link strengthening
;; - Experience-based learning
;;
;; Usage:
;;
;;   (opencog-learning-enable)
;;   (opencog-learning-spread-attention atom)
;;   (opencog-learning-mine-patterns)

;;; Code:

(require 'cl-lib)
(require 'opencog-atomspace)
(require 'opencog-tensor-logic)

;;; Customization

(defgroup opencog-learning nil
  "Learning system for OpenCog."
  :group 'emacogs
  :prefix "opencog-learning-")

(defcustom opencog-learning-attention-spread-rate 0.1
  "Rate at which attention spreads to connected atoms."
  :type 'float
  :group 'opencog-learning)

(defcustom opencog-learning-importance-decay-rate 0.01
  "Rate at which importance decays over time."
  :type 'float
  :group 'opencog-learning)

(defcustom opencog-learning-hebbian-rate 0.05
  "Learning rate for Hebbian link strengthening."
  :type 'float
  :group 'opencog-learning)

(defcustom opencog-learning-forget-threshold -50
  "STI threshold below which atoms are forgotten."
  :type 'integer
  :group 'opencog-learning)

(defcustom opencog-learning-update-interval 60
  "Interval in seconds for learning updates."
  :type 'integer
  :group 'opencog-learning)

;;; Variables

(defvar opencog-learning--timer nil
  "Timer for periodic learning updates.")

(defvar opencog-learning--statistics
  '(:patterns-mined 0
    :atoms-forgotten 0
    :attention-updates 0
    :hebbian-updates 0)
  "Statistics for learning system.")

;;; Attention Spreading

(defun opencog-learning-spread-attention (atom &optional amount)
  "Spread attention from ATOM to connected atoms.
AMOUNT specifies how much attention to spread (default based on atom's STI)."
  (let* ((av (opencog-atom-attention-value atom))
         (sti (if av (opencog-attention-value-sti av) 0))
         (spread-amount (or amount
                           (* sti opencog-learning-attention-spread-rate)))
         (outgoing (opencog-atom-outgoing atom)))
    (when (and outgoing (> spread-amount 0))
      (let ((per-atom (/ spread-amount (length outgoing))))
        (dolist (target outgoing)
          (opencog-learning--increase-sti target per-atom))
        ;; Decrease source atom's STI
        (opencog-learning--increase-sti atom (- spread-amount))
        (cl-incf (plist-get opencog-learning--statistics :attention-updates))))))

(defun opencog-learning--increase-sti (atom amount)
  "Increase the STI of ATOM by AMOUNT."
  (let* ((av (or (opencog-atom-attention-value atom)
                (opencog-attention-value-create 0 0 0)))
         (new-sti (+ (opencog-attention-value-sti av) amount)))
    (setf (opencog-atom-attention-value atom)
          (opencog-attention-value-create
           new-sti
           (opencog-attention-value-lti av)
           (opencog-attention-value-vlti av)))))

(defun opencog-learning-spread-attention-network (&optional iterations)
  "Spread attention across the atomspace network.
ITERATIONS specifies how many spreading steps to perform (default 5)."
  (interactive "p")
  (let ((iterations (or iterations 5))
        (high-attention-atoms '()))
    ;; Find atoms with high attention
    (maphash (lambda (_handle atom)
               (let ((av (opencog-atom-attention-value atom)))
                 (when (and av (> (opencog-attention-value-sti av) 10))
                   (push atom high-attention-atoms))))
             opencog-atomspace)
    ;; Spread from high attention atoms
    (dotimes (_ iterations)
      (dolist (atom high-attention-atoms)
        (opencog-learning-spread-attention atom)))
    (message "Attention spread completed (%d iterations, %d source atoms)"
             iterations (length high-attention-atoms))))

;;; Importance Decay

(defun opencog-learning-decay-importance ()
  "Decay importance (STI) of all atoms over time."
  (let ((decayed-count 0))
    (maphash (lambda (_handle atom)
               (let ((av (opencog-atom-attention-value atom)))
                 (when av
                   (let* ((sti (opencog-attention-value-sti av))
                          (new-sti (* sti (- 1.0 opencog-learning-importance-decay-rate))))
                     (setf (opencog-atom-attention-value atom)
                           (opencog-attention-value-create
                            new-sti
                            (opencog-attention-value-lti av)
                            (opencog-attention-value-vlti av)))
                     (cl-incf decayed-count)))))
             opencog-atomspace)
    decayed-count))

;;; Forgetting

(defun opencog-learning-forget-unimportant ()
  "Remove atoms with very low importance from atomspace."
  (let ((to-forget '()))
    (maphash (lambda (handle atom)
               (let ((av (opencog-atom-attention-value atom)))
                 (when (and av
                           (< (opencog-attention-value-sti av)
                              opencog-learning-forget-threshold))
                   (push handle to-forget))))
             opencog-atomspace)
    (dolist (handle to-forget)
      (remhash handle opencog-atomspace)
      (cl-incf (plist-get opencog-learning--statistics :atoms-forgotten)))
    (length to-forget)))

;;; Pattern Mining

(defun opencog-learning-mine-patterns (&optional min-frequency)
  "Mine frequent patterns from atomspace.
MIN-FREQUENCY specifies minimum occurrences (default 2).
Returns list of discovered patterns."
  (interactive)
  (let ((min-freq (or min-frequency 2))
        (patterns (make-hash-table :test 'equal))
        (results '()))
    ;; Find frequent link patterns
    (maphash (lambda (_handle atom)
               (when (opencog-atom-outgoing atom)
                 (let* ((type (opencog-atom-type atom))
                        (pattern (list type (length (opencog-atom-outgoing atom)))))
                   (puthash pattern
                           (1+ (gethash pattern patterns 0))
                           patterns))))
             opencog-atomspace)
    ;; Filter by frequency
    (maphash (lambda (pattern count)
               (when (>= count min-freq)
                 (push (list pattern count) results)))
             patterns)
    (cl-incf (plist-get opencog-learning--statistics :patterns-mined)
             (length results))
    (when (called-interactively-p 'any)
      (message "Found %d patterns (min-freq: %d)" (length results) min-freq))
    (sort results (lambda (a b) (> (cadr a) (cadr b))))))

(defun opencog-learning-find-similar-patterns (atom &optional threshold)
  "Find atoms with similar patterns to ATOM.
THRESHOLD is similarity threshold (default 0.7)."
  (let ((threshold (or threshold 0.7))
        (type (opencog-atom-type atom))
        (outgoing (opencog-atom-outgoing atom))
        (similar '()))
    (when outgoing
      (let ((arity (length outgoing)))
        (maphash (lambda (_handle candidate)
                   (when (and (eq (opencog-atom-type candidate) type)
                             (opencog-atom-outgoing candidate)
                             (not (eq candidate atom)))
                     (let* ((cand-arity (length (opencog-atom-outgoing candidate)))
                            (similarity (if (= arity cand-arity) 1.0
                                         (/ (float (min arity cand-arity))
                                            (max arity cand-arity)))))
                       (when (>= similarity threshold)
                         (push (list candidate similarity) similar)))))
                 opencog-atomspace)))
    (sort similar (lambda (a b) (> (cadr a) (cadr b))))))

;;; Hebbian Learning

(defun opencog-learning-hebbian-update (link)
  "Apply Hebbian learning to strengthen LINK based on activation.
Atoms that fire together wire together - strengthen links between
frequently co-activated atoms."
  (when (opencog-atom-outgoing link)
    (let* ((tv (opencog-atom-truth-value link))
           (strength (if tv (opencog-truth-value-strength tv) 0.5))
           (confidence (if tv (opencog-truth-value-confidence tv) 0.5))
           ;; Calculate activation from outgoing atoms
           (activations (mapcar
                        (lambda (atom)
                          (let ((av (opencog-atom-attention-value atom)))
                            (if av
                                (max 0 (opencog-attention-value-sti av))
                              0)))
                        (opencog-atom-outgoing link)))
           (avg-activation (/ (apply #'+ activations)
                             (float (length activations))))
           ;; Normalize activation to [0, 1]
           (normalized-activation (min 1.0 (/ avg-activation 100.0)))
           ;; Update strength using Hebbian rule
           (new-strength (+ strength
                           (* opencog-learning-hebbian-rate
                              normalized-activation
                              (- 1.0 strength))))
           (new-confidence (min 1.0 (+ confidence 0.01))))
      (setf (opencog-atom-truth-value link)
            (opencog-truth-value-simple new-strength new-confidence))
      (cl-incf (plist-get opencog-learning--statistics :hebbian-updates)))))

(defun opencog-learning-hebbian-update-all ()
  "Apply Hebbian learning to all links in atomspace."
  (interactive)
  (let ((update-count 0))
    (maphash (lambda (_handle atom)
               (when (opencog-atom-outgoing atom)
                 (opencog-learning-hebbian-update atom)
                 (cl-incf update-count)))
             opencog-atomspace)
    (message "Applied Hebbian updates to %d links" update-count)))

;;; Experience-Based Learning

(defun opencog-learning-record-experience (atoms result-tv)
  "Record an experience with ATOMS and RESULT-TV.
Strengthens connections between atoms that led to positive outcomes."
  (when (and atoms result-tv
            (> (opencog-truth-value-strength result-tv) 0.5))
    ;; Increase attention on involved atoms
    (dolist (atom atoms)
      (opencog-learning--increase-sti atom 10))
    ;; Strengthen links between atoms
    (when (> (length atoms) 1)
      (dotimes (i (1- (length atoms)))
        (let* ((atom1 (nth i atoms))
               (atom2 (nth (1+ i) atoms))
               (link (opencog-atom-create-link
                     'SimilarityLink
                     (list atom1 atom2)
                     result-tv)))
          (opencog-atomspace-add link)
          (opencog-learning-hebbian-update link))))))

;;; Periodic Updates

(defun opencog-learning--periodic-update ()
  "Perform periodic learning maintenance tasks."
  (opencog-learning-decay-importance)
  (let ((forgotten (opencog-learning-forget-unimportant)))
    (when (> forgotten 0)
      (message "Forgot %d unimportant atoms" forgotten))))

(defun opencog-learning-enable ()
  "Enable periodic learning updates."
  (interactive)
  (opencog-learning-disable)
  (setq opencog-learning--timer
        (run-with-timer opencog-learning-update-interval
                       opencog-learning-update-interval
                       #'opencog-learning--periodic-update))
  (message "Learning system enabled (update interval: %ds)"
           opencog-learning-update-interval))

(defun opencog-learning-disable ()
  "Disable periodic learning updates."
  (interactive)
  (when opencog-learning--timer
    (cancel-timer opencog-learning--timer)
    (setq opencog-learning--timer nil)
    (message "Learning system disabled")))

;;; Statistics

(defun opencog-learning-stats ()
  "Display learning system statistics."
  (interactive)
  (message "Learning Stats: patterns=%d forgotten=%d attention=%d hebbian=%d"
           (plist-get opencog-learning--statistics :patterns-mined)
           (plist-get opencog-learning--statistics :atoms-forgotten)
           (plist-get opencog-learning--statistics :attention-updates)
           (plist-get opencog-learning--statistics :hebbian-updates)))

(defun opencog-learning-reset-stats ()
  "Reset learning statistics."
  (interactive)
  (setq opencog-learning--statistics
        '(:patterns-mined 0
          :atoms-forgotten 0
          :attention-updates 0
          :hebbian-updates 0))
  (message "Learning statistics reset"))

(provide 'opencog-learning)

;;; opencog-learning.el ends here
