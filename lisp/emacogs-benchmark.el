;;; emacogs-benchmark.el --- Performance benchmarking for Emacogs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Maintainer: emacogs@gnu.org
;; Keywords: ai, cognitive-architecture, benchmark, performance
;; Version: 1.2.0
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

;; Performance benchmarking framework for Emacogs cognitive architecture.
;;
;; Provides tools to measure and analyze performance of:
;; - Atomspace operations (add, get, query)
;; - PLN inference operations
;; - Agent orchestration
;; - Channel communication
;; - Learning system operations
;;
;; Usage:
;;
;;   M-x emacogs-benchmark-run-all      ; Run all benchmarks
;;   M-x emacogs-benchmark-atomspace    ; Benchmark atomspace
;;   M-x emacogs-benchmark-inference    ; Benchmark PLN
;;   M-x emacogs-benchmark-report       ; Show results
;;
;; API:
;;
;;   (emacogs-benchmark ITERATIONS FUNCTION)
;;   (emacogs-benchmark-compare BM1 BM2)

;;; Code:

(require 'cl-lib)
(require 'opencog-atomspace)
(require 'opencog-tensor-logic)

;;; ===========================================================================
;;; Configuration
;;; ===========================================================================

(defgroup emacogs-benchmark nil
  "Performance benchmarking for Emacogs."
  :group 'emacogs
  :prefix "emacogs-benchmark-")

(defcustom emacogs-benchmark-default-iterations 1000
  "Default number of iterations for benchmarks."
  :type 'integer
  :group 'emacogs-benchmark)

(defcustom emacogs-benchmark-warmup-iterations 100
  "Number of warmup iterations before measuring."
  :type 'integer
  :group 'emacogs-benchmark)

(defcustom emacogs-benchmark-buffer "*Emacogs Benchmark*"
  "Buffer name for benchmark results."
  :type 'string
  :group 'emacogs-benchmark)

;;; ===========================================================================
;;; Data Structures
;;; ===========================================================================

(cl-defstruct (emacogs-benchmark-result
               (:constructor emacogs-benchmark-result-create)
               (:copier nil))
  "Result of a benchmark run."
  (name nil :type string
        :documentation "Benchmark name")
  (iterations 0 :type integer
              :documentation "Number of iterations run")
  (total-time 0.0 :type float
              :documentation "Total time in seconds")
  (mean-time 0.0 :type float
             :documentation "Mean time per iteration in seconds")
  (min-time 0.0 :type float
            :documentation "Minimum time in seconds")
  (max-time 0.0 :type float
            :documentation "Maximum time in seconds")
  (std-dev 0.0 :type float
           :documentation "Standard deviation in seconds")
  (throughput 0.0 :type float
              :documentation "Operations per second")
  (timestamp nil :type float
             :documentation "When benchmark was run")
  (metadata nil :type list
            :documentation "Additional metadata"))

;;; ===========================================================================
;;; Core Benchmarking Functions
;;; ===========================================================================

(defvar emacogs-benchmark--results nil
  "List of benchmark results from last run.")

(defun emacogs-benchmark-measure (fn &optional iterations)
  "Measure execution time of FN over ITERATIONS.
Returns benchmark result structure."
  (let* ((iterations (or iterations emacogs-benchmark-default-iterations))
         (times '())
         (start-time nil)
         (end-time nil))
    ;; Warmup phase
    (dotimes (_ emacogs-benchmark-warmup-iterations)
      (funcall fn))
    ;; Garbage collect before measurement
    (garbage-collect)
    ;; Measurement phase
    (dotimes (_ iterations)
      (setq start-time (float-time))
      (funcall fn)
      (setq end-time (float-time))
      (push (- end-time start-time) times))
    ;; Calculate statistics
    (let* ((times (nreverse times))
           (total (apply #'+ times))
           (mean (/ total iterations))
           (min-t (apply #'min times))
           (max-t (apply #'max times))
           (variance (/ (apply #'+ (mapcar (lambda (t) (expt (- t mean) 2)) times))
                       iterations))
           (std-dev (sqrt variance))
           (throughput (if (> mean 0) (/ 1.0 mean) 0)))
      (emacogs-benchmark-result-create
       :iterations iterations
       :total-time total
       :mean-time mean
       :min-time min-t
       :max-time max-t
       :std-dev std-dev
       :throughput throughput
       :timestamp (float-time)))))

(defun emacogs-benchmark (name fn &optional iterations)
  "Run benchmark NAME with function FN for ITERATIONS.
Stores result and returns it."
  (let ((result (emacogs-benchmark-measure fn iterations)))
    (setf (emacogs-benchmark-result-name result) name)
    (push result emacogs-benchmark--results)
    result))

;;; ===========================================================================
;;; Atomspace Benchmarks
;;; ===========================================================================

(defun emacogs-benchmark-atomspace-add ()
  "Benchmark atomspace add operation."
  (interactive)
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq))
        (counter 0))
    (emacogs-benchmark
     "atomspace-add"
     (lambda ()
       (opencog-atomspace-add
        (opencog-atom-create-node 'ConceptNode (format "Node%d" (cl-incf counter)))))
     emacogs-benchmark-default-iterations)))

(defun emacogs-benchmark-atomspace-get ()
  "Benchmark atomspace get operation."
  (interactive)
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    ;; Setup: add atoms first
    (dotimes (i emacogs-benchmark-default-iterations)
      (opencog-atomspace-add
       (opencog-atom-create-node 'ConceptNode (format "Node%d" i))))
    (let ((counter 0))
      (emacogs-benchmark
       "atomspace-get"
       (lambda ()
         (opencog-atomspace-get 'ConceptNode (format "Node%d" counter))
         (setq counter (mod (1+ counter) emacogs-benchmark-default-iterations)))
       emacogs-benchmark-default-iterations))))

(defun emacogs-benchmark-atomspace-query ()
  "Benchmark atomspace query operation."
  (interactive)
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    ;; Setup: add mixed atoms
    (dotimes (i 100)
      (opencog-atomspace-add
       (opencog-atom-create-node 'ConceptNode (format "Concept%d" i)))
      (opencog-atomspace-add
       (opencog-atom-create-node 'PredicateNode (format "Pred%d" i))))
    (emacogs-benchmark
     "atomspace-query"
     (lambda ()
       (opencog-query 'ConceptNode))
     emacogs-benchmark-default-iterations)))

(defun emacogs-benchmark-atomspace ()
  "Run all atomspace benchmarks."
  (interactive)
  (setq emacogs-benchmark--results nil)
  (message "Running atomspace benchmarks...")
  (emacogs-benchmark-atomspace-add)
  (emacogs-benchmark-atomspace-get)
  (emacogs-benchmark-atomspace-query)
  (emacogs-benchmark-report))

;;; ===========================================================================
;;; Inference Benchmarks
;;; ===========================================================================

(defun emacogs-benchmark-pln-deduction ()
  "Benchmark PLN deduction operation."
  (interactive)
  (let ((tv1 (opencog-truth-value-simple 0.9 0.8))
        (tv2 (opencog-truth-value-simple 0.8 0.7)))
    (emacogs-benchmark
     "pln-deduction"
     (lambda ()
       (opencog-pln-deduction tv1 tv2))
     emacogs-benchmark-default-iterations)))

(defun emacogs-benchmark-pln-and ()
  "Benchmark PLN conjunction operation."
  (interactive)
  (let ((tv1 (opencog-truth-value-simple 0.9 0.8))
        (tv2 (opencog-truth-value-simple 0.7 0.6)))
    (emacogs-benchmark
     "pln-and"
     (lambda ()
       (opencog-pln-and tv1 tv2))
     emacogs-benchmark-default-iterations)))

(defun emacogs-benchmark-pln-or ()
  "Benchmark PLN disjunction operation."
  (interactive)
  (let ((tv1 (opencog-truth-value-simple 0.5 0.8))
        (tv2 (opencog-truth-value-simple 0.6 0.6)))
    (emacogs-benchmark
     "pln-or"
     (lambda ()
       (opencog-pln-or tv1 tv2))
     emacogs-benchmark-default-iterations)))

(defun emacogs-benchmark-inference ()
  "Run all inference benchmarks."
  (interactive)
  (setq emacogs-benchmark--results nil)
  (message "Running inference benchmarks...")
  (emacogs-benchmark-pln-deduction)
  (emacogs-benchmark-pln-and)
  (emacogs-benchmark-pln-or)
  (emacogs-benchmark-report))

;;; ===========================================================================
;;; Truth Value Benchmarks
;;; ===========================================================================

(defun emacogs-benchmark-truth-value-creation ()
  "Benchmark truth value creation."
  (interactive)
  (emacogs-benchmark
   "truth-value-create"
   (lambda ()
     (opencog-truth-value-simple 0.9 0.8))
   emacogs-benchmark-default-iterations))

;;; ===========================================================================
;;; Channel Benchmarks
;;; ===========================================================================

(defun emacogs-benchmark-channel-operations ()
  "Benchmark channel send/receive operations."
  (interactive)
  (require 'infermacs-limbo)
  (let ((chan (infermacs-make-channel 100)))
    (emacogs-benchmark
     "channel-send-receive"
     (lambda ()
       (infermacs-channel-send chan "test-data")
       (infermacs-channel-receive chan))
     emacogs-benchmark-default-iterations)))

;;; ===========================================================================
;;; Reporting
;;; ===========================================================================

(defun emacogs-benchmark-format-time (seconds)
  "Format SECONDS as human-readable time string."
  (cond
   ((< seconds 0.000001) (format "%.2f ns" (* seconds 1e9)))
   ((< seconds 0.001) (format "%.2f µs" (* seconds 1e6)))
   ((< seconds 1) (format "%.2f ms" (* seconds 1000)))
   (t (format "%.2f s" seconds))))

(defun emacogs-benchmark-format-throughput (ops-per-sec)
  "Format OPS-PER-SEC as human-readable throughput."
  (cond
   ((>= ops-per-sec 1e6) (format "%.2f M ops/s" (/ ops-per-sec 1e6)))
   ((>= ops-per-sec 1e3) (format "%.2f K ops/s" (/ ops-per-sec 1e3)))
   (t (format "%.2f ops/s" ops-per-sec))))

(defun emacogs-benchmark-report ()
  "Display benchmark results in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create emacogs-benchmark-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "╔══════════════════════════════════════════════════════════════════════════╗\n")
        (insert "║                    EMACOGS BENCHMARK RESULTS                             ║\n")
        (insert "╚══════════════════════════════════════════════════════════════════════════╝\n\n")
        (insert (format "Run at: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert (format "%-25s %12s %12s %12s %15s\n"
                       "Benchmark" "Mean" "Min" "Max" "Throughput"))
        (insert (make-string 78 ?─) "\n")
        (dolist (result (reverse emacogs-benchmark--results))
          (insert (format "%-25s %12s %12s %12s %15s\n"
                         (emacogs-benchmark-result-name result)
                         (emacogs-benchmark-format-time
                          (emacogs-benchmark-result-mean-time result))
                         (emacogs-benchmark-format-time
                          (emacogs-benchmark-result-min-time result))
                         (emacogs-benchmark-format-time
                          (emacogs-benchmark-result-max-time result))
                         (emacogs-benchmark-format-throughput
                          (emacogs-benchmark-result-throughput result)))))
        (insert (make-string 78 ?─) "\n")
        (insert (format "\nTotal benchmarks: %d\n" (length emacogs-benchmark--results)))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

;;; ===========================================================================
;;; All Benchmarks
;;; ===========================================================================

(defun emacogs-benchmark-run-all ()
  "Run all benchmarks and display report."
  (interactive)
  (setq emacogs-benchmark--results nil)
  (message "Running all Emacogs benchmarks...")
  ;; Atomspace benchmarks
  (emacogs-benchmark-atomspace-add)
  (emacogs-benchmark-atomspace-get)
  (emacogs-benchmark-atomspace-query)
  ;; Inference benchmarks
  (emacogs-benchmark-pln-deduction)
  (emacogs-benchmark-pln-and)
  (emacogs-benchmark-pln-or)
  ;; Truth value benchmarks
  (emacogs-benchmark-truth-value-creation)
  ;; Channel benchmarks
  (emacogs-benchmark-channel-operations)
  ;; Display results
  (emacogs-benchmark-report)
  (message "Benchmarks completed."))

;;; ===========================================================================
;;; Comparison Utilities
;;; ===========================================================================

(defun emacogs-benchmark-compare (result1 result2)
  "Compare two benchmark RESULT1 and RESULT2.
Returns a plist with comparison metrics."
  (let* ((mean1 (emacogs-benchmark-result-mean-time result1))
         (mean2 (emacogs-benchmark-result-mean-time result2))
         (diff (- mean2 mean1))
         (ratio (if (> mean1 0) (/ mean2 mean1) 0))
         (speedup (if (> mean2 0) (/ mean1 mean2) 0)))
    (list :name1 (emacogs-benchmark-result-name result1)
          :name2 (emacogs-benchmark-result-name result2)
          :mean1 mean1
          :mean2 mean2
          :difference diff
          :ratio ratio
          :speedup speedup
          :faster (if (< mean1 mean2) 1 2))))

(defun emacogs-benchmark-clear-results ()
  "Clear all benchmark results."
  (interactive)
  (setq emacogs-benchmark--results nil)
  (message "Benchmark results cleared."))

(provide 'emacogs-benchmark)

;;; emacogs-benchmark.el ends here
