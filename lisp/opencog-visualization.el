;;; opencog-visualization.el --- Visualization for OpenCog -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Maintainer: emacogs@gnu.org
;; Keywords: ai, cognitive-architecture, visualization, graphics
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

;; Visualization system for OpenCog Atomspace and Emacogs components.
;;
;; Features:
;; - ASCII art graph visualization
;; - Attention allocation heat maps
;; - Agent activity visualization
;; - Inference chain diagrams
;; - Truth value distribution charts
;;
;; Usage:
;;
;;   (opencog-visualization-graph)
;;   (opencog-visualization-attention-map)
;;   (opencog-visualization-agent-activity)

;;; Code:

(require 'cl-lib)
(require 'opencog-atomspace)
(require 'agent-zero)

;;; Customization

(defgroup opencog-visualization nil
  "Visualization system for OpenCog."
  :group 'emacogs
  :prefix "opencog-visualization-")

(defcustom opencog-visualization-max-depth 3
  "Maximum depth for graph visualization."
  :type 'integer
  :group 'opencog-visualization)

(defcustom opencog-visualization-max-nodes 50
  "Maximum number of nodes to display in visualization."
  :type 'integer
  :group 'opencog-visualization)

(defcustom opencog-visualization-buffer "*Emacogs Visualization*"
  "Buffer name for visualization output."
  :type 'string
  :group 'opencog-visualization)

;;; Graph Visualization

(defun opencog-visualization-graph (&optional root-atom)
  "Visualize atomspace as ASCII graph starting from ROOT-ATOM.
If ROOT-ATOM is nil, visualizes high-attention atoms."
  (interactive)
  (let ((buffer (get-buffer-create opencog-visualization-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Emacogs Atomspace Graph ===\n\n")
        (if root-atom
            (opencog-visualization--render-subtree root-atom 0 (make-hash-table))
          (opencog-visualization--render-top-atoms))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun opencog-visualization--render-top-atoms ()
  "Render atoms with highest attention values."
  (let ((atoms '()))
    ;; Collect atoms with attention
    (maphash (lambda (_handle atom)
               (let ((av (opencog-atom-attention-value atom)))
                 (when av
                   (push (cons atom (opencog-attention-value-sti av)) atoms))))
             opencog-atomspace)
    ;; Sort by attention
    (setq atoms (sort atoms (lambda (a b) (> (cdr a) (cdr b)))))
    ;; Render top atoms
    (let ((count 0)
          (visited (make-hash-table)))
      (dolist (atom-pair atoms)
        (when (< count opencog-visualization-max-nodes)
          (opencog-visualization--render-subtree (car atom-pair) 0 visited)
          (insert "\n")
          (cl-incf count))))))

(defun opencog-visualization--render-subtree (atom depth visited)
  "Render ATOM and its children at DEPTH, tracking VISITED atoms."
  (when (and atom
            (< depth opencog-visualization-max-depth)
            (not (gethash atom visited)))
    (puthash atom t visited)
    ;; Indent
    (insert (make-string (* depth 2) ?\s))
    ;; Render node
    (let ((prefix (if (opencog-atom-outgoing atom) "├─" "└─")))
      (insert (format "%s %s" prefix (opencog-visualization--atom-display atom)))
      (insert "\n"))
    ;; Render children
    (when (opencog-atom-outgoing atom)
      (dolist (child (opencog-atom-outgoing atom))
        (opencog-visualization--render-subtree child (1+ depth) visited)))))

(defun opencog-visualization--atom-display (atom)
  "Create display string for ATOM with colors and annotations."
  (let* ((type (opencog-atom-type atom))
         (name (opencog-atom-name atom))
         (tv (opencog-atom-truth-value atom))
         (av (opencog-atom-attention-value atom))
         (display (if name
                     (format "%s:%s" type name)
                   (format "%s[%d]" type (length (opencog-atom-outgoing atom))))))
    ;; Add truth value
    (when tv
      (setq display (concat display
                           (format " (%.2f,%.2f)"
                                  (opencog-truth-value-strength tv)
                                  (opencog-truth-value-confidence tv)))))
    ;; Add attention indicator
    (when av
      (let ((sti (opencog-attention-value-sti av)))
        (cond ((> sti 50) (setq display (concat "*** " display)))
              ((> sti 20) (setq display (concat "**  " display)))
              ((> sti 0)  (setq display (concat "*   " display))))))
    display))

;;; Attention Heat Map

(defun opencog-visualization-attention-map ()
  "Display attention allocation as heat map."
  (interactive)
  (let ((buffer (get-buffer-create opencog-visualization-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Attention Heat Map ===\n\n")
        (insert "Legend: ████ >50 | ███░ 20-50 | ██░░ 10-20 | █░░░ 0-10 | ░░░░ <0\n\n")
        ;; Collect atoms with attention
        (let ((atoms '()))
          (maphash (lambda (_handle atom)
                     (let ((av (opencog-atom-attention-value atom)))
                       (when av
                         (push (cons atom (opencog-attention-value-sti av)) atoms))))
                   opencog-atomspace)
          (setq atoms (sort atoms (lambda (a b) (> (cdr a) (cdr b)))))
          ;; Display bars
          (dolist (atom-pair (cl-subseq atoms 0 (min 30 (length atoms))))
            (let* ((atom (car atom-pair))
                   (sti (cdr atom-pair))
                   (bar (opencog-visualization--attention-bar sti))
                   (label (opencog-visualization--atom-label atom)))
              (insert (format "%-40s %s %6.1f\n" label bar sti)))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun opencog-visualization--attention-bar (sti)
  "Create attention bar visualization for STI value."
  (cond ((> sti 50)  "████")
        ((> sti 20)  "███░")
        ((> sti 10)  "██░░")
        ((> sti 0)   "█░░░")
        (t           "░░░░")))

(defun opencog-visualization--atom-label (atom)
  "Create short label for ATOM."
  (let ((type (opencog-atom-type atom))
        (name (opencog-atom-name atom)))
    (if name
        (format "%s:%s" type (truncate-string-to-width name 25 nil nil t))
      (format "%s[%d]" type (length (opencog-atom-outgoing atom))))))

;;; Agent Activity

(defun opencog-visualization-agent-activity ()
  "Display agent activity visualization."
  (interactive)
  (let ((buffer (get-buffer-create opencog-visualization-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Agent Activity ===\n\n")
        (insert (format "%-20s %-12s %-10s %s\n"
                       "Agent" "State" "Priority" "Performance"))
        (insert (make-string 70 ?-) "\n")
        (maphash (lambda (_id agent)
                   (let* ((name (agent-zero-agent-name agent))
                          (state (agent-zero-agent-state agent))
                          (priority (agent-zero-agent-priority agent))
                          (performance (agent-zero-agent-performance agent))
                          (state-icon (opencog-visualization--agent-state-icon state))
                          (perf-bar (opencog-visualization--performance-bar performance)))
                     (insert (format "%-20s %s %-9s %-10.2f %s\n"
                                   (truncate-string-to-width name 20)
                                   state-icon
                                   state
                                   priority
                                   perf-bar))))
                 agent-zero-agents)
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun opencog-visualization--agent-state-icon (state)
  "Get icon for agent STATE."
  (pcase state
    ('running "▶")
    ('idle "⏸")
    ('waiting "⏳")
    ('terminated "⏹")
    (_ "?")))

(defun opencog-visualization--performance-bar (performance)
  "Create performance bar for PERFORMANCE value."
  (let ((bars (floor (* performance 10))))
    (concat (make-string bars ?█)
            (make-string (- 10 bars) ?░))))

;;; Truth Value Distribution

(defun opencog-visualization-truth-distribution ()
  "Display distribution of truth values in atomspace."
  (interactive)
  (let ((buffer (get-buffer-create opencog-visualization-buffer))
        (buckets (make-vector 10 0)))
    ;; Count atoms in strength buckets
    (maphash (lambda (_handle atom)
               (let ((tv (opencog-atom-truth-value atom)))
                 (when tv
                   (let* ((strength (opencog-truth-value-strength tv))
                          (bucket (min 9 (floor (* strength 10)))))
                     (cl-incf (aref buckets bucket))))))
             opencog-atomspace)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Truth Value Distribution ===\n\n")
        (insert "Strength ranges (0.0-1.0):\n\n")
        (dotimes (i 10)
          (let* ((count (aref buckets i))
                 (bar-length (if (> count 0)
                               (min 50 (ceiling (/ (* count 50.0)
                                                  (apply #'max (append buckets nil)))))
                               0))
                 (range-start (* i 0.1))
                 (range-end (* (1+ i) 0.1)))
            (insert (format "%.1f-%.1f: %s %d\n"
                           range-start range-end
                           (make-string bar-length ?█)
                           count))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

;;; Inference Chain

(defun opencog-visualization-inference-chain (atoms)
  "Visualize inference chain for ATOMS."
  (let ((buffer (get-buffer-create opencog-visualization-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Inference Chain ===\n\n")
        (dotimes (i (length atoms))
          (let ((atom (nth i atoms)))
            (insert (format "%d. %s\n"
                           (1+ i)
                           (opencog-atom-to-string atom)))
            (when (< i (1- (length atoms)))
              (insert "   ↓\n"))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

;;; System Overview

(defun opencog-visualization-system-overview ()
  "Display comprehensive system overview."
  (interactive)
  (let ((buffer (get-buffer-create opencog-visualization-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "╔══════════════════════════════════════════════════════════════╗\n")
        (insert "║           EMACOGS SYSTEM OVERVIEW                            ║\n")
        (insert "╚══════════════════════════════════════════════════════════════╝\n\n")
        
        ;; Atomspace stats
        (let ((stats (opencog-atomspace-stats)))
          (insert "┌─ Atomspace ─────────────────────────────────────────────┐\n")
          (insert (format "│ Total Atoms:  %-6d                                    │\n"
                         (plist-get stats :total-atoms)))
          (insert (format "│ Nodes:        %-6d                                    │\n"
                         (plist-get stats :node-count)))
          (insert (format "│ Links:        %-6d                                    │\n"
                         (plist-get stats :link-count)))
          (insert "└─────────────────────────────────────────────────────────┘\n\n"))
        
        ;; Agent stats
        (let ((agent-count 0)
              (running-count 0))
          (maphash (lambda (_id agent)
                     (cl-incf agent-count)
                     (when (eq (agent-zero-agent-state agent) 'running)
                       (cl-incf running-count)))
                   agent-zero-agents)
          (insert "┌─ Agents ────────────────────────────────────────────────┐\n")
          (insert (format "│ Total Agents: %-6d                                    │\n"
                         agent-count))
          (insert (format "│ Running:      %-6d                                    │\n"
                         running-count))
          (insert "└─────────────────────────────────────────────────────────┘\n\n"))
        
        (insert "Press 'q' to close this window\n")
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(provide 'opencog-visualization)

;;; opencog-visualization.el ends here
