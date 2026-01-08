;;; emacogs-test.el --- ERT tests for Emacogs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Keywords: test
;; Version: 1.2.0

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

;; ERT test suite for Emacogs cognitive architecture system.
;; Run with: M-x ert RET "emacogs-" RET
;; Or from command line: emacs -Q -L lisp -l ert -l test/lisp/emacogs-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "../../lisp" (file-name-directory load-file-name)))

(require 'opencog-atomspace)
(require 'opencog-tensor-logic)
(require 'agent-zero)
(require 'infermacs-limbo)
(require 'opencog-org-constellations)

;;; ===========================================================================
;;; Truth Value Tests
;;; ===========================================================================

(ert-deftest emacogs-test-truth-value-creation ()
  "Test basic truth value creation."
  (let ((tv (opencog-truth-value-simple 0.9 0.8)))
    (should (opencog-truth-value-p tv))
    (should (= (opencog-truth-value-strength tv) 0.9))
    (should (= (opencog-truth-value-confidence tv) 0.8))))

(ert-deftest emacogs-test-truth-value-clamping ()
  "Test that truth values are clamped to [0.0, 1.0]."
  (let ((tv-low (opencog-truth-value-simple -0.5 -0.3))
        (tv-high (opencog-truth-value-simple 1.5 2.0)))
    ;; Low values clamped to 0.0
    (should (= (opencog-truth-value-strength tv-low) 0.0))
    (should (= (opencog-truth-value-confidence tv-low) 0.0))
    ;; High values clamped to 1.0
    (should (= (opencog-truth-value-strength tv-high) 1.0))
    (should (= (opencog-truth-value-confidence tv-high) 1.0))))

;;; ===========================================================================
;;; Attention Value Tests
;;; ===========================================================================

(ert-deftest emacogs-test-attention-value-creation ()
  "Test attention value creation."
  (let ((av (opencog-attention-value-create :sti 100 :lti 50 :vlti 10)))
    (should (opencog-attention-value-p av))
    (should (= (opencog-attention-value-sti av) 100))
    (should (= (opencog-attention-value-lti av) 50))
    (should (= (opencog-attention-value-vlti av) 10))))

(ert-deftest emacogs-test-attention-value-defaults ()
  "Test attention value default values."
  (let ((av (opencog-attention-value-create)))
    (should (= (opencog-attention-value-sti av) 0))
    (should (= (opencog-attention-value-lti av) 0))
    (should (= (opencog-attention-value-vlti av) 0))))

;;; ===========================================================================
;;; Atom Tests
;;; ===========================================================================

(ert-deftest emacogs-test-node-creation ()
  "Test node atom creation."
  (let ((node (opencog-atom-create-node 'ConceptNode "TestConcept")))
    (should (opencog-atom-p node))
    (should (eq (opencog-atom-type node) 'ConceptNode))
    (should (equal (opencog-atom-name node) "TestConcept"))
    (should (null (opencog-atom-outgoing node)))))

(ert-deftest emacogs-test-node-with-truth-value ()
  "Test node creation with custom truth value."
  (let* ((tv (opencog-truth-value-simple 0.7 0.5))
         (node (opencog-atom-create-node 'ConceptNode "Test" tv)))
    (should (= (opencog-truth-value-strength (opencog-atom-truth-value node)) 0.7))
    (should (= (opencog-truth-value-confidence (opencog-atom-truth-value node)) 0.5))))

(ert-deftest emacogs-test-link-creation ()
  "Test link atom creation."
  (let* ((node1 (opencog-atom-create-node 'ConceptNode "Cat"))
         (node2 (opencog-atom-create-node 'ConceptNode "Animal"))
         (link (opencog-atom-create-link 'InheritanceLink (list node1 node2))))
    (should (opencog-atom-p link))
    (should (eq (opencog-atom-type link) 'InheritanceLink))
    (should (null (opencog-atom-name link)))
    (should (= (length (opencog-atom-outgoing link)) 2))))

(ert-deftest emacogs-test-atom-to-string ()
  "Test atom string representation."
  (let* ((node (opencog-atom-create-node 'ConceptNode "Test"))
         (str (opencog-atom-to-string node)))
    (should (stringp str))
    (should (string-match-p "ConceptNode" str))
    (should (string-match-p "Test" str))))

;;; ===========================================================================
;;; Atomspace Tests
;;; ===========================================================================

(ert-deftest emacogs-test-atomspace-add-and-get ()
  "Test adding and retrieving atoms from atomspace."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    (let* ((node (opencog-atom-create-node 'ConceptNode "TestNode"))
           (added (opencog-atomspace-add node))
           (retrieved (opencog-atomspace-get 'ConceptNode "TestNode")))
      (should (eq added node))
      (should (eq retrieved node)))))

(ert-deftest emacogs-test-atomspace-deduplication ()
  "Test that atomspace deduplicates atoms."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    (let* ((node1 (opencog-atom-create-node 'ConceptNode "Same"))
           (node2 (opencog-atom-create-node 'ConceptNode "Same"))
           (added1 (opencog-atomspace-add node1))
           (added2 (opencog-atomspace-add node2)))
      ;; Second add should return existing atom
      (should (eq added1 node1))
      (should (eq added2 node1))
      (should (= (opencog-atomspace-size) 1)))))

(ert-deftest emacogs-test-atomspace-clear ()
  "Test atomspace clearing."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    (opencog-atomspace-add (opencog-atom-create-node 'ConceptNode "Test1"))
    (opencog-atomspace-add (opencog-atom-create-node 'ConceptNode "Test2"))
    (should (= (opencog-atomspace-size) 2))
    (opencog-atomspace-clear)
    (should (= (opencog-atomspace-size) 0))))

(ert-deftest emacogs-test-atomspace-get-by-type ()
  "Test getting atoms by type."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    (opencog-atomspace-add (opencog-atom-create-node 'ConceptNode "A"))
    (opencog-atomspace-add (opencog-atom-create-node 'ConceptNode "B"))
    (opencog-atomspace-add (opencog-atom-create-node 'PredicateNode "P"))
    (let ((concepts (opencog-atomspace-get-atoms-by-type 'ConceptNode))
          (predicates (opencog-atomspace-get-atoms-by-type 'PredicateNode)))
      (should (= (length concepts) 2))
      (should (= (length predicates) 1)))))

(ert-deftest emacogs-test-atomspace-incoming-set ()
  "Test that incoming sets are updated for link targets."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    (let* ((node1 (opencog-atom-create-node 'ConceptNode "Cat"))
           (node2 (opencog-atom-create-node 'ConceptNode "Animal")))
      (opencog-atomspace-add node1)
      (opencog-atomspace-add node2)
      (let ((link (opencog-atom-create-link 'InheritanceLink (list node1 node2))))
        (opencog-atomspace-add link)
        ;; Both nodes should have link in their incoming set
        (should (member link (opencog-atom-incoming node1)))
        (should (member link (opencog-atom-incoming node2)))))))

;;; ===========================================================================
;;; Pattern Matching Tests
;;; ===========================================================================

(ert-deftest emacogs-test-pattern-wildcard ()
  "Test wildcard pattern matching."
  (let* ((node (opencog-atom-create-node 'ConceptNode "Test")))
    (should (opencog-pattern-match-atom '_ node))))

(ert-deftest emacogs-test-pattern-type-match ()
  "Test type pattern matching."
  (let* ((node (opencog-atom-create-node 'ConceptNode "Test")))
    (should (opencog-pattern-match-atom 'ConceptNode node))
    (should-not (opencog-pattern-match-atom 'PredicateNode node))))

(ert-deftest emacogs-test-pattern-variable ()
  "Test variable pattern matching."
  (let* ((node (opencog-atom-create-node 'ConceptNode "Test")))
    (should (opencog-pattern-match-atom '?x node))
    (should (opencog-pattern-match-atom '?any node))))

(ert-deftest emacogs-test-query-execution ()
  "Test query execution."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    (opencog-atomspace-add (opencog-atom-create-node 'ConceptNode "A"))
    (opencog-atomspace-add (opencog-atom-create-node 'ConceptNode "B"))
    (opencog-atomspace-add (opencog-atom-create-node 'PredicateNode "P"))
    (let ((results (opencog-query 'ConceptNode)))
      (should (= (length results) 2)))))

;;; ===========================================================================
;;; Tensor Logic Tests
;;; ===========================================================================

(ert-deftest emacogs-test-pln-deduction ()
  "Test PLN deduction rule."
  (let* ((tv-ab (opencog-truth-value-simple 0.9 0.8))
         (tv-bc (opencog-truth-value-simple 0.8 0.7))
         (result (opencog-pln-deduction tv-ab tv-bc)))
    (should (opencog-truth-value-p result))
    ;; Deduction should reduce strength
    (should (< (opencog-truth-value-strength result)
               (opencog-truth-value-strength tv-ab)))
    ;; Deduction should reduce confidence
    (should (< (opencog-truth-value-confidence result)
               (opencog-truth-value-confidence tv-ab)))))

(ert-deftest emacogs-test-pln-and ()
  "Test PLN conjunction."
  (let* ((tv1 (opencog-truth-value-simple 0.9 0.8))
         (tv2 (opencog-truth-value-simple 0.7 0.6))
         (result (opencog-pln-and tv1 tv2)))
    (should (opencog-truth-value-p result))
    ;; AND should be less than or equal to minimum
    (should (<= (opencog-truth-value-strength result) 0.7))))

(ert-deftest emacogs-test-pln-or ()
  "Test PLN disjunction."
  (let* ((tv1 (opencog-truth-value-simple 0.5 0.8))
         (tv2 (opencog-truth-value-simple 0.6 0.6))
         (result (opencog-pln-or tv1 tv2)))
    (should (opencog-truth-value-p result))
    ;; OR should be greater than or equal to maximum
    (should (>= (opencog-truth-value-strength result) 0.6))))

(ert-deftest emacogs-test-pln-not ()
  "Test PLN negation."
  (let* ((tv (opencog-truth-value-simple 0.7 0.8))
         (result (opencog-pln-not tv)))
    (should (opencog-truth-value-p result))
    ;; NOT should invert strength
    (should (< (abs (- (opencog-truth-value-strength result) 0.3)) 0.01))))

;;; ===========================================================================
;;; Agent-Zero Tests
;;; ===========================================================================

(ert-deftest emacogs-test-agent-creation ()
  "Test agent creation."
  (let ((agent-zero-agents (make-hash-table :test 'equal))
        (agent-zero-next-agent-id 0))
    (let ((agent (agent-zero-create "TestAgent" 'worker '(reasoning) #'ignore)))
      (should (agent-zero-agent-p agent))
      (should (equal (agent-zero-agent-name agent) "TestAgent"))
      (should (eq (agent-zero-agent-type agent) 'worker))
      (should (equal (agent-zero-agent-capabilities agent) '(reasoning))))))

(ert-deftest emacogs-test-agent-has-capabilities ()
  "Test agent capability checking."
  (let ((agent-zero-agents (make-hash-table :test 'equal))
        (agent-zero-next-agent-id 0))
    (let ((agent (agent-zero-create "TestAgent" 'worker '(reasoning learning) #'ignore)))
      (should (agent-zero-agent-has-capabilities agent '(reasoning)))
      (should (agent-zero-agent-has-capabilities agent '(learning)))
      (should (agent-zero-agent-has-capabilities agent '(reasoning learning)))
      (should-not (agent-zero-agent-has-capabilities agent '(unknown))))))

(ert-deftest emacogs-test-agent-message-send ()
  "Test agent message sending."
  (let ((agent-zero-agents (make-hash-table :test 'equal))
        (agent-zero-next-agent-id 0))
    (let ((agent (agent-zero-create "TestAgent" 'worker '(reasoning) #'ignore)))
      (agent-zero-send-message agent "Hello")
      (let ((inbox (agent-zero-agent-inbox agent)))
        (should (= (length inbox) 1))
        (should (equal (car inbox) "Hello"))))))

;;; ===========================================================================
;;; Infermacs Channel Tests
;;; ===========================================================================

(ert-deftest emacogs-test-channel-creation ()
  "Test channel creation."
  (let ((chan (infermacs-make-channel 5)))
    (should (infermacs-channel-p chan))
    (should (= (infermacs-channel-capacity chan) 5))
    (should (= (length (infermacs-channel-buffer chan)) 0))))

(ert-deftest emacogs-test-channel-send-receive ()
  "Test channel send and receive."
  (let ((chan (infermacs-make-channel 5)))
    (infermacs-channel-send chan "test-data")
    (should (= (length (infermacs-channel-buffer chan)) 1))
    (let ((received (infermacs-channel-receive chan)))
      (should (equal received "test-data"))
      (should (= (length (infermacs-channel-buffer chan)) 0)))))

(ert-deftest emacogs-test-channel-buffer-order ()
  "Test that channel maintains FIFO order."
  (let ((chan (infermacs-make-channel 5)))
    (infermacs-channel-send chan "first")
    (infermacs-channel-send chan "second")
    (infermacs-channel-send chan "third")
    (should (equal (infermacs-channel-receive chan) "first"))
    (should (equal (infermacs-channel-receive chan) "second"))
    (should (equal (infermacs-channel-receive chan) "third"))))

;;; ===========================================================================
;;; Org Constellation Tests
;;; ===========================================================================

(ert-deftest emacogs-test-constellation-creation ()
  "Test constellation creation from org content."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq))
        (opencog-constellations (make-hash-table :test 'equal)))
    ;; Create a simple org structure in memory
    (let ((constellation (opencog-create-constellation-from-string
                          "TestKB"
                          "* Topic A\n** Subtopic A1\n** Subtopic A2\n* Topic B")))
      (should constellation)
      (should (> (opencog-atomspace-size) 0)))))

;;; ===========================================================================
;;; Integration Tests
;;; ===========================================================================

(ert-deftest emacogs-test-knowledge-inference ()
  "Test knowledge creation and inference chain."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    ;; Create knowledge: Cat is-a Animal, Animal is-a LivingThing
    (let* ((cat (opencog-atomspace-add
                 (opencog-atom-create-node 'ConceptNode "Cat")))
           (animal (opencog-atomspace-add
                    (opencog-atom-create-node 'ConceptNode "Animal")))
           (living (opencog-atomspace-add
                    (opencog-atom-create-node 'ConceptNode "LivingThing")))
           (link1 (opencog-atomspace-add
                   (opencog-atom-create-link
                    'InheritanceLink (list cat animal)
                    (opencog-truth-value-simple 0.95 0.9))))
           (link2 (opencog-atomspace-add
                   (opencog-atom-create-link
                    'InheritanceLink (list animal living)
                    (opencog-truth-value-simple 0.99 0.95)))))
      ;; Verify knowledge was added
      (should (= (opencog-atomspace-size) 5))
      ;; Verify links have correct truth values
      (should (> (opencog-truth-value-strength (opencog-atom-truth-value link1)) 0.9))
      ;; Perform deduction: Cat -> Animal -> LivingThing => Cat -> LivingThing
      (let ((deduced-tv (opencog-pln-deduction
                         (opencog-atom-truth-value link1)
                         (opencog-atom-truth-value link2))))
        ;; Deduced TV should be less than either premise
        (should (< (opencog-truth-value-strength deduced-tv)
                   (opencog-truth-value-strength (opencog-atom-truth-value link1))))))))

(ert-deftest emacogs-test-multi-agent-task ()
  "Test multi-agent task creation and assignment."
  (let ((agent-zero-agents (make-hash-table :test 'equal))
        (agent-zero-next-agent-id 0)
        (agent-zero-tasks nil))
    ;; Create agents with different capabilities
    (agent-zero-create "Reasoner" 'reasoning '(logic inference) #'ignore)
    (agent-zero-create "Learner" 'learning '(pattern-mining) #'ignore)
    ;; Create a task requiring logic
    (let ((task (agent-zero-create-task
                 "InferenceTask"
                 'inference
                 '(logic)
                 #'ignore)))
      (should task)
      (should (= (hash-table-count agent-zero-agents) 2)))))

;;; ===========================================================================
;;; Utility function for testing
;;; ===========================================================================

(defun emacogs-test-run-all ()
  "Run all Emacogs tests."
  (interactive)
  (ert-run-tests-interactively "emacogs-"))

(provide 'emacogs-test)
;;; emacogs-test.el ends here
