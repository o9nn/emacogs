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

;; Add lisp directory to load path when running from file
;; When evaluating interactively, ensure load-path is set manually or
;; run with: emacs --batch -L /path/to/lisp -l emacogs-test.el
(when load-file-name
  (add-to-list 'load-path (expand-file-name "../../lisp" (file-name-directory load-file-name))))

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
    (should (opencog-pattern-match-atom '\?x node))
    (should (opencog-pattern-match-atom '\?any node))))

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
        (agent-zero-next-id 0))
    (let ((agent (agent-zero-create "TestAgent" 'worker '(reasoning) #'ignore)))
      (should (agent-zero-agent-p agent))
      (should (equal (agent-zero-agent-name agent) "TestAgent"))
      (should (eq (agent-zero-agent-type agent) 'worker))
      (should (equal (agent-zero-agent-capabilities agent) '(reasoning))))))

(ert-deftest emacogs-test-agent-has-capabilities ()
  "Test agent capability checking."
  (let ((agent-zero-agents (make-hash-table :test 'equal))
        (agent-zero-next-id 0))
    (let ((agent (agent-zero-create "TestAgent" 'worker '(reasoning learning) #'ignore)))
      (should (agent-zero-agent-has-capabilities agent '(reasoning)))
      (should (agent-zero-agent-has-capabilities agent '(learning)))
      (should (agent-zero-agent-has-capabilities agent '(reasoning learning)))
      (should-not (agent-zero-agent-has-capabilities agent '(unknown))))))

(ert-deftest emacogs-test-agent-message-send ()
  "Test agent message sending."
  (let ((agent-zero-agents (make-hash-table :test 'equal))
        (agent-zero-next-id 0))
    (let* ((sender (agent-zero-create "Sender" 'worker '(reasoning) #'ignore))
           (receiver (agent-zero-create "Receiver" 'worker '(learning) #'ignore)))
      (agent-zero-send-message (agent-zero-agent-id sender)
                               (agent-zero-agent-id receiver)
                               'inform
                               "Hello")
      (let ((inbox (agent-zero-agent-inbox receiver)))
        (should (= (length inbox) 1))
        (should (agent-zero-message-p (car inbox)))
        (should (equal (agent-zero-message-content (car inbox)) "Hello"))))))

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
        (agent-zero-next-id 0)
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
;;; Persistence Tests
;;; ===========================================================================

(require 'opencog-persistence)

(ert-deftest emacogs-test-persistence-serialize-atom ()
  "Test atom serialization."
  (let ((node (opencog-atom-create-node 'ConceptNode "Test"
                                        (opencog-truth-value-simple 0.8 0.7))))
    (let ((serialized (opencog-persistence-serialize-atom node)))
      (should (eq (car serialized) 'atom))
      (should (eq (plist-get (cdr serialized) :type) 'ConceptNode))
      (should (equal (plist-get (cdr serialized) :name) "Test")))))

(ert-deftest emacogs-test-persistence-deserialize-atom ()
  "Test atom deserialization."
  (let ((sexp '(atom :type ConceptNode :name "TestAtom"
                     :outgoing nil
                     :truth-value (:strength 0.9 :confidence 0.8)
                     :attention-value nil)))
    (let ((atom (opencog-persistence-deserialize-atom sexp)))
      (should (opencog-atom-p atom))
      (should (eq (opencog-atom-type atom) 'ConceptNode))
      (should (equal (opencog-atom-name atom) "TestAtom"))
      (should (= (opencog-truth-value-strength (opencog-atom-truth-value atom)) 0.9)))))

(ert-deftest emacogs-test-persistence-serialize-roundtrip ()
  "Test that serialize/deserialize is a roundtrip."
  (let* ((node (opencog-atom-create-node 'PredicateNode "TestPred"
                                         (opencog-truth-value-simple 0.5 0.5)))
         (serialized (opencog-persistence-serialize-atom node))
         (deserialized (opencog-persistence-deserialize-atom serialized)))
    (should (eq (opencog-atom-type node) (opencog-atom-type deserialized)))
    (should (equal (opencog-atom-name node) (opencog-atom-name deserialized)))))

(ert-deftest emacogs-test-persistence-save-load-file ()
  "Test saving and loading atomspace to/from file."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq))
        (test-file (make-temp-file "emacogs-test" nil ".el")))
    (unwind-protect
        (progn
          ;; Add some atoms
          (opencog-atomspace-add
           (opencog-atom-create-node 'ConceptNode "Alpha"))
          (opencog-atomspace-add
           (opencog-atom-create-node 'ConceptNode "Beta"))
          (should (= (opencog-atomspace-size) 2))
          ;; Save
          (opencog-persistence-save-atomspace test-file)
          (should (file-exists-p test-file))
          ;; Clear and verify empty
          (opencog-atomspace-clear)
          (should (= (opencog-atomspace-size) 0))
          ;; Load
          (let ((loaded (opencog-persistence-load-atomspace test-file)))
            (should (= loaded 2))
            (should (= (opencog-atomspace-size) 2))))
      ;; Cleanup
      (when (file-exists-p test-file)
        (delete-file test-file)))))

;;; ===========================================================================
;;; Learning Tests
;;; ===========================================================================

(require 'opencog-learning)

(ert-deftest emacogs-test-learning-increase-sti ()
  "Test increasing STI of an atom."
  (let* ((opencog-atomspace (make-hash-table :test 'equal))
         (opencog-atomspace-index (make-hash-table :test 'eq))
         (node (opencog-atom-create-node 'ConceptNode "Test")))
    (opencog-atomspace-add node)
    ;; Initial STI should be 0
    (should (= (opencog-attention-value-sti
                (or (opencog-atom-attention-value node)
                    (opencog-attention-value-create :sti 0 :lti 0 :vlti 0)))
               0))
    ;; Increase STI
    (opencog-learning--increase-sti node 50)
    (should (= (opencog-attention-value-sti (opencog-atom-attention-value node)) 50))))

(ert-deftest emacogs-test-learning-decay-importance ()
  "Test importance decay."
  (let* ((opencog-atomspace (make-hash-table :test 'equal))
         (opencog-atomspace-index (make-hash-table :test 'eq))
         (opencog-learning-importance-decay-rate 0.1)
         (node (opencog-atom-create-node 'ConceptNode "Test")))
    (setf (opencog-atom-attention-value node)
          (opencog-attention-value-create :sti 100 :lti 0 :vlti 0))
    (opencog-atomspace-add node)
    ;; Decay
    (opencog-learning-decay-importance)
    ;; STI should be reduced by 10%
    (let ((sti (opencog-attention-value-sti (opencog-atom-attention-value node))))
      (should (< sti 100))
      (should (> sti 85)))))

(ert-deftest emacogs-test-learning-mine-patterns ()
  "Test pattern mining."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    ;; Create multiple similar links
    (let ((a (opencog-atom-create-node 'ConceptNode "A"))
          (b (opencog-atom-create-node 'ConceptNode "B"))
          (c (opencog-atom-create-node 'ConceptNode "C"))
          (d (opencog-atom-create-node 'ConceptNode "D")))
      (opencog-atomspace-add a)
      (opencog-atomspace-add b)
      (opencog-atomspace-add c)
      (opencog-atomspace-add d)
      (opencog-atomspace-add
       (opencog-atom-create-link 'InheritanceLink (list a b)))
      (opencog-atomspace-add
       (opencog-atom-create-link 'InheritanceLink (list c d)))
      (opencog-atomspace-add
       (opencog-atom-create-link 'InheritanceLink (list a c)))
      ;; Mine patterns
      (let ((patterns (opencog-learning-mine-patterns 2)))
        ;; Should find InheritanceLink with arity 2 as frequent
        (should (> (length patterns) 0))
        (should (cl-some (lambda (p) (eq (caar p) 'InheritanceLink))
                         patterns))))))

(ert-deftest emacogs-test-learning-hebbian-update ()
  "Test Hebbian learning update."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq))
        (opencog-learning--statistics '(:patterns-mined 0 :atoms-forgotten 0
                                        :attention-updates 0 :hebbian-updates 0)))
    (let* ((a (opencog-atom-create-node 'ConceptNode "A"))
           (b (opencog-atom-create-node 'ConceptNode "B"))
           (link (opencog-atom-create-link 'SimilarityLink (list a b)
                                           (opencog-truth-value-simple 0.5 0.5))))
      ;; Set high attention on nodes
      (setf (opencog-atom-attention-value a)
            (opencog-attention-value-create :sti 100 :lti 0 :vlti 0))
      (setf (opencog-atom-attention-value b)
            (opencog-attention-value-create :sti 100 :lti 0 :vlti 0))
      (opencog-atomspace-add a)
      (opencog-atomspace-add b)
      (opencog-atomspace-add link)
      ;; Apply Hebbian update
      (opencog-learning-hebbian-update link)
      ;; Strength should increase
      (should (> (opencog-truth-value-strength (opencog-atom-truth-value link)) 0.5)))))

;;; ===========================================================================
;;; Visualization Tests
;;; ===========================================================================

(require 'opencog-visualization)

(ert-deftest emacogs-test-visualization-atom-display ()
  "Test atom display string generation."
  (let ((node (opencog-atom-create-node 'ConceptNode "TestNode"
                                        (opencog-truth-value-simple 0.9 0.8))))
    (let ((display (opencog-visualization--atom-display node)))
      (should (stringp display))
      (should (string-match-p "ConceptNode" display))
      (should (string-match-p "TestNode" display))
      (should (string-match-p "0.90" display)))))

(ert-deftest emacogs-test-visualization-attention-bar ()
  "Test attention bar creation."
  (should (equal (opencog-visualization--attention-bar 60) "████"))
  (should (equal (opencog-visualization--attention-bar 30) "███░"))
  (should (equal (opencog-visualization--attention-bar 15) "██░░"))
  (should (equal (opencog-visualization--attention-bar 5) "█░░░"))
  (should (equal (opencog-visualization--attention-bar -5) "░░░░")))

(ert-deftest emacogs-test-visualization-agent-state-icon ()
  "Test agent state icons."
  (should (equal (opencog-visualization--agent-state-icon 'running) "▶"))
  (should (equal (opencog-visualization--agent-state-icon 'idle) "⏸"))
  (should (equal (opencog-visualization--agent-state-icon 'waiting) "⏳"))
  (should (equal (opencog-visualization--agent-state-icon 'terminated) "⏹")))

(ert-deftest emacogs-test-visualization-performance-bar ()
  "Test performance bar creation."
  (should (equal (opencog-visualization--performance-bar 1.0) "██████████"))
  (should (equal (opencog-visualization--performance-bar 0.5) "█████░░░░░"))
  (should (equal (opencog-visualization--performance-bar 0.0) "░░░░░░░░░░")))

;;; ===========================================================================
;;; Network Tests
;;; ===========================================================================

(require 'opencog-network)

(ert-deftest emacogs-test-network-peer-creation ()
  "Test peer structure creation."
  (let ((peer (opencog-network-peer-create
               :id "peer-001"
               :host "localhost"
               :port 9090
               :state 'disconnected)))
    (should (opencog-network-peer-p peer))
    (should (equal (opencog-network-peer-id peer) "peer-001"))
    (should (equal (opencog-network-peer-host peer) "localhost"))
    (should (= (opencog-network-peer-port peer) 9090))
    (should (eq (opencog-network-peer-state peer) 'disconnected))))

(ert-deftest emacogs-test-network-message-creation ()
  "Test message structure creation."
  (let ((msg (opencog-network-message-create
              :type 'sync-request
              :sender "node-001"
              :timestamp 1234567890.0
              :payload '(:data "test"))))
    (should (opencog-network-message-p msg))
    (should (eq (opencog-network-message-type msg) 'sync-request))
    (should (equal (opencog-network-message-sender msg) "node-001"))))

(ert-deftest emacogs-test-network-delta-creation ()
  "Test delta structure creation."
  (let ((delta (opencog-network-delta-create
                :added '(atom1 atom2)
                :modified nil
                :removed '(key1)
                :timestamp 1234567890.0
                :source-id "peer-001")))
    (should (opencog-network-delta-p delta))
    (should (= (length (opencog-network-delta-added delta)) 2))
    (should (= (length (opencog-network-delta-removed delta)) 1))))

(ert-deftest emacogs-test-network-vector-clock-increment ()
  "Test vector clock increment operation."
  (let ((opencog-network-local-id "node-001")
        (opencog-network-vector-clock nil))
    (opencog-network-vector-clock-init)
    (should (= (cdr (assoc "node-001" opencog-network-vector-clock)) 0))
    (opencog-network-vector-clock-increment)
    (should (= (cdr (assoc "node-001" opencog-network-vector-clock)) 1))
    (opencog-network-vector-clock-increment)
    (should (= (cdr (assoc "node-001" opencog-network-vector-clock)) 2))))

(ert-deftest emacogs-test-network-vector-clock-merge ()
  "Test vector clock merge operation."
  (let ((clock1 '(("A" . 2) ("B" . 3)))
        (clock2 '(("A" . 1) ("B" . 4) ("C" . 2))))
    (let ((merged (opencog-network-vector-clock-merge clock1 clock2)))
      ;; A should take max of 2 and 1 = 2
      (should (= (cdr (assoc "A" merged)) 2))
      ;; B should take max of 3 and 4 = 4
      (should (= (cdr (assoc "B" merged)) 4))
      ;; C should be included from clock2
      (should (= (cdr (assoc "C" merged)) 2)))))

;;; ===========================================================================
;;; REPL Tests
;;; ===========================================================================

(require 'emacogs-repl)

(ert-deftest emacogs-test-repl-cmd-node ()
  "Test REPL node command."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    (let ((result (emacogs-repl--cmd-node 'ConceptNode "TestREPL")))
      (should (stringp result))
      (should (string-match-p "Created:" result))
      (should (= (opencog-atomspace-size) 1)))))

(ert-deftest emacogs-test-repl-cmd-atoms ()
  "Test REPL atoms command."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    ;; Empty atomspace
    (should (equal (emacogs-repl--cmd-atoms) "Atomspace is empty"))
    ;; With atoms
    (opencog-atomspace-add (opencog-atom-create-node 'ConceptNode "A"))
    (let ((result (emacogs-repl--cmd-atoms)))
      (should (string-match-p "ConceptNode" result)))))

(ert-deftest emacogs-test-repl-cmd-stats ()
  "Test REPL stats command."
  (let ((opencog-atomspace (make-hash-table :test 'equal))
        (opencog-atomspace-index (make-hash-table :test 'eq)))
    (opencog-atomspace-add (opencog-atom-create-node 'ConceptNode "A"))
    (let ((result (emacogs-repl--cmd-stats)))
      (should (stringp result))
      (should (string-match-p "Atoms:" result)))))

(ert-deftest emacogs-test-repl-cmd-tv ()
  "Test REPL truth value command."
  (let ((tv (emacogs-repl--cmd-tv 0.8 0.9)))
    (should (opencog-truth-value-p tv))
    (should (= (opencog-truth-value-strength tv) 0.8))
    (should (= (opencog-truth-value-confidence tv) 0.9))))

(ert-deftest emacogs-test-repl-cmd-help ()
  "Test REPL help command."
  (let ((result (emacogs-repl--cmd-help)))
    (should (stringp result))
    (should (string-match-p "Commands:" result))
    (should (string-match-p "node" result))))

(ert-deftest emacogs-test-repl-completion-candidates ()
  "Test REPL completion candidates."
  (let ((candidates (emacogs-repl--completion-candidates)))
    (should (member "node" candidates))
    (should (member "link" candidates))
    (should (member "query" candidates))
    (should (member "help" candidates))))

;;; ===========================================================================
;;; Benchmark Tests
;;; ===========================================================================

(require 'emacogs-benchmark)

(ert-deftest emacogs-test-benchmark-result-creation ()
  "Test benchmark result structure creation."
  (let ((result (emacogs-benchmark-result-create
                 :name "test-benchmark"
                 :iterations 100
                 :total-time 1.0
                 :mean-time 0.01
                 :min-time 0.005
                 :max-time 0.02
                 :throughput 100.0)))
    (should (emacogs-benchmark-result-p result))
    (should (equal (emacogs-benchmark-result-name result) "test-benchmark"))
    (should (= (emacogs-benchmark-result-iterations result) 100))))

(ert-deftest emacogs-test-benchmark-format-time ()
  "Test time formatting functions."
  ;; Nanoseconds
  (should (string-match-p "ns" (emacogs-benchmark-format-time 0.0000001)))
  ;; Microseconds
  (should (string-match-p "µs" (emacogs-benchmark-format-time 0.0001)))
  ;; Milliseconds
  (should (string-match-p "ms" (emacogs-benchmark-format-time 0.01)))
  ;; Seconds
  (should (string-match-p "s" (emacogs-benchmark-format-time 1.5))))

(ert-deftest emacogs-test-benchmark-format-throughput ()
  "Test throughput formatting functions."
  ;; Mega ops/s
  (should (string-match-p "M ops/s" (emacogs-benchmark-format-throughput 1500000)))
  ;; Kilo ops/s
  (should (string-match-p "K ops/s" (emacogs-benchmark-format-throughput 15000)))
  ;; Regular ops/s
  (should (string-match-p "ops/s" (emacogs-benchmark-format-throughput 500))))

(ert-deftest emacogs-test-benchmark-measure ()
  "Test benchmark measure function."
  (let ((emacogs-benchmark-default-iterations 10)
        (emacogs-benchmark-warmup-iterations 2)
        (counter 0))
    (let ((result (emacogs-benchmark-measure
                   (lambda () (cl-incf counter))
                   10)))
      (should (emacogs-benchmark-result-p result))
      (should (= (emacogs-benchmark-result-iterations result) 10))
      (should (> (emacogs-benchmark-result-total-time result) 0))
      (should (> (emacogs-benchmark-result-throughput result) 0)))))

(ert-deftest emacogs-test-benchmark-compare ()
  "Test benchmark comparison."
  (let ((result1 (emacogs-benchmark-result-create
                  :name "fast"
                  :mean-time 0.001))
        (result2 (emacogs-benchmark-result-create
                  :name "slow"
                  :mean-time 0.01)))
    (let ((comparison (emacogs-benchmark-compare result1 result2)))
      ;; result1 is faster since mean1 < mean2
      (should (= (plist-get comparison :faster) 1))
      ;; ratio = mean2/mean1 = 10, showing result2 is 10x slower
      (should (= (plist-get comparison :ratio) 10.0))
      ;; speedup = mean1/mean2 = 0.1, showing result1 takes 1/10 the time
      (should (= (plist-get comparison :speedup) 0.1)))))

;;; ===========================================================================
;;; Utility function for testing
;;; ===========================================================================

(defun emacogs-test-run-all ()
  "Run all Emacogs tests."
  (interactive)
  (ert-run-tests-interactively "emacogs-"))

(provide 'emacogs-test)
;;; emacogs-test.el ends here
