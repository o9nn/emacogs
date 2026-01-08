;;; agent-zero.el --- Multi-Agent Autonomous Orchestration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Keywords: ai, agents, orchestration, autonomous
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

;; Agent-Zero: Multi-agent autonomous orchestration workbench for
;; modular deployment of cognitive agents.
;;
;; Features:
;; - Agent lifecycle management
;; - Task scheduling and distribution
;; - Inter-agent communication protocol
;; - Autonomous decision-making
;; - Resource allocation
;; - Agent coordination and collaboration

;;; Code:

(require 'cl-lib)
(require 'opencog-atomspace)

;;; Agent Structure

(cl-defstruct (agent-zero-agent
               (:constructor agent-zero-agent-create)
               (:copier nil))
  "Autonomous agent structure."
  (id nil :type symbol
      :documentation "Unique agent identifier")
  (name "" :type string
        :documentation "Human-readable agent name")
  (type 'generic :type symbol
        :documentation "Agent type/role")
  (state 'idle :type symbol
         :documentation "Current state: idle, running, waiting, terminated")
  (capabilities nil :type list
                :documentation "List of agent capabilities")
  (goals nil :type list
         :documentation "Current goals")
  (beliefs nil :type list
           :documentation "Agent beliefs (atomspace references)")
  (memory nil :type list
          :documentation "Agent memory")
  (inbox nil :type list
         :documentation "Message inbox")
  (priority 5 :type integer
            :documentation "Agent priority (1-10)")
  (performance 1.0 :type float
               :documentation "Performance metric")
  (created-time nil :type (or null float)
                :documentation "Creation timestamp")
  (last-active nil :type (or null float)
               :documentation "Last activity timestamp")
  (process-fn nil :type (or null function)
              :documentation "Main processing function"))

;;; Agent Registry

(defvar agent-zero-agents (make-hash-table :test 'eq)
  "Registry of all active agents.
Note: Not thread-safe. Access should be synchronized if using
concurrent operations.")

(defvar agent-zero-next-id 1
  "Next agent ID to assign.")

(defun agent-zero-register-agent (agent)
  "Register AGENT in the system."
  (puthash (agent-zero-agent-id agent) agent agent-zero-agents)
  agent)

(defun agent-zero-unregister-agent (agent-id)
  "Unregister agent with AGENT-ID."
  (remhash agent-id agent-zero-agents))

(defun agent-zero-get-agent (agent-id)
  "Get agent by AGENT-ID."
  (gethash agent-id agent-zero-agents))

(defun agent-zero-list-agents ()
  "List all active agents."
  (let (agents)
    (maphash (lambda (_id agent) (push agent agents)) agent-zero-agents)
    agents))

;;; Agent Creation

(defun agent-zero-create (name type capabilities process-fn)
  "Create a new agent with NAME, TYPE, CAPABILITIES, and PROCESS-FN."
  (let* ((id (intern (format "agent-%d" agent-zero-next-id)))
         (agent (agent-zero-agent-create
                 :id id
                 :name name
                 :type type
                 :capabilities capabilities
                 :process-fn process-fn
                 :created-time (float-time)
                 :last-active (float-time))))
    (cl-incf agent-zero-next-id)
    (agent-zero-register-agent agent)))

;;; Agent Communication

(cl-defstruct (agent-zero-message
               (:constructor agent-zero-message-create)
               (:copier nil))
  "Inter-agent message."
  (from nil :type symbol
        :documentation "Sender agent ID")
  (to nil :type symbol
      :documentation "Recipient agent ID")
  (type 'inform :type symbol
        :documentation "Message type: inform, request, command, query")
  (content nil :type t
           :documentation "Message content")
  (timestamp nil :type float
             :documentation "Message timestamp")
  (priority 5 :type integer
            :documentation "Message priority"))

(defun agent-zero-send-message (from to type content &optional priority)
  "Send message FROM agent TO agent with TYPE and CONTENT."
  (let* ((msg (agent-zero-message-create
               :from from
               :to to
               :type type
               :content content
               :timestamp (float-time)
               :priority (or priority 5)))
         (recipient (agent-zero-get-agent to)))
    (when recipient
      (push msg (agent-zero-agent-inbox recipient))
      msg)))

(defun agent-zero-broadcast-message (from type content)
  "Broadcast message FROM agent with TYPE and CONTENT to all agents."
  (let (messages)
    (maphash (lambda (id _agent)
               (unless (eq id from)
                 (push (agent-zero-send-message from id type content) messages)))
             agent-zero-agents)
    messages))

(defun agent-zero-receive-messages (agent)
  "Retrieve and clear messages for AGENT."
  (prog1 (nreverse (agent-zero-agent-inbox agent))
    (setf (agent-zero-agent-inbox agent) nil)))

;;; Task Management

(cl-defstruct (agent-zero-task
               (:constructor agent-zero-task-create)
               (:copier nil))
  "Task structure for agent execution."
  (id nil :type symbol
      :documentation "Task identifier")
  (description "" :type string
               :documentation "Task description")
  (type 'generic :type symbol
        :documentation "Task type")
  (priority 5 :type integer
            :documentation "Task priority")
  (required-capabilities nil :type list
                         :documentation "Required capabilities")
  (state 'pending :type symbol
         :documentation "Task state: pending, assigned, running, completed, failed")
  (assigned-agent nil :type (or null symbol)
                  :documentation "Assigned agent ID")
  (created-time nil :type float
                :documentation "Creation time")
  (completed-time nil :type (or null float)
                  :documentation "Completion time")
  (result nil :type t
          :documentation "Task result")
  (action nil :type (or null function)
          :documentation "Task action function"))

(defvar agent-zero-task-queue nil
  "Global task queue.")

(defvar agent-zero-next-task-id 1
  "Next task ID.")

(defun agent-zero-create-task (description type required-capabilities action &optional priority)
  "Create task with DESCRIPTION, TYPE, REQUIRED-CAPABILITIES, and ACTION function."
  (let ((task (agent-zero-task-create
               :id (intern (format "task-%d" agent-zero-next-task-id))
               :description description
               :type type
               :required-capabilities required-capabilities
               :action action
               :priority (or priority 5)
               :created-time (float-time))))
    (cl-incf agent-zero-next-task-id)
    (push task agent-zero-task-queue)
    task))

(defun agent-zero-assign-task (task agent)
  "Assign TASK to AGENT."
  (setf (agent-zero-task-assigned-agent task) (agent-zero-agent-id agent))
  (setf (agent-zero-task-state task) 'assigned)
  (push task (agent-zero-agent-goals agent))
  task)

;;; Task Scheduler

(defun agent-zero-schedule-tasks ()
  "Schedule pending tasks to available agents."
  (let ((pending-tasks (cl-remove-if-not
                        (lambda (task)
                          (eq (agent-zero-task-state task) 'pending))
                        agent-zero-task-queue)))
    (dolist (task (sort pending-tasks
                        (lambda (a b)
                          (> (agent-zero-task-priority a)
                             (agent-zero-task-priority b)))))
      (let ((suitable-agent (agent-zero-find-agent-for-task task)))
        (when suitable-agent
          (agent-zero-assign-task task suitable-agent))))))

(defun agent-zero-find-agent-for-task (task)
  "Find the most suitable agent for TASK."
  (let ((required-caps (agent-zero-task-required-capabilities task))
        (best-agent nil)
        (best-score -1.0))
    (maphash (lambda (_id agent)
               (when (and (eq (agent-zero-agent-state agent) 'idle)
                          (agent-zero-agent-has-capabilities agent required-caps))
                 (let ((score (agent-zero-agent-performance agent)))
                   (when (> score best-score)
                     (setq best-agent agent)
                     (setq best-score score)))))
             agent-zero-agents)
    best-agent))

(defun agent-zero-agent-has-capabilities (agent required-capabilities)
  "Check if AGENT has all REQUIRED-CAPABILITIES."
  (cl-every (lambda (cap)
              (member cap (agent-zero-agent-capabilities agent)))
            required-capabilities))

;;; Agent Execution

(defun agent-zero-run-agent (agent)
  "Run AGENT's processing cycle."
  (setf (agent-zero-agent-state agent) 'running)
  (setf (agent-zero-agent-last-active agent) (float-time))
  
  ;; Process messages
  (let ((messages (agent-zero-receive-messages agent)))
    (dolist (msg messages)
      (agent-zero-process-message agent msg)))
  
  ;; Execute goals/tasks
  (dolist (task (agent-zero-agent-goals agent))
    (when (eq (agent-zero-task-state task) 'assigned)
      (agent-zero-execute-task agent task)))
  
  ;; Call agent's process function
  (when (agent-zero-agent-process-fn agent)
    (funcall (agent-zero-agent-process-fn agent) agent))
  
  (setf (agent-zero-agent-state agent) 'idle))

(defun agent-zero-execute-task (agent task)
  "Execute TASK with AGENT."
  (setf (agent-zero-task-state task) 'running)
  (condition-case err
      (progn
        (when (agent-zero-task-action task)
          (setf (agent-zero-task-result task)
                (funcall (agent-zero-task-action task) agent task)))
        (setf (agent-zero-task-state task) 'completed)
        (setf (agent-zero-task-completed-time task) (float-time))
        (setf (agent-zero-agent-goals agent)
              (delq task (agent-zero-agent-goals agent)))
        ;; Update performance
        (setf (agent-zero-agent-performance agent)
              (* (agent-zero-agent-performance agent) 1.05)))
    (error
     (setf (agent-zero-task-state task) 'failed)
     (setf (agent-zero-task-result task) (error-message-string err))
     ;; Decrease performance
     (setf (agent-zero-agent-performance agent)
           (* (agent-zero-agent-performance agent) 0.95)))))

(defun agent-zero-process-message (agent message)
  "Process MESSAGE for AGENT."
  (pcase (agent-zero-message-type message)
    ('request
     ;; Handle request message
     (when (agent-zero-agent-process-fn agent)
       (funcall (agent-zero-agent-process-fn agent) agent message)))
    ('command
     ;; Execute command
     (agent-zero-execute-command agent (agent-zero-message-content message)))
    ('inform
     ;; Update beliefs
     (push (agent-zero-message-content message)
           (agent-zero-agent-beliefs agent)))
    ('query
     ;; Respond to query
     (let ((response (agent-zero-query-agent agent
                                             (agent-zero-message-content message))))
       (agent-zero-send-message (agent-zero-agent-id agent)
                                (agent-zero-message-from message)
                                'inform
                                response)))))

(defun agent-zero-execute-command (agent command)
  "Execute COMMAND for AGENT."
  (pcase command
    (`(stop) (setf (agent-zero-agent-state agent) 'terminated))
    (`(priority ,new-priority)
     (setf (agent-zero-agent-priority agent) new-priority))
    (_ (message "Unknown command: %S" command))))

(defun agent-zero-query-agent (agent query)
  "Process QUERY for AGENT and return response."
  (pcase query
    ('state (agent-zero-agent-state agent))
    ('capabilities (agent-zero-agent-capabilities agent))
    ('goals (agent-zero-agent-goals agent))
    (_ nil)))

;;; Orchestration Loop

(defvar agent-zero-orchestration-active nil
  "Whether orchestration is active.")

(defvar agent-zero-orchestration-timer nil
  "Timer for orchestration loop.")

(defun agent-zero-start-orchestration ()
  "Start the agent orchestration system."
  (interactive)
  (unless agent-zero-orchestration-active
    (setq agent-zero-orchestration-active t)
    (setq agent-zero-orchestration-timer
          (run-with-timer 0 1.0 #'agent-zero-orchestration-tick))
    (message "Agent-Zero orchestration started")))

(defun agent-zero-stop-orchestration ()
  "Stop the agent orchestration system."
  (interactive)
  (when agent-zero-orchestration-timer
    (cancel-timer agent-zero-orchestration-timer)
    (setq agent-zero-orchestration-timer nil))
  (setq agent-zero-orchestration-active nil)
  (message "Agent-Zero orchestration stopped"))

(defun agent-zero-orchestration-tick ()
  "Execute one orchestration cycle."
  (when agent-zero-orchestration-active
    ;; Schedule tasks
    (agent-zero-schedule-tasks)
    
    ;; Run agents
    (maphash (lambda (_id agent)
               (when (and (not (eq (agent-zero-agent-state agent) 'terminated))
                          (or (agent-zero-agent-goals agent)
                              (agent-zero-agent-inbox agent)))
                 (agent-zero-run-agent agent)))
             agent-zero-agents)))

;;; Utilities

(defun agent-zero-status ()
  "Display agent system status."
  (interactive)
  (let ((buf (get-buffer-create "*Agent-Zero Status*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Agent-Zero Multi-Agent System\n")
      (insert "===============================\n\n")
      (insert (format "Orchestration: %s\n"
                      (if agent-zero-orchestration-active "ACTIVE" "INACTIVE")))
      (insert (format "Active agents: %d\n" (hash-table-count agent-zero-agents)))
      (insert (format "Pending tasks: %d\n\n"
                      (length (cl-remove-if-not
                               (lambda (task)
                                 (eq (agent-zero-task-state task) 'pending))
                               agent-zero-task-queue))))
      (insert "Agents:\n")
      (maphash (lambda (_id agent)
                 (insert (format "  %s (%s) - State: %s, Priority: %d, Performance: %.2f\n"
                                 (agent-zero-agent-name agent)
                                 (agent-zero-agent-type agent)
                                 (agent-zero-agent-state agent)
                                 (agent-zero-agent-priority agent)
                                 (agent-zero-agent-performance agent))))
               agent-zero-agents))
    (display-buffer buf)))

(provide 'agent-zero)
;;; agent-zero.el ends here
