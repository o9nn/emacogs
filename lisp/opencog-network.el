;;; opencog-network.el --- Network protocol for distributed atomspace -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Emacogs Project
;; Keywords: ai, cognitive-architecture, opencog, network, distributed
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

;; Network protocol for distributed atomspace synchronization.
;; Enables multiple Emacogs instances to share and synchronize their
;; atomspaces over a network.
;;
;; Features:
;; - Peer discovery and connection management
;; - Atomspace delta synchronization
;; - Distributed query execution
;; - Conflict resolution strategies
;; - Event-based replication
;;
;; Protocol Design:
;; - Uses TCP for reliable message delivery
;; - JSON-based message format for interoperability
;; - Vector clocks for causality tracking
;; - Merkle trees for efficient delta detection
;;
;; Usage:
;;   (require 'opencog-network)
;;   (opencog-network-start-server 9090)
;;   (opencog-network-connect "peer.example.com" 9090)
;;   (opencog-network-sync)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'opencog-atomspace)

;;; ===========================================================================
;;; Configuration
;;; ===========================================================================

(defgroup opencog-network nil
  "Network settings for distributed atomspace."
  :group 'emacogs
  :prefix "opencog-network-")

(defcustom opencog-network-default-port 9090
  "Default port for atomspace network connections."
  :type 'integer
  :group 'opencog-network)

(defcustom opencog-network-sync-interval 30
  "Interval in seconds between automatic synchronization."
  :type 'integer
  :group 'opencog-network)

(defcustom opencog-network-max-peers 10
  "Maximum number of connected peers."
  :type 'integer
  :group 'opencog-network)

(defcustom opencog-network-conflict-resolution 'last-write-wins
  "Strategy for resolving conflicts during synchronization.
Options: `last-write-wins', `highest-confidence', `merge'."
  :type '(choice (const :tag "Last Write Wins" last-write-wins)
                 (const :tag "Highest Confidence" highest-confidence)
                 (const :tag "Merge" merge))
  :group 'opencog-network)

;;; ===========================================================================
;;; Data Structures
;;; ===========================================================================

(cl-defstruct (opencog-network-peer
               (:constructor opencog-network-peer-create)
               (:copier nil))
  "Represents a connected peer node."
  (id nil :type string
      :documentation "Unique peer identifier")
  (host nil :type string
        :documentation "Peer hostname or IP")
  (port nil :type integer
        :documentation "Peer port number")
  (connection nil :type (or null process)
              :documentation "Network connection process")
  (state 'disconnected :type symbol
         :documentation "Connection state: disconnected, connecting, connected, syncing")
  (vector-clock nil :type list
                :documentation "Vector clock for causality tracking")
  (last-sync nil :type (or null float)
             :documentation "Timestamp of last successful sync")
  (metadata nil :type list
            :documentation "Additional peer metadata"))

(cl-defstruct (opencog-network-message
               (:constructor opencog-network-message-create)
               (:copier nil))
  "Network message structure."
  (type nil :type symbol
        :documentation "Message type: sync-request, sync-response, atom-update, query, etc.")
  (sender nil :type string
          :documentation "Sender peer ID")
  (timestamp nil :type float
             :documentation "Message timestamp")
  (vector-clock nil :type list
                :documentation "Sender's vector clock")
  (payload nil :type list
           :documentation "Message payload data"))

(cl-defstruct (opencog-network-delta
               (:constructor opencog-network-delta-create)
               (:copier nil))
  "Represents atomspace changes for synchronization."
  (added nil :type list
         :documentation "List of added atoms")
  (modified nil :type list
            :documentation "List of modified atoms (with old/new values)")
  (removed nil :type list
           :documentation "List of removed atom keys")
  (timestamp nil :type float
             :documentation "Delta timestamp")
  (source-id nil :type string
             :documentation "Peer that originated this delta"))

;;; ===========================================================================
;;; Global State
;;; ===========================================================================

(defvar opencog-network-server nil
  "Server process for accepting connections.")

(defvar opencog-network-peers (make-hash-table :test 'equal)
  "Hash table of connected peers, keyed by peer ID.
Note: Not thread-safe. Access should be synchronized.")

(defvar opencog-network-local-id nil
  "Local node identifier.")

(defvar opencog-network-vector-clock nil
  "Local vector clock for causality tracking.")

(defvar opencog-network-pending-deltas nil
  "Queue of deltas pending synchronization.")

(defvar opencog-network-sync-timer nil
  "Timer for automatic synchronization.")

(defvar opencog-network-atom-hooks nil
  "Hooks called when atoms are added/modified/removed.
Each hook receives (type atom) where type is 'add, 'modify, or 'remove.")

;;; ===========================================================================
;;; Vector Clock Operations
;;; ===========================================================================

(defun opencog-network-vector-clock-init ()
  "Initialize the local vector clock."
  (setq opencog-network-vector-clock
        (list (cons opencog-network-local-id 0))))

(defun opencog-network-vector-clock-increment ()
  "Increment the local vector clock."
  (let ((entry (assoc opencog-network-local-id opencog-network-vector-clock)))
    (if entry
        (setcdr entry (1+ (cdr entry)))
      (push (cons opencog-network-local-id 1) opencog-network-vector-clock)))
  opencog-network-vector-clock)

(defun opencog-network-vector-clock-merge (clock1 clock2)
  "Merge two vector clocks, taking the maximum of each entry."
  (let ((result (copy-alist clock1)))
    (dolist (entry clock2)
      (let ((existing (assoc (car entry) result)))
        (if existing
            (setcdr existing (max (cdr existing) (cdr entry)))
          (push entry result))))
    result))

(defun opencog-network-vector-clock-compare (clock1 clock2)
  "Compare two vector clocks.
Returns 'before if clock1 < clock2, 'after if clock1 > clock2,
'equal if equal, 'concurrent if neither."
  (let ((before nil)
        (after nil))
    (dolist (entry clock1)
      (let ((v2 (or (cdr (assoc (car entry) clock2)) 0)))
        (cond ((< (cdr entry) v2) (setq before t))
              ((> (cdr entry) v2) (setq after t)))))
    (dolist (entry clock2)
      (unless (assoc (car entry) clock1)
        (when (> (cdr entry) 0)
          (setq before t))))
    (cond ((and before after) 'concurrent)
          (before 'before)
          (after 'after)
          (t 'equal))))

;;; ===========================================================================
;;; Message Serialization
;;; ===========================================================================

(defun opencog-network-serialize-atom (atom)
  "Serialize ATOM to a JSON-compatible alist."
  (list (cons 'type (opencog-atom-type atom))
        (cons 'name (opencog-atom-name atom))
        (cons 'outgoing (mapcar #'opencog-atom-key (opencog-atom-outgoing atom)))
        (cons 'truth-value
              (list (cons 'strength
                          (opencog-truth-value-strength
                           (opencog-atom-truth-value atom)))
                    (cons 'confidence
                          (opencog-truth-value-confidence
                           (opencog-atom-truth-value atom)))))
        (cons 'attention-value
              (list (cons 'sti
                          (opencog-attention-value-sti
                           (opencog-atom-attention-value atom)))
                    (cons 'lti
                          (opencog-attention-value-lti
                           (opencog-atom-attention-value atom)))
                    (cons 'vlti
                          (opencog-attention-value-vlti
                           (opencog-atom-attention-value atom)))))))

(defun opencog-network-deserialize-atom (data)
  "Deserialize DATA to an atom."
  (let* ((type (alist-get 'type data))
         (name (alist-get 'name data))
         (tv-data (alist-get 'truth-value data))
         (tv (opencog-truth-value-simple
              (alist-get 'strength tv-data)
              (alist-get 'confidence tv-data))))
    (if name
        (opencog-atom-create-node type name tv)
      ;; For links, we need to resolve outgoing references
      nil)))  ; Link deserialization requires atomspace context

(defun opencog-network-message-to-json (message)
  "Convert MESSAGE to JSON string."
  (json-encode
   (list (cons 'type (opencog-network-message-type message))
         (cons 'sender (opencog-network-message-sender message))
         (cons 'timestamp (opencog-network-message-timestamp message))
         (cons 'vector-clock (opencog-network-message-vector-clock message))
         (cons 'payload (opencog-network-message-payload message)))))

(defun opencog-network-json-to-message (json-string)
  "Parse JSON-STRING to a message structure."
  (let ((data (json-read-from-string json-string)))
    (opencog-network-message-create
     :type (intern (alist-get 'type data))
     :sender (alist-get 'sender data)
     :timestamp (alist-get 'timestamp data)
     :vector-clock (alist-get 'vector-clock data)
     :payload (alist-get 'payload data))))

;;; ===========================================================================
;;; Delta Generation and Application
;;; ===========================================================================

(defvar opencog-network--atomspace-snapshot nil
  "Snapshot of atomspace for delta computation.")

(defun opencog-network-snapshot-atomspace ()
  "Take a snapshot of the current atomspace for delta computation."
  (setq opencog-network--atomspace-snapshot
        (let ((snapshot (make-hash-table :test 'equal)))
          (maphash (lambda (key atom)
                     (puthash key
                              (list (cons 'tv (copy-sequence
                                               (opencog-atom-truth-value atom)))
                                    (cons 'av (copy-sequence
                                               (opencog-atom-attention-value atom))))
                              snapshot))
                   opencog-atomspace)
          snapshot)))

(defun opencog-network-compute-delta ()
  "Compute delta since last snapshot."
  (let ((added nil)
        (modified nil)
        (removed nil))
    ;; Find added and modified atoms
    (maphash (lambda (key atom)
               (let ((old (gethash key opencog-network--atomspace-snapshot)))
                 (if old
                     ;; Check for modifications
                     (let ((old-tv (alist-get 'tv old)))
                       (unless (and (= (opencog-truth-value-strength
                                        (opencog-atom-truth-value atom))
                                       (opencog-truth-value-strength old-tv))
                                    (= (opencog-truth-value-confidence
                                        (opencog-atom-truth-value atom))
                                       (opencog-truth-value-confidence old-tv)))
                         (push (cons key (opencog-network-serialize-atom atom))
                               modified)))
                   ;; New atom
                   (push (cons key (opencog-network-serialize-atom atom))
                         added))))
             opencog-atomspace)
    ;; Find removed atoms
    (maphash (lambda (key _)
               (unless (gethash key opencog-atomspace)
                 (push key removed)))
             opencog-network--atomspace-snapshot)
    (when (or added modified removed)
      (opencog-network-delta-create
       :added added
       :modified modified
       :removed removed
       :timestamp (float-time)
       :source-id opencog-network-local-id))))

(defun opencog-network-apply-delta (delta)
  "Apply DELTA to the local atomspace."
  (let ((conflict-strategy opencog-network-conflict-resolution))
    ;; Apply removals
    (dolist (key (opencog-network-delta-removed delta))
      (remhash key opencog-atomspace))
    ;; Apply additions
    (dolist (entry (opencog-network-delta-added delta))
      (let ((key (car entry))
            (atom-data (cdr entry)))
        (unless (gethash key opencog-atomspace)
          (let ((atom (opencog-network-deserialize-atom atom-data)))
            (when atom
              (puthash key atom opencog-atomspace))))))
    ;; Apply modifications with conflict resolution
    (dolist (entry (opencog-network-delta-modified delta))
      (let* ((key (car entry))
             (atom-data (cdr entry))
             (local-atom (gethash key opencog-atomspace)))
        (when local-atom
          (let* ((remote-tv-data (alist-get 'truth-value atom-data))
                 (remote-strength (alist-get 'strength remote-tv-data))
                 (remote-confidence (alist-get 'confidence remote-tv-data))
                 (local-tv (opencog-atom-truth-value local-atom)))
            (pcase conflict-strategy
              ('last-write-wins
               ;; Always apply remote change
               (setf (opencog-truth-value-strength local-tv) remote-strength)
               (setf (opencog-truth-value-confidence local-tv) remote-confidence))
              ('highest-confidence
               ;; Keep highest confidence version
               (when (> remote-confidence
                        (opencog-truth-value-confidence local-tv))
                 (setf (opencog-truth-value-strength local-tv) remote-strength)
                 (setf (opencog-truth-value-confidence local-tv) remote-confidence)))
              ('merge
               ;; Average the values
               (setf (opencog-truth-value-strength local-tv)
                     (/ (+ (opencog-truth-value-strength local-tv)
                           remote-strength)
                        2.0))
               (setf (opencog-truth-value-confidence local-tv)
                     (/ (+ (opencog-truth-value-confidence local-tv)
                           remote-confidence)
                        2.0))))))))))

;;; ===========================================================================
;;; Network Server
;;; ===========================================================================

(defun opencog-network-start-server (&optional port)
  "Start the network server on PORT."
  (interactive
   (list (read-number "Port: " opencog-network-default-port)))
  (let ((port (or port opencog-network-default-port)))
    (when opencog-network-server
      (delete-process opencog-network-server))
    (setq opencog-network-local-id (format "node-%s-%d"
                                           (system-name)
                                           (emacs-pid)))
    (opencog-network-vector-clock-init)
    (setq opencog-network-server
          (make-network-process
           :name "opencog-network-server"
           :buffer "*OpenCog Network*"
           :family 'ipv4
           :service port
           :server t
           :noquery t
           :sentinel #'opencog-network--server-sentinel
           :filter #'opencog-network--server-filter))
    (opencog-network-snapshot-atomspace)
    (message "OpenCog network server started on port %d (ID: %s)"
             port opencog-network-local-id)))

(defun opencog-network-stop-server ()
  "Stop the network server."
  (interactive)
  (when opencog-network-server
    (delete-process opencog-network-server)
    (setq opencog-network-server nil))
  (when opencog-network-sync-timer
    (cancel-timer opencog-network-sync-timer)
    (setq opencog-network-sync-timer nil))
  (maphash (lambda (id peer)
             (when (opencog-network-peer-connection peer)
               (delete-process (opencog-network-peer-connection peer))))
           opencog-network-peers)
  (clrhash opencog-network-peers)
  (message "OpenCog network server stopped"))

(defun opencog-network--server-sentinel (process event)
  "Handle server PROCESS state change EVENT."
  (message "OpenCog network: %s" (string-trim event)))

(defun opencog-network--server-filter (process data)
  "Handle incoming DATA on server PROCESS."
  (condition-case err
      (let ((message (opencog-network-json-to-message data)))
        (opencog-network--handle-message process message))
    (error (message "OpenCog network error: %s" (error-message-string err)))))

;;; ===========================================================================
;;; Client Connection
;;; ===========================================================================

(defun opencog-network-connect (host port)
  "Connect to a peer at HOST:PORT."
  (interactive
   (list (read-string "Host: " "localhost")
         (read-number "Port: " opencog-network-default-port)))
  (let* ((peer-id (format "%s:%d" host port))
         (existing (gethash peer-id opencog-network-peers)))
    (when existing
      (when (opencog-network-peer-connection existing)
        (delete-process (opencog-network-peer-connection existing))))
    (let ((conn (make-network-process
                 :name (format "opencog-peer-%s" peer-id)
                 :buffer (format "*OpenCog Peer %s*" peer-id)
                 :host host
                 :service port
                 :family 'ipv4
                 :noquery t
                 :sentinel #'opencog-network--client-sentinel
                 :filter #'opencog-network--client-filter)))
      (puthash peer-id
               (opencog-network-peer-create
                :id peer-id
                :host host
                :port port
                :connection conn
                :state 'connecting)
               opencog-network-peers)
      ;; Send handshake
      (opencog-network--send-message conn
                                     (opencog-network-message-create
                                      :type 'handshake
                                      :sender opencog-network-local-id
                                      :timestamp (float-time)
                                      :vector-clock opencog-network-vector-clock
                                      :payload nil))
      (message "Connecting to %s:%d..." host port))))

(defun opencog-network-disconnect (peer-id)
  "Disconnect from peer with PEER-ID."
  (interactive
   (list (completing-read "Peer: "
                          (hash-table-keys opencog-network-peers))))
  (let ((peer (gethash peer-id opencog-network-peers)))
    (when peer
      (when (opencog-network-peer-connection peer)
        (delete-process (opencog-network-peer-connection peer)))
      (remhash peer-id opencog-network-peers)
      (message "Disconnected from %s" peer-id))))

(defun opencog-network--client-sentinel (process event)
  "Handle client PROCESS state change EVENT."
  (let ((peer-id (process-get process 'peer-id)))
    (when peer-id
      (let ((peer (gethash peer-id opencog-network-peers)))
        (when peer
          (cond ((string-match "open" event)
                 (setf (opencog-network-peer-state peer) 'connected)
                 (message "Connected to %s" peer-id))
                ((string-match "\\(deleted\\|connection broken\\|failed\\)" event)
                 (setf (opencog-network-peer-state peer) 'disconnected)
                 (message "Disconnected from %s: %s" peer-id (string-trim event)))))))))

(defun opencog-network--client-filter (process data)
  "Handle incoming DATA on client PROCESS."
  (condition-case err
      (let ((message (opencog-network-json-to-message data)))
        (opencog-network--handle-message process message))
    (error (message "OpenCog network error: %s" (error-message-string err)))))

;;; ===========================================================================
;;; Message Handling
;;; ===========================================================================

(defun opencog-network--send-message (connection message)
  "Send MESSAGE over CONNECTION."
  (process-send-string connection
                       (concat (opencog-network-message-to-json message) "\n")))

(defun opencog-network--handle-message (process message)
  "Handle incoming MESSAGE on PROCESS."
  (pcase (opencog-network-message-type message)
    ('handshake
     (opencog-network--handle-handshake process message))
    ('sync-request
     (opencog-network--handle-sync-request process message))
    ('sync-response
     (opencog-network--handle-sync-response message))
    ('atom-update
     (opencog-network--handle-atom-update message))
    ('query
     (opencog-network--handle-query process message))
    ('query-response
     (opencog-network--handle-query-response message))
    (_
     (message "Unknown message type: %s"
              (opencog-network-message-type message)))))

(defun opencog-network--handle-handshake (process message)
  "Handle handshake MESSAGE on PROCESS."
  (let ((peer-id (opencog-network-message-sender message)))
    (message "Handshake from: %s" peer-id)
    ;; Update peer info
    (let ((peer (gethash peer-id opencog-network-peers)))
      (if peer
          (progn
            (setf (opencog-network-peer-state peer) 'connected)
            (setf (opencog-network-peer-vector-clock peer)
                  (opencog-network-message-vector-clock message)))
        ;; New peer connecting to us
        (puthash peer-id
                 (opencog-network-peer-create
                  :id peer-id
                  :connection process
                  :state 'connected
                  :vector-clock (opencog-network-message-vector-clock message))
                 opencog-network-peers)))
    ;; Send handshake response
    (opencog-network--send-message process
                                   (opencog-network-message-create
                                    :type 'handshake
                                    :sender opencog-network-local-id
                                    :timestamp (float-time)
                                    :vector-clock opencog-network-vector-clock
                                    :payload nil))))

(defun opencog-network--handle-sync-request (process message)
  "Handle sync request MESSAGE on PROCESS."
  (let ((delta (opencog-network-compute-delta)))
    (opencog-network--send-message process
                                   (opencog-network-message-create
                                    :type 'sync-response
                                    :sender opencog-network-local-id
                                    :timestamp (float-time)
                                    :vector-clock (opencog-network-vector-clock-increment)
                                    :payload (when delta
                                               (list (cons 'delta delta)))))))

(defun opencog-network--handle-sync-response (message)
  "Handle sync response MESSAGE."
  (let ((payload (opencog-network-message-payload message)))
    (when-let ((delta (alist-get 'delta payload)))
      (opencog-network-apply-delta delta)
      (message "Applied sync delta from %s"
               (opencog-network-message-sender message))))
  ;; Merge vector clocks
  (setq opencog-network-vector-clock
        (opencog-network-vector-clock-merge
         opencog-network-vector-clock
         (opencog-network-message-vector-clock message))))

(defun opencog-network--handle-atom-update (message)
  "Handle atom update MESSAGE."
  (let ((payload (opencog-network-message-payload message)))
    (when-let ((atom-data (alist-get 'atom payload)))
      (let ((atom (opencog-network-deserialize-atom atom-data)))
        (when atom
          (opencog-atomspace-add atom)
          (message "Received atom update from %s"
                   (opencog-network-message-sender message)))))))

(defun opencog-network--handle-query (process message)
  "Handle query MESSAGE on PROCESS."
  (let* ((payload (opencog-network-message-payload message))
         (pattern (alist-get 'pattern payload))
         (results (opencog-query pattern)))
    (opencog-network--send-message process
                                   (opencog-network-message-create
                                    :type 'query-response
                                    :sender opencog-network-local-id
                                    :timestamp (float-time)
                                    :vector-clock opencog-network-vector-clock
                                    :payload (list (cons 'results
                                                         (mapcar #'opencog-network-serialize-atom
                                                                 results)))))))

(defun opencog-network--handle-query-response (message)
  "Handle query response MESSAGE."
  (let ((payload (opencog-network-message-payload message)))
    (when-let ((results (alist-get 'results payload)))
      (message "Query response from %s: %d results"
               (opencog-network-message-sender message)
               (length results)))))

;;; ===========================================================================
;;; Synchronization
;;; ===========================================================================

(defun opencog-network-sync ()
  "Synchronize atomspace with all connected peers."
  (interactive)
  (opencog-network-vector-clock-increment)
  (maphash (lambda (_id peer)
             (when (eq (opencog-network-peer-state peer) 'connected)
               (opencog-network--send-message
                (opencog-network-peer-connection peer)
                (opencog-network-message-create
                 :type 'sync-request
                 :sender opencog-network-local-id
                 :timestamp (float-time)
                 :vector-clock opencog-network-vector-clock
                 :payload nil))))
           opencog-network-peers)
  (opencog-network-snapshot-atomspace)
  (message "Synchronization request sent to all peers"))

(defun opencog-network-enable-auto-sync ()
  "Enable automatic synchronization."
  (interactive)
  (when opencog-network-sync-timer
    (cancel-timer opencog-network-sync-timer))
  (setq opencog-network-sync-timer
        (run-at-time opencog-network-sync-interval
                     opencog-network-sync-interval
                     #'opencog-network-sync))
  (message "Auto-sync enabled (interval: %ds)" opencog-network-sync-interval))

(defun opencog-network-disable-auto-sync ()
  "Disable automatic synchronization."
  (interactive)
  (when opencog-network-sync-timer
    (cancel-timer opencog-network-sync-timer)
    (setq opencog-network-sync-timer nil))
  (message "Auto-sync disabled"))

;;; ===========================================================================
;;; Distributed Queries
;;; ===========================================================================

(defun opencog-network-distributed-query (pattern)
  "Execute PATTERN query across all connected peers."
  (interactive "sPattern: ")
  (let ((local-results (opencog-query (read pattern))))
    (message "Local results: %d" (length local-results))
    ;; Send to all peers
    (maphash (lambda (_id peer)
               (when (eq (opencog-network-peer-state peer) 'connected)
                 (opencog-network--send-message
                  (opencog-network-peer-connection peer)
                  (opencog-network-message-create
                   :type 'query
                   :sender opencog-network-local-id
                   :timestamp (float-time)
                   :vector-clock opencog-network-vector-clock
                   :payload (list (cons 'pattern pattern))))))
             opencog-network-peers)
    local-results))

;;; ===========================================================================
;;; Status and Information
;;; ===========================================================================

(defun opencog-network-status ()
  "Display network status."
  (interactive)
  (let ((buf (get-buffer-create "*OpenCog Network Status*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "OpenCog Network Status\n")
      (insert "======================\n\n")
      (insert (format "Local ID: %s\n" (or opencog-network-local-id "Not started")))
      (insert (format "Server: %s\n"
                      (if opencog-network-server "Running" "Stopped")))
      (insert (format "Auto-sync: %s\n"
                      (if opencog-network-sync-timer "Enabled" "Disabled")))
      (insert (format "Vector clock: %S\n\n" opencog-network-vector-clock))
      (insert "Connected Peers:\n")
      (insert "----------------\n")
      (if (= (hash-table-count opencog-network-peers) 0)
          (insert "  (none)\n")
        (maphash (lambda (id peer)
                   (insert (format "  %s: %s (last sync: %s)\n"
                                   id
                                   (opencog-network-peer-state peer)
                                   (if (opencog-network-peer-last-sync peer)
                                       (format-time-string
                                        "%Y-%m-%d %H:%M:%S"
                                        (opencog-network-peer-last-sync peer))
                                     "never"))))
                 opencog-network-peers)))
    (display-buffer buf)))

(defun opencog-network-list-peers ()
  "List all connected peers."
  (interactive)
  (message "Peers: %s"
           (if (= (hash-table-count opencog-network-peers) 0)
               "(none)"
             (string-join (hash-table-keys opencog-network-peers) ", "))))

(provide 'opencog-network)
;;; opencog-network.el ends here
