;;; custom/doomai/autoload.el -*- lexical-binding: t; -*-

;;; Commentary:
;; DoomAI autoloaded functions for AI-powered note enhancement.
;; This module provides server management with automatic failover and
;; core AI functionality for org-mode and org-roam integration.

(require 'url)
(require 'json)

;;
;;; Server Management
;; The server system uses a priority-based failover mechanism:
;; 1. Tailscale server (primary) - secure private network
;; 2. Local network server (secondary) - LAN fallback  
;; 3. LM Studio server (tertiary) - localhost fallback

(defun +doomai--health-check (server-type endpoint)
  "Check if SERVER-TYPE at ENDPOINT is healthy.
Attempts to connect to the /models endpoint to verify server availability.
Returns t if server is responsive, nil otherwise.

SERVER-TYPE: Symbol identifying the server (tailscale, local, lm-studio)
ENDPOINT: Base URL for the server (e.g. 'http://100.109.107.21:8080/v1')"
  (condition-case nil
      (let ((url-request-method "GET")
            (url-request-extra-headers
             (when doomai-api-key
               `(("Authorization" . ,(format "Bearer %s" doomai-api-key)))))
            (url-request-timeout doomai-server-timeout)
            ;; LM Studio requires /v1 prefix, others may have it already
            (models-endpoint (if (eq server-type 'lm-studio)
                                (concat endpoint "/v1/models")
                              (concat endpoint (cdr (assq 'models doomai-server-endpoints))))))
        (with-temp-buffer
          (url-insert-file-contents models-endpoint)
          t))
    (error nil)))

(defun +doomai--update-health-cache ()
  "Update the health status cache for all servers.
Only performs health checks if the cache interval has expired.
This prevents excessive network requests during normal operation."
  (when (> (float-time) (+ doomai--last-health-check doomai--health-check-interval))
    (setq doomai--last-health-check (float-time))
    (dolist (server doomai-servers)
      (let ((server-type (car server))
            (endpoint (cdr server)))
        (puthash server-type
                 (+doomai--health-check server-type endpoint)
                 doomai--server-status-cache)))))

(defun +doomai--get-active-server ()
  "Get the currently active server endpoint.
Implements automatic failover logic:
1. If current server is still healthy, continue using it
2. Otherwise, find the first healthy server from the priority list
3. Update current server and notify user of switch
4. Error if no healthy servers are available"
  (+doomai--update-health-cache)
  
  ;; If current server is still healthy, use it
  (if (and doomai--current-server
           (gethash (car (rassoc doomai--current-server doomai-servers))
                    doomai--server-status-cache))
      doomai--current-server
    
    ;; Find first healthy server from priority list
    (or (cl-loop for server in doomai-servers
                 for server-type = (car server)
                 for endpoint = (cdr server)
                 when (gethash server-type doomai--server-status-cache)
                 do (progn
                      (setq doomai--current-server endpoint)
                      (message "DoomAI: Switched to %s server" server-type))
                 and return endpoint)
        
        ;; No healthy servers found
        (progn
          (setq doomai--current-server nil)
          (user-error "DoomAI: No healthy servers available")))))

(defun +doomai--make-request (messages &optional model)
  "Make an API request with MESSAGES to the active server.
Handles the complete HTTP request cycle including:
1. Server selection via failover logic
2. Request formatting with proper headers
3. Server-specific endpoint handling (LM Studio vs others)
4. JSON response parsing

MESSAGES: Vector of message objects for the chat completion
MODEL: Optional model name (defaults to doomai-default-model)

Returns parsed JSON response from the server."
  (let* ((endpoint (+doomai--get-active-server))
         (server-type (car (rassoc endpoint doomai-servers)))
         ;; LM Studio requires /v1 prefix, others may have it already
         (chat-endpoint (if (eq server-type 'lm-studio)
                           (concat endpoint "/v1/chat/completions")
                         (concat endpoint (cdr (assq 'chat doomai-server-endpoints)))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ,@(when doomai-api-key
                `(("Authorization" . ,(format "Bearer %s" doomai-api-key))))))
         (url-request-data
          (json-encode
           `((model . ,(or model doomai-default-model))
             (messages . ,messages)
             (temperature . 0.7)
             (max_tokens . 2000)))))
    (with-temp-buffer
      (url-insert-file-contents chat-endpoint)
      (goto-char (point-min))
      (json-read))))

;;
;;; Helper Functions for Org-roam Integration

(defun +doomai--get-org-roam-notes ()
  "Get all org-roam notes with their titles and file paths.
Returns a list of cons cells (TITLE . FILE-PATH) for all notes
in the org-roam network."
  (when (fboundp 'org-roam-node-list)
    (mapcar (lambda (node)
              (cons (org-roam-node-title node)
                    (org-roam-node-file node)))
            (org-roam-node-list))))

(defun +doomai--extract-existing-tags ()
  "Extract and analyze existing tags from org-roam files.
Returns a list of tags sorted by frequency of use across the
org-roam network. This helps maintain consistent tagging patterns."
  (let ((tag-counts (make-hash-table :test 'equal))
        (files-to-scan (or doomai-tag-source-files
                          (mapcar #'cdr (+doomai--get-org-roam-notes)))))
    (dolist (file files-to-scan)
      (when (and file (file-exists-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (org-mode)
          (let ((tags (org-get-tags)))
            (dolist (tag tags)
              (puthash tag (1+ (gethash tag tag-counts 0)) tag-counts))))))
    ;; Convert hash table to sorted list by frequency
    (let ((tag-list '()))
      (maphash (lambda (tag count)
                 (push (cons tag count) tag-list))
               tag-counts)
      (mapcar #'car (sort tag-list (lambda (a b) (> (cdr a) (cdr b))))))))

(defun +doomai--format-connection-suggestions (notes)
  "Format a list of org-roam NOTES for display in completion interface.
Each note is a cons cell (TITLE . FILE-PATH). Returns a list of
formatted strings suitable for completing-read."
  (mapcar (lambda (note)
            (let ((title (car note))
                  (file (cdr note)))
              ;; Format: "Title (relative/path/to/file.org)"
              (format "%s (%s)" 
                      title
                      (if (and (boundp 'org-roam-directory) org-roam-directory)
                          (file-relative-name file org-roam-directory)
                        (file-name-nondirectory file)))))
          notes))

(defun +doomai--insert-org-link (title file-path)
  "Insert an org-roam style link for the note with TITLE at FILE-PATH.
Creates a link in the format [[id:node-id][title]] if org-roam is available,
otherwise creates a file link [[file:path][title]]."
  (let ((link-text
         (if (and (fboundp 'org-roam-node-from-title-or-alias)
                  (fboundp 'org-roam-node-id))
             ;; Try to get org-roam ID-based link
             (if-let ((node (org-roam-node-from-title-or-alias title)))
                 (format "[[id:%s][%s]]" (org-roam-node-id node) title)
               ;; Fallback to file link if node not found
               (format "[[file:%s][%s]]" file-path title))
           ;; Fallback to file link if org-roam not available
           (format "[[file:%s][%s]]" file-path title))))
    (insert link-text)))

;;
;;; Interactive Commands
;; These functions provide the user-facing interface for DoomAI functionality.
;; All are autoloaded and available through the SPC d a keymap.

;;;###autoload
(defun +doomai/server-status ()
  "Display status of all configured servers.
Shows a formatted status buffer with:
- Current active server (marked with ★)
- Health status of each configured server
- Server type and endpoint information"
  (interactive)
  (+doomai--update-health-cache)
  (with-current-buffer (get-buffer-create "*DoomAI Server Status*")
    (erase-buffer)
    (insert "DoomAI Server Status\n")
    (insert "===================\n\n")
    (dolist (server doomai-servers)
      (let* ((server-type (car server))
             (endpoint (cdr server))
             (status (gethash server-type doomai--server-status-cache))
             (current (string= endpoint doomai--current-server)))
        (insert (format "%s %s: %s%s\n"
                        (if current "★" " ")
                        (upcase (symbol-name server-type))
                        (if status "HEALTHY" "UNHEALTHY")
                        (if current " (ACTIVE)" "")))))
    (display-buffer (current-buffer))))

;;;###autoload
(defun +doomai/enhance-note ()
  "Enhance the current note with AI-powered content expansion.
Takes the entire buffer content and asks the AI to expand and enhance it
while maintaining the original style and format. Replaces buffer content
with the enhanced version."
  (interactive)
  (let* ((content (buffer-string))
         ;; Create a user message requesting enhancement
         (messages `[((role . "user") 
                      (content . ,(format "Enhance and expand this note with relevant information, maintaining the same style and format:\n\n%s" content)))])
         (response (+doomai--make-request messages)))
    ;; Extract the enhanced content from the API response
    ;; Response structure: choices[0].message.content
    (when-let ((enhanced-content (cdr (assq 'content (cdr (assq 'message (aref (cdr (assq 'choices response)) 0)))))))
      (erase-buffer)
      (insert enhanced-content)
      (message "Note enhanced with AI"))))

;;;###autoload
(defun +doomai/generate-summary ()
  "Generate a summary of the current note.
Creates a concise summary of the buffer content and displays it
in a separate buffer for review before potential integration."
  (interactive)
  (let* ((content (buffer-string))
         (messages `[((role . "user") 
                      (content . ,(format "Create a concise summary of this note:\n\n%s" content)))])
         (response (+doomai--make-request messages)))
    (when-let ((summary (cdr (assq 'content (cdr (assq 'message (aref (cdr (assq 'choices response)) 0)))))))
      (with-current-buffer (get-buffer-create "*DoomAI Summary*")
        (erase-buffer)
        (insert summary)
        (display-buffer (current-buffer))))))

;;;###autoload
(defun +doomai/suggest-connections ()
  "Suggest connections to other notes in the org-roam network.
Analyzes current note content and suggests semantically related notes
from the org-roam network for cross-linking. Uses AI to find non-obvious
connections between ideas and concepts."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "DoomAI connection suggestions only work in org-mode buffers"))
  
  (let* ((all-notes (+doomai--get-org-roam-notes))
         (current-file (buffer-file-name))
         ;; Exclude current note from suggestions
         (other-notes (cl-remove-if (lambda (note) 
                                     (string= (cdr note) current-file))
                                   all-notes)))
    
    (unless other-notes
      (user-error "No other org-roam notes found for connection suggestions"))
    
    (let* ((content (buffer-string))
           ;; Create a summary of available notes for AI context
           (notes-context (mapconcat (lambda (note)
                                      (format "- %s" (car note)))
                                    (take 50 other-notes) "\n"))
           (messages `[((role . "user") 
                        (content . ,(format "Based on this note content, suggest %d most relevant notes to link to from this list. Return ONLY the exact note titles, one per line, no explanation:\n\nAvailable notes:\n%s\n\nCurrent note content:\n%s"
                                           doomai-max-suggestions
                                           notes-context
                                           content)))])
           (response (+doomai--make-request messages)))
      
      (when-let ((suggestions-text (cdr (assq 'content (cdr (assq 'message (aref (cdr (assq 'choices response)) 0)))))))
        ;; Parse line-separated note titles
        (let* ((suggested-titles (mapcar #'string-trim 
                                        (split-string suggestions-text "\n" t)))
               ;; Find matching notes from our list
               (matching-notes (cl-remove-if-not 
                               (lambda (note)
                                 (member (car note) suggested-titles))
                               other-notes))
               ;; Format for display
               (formatted-options (when matching-notes
                                   (+doomai--format-connection-suggestions matching-notes))))
          
          (if formatted-options
              (let ((selection (completing-read "Select note to link: " 
                                               formatted-options nil t)))
                ;; Extract the title from the formatted selection
                (when (string-match "^\\(.*?\\) (" selection)
                  (let* ((selected-title (match-string 1 selection))
                         (selected-note (cl-find selected-title matching-notes 
                                                :key #'car :test #'string=)))
                    (when selected-note
                      (+doomai--insert-org-link (car selected-note) (cdr selected-note))
                      (message "DoomAI: Inserted link to '%s'" (car selected-note))))))
            (message "DoomAI: No relevant connections found")))))))

;;;###autoload
(defun +doomai/auto-tag ()
  "Automatically suggest tags for the current note.
Analyzes note content and suggests appropriate org-mode tags based on:
1. Content semantic analysis via AI
2. Existing tag patterns in the org-roam network
3. Current tags already applied to this note"
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "DoomAI auto-tag only works in org-mode buffers"))
  
  (let* ((content (buffer-string))
         (existing-tags (+doomai--extract-existing-tags))
         (current-tags (org-get-tags))
         (tag-context (if existing-tags
                         (format "Common tags in this knowledge base: %s\n\n"
                                (mapconcat 'identity (take 20 existing-tags) ", "))
                       ""))
         (messages `[((role . "user") 
                      (content . ,(format "%sAnalyze this note and suggest %d relevant tags. Return ONLY a comma-separated list of tags, no explanation:\n\nCurrent tags: %s\n\nNote content:\n%s"
                                         tag-context
                                         doomai-max-suggestions
                                         (if current-tags (mapconcat 'identity current-tags ", ") "none")
                                         content)))])
         (response (+doomai--make-request messages)))
    
    (when-let ((suggested-tags-text (cdr (assq 'content (cdr (assq 'message (aref (cdr (assq 'choices response)) 0)))))))
      ;; Parse comma-separated tags and clean them up
      (let* ((suggested-tags (mapcar (lambda (tag)
                                      (string-trim (downcase tag)))
                                    (split-string suggested-tags-text ",")))
             ;; Remove any tags that are already applied
             (new-tags (cl-remove-if (lambda (tag) (member tag current-tags)) suggested-tags))
             ;; Let user select which tags to apply
             (selected-tags (when new-tags
                             (completing-read-multiple 
                              "Select tags to apply: "
                              new-tags nil nil))))
        
        (if selected-tags
            (progn
              ;; Apply the selected tags
              (org-set-tags (append current-tags selected-tags))
              (message "DoomAI: Applied %d tags: %s" 
                      (length selected-tags)
                      (mapconcat 'identity selected-tags ", ")))
          (message "DoomAI: No new tags selected"))))))

;;;###autoload
(defun +doomai/semantic-search ()
  "Perform semantic search across all notes.
Future feature: Will enable natural language search across
the entire org-roam database using AI embeddings."
  (interactive)
  (message "Semantic search - Feature coming soon!"))

;;;###autoload
(defun +doomai/daily-prompt ()
  "Generate context-aware daily journal prompts.
Future feature: Will create personalized daily journal prompts
based on previous entries and current context."
  (interactive)
  (message "Daily prompts - Feature coming soon!"))

;;;###autoload
(define-minor-mode +doomai-mode
  "Minor mode for DoomAI functionality.
Enables DoomAI features in the current buffer with a robot indicator
in the mode line. Currently provides visual feedback for active buffers."
  :lighter " 🤖"
  :global nil)

;;;###autoload
(defun +doomai/toggle-mode ()
  "Toggle DoomAI mode in the current buffer.
Provides easy access to enable/disable DoomAI features
per buffer as needed."
  (interactive)
  (+doomai-mode 'toggle))