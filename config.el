;;; custom/doomai/config.el -*- lexical-binding: t; -*-

;;
;;; Variables

(defgroup doomai nil
  "AI-powered note enhancement for Doom Emacs."
  :group 'doom
  :prefix "doomai-")

(defcustom doomai-servers
  '(;; (tailscale . "http://100.109.107.21:8080/v1")
    ;; (local . "http://192.168.8.186:8080/v1")
    (lm-studio . "http://127.0.0.1:1234"))
  "AI servers in order of preference.
Each entry is a cons cell of (SERVER-TYPE . ENDPOINT-URL)."
  :type '(alist :key-type symbol :value-type string)
  :group 'doomai)

(defcustom doomai-server-endpoints
  '((chat . "/chat/completions")
    (models . "/models"))
  "API endpoints to append to server URLs."
  :type '(alist :key-type symbol :value-type string)
  :group 'doomai)

(defcustom doomai-server-timeout 5
  "Timeout in seconds for server health checks."
  :type 'integer
  :group 'doomai)

(defcustom doomai-api-key nil
  "API key for AI servers (if required)."
  :type '(choice (const :tag "None" nil)
                 (string :tag "API Key"))
  :group 'doomai)

(defcustom doomai-default-model "deepseek/deepseek-r1-0528-qwen3-8b"
  "Default model to use for AI requests."
  :type 'string
  :group 'doomai)

(defcustom doomai-max-suggestions 5
  "Maximum number of suggestions to show for auto-tag and connections."
  :type 'integer
  :group 'doomai)

(defcustom doomai-tag-source-files nil
  "List of org files to analyze for existing tag patterns.
If nil, will analyze all org-roam files for tag patterns."
  :type '(repeat file)
  :group 'doomai)

;;
;;; Internal variables

(defvar doomai--current-server nil
  "Currently active server endpoint.")

(defvar doomai--server-status-cache (make-hash-table :test 'eq)
  "Cache for server health status.")

(defvar doomai--last-health-check 0
  "Timestamp of last health check.")

(defconst doomai--health-check-interval 60
  "Interval in seconds between health checks.")

;;
;;; Keybindings

(map! :leader
      (:prefix ("d a" . "doomai")
       :desc "Server status" "s" #'+doomai/server-status
       :desc "Enhance note" "e" #'+doomai/enhance-note
       :desc "Generate summary" "u" #'+doomai/generate-summary
       :desc "Suggest connections" "c" #'+doomai/suggest-connections
       :desc "Auto tag" "t" #'+doomai/auto-tag
       :desc "Semantic search" "f" #'+doomai/semantic-search
       :desc "Daily prompt" "d" #'+doomai/daily-prompt
       :desc "Toggle mode" "m" #'+doomai/toggle-mode))
