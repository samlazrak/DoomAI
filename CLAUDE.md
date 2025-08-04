# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is the DoomAI module - a standalone AI-powered note enhancement module for Doom Emacs. It integrates with org-mode and org-roam workflows, providing AI functionality through a multi-server architecture with automatic failover capabilities. The module is designed with zero external dependencies, using only built-in Emacs libraries.

## Essential Commands

### Module Development
```bash
# Sync Doom configuration after module changes
doom sync

# Test the module in isolation
emacs --debug-init

# Check for module-specific issues
doom doctor

# Reload configuration without restart
# In Emacs: SPC h r r (doom/reload)
```

### Testing DoomAI Functionality
```bash
# Test server connectivity from command line
curl -X GET "http://127.0.0.1:1234/v1/models"

# Test AI completion endpoint
curl -X POST "http://127.0.0.1:1234/v1/chat/completions" \
  -H "Content-Type: application/json" \
  -d '{"model":"deepseek/deepseek-r1-0528-qwen3-8b","messages":[{"role":"user","content":"Hello"}],"max_tokens":100}'
```

### Debug and Monitoring
```elisp
;; In Emacs to debug DoomAI
(setq url-debug t)  ; Enable URL debugging
(setq debug-on-error t)  ; Enable error debugging

;; Check server status programmatically
(+doomai--update-health-cache)
(hash-table-keys doomai--server-status-cache)

;; View current active server
doomai--current-server
```

## Architecture Overview

### Module Structure
```
modules/custom/doomai/
├── config.el      # Customization variables, defcustom settings, keybindings
├── packages.el    # Package declarations (empty - uses built-ins only)
├── autoload.el    # All functionality and interactive commands
├── README.md      # User-facing documentation
└── CLAUDE.md      # Developer guidance (this file)
```

### Core Components

#### Server Management System
- **Health Monitoring**: 60-second cached health checks using `/models` endpoint
- **Automatic Failover**: Priority-based server selection (Tailscale → Local → LM Studio)
- **Endpoint Handling**: Server-specific URL construction (LM Studio requires `/v1` prefix)
- **Error Recovery**: Graceful degradation when servers become unavailable

#### API Request Pipeline
1. **Server Selection**: `+doomai--get-active-server()` handles failover logic
2. **Request Formation**: `+doomai--make-request()` constructs HTTP requests
3. **Response Processing**: JSON parsing and content extraction
4. **Error Handling**: Comprehensive error recovery and user feedback

#### Interactive Command Layer
- **Core Functions**: Note enhancement, summary generation, server status
- **Future Features**: Connection suggestions, auto-tagging, semantic search
- **Mode System**: Buffer-local minor mode with visual indicators

### Key Design Patterns

#### Zero Dependencies Strategy
```elisp
;; Uses only built-in libraries
(require 'url)      ; HTTP requests
(require 'json)     ; JSON parsing
;; No external packages required
```

#### Doom Integration Pattern
```elisp
;; Function naming convention
(defun +doomai/public-command ()      ; Public interactive commands
(defun +doomai--private-helper ()     ; Private helper functions

;; Keybinding pattern
(map! :leader
      (:prefix ("d a" . "doomai")  ; Under SPC d a namespace
       :desc "Description" "key" #'function))

;; Customization pattern
(defcustom doomai-variable default-value
  "Documentation string."
  :type 'expected-type
  :group 'doomai)
```

#### Resilient Server Architecture
```elisp
;; Cached health checking
(defvar doomai--server-status-cache (make-hash-table :test 'eq))

;; Automatic failover
(or (find-healthy-server priority-list)
    (user-error "No servers available"))

;; Server-specific endpoint handling
(if (eq server-type 'lm-studio)
    (concat endpoint "/v1" suffix)
  (concat endpoint suffix))
```

## Development Guidelines

### Adding New Commands
1. **Implement in autoload.el** with `;;;###autoload` marker
2. **Add keybinding in config.el** under the `SPC d a` prefix
3. **Follow naming convention**: `+doomai/command-name`
4. **Include comprehensive documentation** in docstrings
5. **Handle errors gracefully** with `condition-case` when appropriate

### Server Configuration
```elisp
;; Add new server type to doomai-servers
(defcustom doomai-servers
  '((new-server . "http://example.com:8080/v1")
    (lm-studio . "http://127.0.0.1:1234")))

;; Update health check logic if needed
(defun +doomai--health-check (server-type endpoint)
  (when (eq server-type 'new-server)
    ;; Special handling for new server type
    ))
```

### API Request Patterns
```elisp
;; Standard request structure
(let* ((messages `[((role . "user") 
                    (content . ,user-input))])
       (response (+doomai--make-request messages model)))
  ;; Extract content from response
  (cdr (assq 'content 
             (cdr (assq 'message 
                        (aref (cdr (assq 'choices response)) 0))))))
```

### Error Handling Standards
```elisp
;; Network operations
(condition-case err
    (network-operation)
  (error 
   (message "DoomAI: Operation failed - %s" (error-message-string err))
   nil))

;; User-facing errors
(user-error "DoomAI: Clear error message for user")

;; Debug information
(when url-debug
  (message "DoomAI Debug: %s" debug-info))
```

## Testing and Debugging

### Manual Testing Checklist
1. **Server Status**: `SPC d a s` should show server health
2. **Note Enhancement**: `SPC d a e` should enhance buffer content
3. **Summary Generation**: `SPC d a u` should create summary in new buffer
4. **Mode Toggle**: `SPC d a m` should toggle robot indicator in mode line
5. **Error Handling**: Test with all servers down (should show clear error)

### Common Issues and Solutions

#### Server Connection Problems
- **Check server URLs**: Ensure correct protocol (http/https) and port
- **Verify endpoints**: LM Studio needs `/v1` prefix, others may not
- **Test manually**: Use curl commands to verify server availability
- **Check firewall**: Ensure network access to configured servers

#### API Response Issues
- **Model availability**: Verify model name exists on target server
- **Request format**: Ensure JSON structure matches server expectations
- **Response parsing**: Check that server returns expected JSON structure
- **Timeout handling**: Increase `doomai-server-timeout` for slow servers

#### Module Loading Issues
- **Run `doom doctor`**: Check for configuration problems
- **Check init.el**: Ensure `:custom doomai` is enabled
- **Autoload problems**: Functions must be marked with `;;;###autoload`
- **Keybinding conflicts**: Verify no conflicts with existing bindings

### Performance Monitoring
```elisp
;; Monitor server selection performance
(benchmark-run 10 (+doomai--get-active-server))

;; Track health check frequency
(add-hook '+doomai--health-check-hook
          (lambda () (message "Health check at %s" (current-time-string))))

;; Monitor request/response times
(let ((start-time (float-time)))
  (+doomai--make-request messages)
  (message "Request took %.2f seconds" (- (float-time) start-time)))
```

## Code Review Focus Areas

### Architecture Review
1. **Server Management**: Ensure failover logic is robust and predictable
2. **Error Handling**: Verify comprehensive error recovery at all levels
3. **Performance**: Check for unnecessary network calls or blocking operations
4. **Security**: Validate that API keys and sensitive data are handled properly

### Code Quality Standards
1. **Documentation**: All functions must have comprehensive docstrings
2. **Naming**: Follow `+doomai/` and `+doomai--` conventions consistently
3. **Error Messages**: Provide clear, actionable error messages to users
4. **Modularity**: Maintain clean separation between server, API, and UI layers

### Doom Integration Review
1. **Use Doom Patterns**: Prefer `map!`, `use-package!`, `after!` over vanilla Emacs
2. **Lazy Loading**: Ensure autoloaded functions don't cause startup delays
3. **Customization**: Use `defcustom` for user-configurable options
4. **Keybindings**: Follow Doom's leader key conventions

This module represents a self-contained AI integration for Doom Emacs, requiring careful attention to network reliability, error handling, and user experience when making modifications.