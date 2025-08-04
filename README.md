# DoomAI Module

A standalone AI-powered note enhancement module for Doom Emacs, designed specifically for org-mode and org-roam workflows.

## Features

### Multi-Server Architecture
- **LM Studio** (Tertiary): `http://127.0.0.1:1234` (requires `/v1` prefix)
- Automatic failover with health monitoring
- Fallback endpoint support (`/chat/completions` if `/v1/chat/completions` fails)

### Current Functionality
- **Note Enhancement**: AI-powered content expansion (`SPC d a e`)
- **Summary Generation**: Create concise summaries (`SPC d a u`)
- **Server Management**: View server status and health (`SPC d a s`)
- **Mode Toggle**: Enable/disable per buffer (`SPC d a m`)

### Planned Features
- Connection suggestions for org-roam cross-linking
- Auto-tagging based on content analysis
- Semantic search across knowledge base
- Context-aware daily journal prompts

## Installation

1. The module is already enabled in `init.el` under `:custom doomai`
2. Run `doom sync` to activate
3. Restart Doom Emacs or run `doom/reload`

## Usage

### Basic Commands
- `SPC d a e` - Enhance current note with AI
- `SPC d a u` - Generate summary of current note
- `SPC d a s` - View server status
- `SPC d a m` - Toggle DoomAI mode in buffer

### Server Configuration
The module automatically tries servers in priority order:
1. Tailscale (secure private network)
2. Local network (LAN fallback)
3. LM Studio (localhost fallback)

### Customization
```elisp
;; In your config.el or personal config
(setq doomai-servers
      '((tailscale . "http://your-tailscale-ip:8080/v1")
        (local . "http://192.168.1.100:8080/v1")
        (lm-studio . "http://127.0.0.1:1234")))

(setq doomai-default-model "deepseek/deepseek-r1-0528-qwen3-8b")
(setq doomai-server-timeout 10)  ; Increase timeout if needed
```

## Architecture

### File Structure
```
modules/custom/doomai/
├── config.el     # Variables, customization, keybindings
├── packages.el   # Package declarations (none needed)
├── autoload.el   # All functionality and interactive commands
└── README.md     # This documentation
```

### Design Principles
- **Zero Dependencies**: Uses only built-in Emacs libraries
- **Doom Integration**: Follows Doom conventions and patterns
- **Performance**: Lazy loading and efficient caching
- **Resilience**: Robust error handling and failover
- **Modularity**: Clean separation of concerns

### Server Health System
- Caches server status for 60 seconds
- Uses `/models` endpoint for health checks
- Automatic failover on server unavailability
- Visual status indicators in status buffer

## Troubleshooting

### Server Connection Issues
1. Check server status: `SPC a n S`
2. Verify server URLs are correct
3. Ensure servers are running and accessible
4. Check network connectivity (ping the IPs)

### API Errors
- Most local LLM servers don't require API keys
- Ensure the model name exists on your server
- Check server logs for detailed error messages

### Module Not Loading
1. Run `doom doctor` to check for issues
2. Ensure module is listed in `init.el` under `:custom doomai`
3. Run `doom sync` after changes
4. Restart Emacs or use `doom/reload`

## Development

The module follows Doom Emacs conventions:
- Functions prefixed with `+doomai/` (public) or `+doomai--` (private)
- All interactive commands are autoloaded
- Configuration uses `defcustom` for user options
- Extensive documentation and error handling

To extend functionality, add new functions to `autoload.el` and keybindings to `config.el`.
