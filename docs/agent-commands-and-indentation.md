# Agent commands and indentation guides

- `M-x claude-code` opens Claude Code. Type a prompt and submit it with Return.
  The `claude` CLI must be installed and authenticated. CLI failures are reported
  in the editor instead of leaving the loading indicator running indefinitely.
- `M-x hermes-agent` opens and focuses the Hermes chat panel. Calling it again
  focuses the existing panel. Type after `> ` and press Return to send. The
  `hermes` CLI must be installed and configured. `C-c C-e` returns to the editor;
  `C-c C-k` closes the panel.
- Code buffers display light gray vertical indentation guides by default, using
  the buffer's indentation width. Guides are display decorations: saved files
  and copied text retain their original whitespace. `M-x toggle-indentation-guides`
  toggles them globally. Text and agent chat buffers do not show guides.

Both agent commands and the guide toggle are available through the existing MCP
`command_execute` tool using their command names. An agent handling a Director
intent can invoke them through that tool.

Rebuild the standalone application after updating the source (`make sdl2` or
`make ncurses`). The regular desktop build scripts include the Enterprise agent
commands; an existing executable does not acquire new commands until rebuilt.
