# Polymuse

An AI-powered "over-the-shoulder" coding assistant for Emacs that provides continuous, context-aware code review and suggestions while you write code or prose.

## Features

- **Automatic periodic reviews** of your work as you code
- **Context-aware suggestions** based on surrounding code
- **Multiple LLM backend support** (Ollama, OpenAI)
- **Customizable review intervals** and instructions
- **Typewriter-style animated output** for a pleasant user experience
- **Mode-specific prompts** for different file types (code vs. prose)

## Requirements

- Emacs 29.3 or later
- gptel 0.9.0 or later
- markdown-mode 2.5 or later
- typewrite 0.1.0 or later (for animated output display)

## Quick Start

1. **Set up a backend**:
   ```elisp
   M-x polymuse-define-default-backend
   ```
   Choose between Ollama (local) or OpenAI backends.

2. **Enable polymuse-mode** in a buffer:
   ```elisp
   M-x polymuse-mode
   ```

3. **Add a reviewer**:
   ```elisp
   M-x polymuse-add-reviewer
   ```

4. **Customize instructions** for the reviewer:
   ```elisp
   C-c C-r e
   ```

The reviewer will automatically provide feedback based on your cursor position and the surrounding context. Reviews appear in a side window and are updated periodically as you work.

## Key Bindings

When `polymuse-mode` is active:

- `C-c C-r e` - Edit reviewer instructions
- `C-c C-r r` - Run review manually
- `C-c C-r d` - Kill a reviewer
- `C-c C-r D` - Kill all reviewers
- `C-c C-r o` - Open review window
- `C-c C-r a` - Add a new reviewer
- `C-c C-r z` - Reset output buffer

## Configuration

```elisp
;; Example configuration for Ollama backend
(setq polymuse-ollama-hostname "localhost:11434"
      polymuse-ollama-protocol "http"
      polymuse-default-interval 60  ; seconds between reviews
      polymuse-max-prompt-characters 12000)

;; Enable polymuse-mode in specific modes
(add-hook 'prog-mode-hook #'polymuse-mode)
```

## Backend Setup

**Ollama (Local LLM)**:
```elisp
M-x polymuse-define-default-backend
;; Choose "Ollama"
;; Enter host (e.g., localhost:11434)
;; Select model from available models
```

**OpenAI**:
```elisp
M-x polymuse-define-default-backend
;; Choose "OpenAI"
;; Enter model name (e.g., gpt-4o-mini)
;; Ensure OPENAI_API_KEY environment variable is set
```

## Installation

### Using Nix (Recommended for NixOS/Home Manager users)

This package provides a Nix flake for easy integration into NixOS or Home Manager configurations.

```nix
{
  inputs.polymuse.url = "github:fudoniten/polymuse";

  # In your Home Manager or NixOS configuration:
  nixpkgs.overlays = [ polymuse.overlays.default ];

  programs.emacs.extraPackages = epkgs: with epkgs; [
    polymuse  # Note: Requires typewrite package (separate repository)
    # Also depends on: gptel, markdown-mode
  ];
}
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/fudoniten/polymuse.git
   ```

2. Add the package to your load path:
   ```elisp
   (add-to-list 'load-path "~/path/to/polymuse")
   ```

3. Require the package:
   ```elisp
   (require 'polymuse)
   ```

### Using use-package

```elisp
(use-package polymuse
  :load-path "~/path/to/polymuse"
  :commands (polymuse-mode polymuse-add-reviewer)
  :bind (:map polymuse-mode-map
              ("C-c C-r e" . polymuse-edit-instructions)
              ("C-c C-r r" . polymuse-run-review))
  :config
  (setq polymuse-default-interval 60))
```

## Optional Integrations

### Canon Integration

Polymuse includes optional integration with `canon.el` (a separate package) for tracking characters, locations, and other entities in prose writing or project documentation.

When `canon-mode` is active alongside `polymuse-mode`, the AI reviewer automatically gains access to tools for:
- Looking up entities (characters, locations, architecture docs, etc.)
- Searching entities by properties
- Suggesting updates to your canon (appended safely without modifying existing content)

To use canon integration:
1. Install the `canon` package separately (from https://github.com/fudoniten/canon)
2. Enable both `canon-mode` and `polymuse-mode` in your buffer
3. The AI reviewer will automatically detect and use canon tools

See the `examples/` directory for sample canon files demonstrating prose writing and code documentation use cases.

## Development & Testing

### Running Tests

This package includes a test suite using ERT (Emacs Lisp Regression Testing).

#### Running Tests from Command Line

```bash
# Requires gptel and markdown-mode
emacs -batch -l package.el \
  --eval "(require 'package)" \
  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
  --eval "(package-initialize)" \
  -l polymuse.el -l polymuse-test.el -f ert-run-tests-batch-and-exit
```

#### Running Tests Interactively

```elisp
;; Load the package and its tests
(load-file "polymuse.el")
(load-file "polymuse-test.el")

;; Run all tests
M-x ert RET t RET

;; Run a specific test
M-x ert RET polymuse-test-name RET
```

## License

Copyright (C) 2025 Niten

This program is free software; you can redistribute it and/or modify it under the terms of your choice.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

When contributing, please ensure:
- All existing tests pass
- New features include appropriate test coverage
- Code follows the existing style conventions

## Author

- **Author**: Niten <niten@fudo.org>
- **Homepage**: https://github.com/fudoniten/polymuse
