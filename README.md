# Polymuse

A collection of Emacs packages for enhanced writing and development workflows, with a focus on AI-assisted coding and creative writing.

## Overview

This repository contains three complementary Emacs packages:

- **polymuse.el** - AI-powered over-the-shoulder coding assistant
- **canon.el** - Creative work entity management system
- **typewrite.el** - Typewriter-style text insertion effect

## Packages

### polymuse.el

An AI-powered "over-the-shoulder" coding assistant for Emacs that provides continuous, context-aware code review and suggestions while you write code or prose.

#### Features

- **Automatic periodic reviews** of your work as you code
- **Context-aware suggestions** based on surrounding code
- **Multiple LLM backend support** (Ollama, OpenAI)
- **Customizable review intervals** and instructions
- **Typewriter-style animated output** for a pleasant user experience
- **Mode-specific prompts** for different file types (code vs. prose)

#### Requirements

- Emacs 29.3 or later
- gptel 0.9.0 or later
- markdown-mode 2.5 or later

#### Quick Start

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

#### Key Bindings

When `polymuse-mode` is active:

- `C-c C-r e` - Edit reviewer instructions
- `C-c C-r r` - Run review manually
- `C-c C-r d` - Kill a reviewer
- `C-c C-r D` - Kill all reviewers
- `C-c C-r o` - Open review window
- `C-c C-r a` - Add a new reviewer
- `C-c C-r z` - Reset output buffer

#### Configuration

```elisp
;; Example configuration for Ollama backend
(setq polymuse-ollama-hostname "localhost:11434"
      polymuse-ollama-protocol "http"
      polymuse-default-interval 60  ; seconds between reviews
      polymuse-max-prompt-characters 12000)

;; Enable polymuse-mode in specific modes
(add-hook 'prog-mode-hook #'polymuse-mode)
```

#### Backend Setup

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

---

### canon.el

A tool for managing the "canon" of a creative work—characters, locations, events, and other entities—using Org-mode files for structured storage and easy cross-referencing.

#### Features

- **Navigate to entities** by ID
- **Insert new entities** with custom types
- **Read and update entity information** programmatically
- **Search entities** by property values
- **Automatic type heading creation** in Org files
- **Reliable cross-referencing** using Org :ID: properties

#### Requirements

- Emacs 29.3 or later

#### Quick Start

1. **Enable canon-mode** in your writing buffer:
   ```elisp
   M-x canon-mode
   ```

2. **Specify a canon file** when prompted (e.g., `canon.org`)

3. **Use the key bindings**:
   - `C-c C-c i` - Insert a new entity
   - `C-c C-c j` - Jump to an entity
   - `C-c C-c t` - Add a new entity type

#### Usage

Entities are stored in an Org-mode file with a structure like:

```org
* Characters

** [Characters] John Doe
:PROPERTIES:
:ID: john-doe
:END:

*** Description
A mysterious character who appears in Chapter 3.

* Locations

** [Locations] The Old Mill
:PROPERTIES:
:ID: old-mill
:END:

*** Description
An abandoned mill on the edge of town.
```

#### API Functions

```elisp
;; Jump to an entity in the canon file
(canon-jump-to-entity "john-doe")

;; Insert a new entity
(canon-insert-entity "Characters" "jane-smith")

;; Get entity text
(canon-get-entity-text "john-doe")

;; Update entity text
(canon-set-entity-text "john-doe" "New description...")

;; Get a specific section
(canon-get-entity-section "john-doe" "Description")

;; Append to an entity
(canon-append-to-entity "john-doe" "Additional notes...")

;; Search by property
(canon-get-ids-by-property "AKA" "Johnny")
```

#### Configuration

```elisp
;; Set a default canon file per project
(setq-local canon-file "~/writing/my-novel/canon.org")

;; Or use dir-locals.el in your project directory:
((nil . ((canon-file . "canon.org"))))
```

---

### typewrite.el

Provides a typewriter effect for inserting text into Emacs buffers, gradually revealing text character by character at a configurable rate. This creates a more dynamic and pleasant visual experience, particularly useful for displaying LLM responses or log output.

#### Features

- **Configurable typing speed** (characters per second)
- **Multiple simultaneous jobs** in different buffers
- **Auto-scrolling** to follow the insertion point
- **Budget-based character accumulation** for smooth animation
- **Proper timer cleanup** when jobs complete
- **Read-only buffer support** with optional override

#### Requirements

- Emacs 24.4 or later

#### Usage

```elisp
;; Basic usage - insert text with typewriter effect
(typewrite-enqueue-job "Hello, world!" (current-buffer)
                       :cps 50           ; characters per second
                       :follow t)        ; auto-scroll

;; Insert at current point (not end of buffer)
(typewrite-enqueue-job "Inline text" (current-buffer)
                       :at-end nil
                       :newline-before nil)

;; With callback when finished
(typewrite-enqueue-job "Processing..." (current-buffer)
                       :cps 30
                       :done-callback (lambda (job)
                                        (message "Typing complete!")))

;; Stop all typewriter jobs
(typewrite-kill-jobs)
```

#### Configuration

```elisp
;; Set default typing speed
(setq typewrite-default-cps 30.0)

;; Set timer tick interval (affects smoothness)
(setq typewrite-tick-interval 0.05)

;; Allow writing to read-only buffers by default
(setq typewrite-default-inhibit-read-only t)
```

#### Integration with Polymuse

`typewrite.el` is used internally by `polymuse.el` to animate the AI's code review responses, creating a more engaging experience as suggestions appear gradually rather than all at once.

---

## Installation

### Using Nix (Recommended for NixOS/Home Manager users)

This repository provides a Nix flake with an overlay for easy integration into NixOS or Home Manager configurations.

```nix
{
  inputs.polymuse.url = "github:fudoniten/polymuse";

  # In your Home Manager or NixOS configuration:
  nixpkgs.overlays = [ polymuse.overlays.default ];

  programs.emacs.extraPackages = epkgs: with epkgs; [
    polymuse  # Includes dependencies: gptel, markdown-mode
    canon
    typewrite
  ];
}
```

See [NIX_USAGE.md](NIX_USAGE.md) for detailed Nix installation instructions and examples.

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/fudoniten/polymuse.git
   ```

2. Add the packages to your load path:
   ```elisp
   (add-to-list 'load-path "~/path/to/polymuse")
   ```

3. Require the packages you want to use:
   ```elisp
   (require 'polymuse)
   (require 'canon)
   (require 'typewrite)
   ```

### Using use-package

```elisp
;; Polymuse
(use-package polymuse
  :load-path "~/path/to/polymuse"
  :commands (polymuse-mode polymuse-add-reviewer)
  :bind (:map polymuse-mode-map
              ("C-c C-r e" . polymuse-edit-instructions)
              ("C-c C-r r" . polymuse-run-review))
  :config
  (setq polymuse-default-interval 60))

;; Canon
(use-package canon
  :load-path "~/path/to/polymuse"
  :commands (canon-mode canon-insert-entity canon-jump-to-entity))

;; Typewrite
(use-package typewrite
  :load-path "~/path/to/polymuse"
  :commands (typewrite-enqueue-job))
```

## Development & Testing

### Running Tests

Each package in this repository has its own test suite using ERT (Emacs Lisp Regression Testing).

#### Running All Tests

To run all tests for all packages:

```bash
# Run typewrite tests
emacs -batch -l typewrite.el -l typewrite-test.el -f ert-run-tests-batch-and-exit

# Run canon tests
emacs -batch -l canon.el -l canon-test.el -f ert-run-tests-batch-and-exit

# Run polymuse tests (requires dependencies)
emacs -batch -l package.el \
  --eval "(require 'package)" \
  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
  --eval "(package-initialize)" \
  -l polymuse.el -l polymuse-test.el -f ert-run-tests-batch-and-exit
```

#### Running Tests Interactively

You can also run tests interactively within Emacs:

```elisp
;; Load the package and its tests
(load-file "canon.el")
(load-file "canon-test.el")

;; Run all tests for the package
M-x ert RET t RET

;; Run a specific test
M-x ert RET canon-test-insert-entity-basic RET

;; Run tests matching a pattern
M-x ert RET "^canon-test-.*entity" RET
```

#### Test Requirements

- **Emacs 29.3 or later** (some tests may work with earlier versions)
- For polymuse tests: `gptel` and `markdown-mode` packages from MELPA

#### Continuous Integration

All tests run automatically on GitHub Actions for every push to main branches and pull requests. The test matrix includes:
- Emacs 29.3 (stable)
- Emacs snapshot (latest development version)

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
