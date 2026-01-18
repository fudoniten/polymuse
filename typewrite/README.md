# Typewrite

Provides a typewriter effect for inserting text into Emacs buffers, gradually revealing text character by character at a configurable rate. This creates a more dynamic and pleasant visual experience, particularly useful for displaying LLM responses or log output.

## Features

- **Configurable typing speed** (characters per second)
- **Multiple simultaneous jobs** in different buffers
- **Auto-scrolling** to follow the insertion point
- **Budget-based character accumulation** for smooth animation
- **Proper timer cleanup** when jobs complete
- **Read-only buffer support** with optional override

## Requirements

- Emacs 24.4 or later

## Usage

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

## Configuration

```elisp
;; Set default typing speed
(setq typewrite-default-cps 30.0)

;; Set timer tick interval (affects smoothness)
(setq typewrite-tick-interval 0.05)

;; Allow writing to read-only buffers by default
(setq typewrite-default-inhibit-read-only t)
```

## Integration with Polymuse

`typewrite.el` is used internally by `polymuse.el` to animate the AI's code review responses, creating a more engaging experience as suggestions appear gradually rather than all at once.

## Installation

### Using Nix (Recommended for NixOS/Home Manager users)

This package provides a Nix flake for easy integration into NixOS or Home Manager configurations.

```nix
{
  inputs.typewrite.url = "github:fudoniten/typewrite";

  # In your Home Manager or NixOS configuration:
  nixpkgs.overlays = [ typewrite.overlays.default ];

  programs.emacs.extraPackages = epkgs: with epkgs; [
    typewrite
  ];
}
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/fudoniten/typewrite.git
   ```

2. Add the package to your load path:
   ```elisp
   (add-to-list 'load-path "~/path/to/typewrite")
   ```

3. Require the package:
   ```elisp
   (require 'typewrite)
   ```

### Using use-package

```elisp
(use-package typewrite
  :load-path "~/path/to/typewrite"
  :commands (typewrite-enqueue-job))
```

## Development & Testing

### Running Tests

This package includes a test suite using ERT (Emacs Lisp Regression Testing).

#### Running Tests from Command Line

```bash
emacs -batch -l typewrite.el -l typewrite-test.el -f ert-run-tests-batch-and-exit
```

#### Running Tests Interactively

```elisp
;; Load the package and its tests
(load-file "typewrite.el")
(load-file "typewrite-test.el")

;; Run all tests
M-x ert RET t RET

;; Run a specific test
M-x ert RET typewrite-test-name RET
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
- **Homepage**: https://github.com/fudoniten/typewrite
