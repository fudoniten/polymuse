# Canon

A tool for managing the "canon" of a creative work—characters, locations, events, and other entities—using Org-mode files for structured storage and easy cross-referencing.

## Features

- **Navigate to entities** by ID
- **Insert new entities** with custom types
- **Read and update entity information** programmatically
- **Search entities** by property values
- **Automatic type heading creation** in Org files
- **Reliable cross-referencing** using Org :ID: properties

## Requirements

- Emacs 29.3 or later

## Quick Start

1. **Enable canon-mode** in your writing buffer:
   ```elisp
   M-x canon-mode
   ```

2. **Specify a canon file** when prompted (e.g., `canon.org`)

3. **Use the key bindings**:
   - `C-c C-c i` - Insert a new entity
   - `C-c C-c j` - Jump to an entity
   - `C-c C-c t` - Add a new entity type

## Usage

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

## API Functions

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

## Configuration

```elisp
;; Set a default canon file per project
(setq-local canon-file "~/writing/my-novel/canon.org")

;; Or use dir-locals.el in your project directory:
((nil . ((canon-file . "canon.org"))))
```

## Examples

See the `examples/` directory for sample canon files that demonstrate the structure and usage.

## Installation

### Using Nix (Recommended for NixOS/Home Manager users)

This package provides a Nix flake for easy integration into NixOS or Home Manager configurations.

```nix
{
  inputs.canon.url = "github:fudoniten/canon";

  # In your Home Manager or NixOS configuration:
  nixpkgs.overlays = [ canon.overlays.default ];

  programs.emacs.extraPackages = epkgs: with epkgs; [
    canon
  ];
}
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/fudoniten/canon.git
   ```

2. Add the package to your load path:
   ```elisp
   (add-to-list 'load-path "~/path/to/canon")
   ```

3. Require the package:
   ```elisp
   (require 'canon)
   ```

### Using use-package

```elisp
(use-package canon
  :load-path "~/path/to/canon"
  :commands (canon-mode canon-insert-entity canon-jump-to-entity))
```

## Development & Testing

### Running Tests

This package includes a test suite using ERT (Emacs Lisp Regression Testing).

#### Running Tests from Command Line

```bash
emacs -batch -l canon.el -l canon-test.el -f ert-run-tests-batch-and-exit
```

#### Running Tests Interactively

```elisp
;; Load the package and its tests
(load-file "canon.el")
(load-file "canon-test.el")

;; Run all tests
M-x ert RET t RET

;; Run a specific test
M-x ert RET canon-test-insert-entity-basic RET

;; Run tests matching a pattern
M-x ert RET "^canon-test-.*entity" RET
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
- **Homepage**: https://github.com/fudoniten/canon
