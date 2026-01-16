# Using Polymuse with Nix

This repository provides a Nix flake with an overlay for easy integration into NixOS or Home Manager configurations.

## Quick Start

### Using with Home Manager

Add the following to your Home Manager configuration:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    polymuse.url = "github:fudoniten/polymuse";
    # ... other inputs
  };

  outputs = { self, nixpkgs, polymuse, ... }: {
    homeConfigurations.youruser = {
      # Apply the polymuse overlay
      nixpkgs.overlays = [ polymuse.overlays.default ];

      # Then use the packages in your Emacs configuration
      programs.emacs = {
        enable = true;
        extraPackages = epkgs: with epkgs; [
          # Polymuse packages are now available
          polymuse
          canon
          typewrite

          # They automatically include their dependencies
          # (gptel and markdown-mode for polymuse)
        ];
      };
    };
  };
}
```

### Using with NixOS

In your `flake.nix`:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    polymuse.url = "github:fudoniten/polymuse";
  };

  outputs = { self, nixpkgs, polymuse, ... }: {
    nixosConfigurations.yourhostname = nixpkgs.lib.nixosSystem {
      modules = [
        {
          nixpkgs.overlays = [ polymuse.overlays.default ];

          environment.systemPackages = with pkgs; [
            (emacsWithPackages (epkgs: with epkgs; [
              polymuse
              canon
              typewrite
            ]))
          ];
        }
      ];
    };
  };
}
```

### Using Packages Directly (without overlay)

You can also reference the packages directly from the flake using `overrideScope`:

```nix
{
  inputs.polymuse.url = "github:fudoniten/polymuse";

  outputs = { self, nixpkgs, polymuse, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # Method 1: Using overrideScope (recommended for proper integration)
      programs.emacs = {
        enable = true;
        package = pkgs.emacs;
        extraPackages = epkgs:
          let
            polymuseScope = epkgs.overrideScope (eself: esuper: {
              inherit (polymuse.packages.${system}) polymuse canon typewrite;
            });
          in with polymuseScope; [
            polymuse
            canon
            typewrite
          ];
      };

      # Method 2: Direct package references (simpler but less integrated)
      programs.emacs.extraPackages = epkgs: [
        polymuse.packages.${system}.polymuse
        polymuse.packages.${system}.canon
        polymuse.packages.${system}.typewrite
      ];
    };
}
```

## Available Packages

The overlay adds three packages to `emacsPackages`:

- **`polymuse`** - AI-powered over-the-shoulder coding assistant
  - Dependencies: gptel, markdown-mode
- **`canon`** - Creative work entity management system
  - No additional dependencies
- **`typewrite`** - Typewriter-style text insertion effect
  - No additional dependencies

## Development Shell

To get a development environment with Emacs and all dependencies:

```bash
nix develop github:fudoniten/polymuse
```

Or if you've cloned the repository:

```bash
cd polymuse
nix develop
```

## Testing the Flake

To test that the packages build correctly:

```bash
# Check flake structure
nix flake check

# Build individual packages
nix build .#polymuse
nix build .#canon
nix build .#typewrite

# Try the packages in a temporary Emacs instance
nix run nixpkgs#emacs -- --quick \
  --eval "(add-to-list 'load-path \"$(nix build .#polymuse --no-link --print-out-paths)/share/emacs/site-lisp\")" \
  --eval "(require 'polymuse)"
```

## Local Development

If you're developing the packages locally and want to use your local checkout in your Nix config:

```nix
{
  inputs.polymuse.url = "path:/path/to/your/local/polymuse";
  # or
  inputs.polymuse.url = "git+file:///path/to/your/local/polymuse";
}
```

## Emacs Configuration Example

After installing via Nix, configure polymuse in your Emacs init file:

```elisp
;; Polymuse configuration
(use-package polymuse
  :commands (polymuse-mode polymuse-add-reviewer)
  :bind (:map polymuse-mode-map
              ("C-c C-r e" . polymuse-edit-instructions)
              ("C-c C-r r" . polymuse-run-review))
  :config
  (setq polymuse-ollama-hostname "localhost:11434"
        polymuse-ollama-protocol "http"
        polymuse-default-interval 60
        polymuse-max-prompt-characters 12000)

  ;; Enable in prog-mode
  (add-hook 'prog-mode-hook #'polymuse-mode))

;; Canon configuration
(use-package canon
  :commands (canon-mode canon-insert-entity canon-jump-to-entity)
  :init
  (setq-default canon-file "~/writing/canon.org"))

;; Typewrite is loaded automatically as a dependency
```
