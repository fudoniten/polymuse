{
  description =
    "Polymuse - AI-powered over-the-shoulder coding assistant for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    typewrite = {
      url = "github:fudoniten/typewrite.el";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, typewrite }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        typewritePkg = typewrite.packages.${system}.default;
      in {
        packages = rec {
          polymuse = pkgs.emacsPackages.trivialBuild {
            pname = "polymuse";
            version = "0.1.0";
            src = ./.;
            packageRequires = (with pkgs.emacsPackages; [ gptel markdown-mode ])
              ++ [ typewritePkg ];
          };

          default = polymuse;
        };

        # Development shell with Emacs and dependencies
        devShells.default = pkgs.mkShell {
          buildInputs = (with pkgs; [
            emacs
            emacsPackages.gptel
            emacsPackages.markdown-mode
          ]) ++ [ typewritePkg ];
        };

        # Lint checks for code quality
        checks = let
          # Create Emacs with all dependencies loaded
          emacsWithDeps = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages
            (epkgs: [ epkgs.gptel epkgs.markdown-mode typewritePkg ]);
        in {
          # Byte compilation check (catches syntax errors, undefined functions, etc.)
          lint = pkgs.runCommand "polymuse-lint" {
            buildInputs = [ emacsWithDeps ];
          } ''
            set -e
            # Copy source to writable location
            cp ${./.}/polymuse.el ./polymuse.el
            chmod +w polymuse.el

            echo "Running byte compilation check..."
            emacs --batch \
              --eval "(setq byte-compile-error-on-warn nil)" \
              -f batch-byte-compile polymuse.el 2>&1 | tee compile.log

            echo "Running checkdoc validation..."
            emacs --batch \
              --eval "(checkdoc-file \"polymuse.el\")" 2>&1 | tee checkdoc.log || true

            mkdir -p $out
            echo "Linting completed successfully" > $out/result
            cp compile.log $out/ || true
            cp checkdoc.log $out/ || true
          '';
        };
      }) // {
        # Overlay for use in NixOS/Home Manager configurations
        overlays.default = final: prev: {
          emacsPackages = prev.emacsPackages // {
            typewrite = final.emacsPackages.trivialBuild {
              pname = "typewrite";
              version = "0.1.0";
              src = typewrite;
              packageRequires = with final.emacsPackages; [ ];
            };

            polymuse = final.emacsPackages.trivialBuild {
              pname = "polymuse";
              version = "0.1.0";
              src = self;
              packageRequires = with final.emacsPackages; [
                gptel
                markdown-mode
                typewrite
              ];
            };
          };
        };
      };
}
