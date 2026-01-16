{
  description = "A collection of Emacs packages for enhanced writing and development workflows";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Helper function to build Emacs packages
        buildEmacsPackage = { pname, version ? "0.1.0", dependencies ? [] }:
          pkgs.emacsPackages.trivialBuild {
            inherit pname version;
            src = ./.;
            packageRequires = dependencies;

            # Ensure only the relevant .el file is included
            buildPhase = ''
              runHook preBuild
              emacs -L . --batch -f batch-byte-compile ${pname}.el
              runHook postBuild
            '';
          };
      in
      {
        packages = rec {
          # typewrite.el - Typewriter effect for text insertion
          typewrite = buildEmacsPackage {
            pname = "typewrite";
            version = "0.1.0";
          };

          # canon.el - Creative work entity management
          canon = buildEmacsPackage {
            pname = "canon";
            version = "0.1.0";
          };

          # polymuse.el - AI-powered coding assistant
          polymuse = buildEmacsPackage {
            pname = "polymuse";
            version = "0.1.0";
            dependencies = with pkgs.emacsPackages; [
              gptel
              markdown-mode
            ];
          };

          # Default package is polymuse (the main package)
          default = polymuse;
        };

        # Development shell with Emacs and dependencies
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacs
            emacsPackages.gptel
            emacsPackages.markdown-mode
          ];
        };
      }
    ) // {
      # Overlay for use in NixOS/Home Manager configurations
      overlays.default = final: prev: {
        emacsPackages = prev.emacsPackages.overrideScope (eself: esuper: {
          typewrite = final.emacsPackages.trivialBuild {
            pname = "typewrite";
            version = "0.1.0";
            src = self;
            packageRequires = [];
            buildPhase = ''
              runHook preBuild
              emacs -L . --batch -f batch-byte-compile typewrite.el
              runHook postBuild
            '';
          };

          canon = final.emacsPackages.trivialBuild {
            pname = "canon";
            version = "0.1.0";
            src = self;
            packageRequires = [];
            buildPhase = ''
              runHook preBuild
              emacs -L . --batch -f batch-byte-compile canon.el
              runHook postBuild
            '';
          };

          polymuse = final.emacsPackages.trivialBuild {
            pname = "polymuse";
            version = "0.1.0";
            src = self;
            packageRequires = with eself; [
              gptel
              markdown-mode
            ];
            buildPhase = ''
              runHook preBuild
              emacs -L . --batch -f batch-byte-compile polymuse.el
              runHook postBuild
            '';
          };
        });
      };
    };
}
