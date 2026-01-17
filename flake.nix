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

        # Helper to create filtered source with only specific files
        mkFilteredSrc = filename:
          pkgs.runCommand "filtered-src" {} ''
            mkdir -p $out
            cp ${self}/${filename} $out/
          '';

        # Package builder functions that can be used with any emacsPackages scope
        mkTypewrite = epkgs: epkgs.trivialBuild {
          pname = "typewrite";
          version = "0.1.0";
          src = mkFilteredSrc "typewrite.el";
          packageRequires = [];
        };

        mkCanon = epkgs: epkgs.trivialBuild {
          pname = "canon";
          version = "0.1.0";
          src = mkFilteredSrc "canon.el";
          packageRequires = [];
        };

        mkPolymuse = epkgs: epkgs.trivialBuild {
          pname = "polymuse";
          version = "0.1.0";
          src = mkFilteredSrc "polymuse.el";
          packageRequires = with epkgs; [
            gptel
            markdown-mode
          ];
        };
      in
      {
        packages = rec {
          # typewrite.el - Typewriter effect for text insertion
          typewrite = mkTypewrite pkgs.emacsPackages;

          # canon.el - Creative work entity management
          canon = mkCanon pkgs.emacsPackages;

          # polymuse.el - AI-powered coding assistant
          polymuse = mkPolymuse pkgs.emacsPackages;

          # Default package is polymuse (the main package)
          default = polymuse;
        };

        # Export the builder functions for use in overrideScope
        lib = {
          inherit mkTypewrite mkCanon mkPolymuse;
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
      overlays.default = final: prev:
        let
          # Helper to create filtered source with only specific files
          mkFilteredSrc = filename:
            final.runCommand "filtered-src" {} ''
              mkdir -p $out
              cp ${self}/${filename} $out/
            '';
        in {
          emacsPackages = prev.emacsPackages.overrideScope (eself: esuper: {
            typewrite = eself.trivialBuild {
              pname = "typewrite";
              version = "0.1.0";
              src = mkFilteredSrc "typewrite.el";
              packageRequires = [];
            };

            canon = eself.trivialBuild {
              pname = "canon";
              version = "0.1.0";
              src = mkFilteredSrc "canon.el";
              packageRequires = [];
            };

            polymuse = eself.trivialBuild {
              pname = "polymuse";
              version = "0.1.0";
              src = mkFilteredSrc "polymuse.el";
              packageRequires = with eself; [
                gptel
                markdown-mode
              ];
            };
          });
        };
    };
}
