{
  description = "A collection of Emacs packages for enhanced writing and development workflows";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      # Package builder functions that can be used with any emacsPackages scope
      mkTypewrite = epkgs: epkgs.trivialBuild {
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

      mkCanon = epkgs: epkgs.trivialBuild {
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

      mkPolymuse = epkgs: epkgs.trivialBuild {
        pname = "polymuse";
        version = "0.1.0";
        src = self;
        packageRequires = with epkgs; [
          gptel
          markdown-mode
        ];
        buildPhase = ''
          runHook preBuild
          emacs -L . --batch -f batch-byte-compile polymuse.el
          runHook postBuild
        '';
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
      overlays.default = final: prev: {
        emacsPackages = prev.emacsPackages.overrideScope (eself: esuper: {
          typewrite = mkTypewrite eself;
          canon = mkCanon eself;
          polymuse = mkPolymuse eself;
        });
      };
    };
}
