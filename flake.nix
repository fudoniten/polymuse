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

        # Helper function to build Emacs packages
        buildEmacsPackage = { pname, version ? "0.1.0", src, dependencies ? [] }:
          pkgs.emacsPackages.trivialBuild {
            inherit pname version src;
            packageRequires = dependencies;
          };

        # Helper to filter source files for a specific package
        filterSource = pname: src:
          pkgs.lib.cleanSourceWith {
            inherit src;
            filter = path: type:
              let
                baseName = baseNameOf path;
                # Include only the main file and not test files or other packages
                isMainFile = baseName == "${pname}.el";
                isTestFile = pkgs.lib.hasSuffix "-test.el" baseName;
                isOtherPackage = (baseName == "polymuse.el" && pname != "polymuse") ||
                                 (baseName == "canon.el" && pname != "canon") ||
                                 (baseName == "typewrite.el" && pname != "typewrite");
              in
                # Include if it's the main file, or if it's not a test file and not another package
                isMainFile || (!isTestFile && !isOtherPackage && baseName != "flake.nix" && baseName != "flake.lock");
          };
      in
      {
        packages = rec {
          # typewrite.el - Typewriter effect for text insertion
          typewrite = buildEmacsPackage {
            pname = "typewrite";
            src = filterSource "typewrite" ./.;
            version = "0.1.0";
          };

          # canon.el - Creative work entity management
          canon = buildEmacsPackage {
            pname = "canon";
            src = filterSource "canon" ./.;
            version = "0.1.0";
          };

          # polymuse.el - AI-powered coding assistant
          polymuse = buildEmacsPackage {
            pname = "polymuse";
            src = filterSource "polymuse" ./.;
            version = "0.1.0";
            dependencies = with pkgs.emacsPackages; [
              gptel
              markdown-mode
            ];
          };

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
          # Helper to filter source files for a specific package in overlay
          filterSource = pname: src:
            final.lib.cleanSourceWith {
              inherit src;
              filter = path: type:
                let
                  baseName = baseNameOf path;
                  # Include only the main file and not test files or other packages
                  isMainFile = baseName == "${pname}.el";
                  isTestFile = final.lib.hasSuffix "-test.el" baseName;
                  isOtherPackage = (baseName == "polymuse.el" && pname != "polymuse") ||
                                   (baseName == "canon.el" && pname != "canon") ||
                                   (baseName == "typewrite.el" && pname != "typewrite");
                in
                  # Include if it's the main file, or if it's not a test file and not another package
                  isMainFile || (!isTestFile && !isOtherPackage && baseName != "flake.nix" && baseName != "flake.lock");
            };
        in
        {
          emacsPackages = prev.emacsPackages // {
            typewrite = final.emacsPackages.trivialBuild {
              pname = "typewrite";
              version = "0.1.0";
              src = filterSource "typewrite" self;
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
              src = filterSource "canon" self;
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
              src = filterSource "polymuse" self;
              packageRequires = with final.emacsPackages; [
                gptel
                markdown-mode
              ];
              buildPhase = ''
                runHook preBuild
                emacs -L . --batch -f batch-byte-compile polymuse.el
                runHook postBuild
              '';
            };
          };
        };
    };
}
