{
  description = "Polymuse - AI-powered over-the-shoulder coding assistant for Emacs";

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
      in
      {
        packages = rec {
          polymuse = pkgs.emacsPackages.trivialBuild {
            pname = "polymuse";
            version = "0.1.0";
            src = ./.;
            packageRequires = (with pkgs.emacsPackages; [
              gptel
              markdown-mode
            ]) ++ [ typewritePkg ];
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
      }
    ) // {
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
