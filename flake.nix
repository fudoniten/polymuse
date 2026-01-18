{
  description = "Polymuse - AI-powered over-the-shoulder coding assistant for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = rec {
          polymuse = pkgs.emacsPackages.trivialBuild {
            pname = "polymuse";
            version = "0.1.0";
            src = ./.;
            packageRequires = with pkgs.emacsPackages; [
              gptel
              markdown-mode
            ];
          };

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
        emacsPackages = prev.emacsPackages // {
          polymuse = final.emacsPackages.trivialBuild {
            pname = "polymuse";
            version = "0.1.0";
            src = self;
            packageRequires = with final.emacsPackages; [
              gptel
              markdown-mode
            ];
          };
        };
      };
    };
}
