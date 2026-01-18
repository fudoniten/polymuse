{
  description = "Canon - Creative work entity management system for Emacs";

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
          canon = pkgs.emacsPackages.trivialBuild {
            pname = "canon";
            version = "0.1.0";
            src = ./.;
            packageRequires = [];
          };

          default = canon;
        };

        # Development shell with Emacs
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            emacs
          ];
        };
      }
    ) // {
      # Overlay for use in NixOS/Home Manager configurations
      overlays.default = final: prev: {
        emacsPackages = prev.emacsPackages // {
          canon = final.emacsPackages.trivialBuild {
            pname = "canon";
            version = "0.1.0";
            src = self;
            packageRequires = [];
          };
        };
      };
    };
}
