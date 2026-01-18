{
  description = "Typewrite - Typewriter effect for Emacs text insertion";

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
          typewrite = pkgs.emacsPackages.trivialBuild {
            pname = "typewrite";
            version = "0.1.0";
            src = ./.;
            packageRequires = [];
          };

          default = typewrite;
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
          typewrite = final.emacsPackages.trivialBuild {
            pname = "typewrite";
            version = "0.1.0";
            src = self;
            packageRequires = [];
          };
        };
      };
    };
}
