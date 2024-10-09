{
  description = "";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages.default = pkgs.writeShellApplication {
          name = "subtitle-merger";
          runtimeInputs = with pkgs;
            [
              (sbcl.withPackages (ps:
                with ps; [
                  alexandria
                  arrow-macros
                  serapeum
                  defmain
                  cl-ppcre
                  trivia
                ]))
            ];
          text = ''
            sbcl --script ${self}/main.lisp "$@"
          '';
        };
        packages.docker = pkgs.dockerTools.buildLayeredImage {
          name = "coruscation/subtitle-merger";
          tag = "latest";
          config = {
            Cmd = [ "${packages.default}/bin/subtitle-merger" ];
          };
        };
      });
}
