
{
  description = "";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.writeShellApplication {
          name = "ytb-whisper";
          runtimeInputs = with pkgs;[
            (sbcl.withPackages (ps:
              with ps; [ alexandria arrow-macros serapeum defmain cl-ppcre trivia ]))
          ];
          text = ''
                 sbcl --script ${self}/main.lisp "$@"
                 '';
        };
      }
    );
}
