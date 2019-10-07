{ pkgs ? import ./pinned-nixpkgs.nix {}
, coq ? pkgs.coq
}:

let
  systemPkgs = with pkgs; [
    coq
    ocaml
  ];
  ocamlPkgs = with pkgs.ocamlPackages; [
    camlp5
    findlib
  ];
  coqPkgs = with pkgs.coqPackages; [
    coq-ext-lib
    QuickChick
    ssreflect
  ];
  gitignoreSource = import ./gitignore.nix {};
in
pkgs.stdenv.mkDerivation {
  name = "coq-env";

  src = gitignoreSource ./.;

  buildInputs = systemPkgs ++ ocamlPkgs ++ coqPkgs;

  buildPhase = ''
    mkdir $out
  '';

  installPhase = ''
    # Build logical foundations

    files=(
      "Basics.v"
      "Induction.v"
      "Lists.v"
      "Poly.v"
      "Tactics.v"
      "Logic.v"
      "IndProp.v"
      "Maps.v"
      "ProofObjects.v"
    )

    for i in "$files";do
      coqc -Q LF/ LF LF/"$i"
    done
  '';
}
