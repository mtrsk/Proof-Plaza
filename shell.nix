{ nixpkgs ? import <nixpkgs> {}
, coq ? nixpkgs.coq
, ocaml ? nixpkgs.ocaml
}:

with nixpkgs;

stdenv.mkDerivation {
  inherit coq;
  inherit ocaml;

  name = "coq-env";

  buildInputs = [
    coq
  ] ++ (with coq.ocamlPackages;
    [ ocaml camlp5 findlib ]
  ) ++ (with coqPackages; [
    coq-ext-lib
    QuickChick
    ssreflect
  ]);
}
