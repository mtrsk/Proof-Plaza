{ nixpkgs ? import <nixpkgs> {}
, coq ? nixpkgs.coq
}:

with nixpkgs;

stdenv.mkDerivation {
  inherit coq;

  name = "coq-env";

  buildInputs = [
    (coq.override {
      buildIde = false;
    })
  ];
}
