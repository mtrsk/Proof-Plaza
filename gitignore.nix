{pkgs ? import ./pinned-nixpkgs.nix {} }:

let
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "6c4ab20841d2a20cf69d52c8e848c4d6b0aa73fe";
    sha256 = "0nbwg01z0girs8c5zxg5zqivhny064rafzf47pygsmxlag0jiliq";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
in
  gitignoreSource
