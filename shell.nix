with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "cerl-shell";
  buildInputs = [ erlang rebar3-open ];
}
