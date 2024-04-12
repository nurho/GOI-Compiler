let
  pkgs = import <nixpkgs> { };
in
myNixPkgs.mkShell {
  nativeBuildInputs = with myNixPkgs; [
    ghc
    cabal-install
    haskell-language-server
  ];
}
