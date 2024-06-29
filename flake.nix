{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
  }: let
    system = "x86_64-linux";
    ghcVersion = "94"; # needs to be synced to stack ghc version
  in {
    devShells.${system}.default = let
      pkgs = import nixpkgs {
        inherit system;
      };
    in
      pkgs.mkShell {
        buildInputs = with pkgs; [
          stack
          (pkgs.haskell-language-server.override {supportedGhcVersions = [ghcVersion];})
        ];
      };
  };
}
