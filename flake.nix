{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      haskellPackages = pkgs.haskellPackages;
      project = haskellPackages.developPackage {
        root = ./.;
        modifier = drv: drv;
      };
    in
    {
      packages.${system}.default = project;
      devShells.${system}.default = haskellPackages.shellFor {
        packages = p: [ project ];
        buildInputs = with pkgs.haskellPackages; [
          haskell-language-server
          ghcid
          cabal-install
          hlint
        ];
      };
    };
}
