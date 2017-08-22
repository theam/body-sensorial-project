let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          app =
            haskellPackagesNew.callPackage ./app.nix {
              R = pkgs.R;
              data_table = pkgs.rPackages.data_table;
              zoo = pkgs.rPackages.zoo;
              ffmpeg = pkgs.ffmpeg;
              sox = pkgs.sox;
            };

          analyze =
            haskellPackagesNew.callPackage ./analyze.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { app = pkgs.haskellPackages.app;
  }
