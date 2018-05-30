(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    reflex-rwa-common = ./reflex-rwa-common;
    reflex-rwa-backend = ./reflex-rwa-backend;
    reflex-rwa-frontend = ./reflex-rwa-frontend;
  };

  shells = {
    ghc = ["reflex-rwa-common" "reflex-rwa-backend" "reflex-rwa-frontend"];
    ghcjs = ["reflex-rwa-common" "reflex-rwa-frontend"];
  };
})
