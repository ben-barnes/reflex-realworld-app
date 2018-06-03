(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    conduit-common = ./conduit-common;
    conduit-backend = ./conduit-backend;
    conduit-frontend = ./conduit-frontend;
  };

  shells = {
    ghc = ["conduit-common" "conduit-backend" "conduit-frontend"];
    ghcjs = ["conduit-common" "conduit-frontend"];
  };
})
