{
  config,
  pkgs,
  lib,
  ...
}: {
  # TODO? This has currently no configuration options. Especially, the flake paths are unconfigurable and one of them is secret and only accessible by me…
  users.users.sfbs = {
    isSystemUser = true;
    group = "sfbs";
    home = "/var/lib/sfbs";
  };
  users.groups.sfbs = {};
  systemd.services.sfbs = let
    pkgDef = {rustPlatform}:
      rustPlatform.buildRustPackage {
        pname = "sfbs";
        inherit ((builtins.fromTOML (builtins.readFile ./build/Cargo.toml)).package) version;
        src = ./.;
        cargoLock = {
          lockFile = ./Cargo.lock;
          allowBuiltinFetchGit = true;
        };
      };
    pkg = pkgs.callPackage pkgDef {};
  in {
    path = [
      pkg
      pkgs.nushell
      pkgs.git
      pkgs.openssh
      config.nix.package
    ];
    script = ''
      ${./scripts}/gc.nu
      nix store gc -v
      ${./scripts}/up.nu
      ${./scripts}/bld.nu
    '';
    environment.SFBS_BUILD_ARGS = "-j1";
    serviceConfig = {
      User = "sfbs";
      Group = "sfbs";
      StateDirectory = "sfbs";
      WorkingDirectory = "/var/lib/sfbs";
      RuntimeMaxSec = "10h";
      PrivateTmp = true;
      LoadCredential = "id_ed25519:/etc/secrets/sfbs/id_ed25519";
      BindReadOnlyPaths = let
        cfg = pkgs.writeText "ssh_config" ''
          Host github.com
            User git
            IdentityFile ''${CREDENTIALS_DIRECTORY}/id_ed25519
          # i have the github pubkey in here
          GlobalKnownHostsFile ${lib.concatStringsSep " " config.programs.ssh.knownHostsFiles}
        '';
      in ["${cfg}:/etc/ssh/ssh_config"];
    };
  };
  systemd.timers.sfbs = {
    timerConfig.OnCalendar = "07:53:42 Asia/Tokyo";
    wantedBy = ["timers.target"];
  };
  nix.settings.keep-outputs = true; # necessary for the link derivation & gc thing
}
