# [NixOS](https://nixos.org) configuration

![NixOS](assets/nixos-official-logo.png)

[![check](https://github.com/moni-dz/nix-config/actions/workflows/check.yml/badge.svg)](https://github.com/moni-dz/nix-config/actions/workflows/check.yml)

## Description

NixOS/nix-darwin/hjem configuration that I use daily.

See also:

- [hjem](https://github.com/feel-co/hjem) and [hjem-rum](https://github.com/snugnug/hjem-rum)
- [flakes](https://wiki.nixos.org/wiki/Flakes)
- [flake-parts](https://flake.parts), for the flake outputs, e.g. [my system configurations](https://github.com/moni-dz/nix-config/blob/master/hosts/default.nix)
- [nix-darwin](https://github.com/LnL7/nix-darwin), for `hosts/riscake`

Resources and configurations I used to base on:

- [colemickens/nixos-flake-example](https://github.com/colemickens/nixos-flake-example)
- [NixOS modules on NixOS Wiki](https://wiki.nixos.org/wiki/NixOS_modules)
- [Writing NixOS modules](https://nixos.org/manual/nixos/stable/index.html#sec-writing-modules)
- [Nix Flakes talk by Eelco Dolstra on NixCon](https://www.youtube.com/watch?v=UeBX7Ide5a0)
- [flake.parts module arguments](https://flake.parts/module-arguments.html)

## Directory Map

```text
hosts/       per-machine configs (hands: NixOS, riscake: nix-darwin)
modules/     custom modules (common, nixos, darwin, hjem)
profiles/    reusable config shared between hosts (nix settings, hjem programs)
packages/    overlays and custom packages (flake partition)
dev/         dev tooling: treefmt (flake partition)
secrets/     agenix-rekey secrets (`agenix` in the dev shell), with per-host copies in `rekeyed/`
```

### Why flakes?

[jonringer](https://github.com/jonringer) gave a [brief explanation](https://discourse.nixos.org/t/what-are-nix-flakes-and-why-should-i-care/12910/3) as to why one might want to use Nix flakes.

### Why `flake-parts`?

`flake-parts` allow reusable configuration powered by the module system everyone knows and loves.

See [my system configurations](https://github.com/moni-dz/nix-config/blob/master/hosts/default.nix), which build each system directly with `withSystem`.

## Installer ISOs

`hands` and `hyper-v` each have an installer ISO that bakes in the host's system and installs it with
almost no input. Both erase the target disk.

One-time setup (needs the master key, `~/.ssh/id_ed25519`):

```sh
# a host key per host, encrypted to the master key; hostPubkey comes from secrets/host-keys/<host>.pub
agenix rekey -a
printf %s "$password" | age -r "$(cat ~/.ssh/id_ed25519.pub)" -o secrets/default-password.age
```

Build (needs a Linux builder, `hyper-v` works; `hands` also needs `i686-linux`):

```sh
nix build .#nixosConfigurations.installer-hyper-v.config.system.build.isoImage
nix build .#nixosConfigurations.installer-hands.config.system.build.isoImage
```

Boot the ISO, approve the Tailscale login URL on the console, then from the Mac:

```sh
rex -S https://root@installer-<host>
install-host
```

`install-host` asks for the target device, a typed confirmation, and the master private key. It
installs the host, places its agenix host key, and sets the default password for the login user, which
you can change afterwards with `passwd`. A machine still running with an old host key has to be
reinstalled with the ISO rather than switched to the rekeyed configuration.
