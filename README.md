# [NixOS](https://nixos.org) configuration

![NixOS](assets/nixos-official-logo.png)

[![check](https://github.com/moni-dz/nix-config/actions/workflows/check.yml/badge.svg)](https://github.com/moni-dz/nix-config/actions/workflows/check.yml)

## Description

NixOS/nix-darwin/hjem configuration that I use daily.

See also:

- [hjem](https://github.com/feel-co/hjem) and [hjem-rum](https://github.com/snugnug/hjem-rum)
- [flakes](https://wiki.nixos.org/wiki/Flakes)
- [flake-parts](https://flake.parts), for the flake modules at [modules/parts](https://github.com/moni-dz/nix-config/blob/master/modules/parts)
- [nix-darwin](https://github.com/LnL7/nix-darwin), for `hosts/riscake`

Resources and configurations I used to base on:

- [colemickens/nixos-flake-example](https://github.com/colemickens/nixos-flake-example)
- [NixOS modules on NixOS Wiki](https://wiki.nixos.org/wiki/NixOS_modules)
- [Writing NixOS modules](https://nixos.org/manual/nixos/stable/index.html#sec-writing-modules)
- [Nix Flakes talk by Eelco Dolstra on NixCon](https://www.youtube.com/watch?v=UeBX7Ide5a0)
- [flake.parts module arguments](https://flake.parts/module-arguments.html)

### Why flakes?

[jonringer](https://github.com/jonringer) gave a [brief explanation](https://discourse.nixos.org/t/what-are-nix-flakes-and-why-should-i-care/12910/3) as to why one might want to use Nix flakes.

### Why `flake-parts`?

`flake-parts` allow reusable configuration powered by the module system everyone knows and loves.

See [modules/parts](https://github.com/moni-dz/nix-config/blob/master/modules/parts), and its implementation in [my system configurations](https://github.com/moni-dz/nix-config/blob/master/hosts/default.nix).
