{ pkgs }:

let
  installHost = import ./script.nix { inherit pkgs; } {
    targetSystem = "/nix/store/test-system";
    diskoScript = pkgs.writeText "disko-script" "wipe @DEVICE@\n";
    user = "tester";
    passwordAge = "/nonexistent";
    hostKeyAge = "/nonexistent";
    masterPubkey = "ssh-ed25519 REPLACED-AT-RUNTIME";
    defaultDevice = "/dev/vda";
    hostName = "testhost";
  };
in
pkgs.runCommand "installer-script-test"
  {
    nativeBuildInputs = [ installHost ] ++ __attrValues { inherit (pkgs) age mkpasswd openssh; };
  }
  ''
    export INSTALLER_DRY_RUN=1
    export INSTALLER_SALT=abcdefgh
    mkdir keys && cd keys

    ssh-keygen -q -t ed25519 -N "" -f master
    ssh-keygen -q -t ed25519 -N "" -f other
    ssh-keygen -q -t ed25519 -N "" -f host
    printf 'pw\n' | age -R master.pub -o password.age
    age -R master.pub -o hostkey.age host

    export INSTALLER_MASTER_PUBKEY="$(cut -d' ' -f1,2 master.pub)"
    export INSTALLER_PASSWORD_AGE=$PWD/password.age
    export INSTALLER_HOST_KEY_AGE=$PWD/hostkey.age

    # pasted key as the user would send it: CRLF endings and trailing spaces
    sed 's/$/ \r/' master > master.crlf

    run() { # run NAME STDIN-FILE-OR-TEXT -> out.NAME, status.NAME
      name=$1; shift
      export INSTALLER_WORKDIR=$PWD/work.$name
      mkdir -p "$INSTALLER_WORKDIR"
      set +e
      printf '%s' "$1" | install-host > out.$name 2>&1
      echo $? > status.$name
      set -e
    }
    fail() { echo "FAIL: $*"; for f in out.*; do echo "--- $f"; cat "$f"; done; exit 1; }
    ok() { [ "$(cat status.$1)" = 0 ] || fail "$1 expected success"; }
    bad() { [ "$(cat status.$1)" != 0 ] || fail "$1 expected failure"; }
    has() { grep -qF -- "$2" out.$1 || fail "$1 missing: $2"; }
    lacks() { ! grep -qF -- "$2" out.$1 || fail "$1 should not contain: $2"; }

    run wrongconfirm "$(printf '/dev/vda\nno\n'; cat master)"
    bad wrongconfirm; lacks wrongconfirm "RUN:"

    run default "$(printf '\n/dev/vda\n'; cat master)"
    ok default; has default "RUN: $PWD/work.default/disko"
    grep -q 'wipe /dev/vda' work.default/disko || fail "default device not substituted"

    run wrongkey "$(printf '/dev/vda\n/dev/vda\n'; cat other)"
    bad wrongkey; has wrongkey "does not match"; lacks wrongkey "RUN:"

    run crlf "$(printf '/dev/vda\n/dev/vda\n'; cat master.crlf)"
    ok crlf

    run pw "$(printf '/dev/vda\n/dev/vda\n'; cat master)"
    want=$(printf '%s' pw | mkpasswd -m sha-512 -s -S abcdefgh)
    has pw "tester:$want"

    grep -n '^RUN:' out.pw | sed 's/^[0-9]*://' > runs
    for step in "RUN: $PWD/work.pw/disko" "RUN: install -m 600" "RUN: nixos-install --system" \
                "authorized_keys" "chpasswd -e"; do
      grep -qF -- "$step" runs || fail "order: missing $step"
    done
    [ "$(grep -nF "work.pw/disko" runs | cut -d: -f1)" -lt "$(grep -nF "nixos-install" runs | cut -d: -f1)" ] \
      || fail "disko must run before nixos-install"
    [ "$(grep -nF "install -m 600" runs | head -1 | cut -d: -f1)" -lt "$(grep -nF "nixos-install" runs | cut -d: -f1)" ] \
      || fail "host key must be placed before nixos-install"
    [ "$(grep -nF "nixos-install" runs | cut -d: -f1)" -lt "$(grep -nF "chpasswd" runs | cut -d: -f1)" ] \
      || fail "chpasswd must follow nixos-install"

    has pw "install -m 600 $PWD/work.pw/hostkey /mnt/etc/ssh/ssh_host_ed25519_key"
    has pw "install -m 644 $PWD/work.pw/hostkey.pub /mnt/etc/ssh/ssh_host_ed25519_key.pub"
    ! grep '^RUN:' out.pw | grep -qF "work.pw/master" || fail "master key path appeared in a RUN line"

    INSTALLER_LIVE_DEVICE=/dev/vda run bootdev "$(printf '/dev/vda\n/dev/vda\n'; cat master)"
    bad bootdev; lacks bootdev "RUN:"

    INSTALLER_FAIL_AT=nixos-install run fail "$(printf '/dev/vda\n/dev/vda\n'; cat master)"
    bad fail; lacks fail "RUN: reboot"; has fail "do not reboot"

    for phrase in erased host-keys "not copied" "old host key"; do has pw "$phrase"; done
    [ "$(grep -nF erased out.pw | head -1 | cut -d: -f1)" -lt "$(grep -nF 'RUN:' out.pw | head -1 | cut -d: -f1)" ] \
      || fail "notice must precede any command"

    touch $out
  ''
