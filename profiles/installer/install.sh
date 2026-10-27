umask 077

TARGET_SYSTEM="@targetSystem@"
DISKO_SCRIPT="@diskoScript@"
INSTALL_USER="@user@"
PASSWORD_AGE="@passwordAge@"
HOST_KEY_AGE="@hostKeyAge@"
MASTER_PUBKEY="@masterPubkey@"
DEFAULT_DEVICE="@defaultDevice@"
HOST_NAME="@hostName@"
placeholder="/dev/@""DEVICE""@"

dry=${INSTALLER_DRY_RUN:-}
if [[ -n $dry ]]; then
  PASSWORD_AGE=${INSTALLER_PASSWORD_AGE:-$PASSWORD_AGE}
  HOST_KEY_AGE=${INSTALLER_HOST_KEY_AGE:-$HOST_KEY_AGE}
  MASTER_PUBKEY=${INSTALLER_MASTER_PUBKEY:-$MASTER_PUBKEY}
fi

die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

run() {
  printf 'RUN: %s\n' "$*"
  if [[ -n $dry ]]; then
    [[ ${INSTALLER_FAIL_AT:-} != "$1" ]]
    return
  fi
  "$@"
}

run_stdin() {
  local input=$1
  shift
  if [[ -n $dry ]]; then
    printf 'RUN: %s (stdin: %s)\n' "$*" "$input"
    [[ ${INSTALLER_FAIL_AT:-} != "$1" ]]
    return
  fi
  printf 'RUN: %s\n' "$*"
  printf '%s\n' "$input" | "$@"
}

trim() {
  local s=$1
  s=${s%$'\r'}
  s=${s%"${s##*[![:space:]]}"}
  s=${s#"${s%%[![:space:]]*}"}
  printf '%s' "$s"
}

key_type_and_blob() {
  local type blob _rest
  read -r type blob _rest <<<"$1"
  printf '%s %s' "$type" "$blob"
}

cat <<NOTICE
installing $HOST_NAME...
the target disk will be erased. nothing is wiped until you confirm.
NOTICE

if [[ -n ${INSTALLER_WORKDIR:-} ]]; then
  work=$INSTALLER_WORKDIR
else
  work=$(mktemp -d)
  trap 'rm -rf "$work"' EXIT
fi

lsblk -dno NAME,SIZE,MODEL 2>/dev/null || true
read -r -p "Target device [$DEFAULT_DEVICE]: " device
device=$(trim "${device:-$DEFAULT_DEVICE}")

live=${INSTALLER_LIVE_DEVICE:-}
if [[ -z $live && -z $dry ]]; then
  live_source=$(findmnt -no SOURCE /iso 2>/dev/null || true)
  [[ -z $live_source ]] || live=/dev/$(lsblk -no PKNAME "$live_source" | head -n1)
fi
[[ $device != "$live" ]] || die "$device is the device this installer booted from"

if [[ -z $dry ]]; then
  [[ -b $device ]] || die "$device is not a block device"
  if lsblk -nro MOUNTPOINTS "$device" | grep -q .; then
    die "$device has mounted partitions"
  fi
fi

read -r -p "Type $device again to erase it: " confirm
[[ $(trim "$confirm") == "$device" ]] || die "confirmation did not match; nothing was changed"

echo "Paste the master private key, ending with its -----END line:"
: >"$work/master"
found_end=
while IFS= read -r line || [[ -n $line ]]; do
  line=$(trim "$line")
  printf '%s\n' "$line" >>"$work/master"
  if [[ $line == -----END*PRIVATE\ KEY----- ]]; then
    found_end=1
    break
  fi
done
[[ -n $found_end ]] || die "no -----END line found in the pasted key"

derived=$(ssh-keygen -y -f "$work/master") || die "that is not a valid private key"
[[ $(key_type_and_blob "$derived") == "$(key_type_and_blob "$MASTER_PUBKEY")" ]] ||
  die "the pasted key does not match the master identity"

password=$(age -d -i "$work/master" "$PASSWORD_AGE") || die "could not decrypt the default password"
age -d -i "$work/master" -o "$work/hostkey" "$HOST_KEY_AGE" || die "could not decrypt the host key"
ssh-keygen -y -f "$work/hostkey" >"$work/hostkey.pub" || die "the decrypted host key is invalid"

if [[ -n ${INSTALLER_SALT:-} ]]; then
  hash=$(printf '%s' "$password" | mkpasswd -m sha-512 -s -S "$INSTALLER_SALT")
else
  hash=$(printf '%s' "$password" | mkpasswd -m sha-512 -s)
fi

sed "s|$placeholder|$device|g" "$DISKO_SCRIPT" >"$work/disko"
chmod +x "$work/disko"
run "$work/disko"

run install -d -m 755 /mnt/etc/ssh
run install -m 600 "$work/hostkey" /mnt/etc/ssh/ssh_host_ed25519_key
run install -m 644 "$work/hostkey.pub" /mnt/etc/ssh/ssh_host_ed25519_key.pub

run nixos-install --system "$TARGET_SYSTEM" --no-root-passwd --no-channel-copy --root /mnt ||
  die "nixos-install failed after the disk was erased; do not reboot, fix the cause and run install-host again"

run_stdin "$derived" nixos-enter --root /mnt -c \
  "install -d -m 700 -o $INSTALL_USER -g users /home/$INSTALL_USER/.ssh && install -m 600 -o $INSTALL_USER -g users /dev/stdin /home/$INSTALL_USER/.ssh/authorized_keys"
run_stdin "$INSTALL_USER:$hash" nixos-enter --root /mnt -c 'chpasswd -e'

echo "Installed $HOST_NAME. Rebooting."
run reboot
