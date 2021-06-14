{ dotDir, home }:

with home;

''
  compinit -C -d ${dotDir}/zcompdump
  # You should comment this out, this is useless without my private key
  . /run/secrets/github-token
  rm ${homeDirectory}/.xsession-errors ${homeDirectory}/.xsession-errors.old >/dev/null 2>&1
''
