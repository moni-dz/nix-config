{ dotDir, home }:

with home;

''
  compinit -C -d ${dotDir}/zcompdump
  . /run/secrets/github-token
  rm ${homeDirectory}/.xsession-errors ${homeDirectory}/.xsession-errors.old >/dev/null 2>&1
''
