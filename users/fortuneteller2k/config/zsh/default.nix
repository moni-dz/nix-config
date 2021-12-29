{ dotDir }:

''
  compinit -C -d ${dotDir}/zcompdump
  # You should comment this out, this is useless without my private key
  . /run/agenix/github-token
  alias vi="nvim"
''
