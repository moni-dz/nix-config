''
  #!/bin/sh

  printf "\n"

  for i in $(seq 0 15); do
    [ "$i" -eq 8 ] && tput sgr0 && printf "\n"
      tput setab "$i"
    printf "    "
  done

  tput sgr0
  printf "\n"
''
