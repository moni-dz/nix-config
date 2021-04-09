''
#!/usr/bin/env dash

printf "\n"

for i in 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15; do
  [ $i -eq 8 ] && tput sgr0 && printf "\n"
    tput setab $i
  printf "    "
done

tput sgr0
printf "\n"
''
