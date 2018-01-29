#!/bin/bash
# Author: Subj (subjrs@gmail.com)
#
###
mass=("_1" "_2" "_3" "_4" "_5" "_6" "_7" "_8" "_9" "10" "11" "12" "13" "14" "15" "__")
label=16
###
function cecho
{
  red='\E[31;48m'
  echo -en "\033[1m"$red
  echo -en $1"\033[0m"
  tput sgr0
}
###
function tprint
{
  clear
  echo '-------------'
  for i in `seq 1 4`; do
    echo -n '|'
    for j in `seq 1 4`;do
      (( c=$i*4-(4-$j)-1 ))
      c1=${mass[$c]}
      (( c++ ))
      if [ $c -eq $label ]; then
        cecho $c1
      else
        echo -n $c1
      fi
      echo -n '|'
    done
    echo
  done
  echo '-------------'
}
###
function crnd
{
  count=0
  while [ : ]; do
    rnd=$RANDOM
    let "rnd %= 16 "
    (( rnd++ ))
    fm="$rnd"
    if [ $rnd -eq 16 ]; then
      fm="__"
    elif [ $rnd -lt 10 ]; then
      fm="_"$rnd
    fi
    for i in `seq 0 $count`; do
      if [ "${mass[$i]}" == "$fm" ]; then
        continue 2
      fi
    done
    mass[$count]=$fm
    (( count++ ))
    if [ $count -eq 16 ]; then
      break
    fi
  done
}
###
# Begin
if [ "$1" == "--help" ]; then
  echo "n - New game"
  echo "z - Move"
  echo "q - Quit"
  exit 0
fi
###
crnd
tprint
char=""
###
# Main loop
while [ "$char" != "q" ]; do
  read -s -n1 char
  case "$char" in
    "A"   )
      if [ $label -gt 4 ]; then
        (( label-=4 ))
      fi
      # Up
     ;;
    "B"   )
      if [ $label -lt 13 ]; then
        (( label+=4 ))
      fi
      # Down
     ;;
    "C"   )
      if [ $label -ne 16 ]; then
        (( label++ ))
      fi
      # Right
     ;;
    "D"   )
      if [ $label -ne 1 ]; then
        (( label-- ))
      fi
      # Left
     ;;
    "n"|"N"  )
      crnd
     ;;
    "z"|"Z"  )
      ###
      # Left
      if [ $label -ne 1 -a $label -ne 5 -a $label -ne 9 -a $label -ne 13 ]; then
        if [ ${mass[$(( $label-1-1 ))]} = "__" ]; then
          mass[$(( $label-1-1 ))]=${mass[$(( $label-1 ))]}
          mass[$(( $label-1 ))]="__"
        fi
      fi
      # Right
      if [ $label -ne 4 -a $label -ne 8 -a $label -ne 12 -a $label -ne 16 ]; then
        if [ ${mass[$(( $label+1-1 ))]} = "__" ]; then
          mass[$(( $label+1-1 ))]=${mass[$(( $label-1 ))]}
                                        mass[$(( $label-1 ))]="__"
        fi
      fi
      # Up
      if [ $label -gt 4 ]; then
        if [ ${mass[$(( $label-4-1 ))]} = "__" ]; then
          mass[$(( $label-4-1 ))]=${mass[$(( $label-1 ))]}
                                        mass[$(( $label-1 ))]="__"
        fi
      fi
      # Down
      if [ $label -lt 13 ]; then
        if [ ${mass[$(( $label+4-1 ))]} = "__" ]; then
          mass[$(( $label+4-1 ))]=${mass[$(( $label-1 ))]}
                                        mass[$(( $label-1 ))]="__"
        fi
      fi
     ;;
      ###
  esac
  tprint
done
#
exit 0
