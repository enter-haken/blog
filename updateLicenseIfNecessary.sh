#!/bin/sh
if [ ! -f LICENSE ]; then
    break; 
fi

copyright=$(git log --pretty=format:"%an|%ad" \
            --date=format:%Y | sort | uniq |  \
    awk 'BEGIN {FS="|"}                       \
    {                                         \
      if ($1==prev) {                         \
              sec=sec "," $2;                 \
      }                                       \
      else {                                  \
          if (prev) {                         \
              print "(c) " sec " " prev;      \
          };                                  \
          prev=$1;                            \
          sec=$2;                             \
      }                                       \
    }                                         \
    END {                                     \
        if (prev) {                           \
            print "(c) " sec " " prev;        \
        }                                     \
    }')

license=$(cat LICENSE | sed -e "s/(c).*$/$copyright/g")
echo "$license" > LICENSE
