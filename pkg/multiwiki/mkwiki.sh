set -e
set -x

IFS='
'

tgzfile="$1"

test -e "$tgzfile"

mkdir -p ./out/{raw,clean,linked,wiki,final}

ghc -O3 wiki-words.hs
ghc -O3 find-title.hs

rm -f *.{dyn_hi,hi,dyn_o,o}

tar xfz "$tgzfile" --strip-components=2 -C ./out/raw

# Move non-xml files directly to the final output directory
mv $(file ./out/raw/* | grep -v XML | sed 's/\(.*\):      *[A-Z].*/\1/') ./out/final

# Move huge files directly to the final output directory
set +x
for x in $(cd ./out/raw; ls)
do if [[ "$(du -s "./out/raw/$x" | sed 's/[ \t][ \t]*.*//')" -gt 100 ]]
   then set -x; mv "./out/raw/$x" "./out/final/rawnote-$x.html"; set +x
   fi
done

set -x

for x in $(cd ./out/raw; ls)
do html2xhtml "./out/raw/$x" -o "./out/clean/$x"
done

for x in $(cd ./out/clean; ls)
do ./wiki-words "./out/clean/$x" "./out/linked/$x"
done

cp ./out/linked/* ./out/wiki/

(
  cd ./out/wiki

  for f in $(ls | egrep -v '\.html$')
  do mv "$f" "$f.html"
  done

  for f in $(ls)
  do mv "$f" "rawnote-$f"
  done
)

for x in $(cd ./out/linked; ls)
do title="$(./find-title "./out/linked/$x")"
   if [ -n "$title" ]
   then cp "./out/linked/$x" "./out/wiki/${title}.html"
   fi
done

cp ./out/wiki/* ./out/final
