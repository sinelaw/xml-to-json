find ../../data/xmls/*.xml -type f | xargs -n1 -t -iyosi bash -c "dist/build/xml2json/xml2json yosi > yosi.js"
