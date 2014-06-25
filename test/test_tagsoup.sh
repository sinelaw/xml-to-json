cat test/*.xml | xargs -rn1 -iyosi -t bash -c 'dist/build/tagsouptest/tagsouptest yosi  | aeson-pretty > /dev/null'
