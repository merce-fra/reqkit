dune clean
rm -Rf _coverage
rm *.coverage
BISECT_ENABLE=yes dune build
./_build/default/req2something.exe --input-dir /home/cellier/req2something/reqs 
bisect-ppx-report html
echo "Open _coverage/index.html file with your favorite web browser to see coverage"