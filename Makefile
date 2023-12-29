byte-compile:
	emacs -Q --batch --eval '(byte-compile-file "noman.el")'

checkdoc:
	emacs -Q --batch --eval '(checkdoc-file "noman.el")'
