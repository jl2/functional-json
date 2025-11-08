# Test functional-json

test: t/*.lisp *.lisp *.asd Makefile
	sbcl --eval "(ql:quickload :functional-json.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

clean:
	rm -Rf *.fasl

.PHONY: clean test
