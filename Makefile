LISP ?= ${shell which sbcl}

example:
	$(LISP) --eval "(asdf:make :cot/example)" \
		--eval "(quit)"
