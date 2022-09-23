BASENAME = sweep

EMACS   ?= emacs

.PHONY: clean check info

check: $(TARGET)
	$(EMACS) --batch --eval '(add-to-list (quote load-path) (expand-file-name "."))' \
		-l ert -l sweep -l sweep-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f $(BASENAME).info $(BASENAME).texi $(BASENAME).html

info: $(BASENAME).info

$(BASENAME).info:: README.org
	$(EMACS) -Q --batch --eval "(require 'ox-texinfo)" \
		--eval "(with-current-buffer (find-file \"README.org\") (org-export-to-file (quote texinfo) \"$@\" nil nil nil nil nil (quote org-texinfo-compile)))"
