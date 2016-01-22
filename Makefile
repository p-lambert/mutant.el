PACKAGE := mutant
PACKAGE_FILE = $(PACKAGE).el

README.md: make-readme-markdown.el $(PACKAGE_FILE)
	emacs --script $< <$(PACKAGE_FILE) >$@ 2>/dev/null

clean:
	rm make-readme-markdown.el

make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

.INTERMEDIATE: make-readme-markdown.el
