
CURRENT_DIR = $(notdir $(shell pwd))
TARBALL	    = $(NAME)-$(PROCESSED_AUTHORS).tar.gz

all byte native setup.log: configure
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml $< -configure --enable-tests

setup.ml: _oasis
	oasis setup -setup-update dynamic

doc install uninstall reinstall: setup.log
	ocaml setup.ml -$@


# Make a tarball
tar dist: clean
	touch "$(TARBALL)"
	cd .. && tar --dereference --exclude="*~" --exclude="*.tar.gz" \
	  --exclude="._*" --exclude=".DS_Store" \
	  --exclude="doc/*.html" --exclude="doc/*.css" \
	  -zcvf "$(CURRENT_DIR)/$(TARBALL)" "$(CURRENT_DIR)"

clean::
	ocamlbuild -clean
	-$(RM) -r $(wildcard *~ *.tar.gz) *.docdir setup.data setup.log
	$(RM) $(wildcard $(addprefix doc/, *.html *.css *.aux *.log))

.PHONY: all byte native tar dist clean
