EMACS=emacs
DESTDIR=/usr/share/emacs/site-lisp
EL = \
	autoload.el	\
	background.el	\
	cfg.el	\
	checkdoc.el	\
	codeline.el	\
	comment.el	\
	cvs.el	\
	defaultbanner.el	\
	defaultcontent.el	\
	desktop-phase.el	\
	eldoc.el	\
	expand.el	\
	file-log.el	\
	flashparen.el	\
	fnexpand.el	\
	foldingo.el	\
	font-lock-menu.el	\
	globrep.el	\
	granny.el	\
	header.el	\
	igrep.el	\
	iso-sgml.el	\
	lazy-lock.el	\
	lispdir.el	\
	locate.el	\
	m4-mode.el	\
	man-xref.el	\
	mode-compile.el	\
	mode-line.el	\
	module.el	\
	mouse-extras.el	\
	pdb.el	\
	python-mode.el	\
	refontify.el	\
	ssh.el	\
	steno.el	\
	todo-mode.el	\
	vapropos.el	\
	vrac.el	\
	webjump.el	\
	which-function.el	\
	word-help.el

ELC = $(EL:.el=.elc)

all:
	$(EMACS) -q -no-site-file -batch -f batch-byte-compile $(EL)

clean:
	rm -f $(ELC)

cleaninstall:
	cd $(DESTDIR); rm -f $(ELC)

install:
	install -m 644 $(ELC) $(DESTDIR)
