EBINDIR  ?= ebin
SRCDIR    = src
ERLC     ?= erlc
APP_DEF   = idris_cerl.app
ESRCS     = $(notdir $(wildcard $(SRCDIR)/*.erl))
EBINS     = $(ESRCS:.erl=.beam)

.SUFFIXES: .erl .beam

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl
	@ mkdir -p $(EBINDIR)
	$(ERLC) -o $(EBINDIR) $^

all: compile

.PHONY: compile clean

compile: $(addprefix $(EBINDIR)/, $(EBINS)) $(EBINDIR)/$(APP_DEF)

$(EBINDIR)/$(APP_DEF): $(SRCDIR)/$(APP_DEF).src
	cp $(SRCDIR)/$(APP_DEF).src $(EBINDIR)/$(APP_DEF)

clean:
	$(RM) $(addprefix $(EBINDIR)/, $(EBINS)) $(EBINDIR)/$(APP_DEF)
