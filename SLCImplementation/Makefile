FSC = /usr/local/bin/fsharpc
FSCFLAGS = --debug:full --debug+
FSFILES =	\
    lexer.fs	\
    parser.fs	\
    form.fs	\
    preparse.fs	\
    slc.fs	\
    parse.fs	\
    unify.fs	\
    typesys.fs	\
    slctype.fs	\
    cbveval.fs	\
    slcint.fs	\
    toplev.fs	\
    Program.fs
TARGET = slc.exe

.PHONY: clean

$(TARGET): $(FSFILES)
	$(FSC) $(FSCFLAGS) $(FSFILES) -o:$(TARGET)

clean:
	rm -f $(TARGET) $(TARGET).mdb
