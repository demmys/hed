COMPILER = ghc

TARGET = ed
SOURCE = Main.hs Ed.hs EdCommand.hs EdOption.hs EdExecute.hs EdError.hs

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SOURCE)
	$(COMPILER) -o $@ $^

clean:
	rm -f $(addsuffix .o, $(basename $(SOURCE)))
	rm -f $(addsuffix .hi, $(basename $(SOURCE)))
