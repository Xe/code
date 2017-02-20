VERSION=0.1
CFLAGS=-Werror -Wextra -Wall -O2 -g3
INCLUDES=-I./include/
NAME=liboleg-http.so

PREFIX?=/usr/local
INSTALL_LIB=$(PREFIX)/lib/
INSTALL_INCLUDE=$(PREFIX)/include/oleg-http/

all: lib

clean:
	rm -f *.o
	rm -f $(NAME)

%.o: ./src/%.c
	$(CC) $(CFLAGS) $(INCLUDES) -c -fPIC $<

lib: $(NAME)
$(NAME): oleg-http.o http.o
	$(CC) $(CLAGS) $(INCLUDES) -o $(NAME) -shared $^ -lm

install: lib
	@mkdir -p $(INSTALL_LIB)
	@mkdir -p $(INSTALL_INCLUDE)
	@install $(NAME) $(INSTALL_LIB)$(NAME).$(VERSION)
	@ln -fs $(INSTALL_LIB)$(NAME).$(VERSION) $(INSTALL_LIB)$(NAME)
	@ln -fs $(INSTALL_LIB)$(NAME).$(VERSION) $(INSTALL_LIB)$(NAME).0
	@cp ./include/oleg-http.h $(INSTALL_INCLUDE)
	@cp ./include/http.h $(INSTALL_INCLUDE)
	@ldconfig $(INSTALL_LIB)
	@echo "$(NAME) installed to $(PREFIX) :^)."
