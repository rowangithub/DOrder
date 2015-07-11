CC=gcc
CFLAGS=-O3 -Wall -std=c99 -I. `pkg-config --cflags glib-2.0` -fnested-functions
LIBS=`pkg-config --libs glib-2.0`

ifdef CYGWIN
CFLAGS=-static -s -O3 -Wall -std=c99 -I. `pkg-config --cflags glib-2.0`
endif

gpa: gpa.o
	$(CC) $(CFLAGS) $^ $(LIBS) -o $@

gpa.o: gpa.c foreach.h msg.h
	$(CC) -c $(CFLAGS) $< -o $@

clean:
	-rm *.exe *.o gpa
