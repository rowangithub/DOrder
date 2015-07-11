###############################################################################
#
#
# Author: Bow-Yaw Wang
# Copyright reserved
###############################################################################

CC=gcc
AR=ar
RANLIB=ranlib
CFLAGS=-pthread -g

SOURCES = bitvector.c vector.c cdnfformula.c boolformula.c cdnf.c
OBJS = bitvector.o vector.o cdnfformula.o boolformula.o cdnf.o
TARGET = libcdnfp.a

all:	$(TARGET)

$(TARGET): $(OBJS)
	rm -f $(TARGET)
	$(AR) q $(TARGET) $(OBJS)
	$(RANLIB) $(TARGET)

clean:
	rm -f $(OBJS) $(TARGET)

.c.o:
	$(CC) $(CFLAGS) -c -fPIC $<

