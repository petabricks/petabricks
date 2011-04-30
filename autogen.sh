#!/bin/sh

aclocal
autoheader
automake --foreign 
autoconf

