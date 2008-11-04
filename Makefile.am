AUTOMAKE_OPTIONS = foreign
SUBDIRS = src

check:
	+./scripts/smoketest.sh

examples: all
	+(cd examples && make)

