# Really basic Mono Makefile

all: Scheme.dll SchemeTest.exe

Scheme.dll: Scheme/*.cs Scheme/*/*.cs Scheme/*/*/*.cs
	mcs -target:module -out:Scheme.dll -recurse:"Scheme/*.cs"

SchemeTest.exe: Scheme.dll SchemeTest/*.cs
	mcs -target:exe -addmodule:Scheme.dll -out:SchemeTest.exe -recurse:"SchemeTest/*.cs"
