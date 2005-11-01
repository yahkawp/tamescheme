# Really basic Mono Makefile

all: Scheme.dll SchemeTest.exe

Scheme.dll: Scheme/*.cs Scheme/*/*.cs Scheme/*/*/*.cs
	mcs -target:module -out:Scheme.dll -recurse:"Scheme/*.cs"

SchemeTest.exe: Scheme.dll SchemeTest/*.cs
	mcs -target:exe -addmodule:Scheme.dll -out:SchemeTest.exe -recurse:"SchemeTest/*.cs"

SchemeUnit.exe: Scheme.dll SchemeUnit/*.cs SchemeUnit/*/*.cs SchemeUnit/*/*/*.cs
	mcs -target:exe -addmodule:Scheme.dll -out:SchemeUnit.exe -recurse:"SchemeUnit/*.cs" -define:NoTestWait

test: SchemeUnit.exe
	mono SchemeUnit.exe
