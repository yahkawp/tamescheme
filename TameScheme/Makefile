# Really basic Mono Makefile

all: Scheme.dll SchemeTest.exe

Scheme.dll: Scheme/*.cs Scheme/*/*.cs Scheme/*/*/*.cs
	mcs -target:module -out:Scheme.dll -recurse:"Scheme/*.cs"

SchemeUI.dll: SchemeUI/*.cs SchemeUI/*/*.cs
	mcs -target:module -addmodule:Scheme.dll -out:SchemeUI.dll -recurse:"SchemeUI/*.cs"

SchemeTest.exe: Scheme.dll SchemeUI.dll SchemeTest/*.cs
	mcs -target:exe -addmodule:Scheme.dll -addmodule:SchemeUI.dll -out:SchemeTest.exe -recurse:"SchemeTest/*.cs"

SchemeUnit.exe: Scheme.dll SchemeUnit/*.cs SchemeUnit/*/*.cs SchemeUnit/*/*/*.cs
	mcs -target:exe -addmodule:Scheme.dll -out:SchemeUnit.exe -recurse:"SchemeUnit/*.cs" -define:NoTestWait

test: SchemeUnit.exe
	mono SchemeUnit.exe
