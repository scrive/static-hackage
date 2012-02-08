

Static Hackage
==============

Static hackage generator program

Installation
------------

    cabal install static-hackage

Usage
-----

    static-hackage package1.tar.gz package2.tar.gz ...

The above command will generate 00-index.tar.gz in current directory
and appropriate directory hierarchy in ./package/ subdirectory. As
such this is ready for serving via HTTP. You may use nginx or another
static file server for that purpose.

Files and directories generated:

    ./00-index.tar.gz       -- all .cabal files tared
    ./$N/$V/$N-$V.cabal     -- same cabal files, static
    ./package/$N-$V.tar.gz  -- full contents of package

Where $N is package name and $V is package version.
