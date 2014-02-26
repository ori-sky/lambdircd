#!/bin/sh
for f in spec/*.hs
do
    runhaskell -isrc $f
done
