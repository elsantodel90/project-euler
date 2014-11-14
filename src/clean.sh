#!/bin/bash

rm -f $(find            . -maxdepth 1 -type f | grep -v "\.in" | grep -v "\.csv" | grep -v "svn" | grep -v "\.hs$" | grep -v "\.sh$")
rm -f $(find Elsantodel90 -maxdepth 1 -type f | grep -v "\.in" | grep -v "\.csv" | grep -v "svn" | grep -v "\.hs$" | grep -v "\.sh$")

