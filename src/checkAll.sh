#!/bin/bash

./check.sh $(ls *.hs | cut -d "." -f 1  | sort -n)
