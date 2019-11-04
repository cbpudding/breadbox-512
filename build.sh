#!/bin/bash
dd if=/dev/zero of=game.flp bs=1K count=1440 status=none
nasm -f bin -o boot.bin boot.asm
dd if=boot.bin of=game.flp conv=notrunc status=none
