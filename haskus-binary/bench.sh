#!/bin/sh

sizes=(128b 3K 3M 15M)

function bench_haskus {
   echo "================================"
   echo "| Benchmarking haskus-binary   |"
   echo "================================"

   echo "Warming up..."
   dd if=/dev/zero of=bench/data.bin bs=256 count=1 2> /dev/null
   stack exec -- ghc -v0 bench/BenchEmbed.hs -fforce-recomp -O > /dev/null
   rm -f bench/data.bin

   for sz in ${sizes[*]}
   do
      echo -n "# Benchmarking size: $sz"

      dd if=/dev/zero of=bench/data.bin bs=$sz count=1 2> /dev/null
      time stack exec -- ghc -v0 bench/BenchEmbed.hs -fforce-recomp -O
      rm -f bench/data.bin
      echo ""
   done
   echo ""
}

function bench_file_embed {
   echo "================================"
   echo "| Benchmarking file-embed      |"
   echo "================================"

   echo "Warming up..."
   dd if=/dev/zero of=bench/data.bin bs=256 count=1 2> /dev/null
   stack exec -- ghc -v0 bench/BenchFileEmbed.hs -fforce-recomp -O > /dev/null
   rm -f bench/data.bin

   for sz in ${sizes[*]}
   do
      echo -n "# Benchmarking size: $sz"

      dd if=/dev/zero of=bench/data.bin bs=$sz count=1 2> /dev/null
      time stack exec -- ghc -v0 bench/BenchFileEmbed.hs -fforce-recomp -O
      rm -f bench/data.bin
      echo ""
   done
   echo ""
}

bench_haskus
bench_file_embed
