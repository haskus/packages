#!/bin/sh

sizes_fileembed=(128b 3K 3M 15M)
sizes_haskus=(128b 3K 3M 15M 150M 1G)

function cleanup {
   rm -f bench/*.o
   rm -f bench/*.hi
   rm -f bench/*.dyn_o
   rm -f bench/*.dyn_hi
   rm -f bench/BenchEmbed
   rm -f bench/BenchFileEmbed
   rm -f bench/data.bin
}

function bench_haskus {
   echo "================================"
   echo "| Benchmarking haskus-binary   |"
   echo "================================"

   echo "Warming up..."
   dd if=/dev/zero of=bench/data.bin bs=256 count=1 2> /dev/null
   stack exec -- ghc -v0 bench/BenchEmbed.hs -fforce-recomp -O > /dev/null
   cleanup

   for sz in ${sizes_haskus[*]}
   do
      echo -n "# Benchmarking size: $sz"

      dd if=/dev/zero of=bench/data.bin bs=$sz count=1 2> /dev/null
      time stack exec -- ghc -v0 bench/BenchEmbed.hs -fforce-recomp -O
      cleanup
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
   cleanup

   for sz in ${sizes_fileembed[*]}
   do
      echo -n "# Benchmarking size: $sz"

      dd if=/dev/zero of=bench/data.bin bs=$sz count=1 2> /dev/null
      time stack exec -- ghc -v0 bench/BenchFileEmbed.hs -fforce-recomp -O
      cleanup
      echo ""
   done
   echo ""
}

bench_haskus
bench_file_embed

