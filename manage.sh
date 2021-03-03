#!/bin/sh
dirs=$(ls -d haskus-*/ | cut -d'/' -f1)

function last_tag {
   git tag -l | grep "$1-[0-9]" | sort -r -V | head -n 1
}

function check_tag {
   local t=$(git tag -l | grep "$1")
   if [ "$t" = "" ]
      then echo "NO"
      else echo "YES"
   fi
}

function is_uploaded {
   local v=$(curl -s -I https://hackage.haskell.org/package/$1 | head -n 1 | cut -d' ' -f2)
   if [ "$v" = "200" ]
      then echo "YES"
      else if [ "$v" = "404" ]
         then echo "NO"
         else echo "Not found"
      fi
   fi
}

function package_version {
   ver=$(cd $1 && stack query locals $1 version)
   echo $ver | cut -d"'" -f2
}

function report {
   tag=$(last_tag $1)
   echo ""
   echo "---------------------------------------------------------------"
   echo "$1:"
   echo "  - Latest tag: $tag"
   echo "  - Latest tag uploaded on hackage: $(is_uploaded $tag)"
   echo "  - Dev version: $(package_version $1)"
   echo "  - Log since last tag:"
   git --no-pager log --oneline $tag..HEAD -- $1/
   echo "---------------------------------------------------------------"
}

function report_all {
   echo "==============================================================="
   echo "Reporting package infos"
   echo "==============================================================="
   for i in $dirs
   do
      report $i
   done
}

function build_all {
   echo "==============================================================="
   echo "Building"
   echo "==============================================================="

   stack test --pedantic -j1
}

function build_compat {
   set -e

   echo "==============================================================="
   echo "Building for GHC 8.6"
   echo "==============================================================="
   stack build --stack-yaml stack-8.6.yaml

   echo "==============================================================="
   echo "Building for GHC 8.8"
   echo "==============================================================="
   stack build --stack-yaml stack-8.8.yaml

   echo "==============================================================="
   echo "Building for GHC 8.10"
   echo "==============================================================="
   stack build --stack-yaml stack-8.10.yaml

   echo "==============================================================="
   echo "Building for GHC 9.0.1"
   echo "==============================================================="
   cabal build all -w ghc-9.0.1
}


function showdone {
   echo ""
   echo "==============================================================="
   echo "Done"
   echo "==============================================================="
}

function check_dev_version {
   echo "Package: $1"
   local v="$(package_version $1)"
   echo "  - Version: $v"
   local nt="$1-$v"
   local upl=$(is_uploaded $nt)
   echo "  - Already on Hackage: $upl"
   local ct=$(check_tag $nt)
   echo "  - Tag exist: $ct"
   if [ "$ct" = "NO" ]
      then echo "You need to create the tag $nt" && exit 1
   fi
}

function check_dev_versions {
   echo "==============================================================="
   echo "Checking release versions"
   echo "==============================================================="
   for i in $dirs
   do
      check_dev_version $i
   done
}

function check_rep_state {
   echo "==============================================================="
   echo "Checking repository state"
   echo "==============================================================="

   local r=$(git status -s --untracked-files=no)
   if [ "$r" != "" ]
      then echo "Repository isn't clean:" && git status && exit 1
      else echo "Repositry is clean"
   fi
}

case "$1" in
   report)
      report_all
      showdone
      ;;
   check)
      check_dev_versions
      check_rep_state
      showdone
      ;;
   build-compat)
      build_compat
      showdone
      ;;
   build)
      build_all
      showdone
      ;;
   release)
      check_dev_versions
      check_rep_state
      report_all
      build_all
      showdone
      ;;
   *)
      echo "Missing command: report, check, build, build-compat, release"
      exit 0
esac
