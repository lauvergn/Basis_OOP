#!/bin/bash

#EXTLIB_TYPE=$1
#BaseName=QDUtilLib
BaseName=$1


ExtLibSAVEDIR=/Users/lauvergn/git/Ext_Lib

echo In get_Lib.sh $BaseName


LOC_version=$BaseName

if (test -d $LOC_version) then
  echo $LOC_version directory exist
  exit 0
else
  echo $LOC_version does not directory exist.
  if (test -d $LOC_version"_loc") then
    echo $LOC_version"_loc" directory exist. Adding the link.
    ln -s $LOC_version"_loc" $LOC_version
    test -e $LOC_version && ( echo End get_Lib.sh $BaseName; exit 0 )
  fi
fi




#1) try to get from github
#latest HEAD version
version="https://github.com/lauvergn/"$BaseName"/archive/refs/heads/main.zip"
curl -LJ $version --output $LOC_version.zip
zipfile=$LOC_version.zip
if (test -f $zipfile) then
  echo $zipfile file exist
  DIRName=`unzip -Z -1 $zipfile | head -1 `
  unzip $zipfile
  mv $DIRName $LOC_version"_loc"
  ln -s $LOC_version"_loc" $LOC_version
  rm -f $zipfile
else
  echo $LOC_version.zip from github does not exist.
fi

if (test -d $LOC_version) then 
  echo $LOC_version file exist from github
  echo End get_Lib.sh $BaseName
  exit 0
fi



#2) try to get from a local directory $ExtLibSAVEDIR
echo Try to get a zip file from: $ExtLibSAVEDIR
test -d $ExtLibSAVEDIR || (echo $ExtLibSAVEDIR directory des not exist! ; exit 1 )
zipfile=$ExtLibSAVEDIR"/Save_"$BaseName"_devloc.zip"
if (test -f $zipfile) then
  echo $zipfile file exist
  DIRName=`unzip -Z -1 $zipfile | head -1 `
  unzip $zipfile
  mv $DIRName $LOC_version"_loc"
  ln -s $LOC_version"_loc" $LOC_version
fi

#3) last test
if (test -d $LOC_version) then
  echo $LOC_version file exist from $ExtLibSAVEDIR
  echo End get_Lib.sh $BaseName
  exit 0
else
  echo $LOC_version file does not exist from
  echo End get_Lib.sh $BaseName
  exit 1
fi

