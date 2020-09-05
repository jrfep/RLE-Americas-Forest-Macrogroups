export MIHOST=$(hostname -s)

case $MIHOST in
terra)
  export GISDATA=/opt/gisdata
  export GISDB=/opt/gisdb
  ;;
roraima)
  export GISDATA=$HOME/gisdata
  export GISDB=$HOME/gisdb
  ;;
esac

# store the zenodo API token in a file in home directory
export ZENODOTOKEN=$(cat $HOME/.ZenodoToken)

export PROJECTNAME=RLE-Americas-Forest-Macrogroups
export SCRIPTDIR=$HOME/proyectos/IUCN/$PROJECTNAME
export WORKDIR=$HOME/tmp/$PROJECTNAME

export NSDATA=$HOME/Cloudstor/UNSW/data/NatureServe_IVC

mkdir -p $WORKDIR
