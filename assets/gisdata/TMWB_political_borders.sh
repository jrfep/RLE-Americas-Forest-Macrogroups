## World Borders Dataset
## Provided by Bjorn Sandvik, thematicmapping.org
## The dataset is available under a Creative Commons Attribution-Share Alike License.
## Downloaded from
## http://thematicmapping.org/downloads/world_borders.php

# Attribute columns
# COLUMN	TYPE	DESCRIPTION
# Shape	Polygon	Country/area border as polygon(s)
# FIPS	String(2)	FIPS 10-4 Country Code
# ISO2	String(2)	ISO 3166-1 Alpha-2 Country Code
# ISO3	String(3)	ISO 3166-1 Alpha-3 Country Code
# UN	Short Integer(3)	ISO 3166-1 Numeric-3 Country Code
# NAME	String(50)	Name of country/area
# AREA	Long Integer(7)	Land area, FAO Statistics (2002)
# POP2005	Double(10,0)	Population, World Population Prospects (2005)
# REGION	Short Integer(3)	Macro geographical (continental region), UN Statistics
# SUBREGION	Short Integer(3)	Geographical sub-region, UN Statistics
# LON	FLOAT (7,3)	Longitude
# LAT	FLOAT (6,3)	Latitude

## first set a folder path for gisdata, see env/
mkdir -p $GISDATA/admin/TMWB
cd $GISDATA/admin/TMWB
## download with wget
wget --continue http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip
