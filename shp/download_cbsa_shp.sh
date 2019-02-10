#!/usr/bin/bash

cd /Users/cecilemurray/Documents/CAPP/data-viz/jobs

# mkdir shp
cd shp

wget https://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_cbsa_500k.zip
unzip cb_2017_us_cbsa_500k

ogr2ogr -f GeoJSON cbsa_2017.geojson cb_2017_us_cbsa_500k.shp

rm cb_2017*

cd -