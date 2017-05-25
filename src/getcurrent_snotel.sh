#================================================
#Script to get Current NWCC SNOTEL/SCAN Data
#================================================
#!/bin/sh
# Script: getcurrent.sh
# Purpose: get current data for a station
# Input: station_id, report_type, series and interval
# Output: csv-formatted file
# Maggie Dunklee, 2010, National Water Climate Center, All Data is Provisional and subject to change
# Email: maggie.dunklee@por.usda.gov
if  [[ $# -ne 4  ]]
then
    echo "Script to get Current data"
    echo "usage: $0 STATION  REPORT_TYPE   TIME_SERIES  INTERVAL "
    echo "where: "
    echo "      STATION is numeric is 2095, 302, etc"
    echo "      REPORT_TYPE is  STAND (standard snotel), SOIL (soil temp and moisture) SCAN (standard scan), ALL (all), WEATHER (atmospheric)"
    echo "      TIME_SERIES is Daily or Hourly or Hour:HH"
    echo "      INTERVAL is current DAY or WEEK or YEAR (calendar year) or WATERYEAR (current waterhyear)"
    exit
fi

export STATION=$1
export REPORT=$2
export SERIES=$3
export INTERVAL=$4
export INTERVALTYPE=Current

curl "http://wcc.sc.egov.usda.gov/nwcc/view?intervalType=$INTERVALTYPE+&report=$REPORT&timeseries=$SERIES&format=copy&sitenum=$STATION&interval=$INTERVAL" -o data/snoteldownloads/$STATION-$INTERVAL.csv
