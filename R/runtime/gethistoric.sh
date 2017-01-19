#!/bin/sh
#Script: gethistoric.sh
#Purpose: get historic data for a station
#Input: station_id, report_type, series year mon <optional day>
#Output: csv-formatted file
#Maggie Dunklee, 2010, National Water Climate Center, All Data is Provisional
#
if  [[ $# -lt 5  ]]
then
    echo "Script to get historic data"
    echo "usage: $0 STATION   REPORT_TYPE   TIME_SERIES   YEAR   MONTH   DAY"
    echo "where: "
    echo "      STATION is numeric is 2095, 302, etc"
    echo "      REPORT_TYPE is  STAND (standard snotel), SOIL (soil temp and moisture) SCAN (standard scan), ALL (all), WEATHER (atmospheric)"
    echo "      TIME_SERIES is Daily or Hourly or Hour:HH"
    echo "      YEAR is YYYY"
    echo "      MONTH is MM or CY (calendar year), or WY  (water year)"
    echo "      Optional DAY is DD"
    exit
fi

export STATION=$1
export REPORT=$2
export SERIES=$3
export YEAR=$4
export MONTH=$5
export DAY=$6

export INTERVALTYPE=Historic

curl "http://wcc.sc.egov.usda.gov/nwcc/view?intervalType=$INTERVALTYPE+&report=$REPORT&timeseries=$SERIES&format=copy&sitenum=$STATION&year=$YEAR&month=$MONTH&day=$DAY" -o data/snoteldownloads/$STATION-$YEAR$MONTH$DAY.csv
