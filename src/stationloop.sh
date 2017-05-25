#!/bin/sh
#Script: stationloop.sh
#Purpose: Prints out commands to call gethistoric.sh to get a calendar year SNOTEL report
#Input: none
#Output: commands to standard out
#Calls: script gethistoric.sh
#Maggie Dunklee, 2010, National Water Climate Center, All Data is Provisional
#
if  [[ $# -lt 4 ]]
then
    echo "usage: $0 STATION_ID  START_YEAR  END_YEAR  REPORT_TYPE  TIME_SERIES"
    echo "where: "
    echo "      STATION is numeric is 2095, 302, etc"
    echo "      START_YEAR is YYYY"
    echo "      END_YEAR is YYYY"
    echo "      REPORT_TYPE is  SOIL (soil temp and moisture) SCAN (standard scan), ALL (all), WEATHER (atmospheric)"
    echo "      SERIES is Daily or Hourly"
    exit
fi

export STATION=$1
export START_YEAR=$2
export END_YEAR=$3
export REPORT_TYPE=$4
export TIME_SERIES=$5

# Deal with future date
export THIS_YEAR=`date +%G`
if [[ $END_YEAR -eq '2100' ]]
then
        END_YEAR=$THIS_YEAR
fi

export YEAR=$START_YEAR
while [ $YEAR -le $END_YEAR ]
do
        echo "sh gethistoric.sh $STATION $REPORT_TYPE $TIME_SERIES $YEAR CY"
        YEAR=$(($YEAR+1))
done
