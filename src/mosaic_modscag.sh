#!/bin/bash


doy=$1
yr=$2

downloadpn='data/modscagdownloads/'$yr$doy
outpn=$downloadpn'/mosaicked'
outpn='data/fsca'
mkdir -p $outpn


fns=`find -P $downloadpn/*$yr$doy*snow_fraction.tif`

gdalwarp -t_srs '+proj=longlat +datum=WGS84' -te -112.25 33 -104.125 43.75 -tr 0.00416666667 0.0041666667 -r near $fns $outpn/modscag_fsca_$yr$doy.tif
