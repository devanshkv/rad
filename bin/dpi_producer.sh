#!/bin/bash

#########################################################
#                                                       #
#   Uses a python script to produce dpi for epn data    #
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################

workingdir=`pwd`

for scripts in `ls -1 ${workingdir}/bin/file_list_*.dir`
	do
	chmod +x ${scripts}
	freq=$( echo "${scripts}" | tail -c 9 | head -c 4 )
	cat ${scripts} | rev | cut -d/ -f1 | rev | cut -d_ -f1 > ${workingdir}/temp/${freq}.dpsr
	echo "Calculating degree of polarization for ${freq} MHz"
	if [[ -z `exec ${scripts} > ${workingdir}/temp/${freq}.dpol` ]];
	then
	rm -rf ${workingdir}/temp/${freq}.atn ${workingdir}/temp/${freq}.dspr
	paste ${workingdir}/temp/${freq}.dpsr $workingdir/temp/${freq}.dpol > ${workingdir}/temp/${freq}.atn
	rm -rf ${workingdir}/temp/${freq}.d* ${scripts}
	else
	echo "Error in calculating degree of polarization for ${freq} MHz"
	exit
	fi
	done
