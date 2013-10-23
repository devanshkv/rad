#!/bin/bash

#########################################################
#                                                       #
#	Filter EPN files with Stokes Paramteres         #
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################

workingdir=`pwd`

rm -rf ${workingdir}/bin/file_list_*.dir

if [[ ! -z $2 ]]
	then
	freq=$2
	echo "Making list of Pulsars for ${freq} MHz"
		for line in `ls -1d ${workingdir}/epn/freq/new/${freq}/*`
        		do
		        if [[ ! -z `grep "I Q U V" ${line}` ]]
                		then
		                echo "python ${workingdir}/bin/ascii_read.py ${line}" >> ${workingdir}/bin/file_list_${freq}.dir
		        fi
		done


else
	for line in `ls -1d ${workingdir}/epn/freq/new/*`
		do
		freq=$(echo "${line}" | rev | cut -c -4 | rev)
		echo "Making list of Pulsars for ${freq} MHz"
			for line in `ls -1d ${workingdir}/epn/freq/new/${freq}/*`
			do
				if [[ ! -z `grep "I Q U V" ${line}` ]]
				then
				echo "python ${workingdir}/bin/ascii_read.py ${line}" >> ${workingdir}/bin/file_list_${freq}.dir
				fi
done
done
fi
echo "Completed making pulsar lists!"
