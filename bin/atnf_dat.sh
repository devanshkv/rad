#!/bin/bash
#########################################################
#                                                       #
#    Uses PSRCAT and makes the ascii file for ploting   #
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################
#	File info
#*.atn	: PSR	dpi	sigma
#*.junk	: PSR names for psrcat
#*.tmp	: PSR names & ATNF data
#*.imp	: ATNF data files lined up for useage
#*.prq	: to screw VLOOKUP and DIGDB

workingdir=`pwd`
	
	for line in `ls -1 ${workingdir}/temp/*.atn`
        do

	freq=$( echo "${line}" | tail -c 9 | head -c 4 )
        echo "Adding ATNF data to ${freq} MHz"
	
	#Makes atnf.junk which has the names of the pulsars
	
	rm -rf atnf.junk
        cat ${line} | awk '{print $1}' | uniq -u | sed ':a;N;$!ba;s/\n/ /g' > ${workingdir}/temp/atnf.junk
	
	#Run psrcat to add ATNF data
	${workingdir}/bin/psrcat_tar/psrcat -db_file ${workingdir}/bin/psrcat_tar/psrcat.db -c "name p0 p1 age bsurf_i edot_i" `cat ${workingdir}/temp/atnf.junk` > ${workingdir}/temp/atnf_${freq}.tmp

	cat ${workingdir}/temp/atnf_${freq}.tmp | grep -v "WARNING" | tail -n +5 | head -n -1 |sed ':a;N;$!ba;s/\n\n/\n/g' > ${workingdir}/temp/atnf_${freq}.imp
	
	#Screw VLOOKUP and DIGDB
	rm -rf ${workingdir}/temp/${freq}.prq
	for row in `cat ${workingdir}/temp/atnf_${freq}.imp | awk '{print $2}'`
	do
	grep ${row} ${workingdir}/temp/${freq}.atn | awk '{print $2"\t"$3"\t"$4}' | sort -k 2 | tail -1  >> ${workingdir}/temp/${freq}.prq
	done
	rm -rf ${workingdir}/temp/${freq}.dat
	paste ${workingdir}/temp/atnf_${freq}.imp ${workingdir}/temp/${freq}.prq > ${workingdir}/res/${freq}.dat
	
	#Add Header
	sed -i '1i#	B_name			P0					P1				Age	Bsurf	Edot		Dpi		Cpi' ${workingdir}/res/${freq}.dat
	done
