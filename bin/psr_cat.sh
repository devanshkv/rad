#!/bin/bash
#########################################################
#                                                       #
#    Uses PSRCAT and makes the ascii file for ploting   #
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################

workingdir=`pwd`
        rm -rf ${workingdir}/temp/atnf*
for line in `ls -1 ${workingdir}/temp/*.atn`
        do
        rm -rf atnf.?unk
        freq=$( echo "${line}" | tail -c 9 | head -c 4 )
        echo "Adding ATNF data to ${freq} MHz"i
        cat ${line} | awk '{print $1}' | sed ':a;N;$!ba;s/\n/ /g' > ${workingdir}/temp/atnf.junk
        ${workingdir}/bin/psrcat_tar/psrcat -db_file ${workingdir}/bin/psrcat_tar/psrcat.db -c "name p0 p1 age bsurf_i edot_i" `cat ${workingdir}/temp/atnf.junk` > ${workingdir}/temp/atnf_${freq}.tmp
	rm -rf ${workingdir}/temp/crow
        for row in `cat ${line} | awk '{print $1}'`
        do
        if [[ `cat ${line} | grep ${row} | wc -l ` > 1 ]];then
        cat ${line} | grep ${row} | grep `cat ${line} | grep ${row} | awk '{print $3}' | awk 'NR==1||$1<min{line = $1;min=$1}END{print line}'` >> ${workingdir}/temp/crow
        fi
        uniq ${workingdir}/temp/crow > ${workingdir}/temp/atnf_${freq}.crw
#	mv ${workingdir}/temp/atnf_${freq}.crw ${workingdir}/temp/atnf_${freq}.tmp
#	rm -rf ${workingdir}/temp/crow
#        if [[ -z `grep ${row} ${workingdir}/temp/atnf_${freq}.tmp | grep "WARNING:"` ]];then
	 grep ${row} ${workingdir}/temp/atnf_${freq}.tmp  >> ${workingdir}/temp/temp_${freq}
        grep ${row} ${workingdir}/temp/atnf_${freq}.tmp | awk '{printf"%2.10f\t%4.3e\t%4.3e\t%4.3e\t%4.3e\n",$4,$7,$10,$11,$12}' >> ${workingdir}/temp/atnf.punk
#        else
#        echo "* *       *       *       *" >> ${workingdir}/temp/atnf.punk
#        fi
        echo "#name     dpi     sigma   period  p_dot   age     b_surf  edot " >${workingdir}/res/${freq}.dat
        paste ${line} ${workingdir}/temp/atnf.punk >> ${workingdir}/res/${freq}.dat
        rm -rf atnf.?unk 
        done
        done

