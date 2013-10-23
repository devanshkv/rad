#!/bin/bash
#########################################################
#                                                       #
#	Uses dat files in the res folder to make plots  #
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################
#	File list
#plot*.gnu	: gnuplot input files
#freq*.pdf	: intermediate pdf files

workingdir=`pwd`

rm -rf ${workingdir}/res/*.pdf
for line in ${workingdir}/res/*.dat
do
rm -rf ${workingdir}/temp/*.gnu
# P plot
echo "set terminal pdf color"											>> ${workingdir}/temp/plot1.gnu
echo "set grid"													>> ${workingdir}/temp/plot1.gnu
echo "unset key"												>> ${workingdir}/temp/plot1.gnu
echo "pl \"${line}\" u 4:13"                                                                                    >> ${workingdir}/temp/plot1.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_01.pdf'"                                           >> ${workingdir}/temp/plot1.gnu
echo "set yrange [0:1]"												>> ${workingdir}/temp/plot1.gnu
echo "set title \"dpi vs p\""											>> ${workingdir}/temp/plot1.gnu
echo "set xlabel \"Period (s)\""										>> ${workingdir}/temp/plot1.gnu
echo "set ylabel \"Degree of Pol\""										>> ${workingdir}/temp/plot1.gnu
echo "rep"													>> ${workingdir}/temp/plot1.gnu
#Pdot
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot2.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot2.gnu
echo "set logscale x"												>> ${workingdir}/temp/plot2.gnu
echo "set format x \"%L\""											>> ${workingdir}/temp/plot2.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot2.gnu
echo "pl \"${line}\" u 7:13"                                                                                    >> ${workingdir}/temp/plot2.gnu
echo "set title \"dpi vs log p dot\""                                                                           >> ${workingdir}/temp/plot2.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_02.pdf'"                                           >> ${workingdir}/temp/plot2.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot2.gnu
echo "set xlabel \"log P Dot\"" 	                                                                        >> ${workingdir}/temp/plot2.gnu
echo "set ylabel \"Degree of Pol\""                                                                             >> ${workingdir}/temp/plot2.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot2.gnu
#Age
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot3.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot3.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot3.gnu
echo "set format x \"%L\""                                                                                 	>> ${workingdir}/temp/plot3.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot3.gnu
echo "pl \"${line}\" u 10:13"                                                                                   >> ${workingdir}/temp/plot3.gnu
echo "set title \"dpi vs log age\""                                                                             >> ${workingdir}/temp/plot3.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_03.pdf'"                                           >> ${workingdir}/temp/plot3.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot3.gnu
echo "set xlabel \"log Age\""                                                                                   >> ${workingdir}/temp/plot3.gnu
echo "set ylabel \"Degree of Pol\""                                                                             >> ${workingdir}/temp/plot3.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot3.gnu
#Bsurf
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot4.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot4.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot4.gnu
echo "set format x \"%L\""                                                                                      >> ${workingdir}/temp/plot4.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot4.gnu
echo "pl \"${line}\" u 11:13"                                                                                   >> ${workingdir}/temp/plot4.gnu
echo "set title \"dpi vs log b surf\""                                                                          >> ${workingdir}/temp/plot4.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_04.pdf'"                                           >> ${workingdir}/temp/plot4.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot4.gnu
echo "set xlabel \"log B Surf\""                                                                                >> ${workingdir}/temp/plot4.gnu
echo "set ylabel \"Degree of Pol\""                                                                             >> ${workingdir}/temp/plot4.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot4.gnu
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot5.gnu
#Edot
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot5.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot5.gnu
echo "set format x \"%L\""                                                                                 	>> ${workingdir}/temp/plot5.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot5.gnu
echo "pl \"${line}\" u 12:13"                                                                                   >> ${workingdir}/temp/plot5.gnu
echo "set title \"dpi vs log e dot\""                                                                           >> ${workingdir}/temp/plot5.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_05.pdf'"                                           >> ${workingdir}/temp/plot5.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot5.gnu
echo "set xlabel \"log E dot\""                                                                                 >> ${workingdir}/temp/plot5.gnu
echo "set ylabel \"Degree of Pol\""                                                                             >> ${workingdir}/temp/plot5.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot5.gnu
#plot generation
gnuplot < "${workingdir}/temp/plot1.gnu"
gnuplot < "${workingdir}/temp/plot2.gnu"
gnuplot < "${workingdir}/temp/plot3.gnu"
gnuplot < "${workingdir}/temp/plot4.gnu"
gnuplot < "${workingdir}/temp/plot5.gnu"
#pdfunite *.pdf ${workingdir}/res/`echo "${line}" | tail -c -9 | head -c 4`.pdf
#rm -rf *.pdf ${workingdir}/temp/*.pdf 
rm -rf ${workingdir}/temp/*.gnu
done
