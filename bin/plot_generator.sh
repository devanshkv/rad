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
echo "set terminal pdf color"											>> ${workingdir}/temp/plot01.gnu
echo "set grid"													>> ${workingdir}/temp/plot01.gnu
echo "unset key"												>> ${workingdir}/temp/plot01.gnu
echo "pl \"${line}\" u 4:13"                                                                                    >> ${workingdir}/temp/plot01.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_01.pdf'"                                           >> ${workingdir}/temp/plot01.gnu
echo "set yrange [0:1]"												>> ${workingdir}/temp/plot01.gnu
echo "set title \"dpi vs p\""											>> ${workingdir}/temp/plot01.gnu
echo "set xlabel \"Period (s)\""										>> ${workingdir}/temp/plot01.gnu
echo "set ylabel \"Degree of Pol\""										>> ${workingdir}/temp/plot01.gnu
echo "rep"													>> ${workingdir}/temp/plot01.gnu
#Pdot
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot02.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot02.gnu
echo "set logscale x"												>> ${workingdir}/temp/plot02.gnu
echo "set format x \"%L\""											>> ${workingdir}/temp/plot02.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot02.gnu
echo "pl \"${line}\" u 7:13"                                                                                    >> ${workingdir}/temp/plot02.gnu
echo "set title \"dpi vs log p dot\""                                                                           >> ${workingdir}/temp/plot02.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_02.pdf'"                                           >> ${workingdir}/temp/plot02.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot02.gnu
echo "set xlabel \"log P Dot\"" 	                                                                        >> ${workingdir}/temp/plot02.gnu
echo "set ylabel \"Degree of Pol\""                                                                             >> ${workingdir}/temp/plot02.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot02.gnu
#Age
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot03.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot03.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot03.gnu
echo "set format x \"%L\""                                                                                 	>> ${workingdir}/temp/plot03.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot03.gnu
echo "pl \"${line}\" u 10:13"                                                                                   >> ${workingdir}/temp/plot03.gnu
echo "set title \"dpi vs log age\""                                                                             >> ${workingdir}/temp/plot03.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_03.pdf'"                                           >> ${workingdir}/temp/plot03.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot03.gnu
echo "set xlabel \"log Age\""                                                                                   >> ${workingdir}/temp/plot03.gnu
echo "set ylabel \"Degree of Pol\""                                                                             >> ${workingdir}/temp/plot03.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot03.gnu
#Bsurf
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot04.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot04.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot04.gnu
echo "set format x \"%L\""                                                                                      >> ${workingdir}/temp/plot04.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot04.gnu
echo "pl \"${line}\" u 11:13"                                                                                   >> ${workingdir}/temp/plot04.gnu
echo "set title \"dpi vs log b surf\""                                                                          >> ${workingdir}/temp/plot04.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_04.pdf'"                                           >> ${workingdir}/temp/plot04.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot04.gnu
echo "set xlabel \"log B Surf\""                                                                                >> ${workingdir}/temp/plot04.gnu
echo "set ylabel \"Degree of Pol\""                                                                             >> ${workingdir}/temp/plot04.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot04.gnu
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot05.gnu
#Edot
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot05.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot05.gnu
echo "set format x \"%L\""                                                                                 	>> ${workingdir}/temp/plot05.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot05.gnu
echo "pl \"${line}\" u 12:13"                                                                                   >> ${workingdir}/temp/plot05.gnu
echo "set title \"dpi vs log e dot\""                                                                           >> ${workingdir}/temp/plot05.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_05.pdf'"                                           >> ${workingdir}/temp/plot05.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot05.gnu
echo "set xlabel \"log E dot\""                                                                                 >> ${workingdir}/temp/plot05.gnu
echo "set ylabel \"Degree of Pol\""                                                                             >> ${workingdir}/temp/plot05.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot05.gnu

# P plot
echo "set terminal pdf color"											>> ${workingdir}/temp/plot06.gnu
echo "set grid"													>> ${workingdir}/temp/plot06.gnu
echo "unset key"												>> ${workingdir}/temp/plot06.gnu
echo "pl \"${line}\" u 4:15"                                                                                    >> ${workingdir}/temp/plot06.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_06.pdf'"                                           >> ${workingdir}/temp/plot06.gnu
echo "set yrange [0:1]"												>> ${workingdir}/temp/plot06.gnu
echo "set title \"cpi vs p\""											>> ${workingdir}/temp/plot06.gnu
echo "set xlabel \"Period (s)\""										>> ${workingdir}/temp/plot06.gnu
echo "set ylabel \"Degree of Circ Pol\""									>> ${workingdir}/temp/plot06.gnu
echo "rep"													>> ${workingdir}/temp/plot06.gnu
#Pdot
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot07.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot07.gnu
echo "set logscale x"												>> ${workingdir}/temp/plot07.gnu
echo "set format x \"%L\""											>> ${workingdir}/temp/plot07.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot07.gnu
echo "pl \"${line}\" u 7:15"                                                                                    >> ${workingdir}/temp/plot07.gnu
echo "set title \"cpi vs log p dot\""                                                                           >> ${workingdir}/temp/plot07.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_07.pdf'"                                           >> ${workingdir}/temp/plot07.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot07.gnu
echo "set xlabel \"log P Dot\"" 	                                                                        >> ${workingdir}/temp/plot07.gnu
echo "set ylabel \"Degree of Circ Pol\""                                                                        >> ${workingdir}/temp/plot07.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot07.gnu
#Age
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot08.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot08.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot08.gnu
echo "set format x \"%L\""                                                                                 	>> ${workingdir}/temp/plot08.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot08.gnu
echo "pl \"${line}\" u 10:15"                                                                                   >> ${workingdir}/temp/plot08.gnu
echo "set title \"cpi vs log age\""                                                                             >> ${workingdir}/temp/plot08.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_08.pdf'"                                           >> ${workingdir}/temp/plot08.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot08.gnu
echo "set xlabel \"log Age\""                                                                                   >> ${workingdir}/temp/plot08.gnu
echo "set ylabel \"Degree of Circ Pol\""                                                                        >> ${workingdir}/temp/plot08.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot08.gnu
#Bsurf
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot09.gnu
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot09.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot09.gnu
echo "set format x \"%L\""                                                                                      >> ${workingdir}/temp/plot09.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot09.gnu
echo "pl \"${line}\" u 11:15"                                                                                   >> ${workingdir}/temp/plot09.gnu
echo "set title \"cpi vs log b surf\""                                                                          >> ${workingdir}/temp/plot09.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_09.pdf'"                                           >> ${workingdir}/temp/plot09.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot09.gnu
echo "set xlabel \"log B Surf\""                                                                                >> ${workingdir}/temp/plot09.gnu
echo "set ylabel \"Degree of Circ Pol\""                                                                        >> ${workingdir}/temp/plot09.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot09.gnu
echo "set terminal pdf color"                                                                                   >> ${workingdir}/temp/plot10.gnu
#Edot
echo "set grid"                                                                                                 >> ${workingdir}/temp/plot10.gnu
echo "set logscale x"                                                                                           >> ${workingdir}/temp/plot10.gnu
echo "set format x \"%L\""                                                                                 	>> ${workingdir}/temp/plot10.gnu
echo "unset key"                                                                                                >> ${workingdir}/temp/plot10.gnu
echo "pl \"${line}\" u 12:15"                                                                                   >> ${workingdir}/temp/plot10.gnu
echo "set title \"cpi vs log e dot\""                                                                           >> ${workingdir}/temp/plot10.gnu
echo "set output '` echo "${line}" | tail -c -9 | head -c 4`_10.pdf'"                                           >> ${workingdir}/temp/plot10.gnu
echo "set yrange [0:1]"                                                                                         >> ${workingdir}/temp/plot10.gnu
echo "set xlabel \"log E dot\""                                                                                 >> ${workingdir}/temp/plot10.gnu
echo "set ylabel \"Degree of Circ Pol\""                                                                        >> ${workingdir}/temp/plot10.gnu
echo "rep"                                                                                                      >> ${workingdir}/temp/plot10.gnu
#plot generation
gnuplot < "${workingdir}/temp/plot01.gnu"
gnuplot < "${workingdir}/temp/plot02.gnu"
gnuplot < "${workingdir}/temp/plot03.gnu"
gnuplot < "${workingdir}/temp/plot04.gnu"
gnuplot < "${workingdir}/temp/plot05.gnu"
gnuplot < "${workingdir}/temp/plot06.gnu"
gnuplot < "${workingdir}/temp/plot07.gnu"
gnuplot < "${workingdir}/temp/plot08.gnu"
gnuplot < "${workingdir}/temp/plot09.gnu"
gnuplot < "${workingdir}/temp/plot10.gnu"

pdfunite *.pdf ${workingdir}/res/`echo "${line}" | tail -c -9 | head -c 4`.pdf
rm -rf *.pdf ${workingdir}/temp/*.pdf ${workingdir}/temp/*.gnu
done
