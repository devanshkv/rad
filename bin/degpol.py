#########################################################
#                                                       #
#           Degree of Polarization Calculation          #
#		Perhaps the heart!			#
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################

import numpy
import math

def dpi(I,Q,U,V,sigma):
	raw_l=[]
	l_true=[]
	mean_i=[]
	k=int(0)
	
	for j in range (0, len(I)-1):
	
		if ((Q[j]**2)+(U[j]**2)+(V[j]**2))<(I[j]**2) and (I[j])>=3*sigma:
			
			raw_l.append(math.sqrt((Q[j]**2)+(U[j]**2)))
			
			#Everett el all correction

			if raw_l[k]/sigma >= 1.57:
				l_true.append(math.sqrt((raw_l[k]**2)-sigma**2))
			else:
				l_true.append(0)
			mean_i.append(I[j])
			k=k+1

	#mean and standard deviations

	l_mean=float(numpy.mean(numpy.array(l_true)))
	i_mean=float(numpy.mean(numpy.array(mean_i)))
	l_std=float(numpy.std(l_true))
	
	#dpi
	
	dpi=float(l_mean/i_mean)
	return(dpi)
