#########################################################
#                                                       #
#          Degree of Circular Polarization Calculation  #
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################

import numpy
import math

def cpi(I,Q,U,V,sigma):
	v_true=[]
	v_modl=[]
	mean_i=[]
	
	for j in range (0, len(I)-1):
	
		if ((Q[j]**2)+(U[j]**2)+(V[j]**2))<(I[j]**2) and (I[j])>=3*sigma:
			
			#CPI correction

			if abs(V[j])/sigma >= 1:
				v_true.append(abs(((V[j]**4)-sigma**4)**0.25))
			else:
				v_true.append(0)
			mean_i.append(I[j])

	#mean and standard deviations

	v_mean=float(numpy.mean(numpy.array(v_true)))
	i_mean=float(numpy.mean(numpy.array(mean_i)))
#	l_std=float(numpy.std(l_true))
	
	#dpi
	
	cpi=float(v_mean/i_mean)
	return(cpi)
