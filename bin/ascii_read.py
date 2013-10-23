#########################################################
#							#
#		 Read ascii stokes data			#
#		Written by : Devansh Agarwal		#
#		devansh@iisertvm.ac.in			#
#							#
#########################################################

#imports

import centprof
import numpy
import degpol
import degcpol
import math
import sigmacal
import sys

# File list

f = open(sys.argv[1],'r')

# Read and ignore header lines
	
header1 = f.readline()
header2 = f.readline()

#Variable Pre-initialization
raw_t=[]
raw_I=[]
raw_Q=[]
raw_U=[]
raw_V=[] 

for line in f:
			#Read and Split

			rows = line.strip()
			columns = line.split()

			#Read Raw Stokes from ascii
				
			raw_t.append(float(columns[0]))
			raw_I.append(float(columns[1]))
			raw_Q.append(float(columns[2]))
			raw_U.append(float(columns[3]))
			raw_V.append(float(columns[4]))

f.close()
#Center them

(I1,Q1,U1,V1)=centprof.center(raw_I,raw_Q,raw_U,raw_V)

#Normalization

I=[x/max(I1) for x in I1]
Q=[x/max(I1) for x in Q1]
U=[x/max(I1) for x in U1]
V=[x/max(I1) for x in V1]

#Find Their Simga
if len(I)<60:
	sigma=0.05
else:
	sigma=sigmacal.sigma(I)

#Corrections
	
#Dpi

dpi=degpol.dpi(I,Q,U,V,sigma)
cpi=degcpol.cpi(I,Q,U,V,sigma)

#result
#print sigma,"\t",dpi
print "%011.8f" % dpi,"\t","%011.8f" %sigma,"%011.8f" %cpi
