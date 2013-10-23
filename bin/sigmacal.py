#########################################################
#                                                       #
#		Sigma Calculator			#
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################

def sigma(I):
	ten=[]
	sizeI=int(len(I))
	last=int(sizeI-10)
	ten=I[last:] + I[:9]

	import numpy
	arr = numpy.array(ten)
	sigma=float(numpy.std(arr, axis=0))

	for x in range(10, sizeI-5):
		if I[x]<=3*sigma:
			ten.extend([I[x]])
			sigma=float(numpy.std(ten, axis=0))
	return(sigma)
