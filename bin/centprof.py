#########################################################
#                                                       #
#                Center the profile	                #
#               Written by : Devansh Agarwal            #
#               devansh@iisertvm.ac.in                  #
#                                                       #
#########################################################

def center(a,b,c,d):

	#Find the position of maxmima

	I = int(a.index(max(a)))

	#Find the shift

    	if I%2==0:
        	shift =int(-(len(a)/2)+I)
    	else:
		shift =int(-((len(a)+1)/2)+I)

	#Center the profile

	I =  a[shift:] + a[:shift]
	Q =  b[shift:] + b[:shift]
	U =  c[shift:] + c[:shift]
	V =  d[shift:] + d[:shift]

	return(I,Q,U,V)
