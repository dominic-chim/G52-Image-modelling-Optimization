
gensurf3d <- function(fis, ix1=1, ix2=2, ox1=1) {
#Inputs		: A FIS structure
#Outputs	: A 3-D graph with two inputs on the x and y axes, and one ouput on the z
	
	i1 = fis$inputList[[ix1]]
	i2 = fis$inputList[[ix2]]
	o1 = fis$outputList[[ox1]]

	i1b = i1$inputBounds
	i2b = i2$inputBounds

	i1_min = i1b[1]
	i1_max = i1b[length(i1b)]

	i2_min = i2b[1]
	i2_max = i2b[length(i2b)]
  
	x_values = seq(i1_min, i1_max, length = 15)
	y_values = seq(i2_min, i2_max, length = 15)
	
	m_values = meshgrid(x_values, y_values)

	o_values = evalFIS(cbind(c(m_values$x), c(m_values$y)), fis)


	z_values = matrix(o_values[,ox1], 15, 15, byrow=TRUE)

  	h_values = (z_values[-15,-15] + z_values[-1,-15] + z_values[-15,-1] + z_values[-1,-1]) / 4
  	h_values= floor((h_values-min(h_values))/(max(h_values)-min(h_values))*14+.5)+1

	col3d= rbind(1, cbind(matrix(16-h_values, 14, 14), 1))
	persp3d(x_values, y_values, z_values, 
    	xlab=i1$inputName, ylab=i2$inputName, zlab=o1$outputName, 
    	theta=-30, phi=30, col=rainbow(15)[col3d], ticktype='detailed', smooth=F, lit=F)
}

