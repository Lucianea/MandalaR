#'@title creates a dataframe containing the points for the elipse mandala
#'@name pelipse
#'@author Luciane Ferreira Alcoforado
#

#'@description  Function to build point for the elipse base
#'
#'@param theta is a vector length 2 with start angle and end angle
#'@param a is one of the parameters of the curves; for the ellipse is the radius on the x axis
#'@param b is one of the parameters of the curves; for the ellipse is the radius on the y axis
#'@param k is a vector of length 1 with angles in degree to rotate the point (x,y)
#'@param n is a number of points
#'@return Returns a dataframe with the original points plus the respective rotations of these points.
#'@examples
#'theta = c(0,2*pi) #half turn angle
#'a = 1
#'b=2
#'k = 90
#'n=20
#'pelipse(theta, a, b, k, n)
#'
#'@export
#'

pelipse = function(theta, a, b, k, n){
theta = seq(theta[1],theta[2],length.out=n)
x=a*sin(theta)
y=b*cos(theta)
dt0=f_trans(x,y,t=c(-1,-2),1) #fixei a translação, pode ser modificado futuramente
x=dt0$x
y=dt0$y

rotacao = seq(k*pi/180,2*pi, length.out=floor(360/k))

dt=f_rotacao(x,y,rotacao)

return(dt)}

