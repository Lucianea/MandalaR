#'@title creates a dataframe containing the points for the astroide mandala
#'@name pastroide
#'@author Luciane Ferreira Alcoforado
#
#'
#'@description  Function to build a astroide
#'@param theta is a vector length 2 with start angle and end angle
#'@param raio is a vector length 1 with radius value. For astroide we do r=3.
#'@param k is a vector of length 1 with angles in degree to rotate the point (x,y)
#'@param n is a number of points
#'@return Returns a dataframe with the original points plus the respective rotations of these points.
#'@examples
#'theta = c(0,2*pi)
#'k=45
#'raio = 1
#'n=20
#'pastroide(theta, raio, k, n)
#'

#'@export
#'

pastroide = function(theta, raio, k, n){
theta = seq(theta[1],theta[2],length.out=n)
raio=3
x=cos(theta)^raio
y=sin(theta)^raio
dt0=f_trans(x,y,t=c(-1,-2),1) #fixei a translação, pode ser modificado futuramente
x=dt0$x
y=dt0$y

rotacao = seq(k*pi/180,2*pi, length.out=floor(360/k))

dt=f_rotacao(x,y,rotacao)

return(dt)}

