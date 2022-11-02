#'Mandalar: package for building mandalas from parametric equations of classical curves
#'
#'If x and y dimension is n and rotation dimension is k, then function f_rotacao will return a dataframe with two columns and (n+1)k rows
#'@export
#'
#'@description  Function to rotate points by one or more angles
#'@param x is a vector length n with coordinate x of point
#'@param y is a vector length n with coordinate y of point
#'@param rotacao is a vector of length k with angles in radians to rotate the point (x,y)
#'@return Returns a dataframe with the original points plus the respective rotations of these points.
#'@examples
#'x=c(1,1)
#'y=c(0,1)
#'rotacao=c(pi/3, pi/2, pi)
#'f_rotacao(x,y,rotacao)
#'


f_rotacao = function(x,y,rotacao){
n=length(x)
xt=x
yt=y
for(i in 1:length(rotacao)){
xt=c(xt,x[1:n]*cos(rotacao[i])-y[1:n]*sin(rotacao[i]))
yt=c(yt,x[1:n]*sin(rotacao[i])+y[1:n]*cos(rotacao[i]))
}
return(data.frame(x=xt,y=yt))}

