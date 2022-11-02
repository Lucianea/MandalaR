#'Mandalar: package for building mandalas from parametric equations of classical curves
#'

#'@export
#'
#'@description  Function to reduce points
#'@param x is a vector length n with coordinate x of point
#'@param y is a vector length n with coordinate y of point
#'@param k is a vector with factor of decrease or increase points
#'@return Returns a dataframe with the original points plus the respective changed points.
#'@examples
#'x=c(1,1)
#'y=c(0,1)
#'k=c(0.5)
#'f_factor(x,y,k)
#'
#'
f_factor = function(x,y,k){
xt=x
yt=y
n=length(x)
for(i in 1:length(k)){
xt=c(xt,x[1:n]*k[i])
yt=c(yt,y[1:n]*k[i])
}
return(data.frame(x=xt,y=yt))}





