#'@title creates a dataframe containing the points for the espiral hiperbolica mandala
#'@name f_trans
#'@author Luciane Ferreira Alcoforado

#'
#'@description  Function to translation points by shifts on the x-axis or y-axis
#'@param x is a vector length n with coordinate x of point
#'@param y is a vector length n with coordinate y of point
#'@param t is a vector with shifts on the x or y-axis
#'@param d is a direction translation, 1)x or 2)y
#'@return Returns a dataframe with the original points plus the respective translation of these points.
#'@examples
#'x=c(1,1)
#'y=c(0,1)
#'t=c(-3, 3)
#'d=1
#'f_trans(x,y,t,d)
#'
#'@export
#'
f_trans = function(x,y,t,d){
xt=x
yt=y
n=length(x)
if(d==1){
for(i in 1:length(t)){
xt=c(xt,x[1:n]+t[i])
}
yt=rep(yt,length(t)+1)
}
else{
for(i in 1:length(t)){
yt=c(yt,y[1:n]+t[i])
}
xt=rep(xt,length(t)+1)
}
return(data.frame(x=xt,y=yt))}

