#'@title creates a dataframe containing the points for the espiral hiperbolica mandala
#'@name f_transxy
#'@author Luciane Ferreira Alcoforado
#
#'
#'@description  Function to translation points by shifts on the x-axis or y-axis or both
#'@param x is a vector length n with coordinate x of point
#'@param y is a vector length n with coordinate y of point
#'@param tx is a vector with with shifts on the x-axis
#'@param ty is a vector with with shifts on the y-axis
#'@return Returns a dataframe with the original points plus the respective translation of these points.
#'@examples
#'x=c(1,1)
#'y=c(0,1)
#'tx=c(-1,-2)
#'ty=c(0,0)
#'f_transxy(x,y,tx,ty)
#'
#'
#'@export
f_transxy = function(x,y,tx,ty){
min=min(length(tx),length(ty))
max=max(length(tx),length(ty))
k=which.min(c(length(tx),length(ty)))
if(k==2){
ty=c(ty,rep(0,(max-min)))}
     else {tx=c(tx,rep(0,(max-min)))
     }
n=length(x)
xt=x
yt=y
for(i in 1:length(tx)){
xt=c(xt,x[1:n]+tx[i])
}
for(i in 1:length(ty)){
yt=c(yt,y[1:n]+ty[i])
}
return(data.frame(x=xt,y=yt))}

