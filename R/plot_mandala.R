#'@title creates a mandala visualization
#'@name plot_mandala
#'@author Luciane Ferreira Alcoforado
#
#'
#'@description  Function to plot a mandala with points in dataframe
#'@param dt dataframe with points x and y
#'@return Returns a plot
#'@examples
#'require(ggplot2)
#'n=500; raio=1; t=seq(0,2*pi, length.out = n)
#'x1=raio*cos(t)
#'y1=raio*sin(t)
#'#pontos para os 3 círculos: translação dos pontos iniciais (x1,x=c(x1,x1-raio,x1-2*raio)
#'x=c(x1,x1-raio,x1-2*raio)
#'y=c(y1,y1,y1)
#'dt=data.frame(x,y,z="circulo")
#'rotacao = (pi/8)*(1:16); n=length(x); xt1=x; yt1=y
#'dt=f_rotacao(x=dt$x, y=dt$y, rotacao)
#'plot_mandala(dt)

#'@export
require(ggplot2)
plot_mandala= function(dt){
  x=dt$x
  y=dt$y
p = ggplot2::ggplot() + ggplot2::coord_fixed() + ggplot2::theme_void()+
 ggplot2::geom_point(data=dt, ggplot2::aes(x=x, y=y), color='black', size=0.05)
return(p)}

