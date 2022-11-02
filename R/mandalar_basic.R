#'@title Create a mandala with algorithm basic
#'@name mandalar_basic
#'@author Luciane Ferreira Alcoforado
#
#'
#'@description  Function to create a mandala with the basic method
#'
#'@param curve Either a character string or a function returning curve equation evaluated at its first argument.
#'Curves "circle","elipse", "cardioide","limacon", "espiral1", "espiral2", "lemniscata", "deltoide" and "astroide" are recognised, case being ignored.
#'@param theta is a vector length 2 with start angle and end angle
#'@param k is a angle of rotations, k in (0,360) graus
#'@param n is a number of points
#'@param raio is a positive number for the radius of circle
#'@param a is one of the parameters of the curves; for the ellipse is the radius on the x axis
#'@param b is one of the parameters of the curves; for the ellipse is the radius on the y axis
#'
#'@return Returns a dataframe with the original points plus the respective rotations of these points.
#'
#'@examples
#'require(ggplot2)
#'mandalar_basic("circle", theta = c(0,2*pi), raio=1, k = 45, n=500)
#'mandalar_basic("cardioide", theta = c(0,2*pi), raio=1, k = 60, n=500)
#'mandalar_basic("elipse", theta = c(0,2*pi), a=1, b=2, k = 30, n=500)

#'@export
mandalar_basic = function(curve,theta,k,n,raio,a,b){
curvas=c("circle","elipse", "cardioide","limacon", "espiral1", "espiral2", "lemniscata", "deltoide", "astroide")


if (is.character(curve)) {
        curvename <- tolower(curve)
        curvefun <- switch(curvename, circle = pcircle, elipse = pelipse,
            cardioide = pcardioide,limacon = plimacon,
            espiral1 = pespiral1, espiral2 = pespiral2, lemniscata = plemniscata,
            deltoide = pdeltoide, astroide = pastroide,
            NULL)
        if (is.null(curvefun))
            stop("unsupported curve")

   if (any(k < 0))
                stop("need positive value of rotation angle in degrees (k>0)")
   if (any(n <= 0))
                stop("need positive values number of points (n>0)")

   if (curvename == "circle") {
   dt=pcircle(theta, raio, k, n)
   return(plot_mandala(dt))
   }
   if (curvename == "elipse") {
   dt=pelipse(theta, a, b, k, n)
   return(plot_mandala(dt))
   }
   if (curvename == "cardioide") {
   dt=pcardioide(theta, raio, k, n)
   return(plot_mandala(dt))
   }
   if (curvename == "limacon") {
   dt=plimacon(theta, raio, k, n)
   return(plot_mandala(dt))
   }
   if (curvename == "espiral1") {
   dt=pespiral1(theta, raio, k, n)
   return(plot_mandala(dt))
   }
   if (curvename == "espiral2") {
   dt=pespiral2(theta, raio, k, n)
   return(plot_mandala(dt))
   }
  if (curvename == "lemniscata") {
   dt=plemniscata(theta, raio, k, n)
   return(plot_mandala(dt))
  }
  if (curvename == "deltoide") {
   dt=pdeltoide(theta, raio, k, n)
   return(plot_mandala(dt))
  }
  if (curvename == "astroide") {
   dt=pastroide(theta, raio, k, n)
   return(plot_mandala(dt))
   }
    stop(gettextf("supplying pars for the %s curve is not supported",
                  "not circle"), domain = NA)
 }

}
