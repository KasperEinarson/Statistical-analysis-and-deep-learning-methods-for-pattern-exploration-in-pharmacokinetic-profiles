WilksLambda.kasper <-
  function(rho,p,q) 
  {
    minpq <- min(p, q)
    WilksLambda <- numeric(minpq)
    for (rhostart in 1:minpq) {
      WilksLambda[rhostart] <- prod((1 - rho^2)[rhostart:minpq])
    }
    invisible(WilksLambda)
  }
  


"HotellingLawleyTrace" <-
  function (rho, p, q) 
  {
    minpq <- min(p, q)   
    HotellingLawleyTrace <- numeric(minpq)
    for (rhostart in 1:minpq) {
      rhosq = rho^2
      HotellingLawleyTrace[rhostart] = sum((rhosq/(1-rhosq))[rhostart:minpq])        
    }
    invisible(HotellingLawleyTrace)
  }



"PillaiBartlettTrace" <-
  function (rho, p, q) 
  {
    minpq <- min(p, q)   
    PillaiBartlettTrace <- numeric(minpq)
    for (rhostart in 1:minpq) {
      PillaiBartlettTrace[rhostart] = sum((rho^2)[rhostart:minpq])        
    }
    invisible(PillaiBartlettTrace)
  }
