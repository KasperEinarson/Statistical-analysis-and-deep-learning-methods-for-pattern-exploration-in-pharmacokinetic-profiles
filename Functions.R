
quantile.plot2 <- function(x, Means = NULL, main, obs = NULL,ymax=3,ymin=0, ...)#, yside = 2, 
{
  nx <- x
  plot(x = c(1,ncol(nx)), y = c(ymin,ymax), main = main,xlab = "Time",ylab="Respons")
  abline(v = 1:ncol(nx), lty = 3, col = "grey")
  for(i in 1:floor(nrow(nx)/2))
    polygon(x = c(1:ncol(nx),ncol(nx):1), y = c(nx[i,],rev(nx[nrow(nx)-i+1,])),
            col = grey(1 - i/ceiling(nrow(nx)/2+1)), border = grey(1 - i/ceiling(nrow(nx)/2+1)))
  if(is.null(Means)) {
    Means <- apply(nx, 2, mean)
    lines(x = 1:ncol(nx), y = Means, col = "red", lwd = 2)
  }
  if(!is.null(obs)) lines(obs, type = "b", lwd = 2, col = "blue")
  legend("topright","Mean",lty=1,col="red")
}


Plot_quantile2 <- function(Data,main = ""){
  Data_d <- Data %>% group_by(Time) %>% summarise(kvartil1 = quantile(Var,0.025),
                                                  kvartil2 = quantile(Var,0.1),
                                                  kvartil3 = quantile(Var,0.15),
                                                  kvartil4 = quantile(Var,0.2),
                                                  kvartil5 = quantile(Var,0.25),
                                                  kvartil6 = quantile(Var,0.3),
                                                  kvartil7 = quantile(Var,0.35),
                                                  kvartil8 = quantile(Var,0.4),
                                                  kvartil9 = quantile(Var,0.45),
                                                  kvartil10 = quantile(Var,0.475),
                                                  kvartil11 = quantile(Var,0.525),
                                                  kvartil12 = quantile(Var,0.55),
                                                  kvartil13 = quantile(Var,0.6),
                                                  kvartil14 = quantile(Var,0.65),
                                                  kvartil15 = quantile(Var,0.70),
                                                  kvartil16 = quantile(Var,0.75),
                                                  kvartil17 = quantile(Var,0.8),
                                                  kvartil18 = quantile(Var,0.85),
                                                  kvartil19 = quantile(Var,0.9),
                                                  kvartil20 = quantile(Var,0.975)
                                                  
                                                  
                                                  
                                                  
                                                  
  )
  
  
  Data_d1 <- Data_d[,-1]
  Data_d2 <- t(Data_d1)
  return(Data_d2)
}



Kasper.perm <- function (X, Y, nboot = 999, rhostart = 1, type = "Wilks") 
{
  if (nrow(Y) != nrow(X)) 
    stop(" Function p.perm: Input arrays must not have unequal number of rows.\n")
  if ((type == "Roy") && (rhostart > 1)) 
    stop(" Function p.perm: When using Roy's Largest Root, parameter rhostart can only be 1.\n")
  N <- nrow(Y)
  ind <- 1:N
  p = dim(X)[2]
  q = dim(Y)[2]
  minpq = min(p, q)
  if (rhostart > minpq) 
    stop(" Function p.perm: Parameter rhostart too big.\n")
  rho0 <- rcc(X, Y,lambda1 = 0.00154,lambda2 = 0.0155)$cor
  if (type == "Wilks") 
    stat0 <- WilksLambda(rho0, p, q)[rhostart]
  else if (type == "Hotelling") 
    stat0 <- HotellingLawleyTrace(rho0, p, q)[rhostart]
  else if (type == "Pillai") 
    stat0 <- PillaiBartlettTrace(rho0, p, q)[rhostart]
  else if (type == "Roy") 
    stat0 <- p.Roy(rho0, N, p, q)$stat
  else stop(" Function p.perm: Illegal type of test statistic.\n")
  stat <- numeric(nboot)
  for (i in 1:nboot) {
    ind.mixed <- sample(ind, size = N, replace = FALSE)
    rho <- rcc(X, Y,lambda1 = 0.00154,lambda2 = 0.0155)$cor
    if (type == "Wilks") 
      stat[i] <- WilksLambda(rho, p, q)[rhostart]
    else if (type == "Hotelling") 
      stat[i] <- HotellingLawleyTrace(rho, p, q)[rhostart]
    else if (type == "Pillai") 
      stat[i] <- PillaiBartlettTrace(rho, p, q)[rhostart]
    else if (type == "Roy") 
      stat[i] <- p.Roy(rho, N, p, q)$stat
  }
  if (type == "Wilks") {
    nexcess <- sum(stat <= stat0)
    p <- mean(stat <= stat0)
  }
  else {
    nexcess <- sum(stat >= stat0)
    p <- mean(stat >= stat0)
  }
  mstat = mean(stat)
  tab <- cbind(stat0, mstat, nboot, nexcess, p)
  rownames(tab) = ""
  cat(" Permutation resampling using", type, "'s statistic:\n")
  print(tab)
  invisible(list(id = "Permutation", type = type, stat0 = stat0, 
                 stat = stat, nexcess = nexcess, p.value = p))
}