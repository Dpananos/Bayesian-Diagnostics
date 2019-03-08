#----Define Prior Parameters----
# CAUTION: Assumes Beta Prior

prior.params = list(
  'pi.prior.a' = 1,
  'pi.prior.b' = 1,
  'S1.prior.a' = 21.96,
  'S1.prior.b' = 5.49,
  'C1.prior.a' = 4.1,
  'C1.prior.b' = 1.76,
  'S2.prior.a' = 4.44,
  'S2.prior.b' = 13.31,
  'C2.prior.a' = 71.25,
  'C2.prior.b' = 3.75
)

#----Define Simulation Function----

simulate.bayesian.diagnostic<-function(u,v,w,x,prior.params, N.draws = 10000, chainID, warmup = 1000, seed = NULL){
  if(N.draws<warmup) stop('N.draws must be larger than warmup')
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  env = environment()
  list2env(prior.params, envir = env)
  
  pi.samples<-rep(0,N.draws)
  pi<-rbeta(1,pi.prior.a, pi.prior.b)
  
  S1.samples<-rep(0, N.draws)
  S1<-rbeta(1,S1.prior.a, S1.prior.b)
  
  S2.samples<-rep(0, N.draws)
  S2<-rbeta(1,S2.prior.a, S2.prior.b)
  
  C1.samples<-rep(0,N.draws)
  C1<-rbeta(1,C1.prior.a, C1.prior.b)
  
  C2.samples<-rep(0,N.draws)
  C2<-rbeta(1,C2.prior.a, C2.prior.b)
  
  Y1.samples<-rep(0,N.draws)
  Y2.samples<-rep(0, N.draws)
  Y3.samples<-rep(0, N.draws)
  Y4.samples<-rep(0, N.draws)
  
  for( i in 1:N.draws){

    Y1<- rbinom(1,u,pi*S1*S2/( pi*S1*S2 + (1-pi)*(1-C1)*(1-C2)) )
    Y1.samples[i]<- Y1
    
    Y2<- rbinom(1,v ,pi*S1*(1-S2)/(pi*S1*(1-S2) + (1-pi)*(1-C1)*C2))
    Y2.samples[i]<- Y2
    
    Y3<- rbinom(1,w, pi*(1-S1)*S2/(pi*(1-S1)*S2 + (1-pi)*C1*(1-C2)) )
    Y3.samples[i]<-Y3
    
    Y4<- rbinom(1,x, pi*(1-S1)*(1-S2)/(pi*(1-S1)*(1-S2) + (1-pi)*C1*C2 ) )
    Y4.samples[i]<-Y4
    
    
    N = 
    pi<- rbeta(1,Y1+Y2 + Y3 + Y4 + pi.prior.a, u + v + w + x - Y1 - Y2 - Y3 - Y4 + pi.prior.b)
    pi.samples[i]<-pi
    
    S1<- rbeta(1,Y1 + Y2 + S1.prior.a, Y3+Y4 + S1.prior.b)
    S1.samples[i]<-S1
    
    C1<- rbeta(1,w + x - Y3 - Y4 + C1.prior.a, u+v - Y1 - Y2 + C1.prior.b)
    C1.samples[i]<-C1

    S2<- rbeta(1,Y1+Y3+S2.prior.a, Y2+Y4 + S2.prior.b)
    S2.samples[i] <- S2
    
    C2<- rbeta(1,v+x-(Y2+Y4) + C2.prior.a, u+w - Y1 - Y3 + C2.prior.b)
    C2.samples[i]<- C2
  }
  
  results=data.frame(
    'pi' = pi.samples,
    'S1' = S1.samples,
    'S2' = S2.samples,
    'C1' = C1.samples,
    'C2' = C2.samples,
    'Y1'= Y1.samples,
    'Y2' = Y2.samples,
    'Y3' = Y3.samples,
    'Y4' = Y4.samples,
    'chain' = chainID,
    'iteration' = 1:N.draws
  ) 
  
  return(results)
  
}

