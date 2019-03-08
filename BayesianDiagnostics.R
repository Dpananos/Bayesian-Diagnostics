library(rstan)
library(parallel)
library(tidyverse)
library(abind)
numCores<- detectCores()
cl <- makeCluster(numCores)
#----Define Prior Parameters----
# CAUTION: Assumes Beta Prior

prior.params = list(
  'pi.prior.a' = 1,
  'pi.prior.b' = 1,
  'S.prior.a' = 21.96,
  'S.prior.b' = 5.49,
  'C.prior.a' = 4.1,
  'C.prior.b' = 1.76
)

#----Define Simulation Function----

simulate.bayesian.diagnostic<-function(a,b,prior.params, N.draws, chainID, warmup = 1000, seed = NULL){
  if(N.draws<warmup) stop('N.draws must be larger than warmup')
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  env = environment()
  list2env(prior.params, envir = env)
  
  pi.samples<-rep(0,N.draws)
  pi<-rbeta(1,pi.prior.a, pi.prior.b)
  
  S.samples<-rep(0, N.draws)
  S<-rbeta(1,S.prior.a, S.prior.b)
  
  C.samples<-rep(0,N.draws)
  C<-rbeta(1,C.prior.a, C.prior.b)
  
  Y1.samples<-rep(0,N.draws)
  Y2.samples<-rep(0, N.draws)
  
  for( i in 1:N.draws){

    Y1<- rbinom(1,a,pi*S/(pi*S + (1-pi)*(1-C)))
    Y1.samples[i]<- Y1
    
    Y2<- rbinom(1,b,pi*(1-S)/(pi*(1-S) + (1-pi)*C))
    Y2.samples[i]<- Y2
    
    pi<- rbeta(1,Y1+Y2+pi.prior.a, a+b - Y1 - Y2 + pi.prior.b)
    pi.samples[i]<-pi
    
    S<- rbeta(1,Y1 + S.prior.a, Y2 + S.prior.b)
    S.samples[i]<-S
    
    C<- rbeta(1,b-Y2+C.prior.a, a - Y1 + C.prior.b)
    C.samples[i]<-C

  }
  
  results=data.frame(
    'pi' = pi.samples,
    'S' = S.samples,
    'C' = C.samples,
    'Y1'= Y1.samples,
    'Y2' = Y2.samples,
    'chain' = chainID,
    'iteration' = 1:N.draws
  ) 
  
  return(results)
  
}




clusterExport(cl, list('simulate.bayesian.diagnostic', 'prior.params'))
results = parLapply(cl,
                    1:4,
                    function(x) simulate.bayesian.diagnostic(125,37,prior.params = prior.params, N.draws = 25000, chainID = x,))
stopCluster(cl)


draws = map_dfr(results, bind_rows) %>%
  as_tibble() %>% 
  gather(key, var, -iteration, -chain) %>% 
  spread(chain, var)


vars = draws$key
draws = draws %>% select(-key, -iteration)
for.monitoring = split(draws, vars )
monitor.me = abind(for.monitoring, along = 3)


monitor(monitor.me, warmup = 1000)
