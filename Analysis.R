library(rstan)
library(parallel)
library(tidyverse)
library(abind)
source('BayesianDiagnostics.R')

#----Cores for parallel----
numCores<- detectCores()
cl <- makeCluster(numCores)


#----Perform computation in parallel----


clusterExport(cl, list('simulate.bayesian.diagnostic', 'prior.params'))
results = parLapply(cl,
                    1:4,
                    function(x) simulate.bayesian.diagnostic(38,87,2,35,prior.params = prior.params, chainID = x,))
stopCluster(cl)

#----Rearrange data to be monitorable by stan----


draws = map_dfr(results, bind_rows) 

o.draws = draws%>%
  as_tibble() %>% 
  gather(key, var, -iteration, -chain) %>% 
  spread(chain, var)


vars = o.draws$key
o.draws = o.draws %>% select(-key, -iteration)
for.monitoring = split(o.draws, vars )
monitor.me = abind(for.monitoring, along = 3)
monitor(monitor.me, warmup = 1000, digits_summary = 2, probs = c(0.5,0.025, 0.975))

draws %>% 
  ggplot(aes(pi))+
  geom_histogram(color = 'white')+
  theme(aspect.ratio = 1/1.61)
