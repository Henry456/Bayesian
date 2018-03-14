rm(list=ls())
set.seed(1)
reps = 50000

target.func = function(lambda){
  x = c(0,2,0,0,1,0)
  return(lambda^(sum(x)-1/2) * exp(-6*lambda) /2)
}

chain = c(0.1)
for(i in 1:reps){
  proposal = runif(1, -1, 1)
  while(proposal < 0 ){
    proposal = chain[i] + runif(1, -1, 1)
  }
  
  accept = runif(1) < target.func(proposal) / target.func(chain[i])
  chain[i+1] = ifelse(accept==T, proposal, chain[i])
}

plot(density(chain[1000:50000]), ylim=c(0,1.5))
den=target.func(seq(from=0, to=3, by=.1))
mean(chain[10000:50000])
quantile(chain[1000:50000],  probs = c(2.5, 25, 50, 75, 97.5)/100)
