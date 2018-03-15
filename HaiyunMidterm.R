#Problem 2
f_theta1 = c(0.7, 0.2, 0.1)
f_theta2 = c(0.2, 0.4, 0.4)
f_theta3 = c(0.4, 0.4, 0.2)

loss = function(p1, p2, p3, f_theta){
  return(p1*log(p1/f_theta[1]) + p2*log(p2/f_theta[2]) + p3*log(p3/f_theta[3]))
}
(Rkl_theta1_theta1Hat = sum(c(0.7, 0.2, 0.1) * c( loss(0.7,0.2,0.1, f_theta1), 
                                                  loss(0.7,0.2,0.1, f_theta2),
                                                  loss(0.7, 0.2,0.1, f_theta3))))

(Rkl_theta1_theta2Hat = sum(c(0.7, 0.2, 0.1) * c(loss(0.7,0.2,0.1, f_theta1), 
                                                 loss(0.7,0.2,0.1, f_theta3),
                                                 loss(0.7, 0.2,0.1, f_theta2))))

(Rkl_theta1_theta3Hat = sum(c(0.7, 0.2, 0.1) * c(loss(0.7,0.2,0.1, f_theta3), 
                                                 loss(0.7,0.2,0.1, f_theta2),
                                                 loss(0.7, 0.2,0.1, f_theta1))))

(Rkl_theta2_theta1Hat = sum(c(0.2, 0.4, 0.4) * c(loss(0.2,0.4,0.4, f_theta1), 
                                                 loss(0.2,0.4,0.4, f_theta2),
                                                 loss(0.2,0.4,0.4, f_theta3))))

(Rkl_theta2_theta2Hat = sum(c(0.2, 0.4, 0.4) * c(loss(0.2,0.4,0.4, f_theta1), 
                                                 loss(0.2,0.4,0.4, f_theta3),
                                                 loss(0.2,0.4,0.4, f_theta2))))

(Rkl_theta2_theta3Hat = sum(c(0.2, 0.4, 0.4) * c(loss(0.2,0.4,0.4, f_theta3), 
                                                 loss(0.2,0.4,0.4, f_theta2),
                                                 loss(0.2,0.4,0.4, f_theta1))))

(Rkl_theta3_theta1Hat = sum(c(0.4, 0.4, 0.2) * c(loss(0.4, 0.4, 0.2, f_theta1), 
                                                 loss(0.4, 0.4, 0.2, f_theta2),
                                                 loss(0.4, 0.4, 0.2, f_theta3))))

(Rkl_theta3_theta2Hat = sum(c(0.4, 0.4, 0.2) * c(loss(0.4, 0.4, 0.2, f_theta1), 
                                                 loss(0.4, 0.4, 0.2, f_theta3),
                                                 loss(0.4, 0.4, 0.2, f_theta2))))

(Rkl_theta3_theta1Hat = sum(c(0.4, 0.4, 0.2) * c(loss(0.4, 0.4, 0.2, f_theta3), 
                                                 loss(0.4, 0.4, 0.2, f_theta2),
                                                 loss(0.4, 0.4, 0.2, f_theta1))))

#problem4
rm(lis)
set.seed(100)
reps = 50000
target.func = function(theta){
  x = c(2.7, -2.9, -3.0, 3.1, 2.6, 1.7, 2.9, -2.2)
  
  return(exp(sum(cos(x-theta))) * 1/(2*pi) )
}



chain = c(2.9)
for(i in 1:reps){
  proposal = chain[i] + 3/4*pi*runif(1, -1, 1)
  while(proposal < -pi || proposal >= pi ){
    proposal = chain[i] + 3/4*pi*runif(1, -1, 1)
  }
  accept = runif(1) <  target.func(proposal)/ target.func(chain[i])
  chain[i+1] = ifelse(accept==T, proposal, chain[i])
}
#Simulate 1000 point
chain[1:1000]
t = cut(chain[1:1000], breaks=c(-pi, -7*pi/8, -6*pi/8, -5*pi/8, -4*pi/8, -3*pi/8, 
                                -2*pi/8, -1*pi/8, 0,pi/8, 2*pi/8, 3*pi/8, 4*pi/8, 
                                5*pi/8, 6*pi/8, 7*pi/8, 8*pi/8 ), include.lowest=TRUE)
table(t)
#After converge, our mean
mean(chain[10000:50000])
quantile(chain[1000:50000],  probs = c(2.5, 25, 50, 75, 97.5)/100)


