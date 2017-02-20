#Seasonal change in contact rate 
#20/2/2017 Sean Browning
#Utilize cosine function with a period of 365pi for a rough model 

new_RateF <- function(x, p, t) {
    #take beta parameters and make them vary by time 
  beta1 <- (p$beta[1]-(.5*p$beta[1])cos(t/(365*pi))+(.1*p$beta[1])
  beta2 <- (p$beta[2]-(.5*p$beta[2])cos(t/(365*pi))+(.1*p$beta[2])
    #return rate functions
  return(c(x["S1Y"] * (x["I1Y"]+x["I1O"])*beta1, #infection rates
           x["S1Y"] * (x["I2Y"]+x["I2O"])*beta2,
           x["S1Y"] * (x["I3Y"]+x["I3O"])*beta2,
           x["S1O"] * (x["I1Y"]+x["I1O"])*beta1, 
           x["S1O"] * (x["I2Y"]+x["I2O"])*beta2,
           x["S1O"] * (x["I3Y"]+x["I3O"])*beta2,
           x["S2Y"] * (x["I2Y"]+x["I2O"])*beta1,
           x["S2Y"] * (x["I1Y"]+x["I1O"])*beta2,
           x["S2Y"] * (x["I3Y"]+x["I3O"])*beta2,
           x["S2O"] * (x["I2Y"]+x["I2O"])*beta1,
           x["S2O"] * (x["I1Y"]+x["I1O"])*beta2,
           x["S2O"] * (x["I3Y"]+x["I3O"])*beta2,
           x["S3Y"] * (x["I3Y"]+x["I3O"])*beta1, 
           x["S3Y"] * (x["I1Y"]+x["I1O"])*beta2,
           x["S3Y"] * (x["I2Y"]+x["I2O"])*beta2,
           x["S3O"] * (x["I3Y"]+x["I3O"])*beta1,
           x["S3O"] * (x["I1Y"]+x["I1O"])*beta2,
           x["S3O"] * (x["I2Y"]+x["I2O"])*beta2,
           x["I1Y"] * p$gamma,             #recovery rates
           x["I1O"] * p$gamma,
           x["I2Y"] * p$gamma,
           x["I2O"] * p$gamma,
           x["I3Y"] * p$gamma,
           x["I3O"] * p$gamma,
           x["S1O"] * p$alpha,             #birth rates
           x["S2O"] * p$alpha,
           x["S3O"] * p$alpha,
           x["S1Y"] * p$omega[1],             #death rates
           x["I1Y"] * p$omega[1],
           x["R1Y"] * p$omega[1],
           x["S1O"] * p$omega[2],            
           x["I1O"] * p$omega[2],
           x["R1O"] * p$omega[2],
           x["S2Y"] * p$omega[1],
           x["I2Y"] * p$omega[1],
           x["R2Y"] * p$omega[1],
           x["S2O"] * p$omega[2],
           x["I2O"] * p$omega[2],
           x["R2O"] * p$omega[2],
           x["S3Y"] * p$omega[1],
           x["I3Y"] * p$omega[1],
           x["R3Y"] * p$omega[1],
           x["S3O"] * p$omega[2],
           x["I3O"] * p$omega[2],
           x["R3O"] * p$omega[2],
           x["S1Y"] * p$V[1] * (t>x["D1"]),
           x["S2Y"] * p$V[2] * (t>x["D2"]),
           x["S3Y"] * p$V[3] * (t>x["D3"])
  ))
}
