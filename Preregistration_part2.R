#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Preregistration FlexibleSexualAppetite part 2
#  simulation of data to see whether planned analyses code works
#	 Start : 29/05/2018
#	 last modif : 
#	 commit: start
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



rm(list = ls(all = TRUE))

{# packages
  library(lme4)
  library(pbapply)
}


nF <- 180 # number of females to be tested
pbrep <- 1000 # number of simulation replicates
prob <- 0.9 # probability of DpdtYN being yes


#Function to check number of significant result by chance
Simulate_and_analyse <-function(){

{# simulation data
  
  FID <- 1:nF
  FTrt <- c(rep(0.5,nF/2), rep (1,nF/2))
  MID <-1:nF
  MTrt <- rep(c(0,1,2), nF/3)

  DpdtYN <- sample(c(0,1), nF,replace=TRUE, prob = c(1-prob,prob))
  table(DpdtYN)
  
  DelayTo <- rnorm(nF, mean = 15, sd = 7) 
  
  Fsize <- sort(rnorm(nF,mean = 1.475, sd=0.128))
  Fweight <- rnorm(nF,mean = 229.0, sd=87.3)
  Fcondition <- resid(lm(Fweight~Fsize))
  Msize <- sort(rnorm(nF*2,mean = 1.295, sd=0.065))
  Mweight <- rnorm(nF*2,mean = 119.2, sd=33.6)
  Mcondition <- resid(lm(Mweight~Msize))
  
  MY_TABLE <- data.frame(FID, FTrt, DpdtYN, DelayTo, Fcondition, MID, MTrt, Msize, Mcondition, row.names = NULL)
  

}

head(MY_TABLE)

plot(MY_TABLE$Msize, MY_TABLE$Mcondition)
cor.test(MY_TABLE$Msize, MY_TABLE$Mcondition)

## Trt Red Averse is the reference (intercept)

mod <- glm (DpdtYN ~ FTrt* MTrt +Msize + Mcondition + Fcondition, family = "binomial", data = MY_TABLE)

#par(mfrow=c(2,2))
#plot(mod3)

summary(mod)

## to get one sided test p value
modp <-  coef(summary(mod))[-1, 4]




## to check equality of male motivation to court
#summary(lm(MY_TABLE$DelayTo ~ MY_TABLE$MTrt))



 return(list(modp))  
}  

OutputSimulation <- do.call(rbind, pbreplicate(pbrep,Simulate_and_analyse()))
OutputSimulation <- OutputSimulation<0.05
colSums(OutputSimulation)/pbrep



