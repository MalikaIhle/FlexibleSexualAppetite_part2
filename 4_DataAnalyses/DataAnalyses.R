#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data analyses FlexibleSexualAppetite part 2
#	 Start : 06/25/2019 
#	 commit: data analyse
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
## CopulateYN reflects copulation that occur at any time, during the video, seen after on the shelf, or could be extrapolated from the fact that spiderlings emerged
## CannibaliseYN reflects cannibalism that accour during the 2h video or the 46 hours after that (after which male and female were separated)
## CopDuringVideo are the copulation that occured within the 2h video
## EatDuringVideo are the cannibalism that occured during the 2h video
  
}

{# packages
library(reshape2) # for function dcast for pivot table
library(here)
}

{# load data
MY_TABLE_all <- read.csv(file = paste(here(),"3_ExtractedData/MY_TABLE.csv", sep="/"), header=TRUE, sep=",") # with 20 unmanipulated male tests
summary(MY_TABLE_all)

MY_TABLE <- MY_TABLE_all[MY_TABLE_all$MTrt != "Unmanipulated",]
summary(MY_TABLE)

}

head(MY_TABLE)

# decision of dependent variable as preregistered
{
## is frequency of cannibalism within 48h between 15% and 85%?
summary(MY_TABLE$CannibalizeYN)*100 # 36.2% yes

## is frequency of copulation during video between 17% and 83%?
summary(MY_TABLE$CopDuringVideo)*100 # 58.37% yes
}

# preregistered analyses
{#### in preregistration
#### Model 1: glm (CannibalismY/N ~ male treatment * female treatment + female body condition, family = binomial).
#### Model 2: in all red males: glm (CannibalismY/N ~ female treatment + female body condition, family = binomial
#### Model 3: glm (CopulationY/N ~ male treatment * female treatment + male size + male body condition, family = binomial).
#### Model 4: in all red males: glm (CopulationY/N ~ female treatment + male size + male body condition, family = binomial).

#### If Fcondition is significantly leading to more cannibalism this should be removed (Preregistered)

# Model 1
modCannibalism <- glm (CannibalizeYN ~ FTrtCode* MTrtCode   
                         #+ Fcondition
                       , family = "binomial", data = MY_TABLE)

par(mfrow=c(2,2))
plot(modCannibalism)

summary(modCannibalism)


# Model 2
modCannibalism2 <- glm (CannibalizeYN ~ FTrtCode 
                        #+  Fcondition
                        , family = "binomial", data = MY_TABLE[MY_TABLE$MTrt == "AllRed",])

par(mfrow=c(2,2))
plot(modCannibalism2)

summary(modCannibalism2)


# Model 3

modCop <- glm (CopDuringVideo ~ FTrtCode* MTrtCode + MCarapaceWidth + Mcondition
               , family = "binomial", data = MY_TABLE)

par(mfrow=c(2,2))
plot(modCop)

summary(modCop)


# Model 4

modCop2 <- glm (CopDuringVideo ~ FTrtCode + MCarapaceWidth + Mcondition
                , family = "binomial", data = MY_TABLE[MY_TABLE$MTrt == "AllRed",])

par(mfrow=c(2,2))
plot(modCop2)

summary(modCop2)

}

# descriptive pivot table /raw data
{
PivotSampleSizes <- dcast(MY_TABLE_all, FTrt~MTrt, value.var="MID", length)
PivotCop <- dcast(MY_TABLE_all, FTrt~MTrt, value.var="CopDuringVideo", sum)
PivotCannibalism <- dcast(MY_TABLE_all, FTrt~MTrt, value.var="CannibalizeYN", sum)

PivotSampleSizes
PivotCop
PivotCannibalism

PivotCopPerc <- data.frame(FTrt = unique(MY_TABLE_all$FTrt) , PivotCop[,-1]/PivotSampleSizes[,-1]*100)
PivotCannibalismpPerc <- data.frame(FTrt = unique(MY_TABLE_all$FTrt) , PivotCannibalism[,-1]/PivotSampleSizes[,-1]*100)

PivotCopPerc
PivotCannibalismpPerc

}

# exploratory analyses
{## Are effect sizes stronger when subsetting to females that had more than 2 weeks training (which may increase the strength of learning the color biases)?
# Model 1sub
modCannibalismSub <- glm (CannibalizeYN ~ FTrtCode* MTrtCode   
                       #+ Fcondition
                       , family = "binomial", data = MY_TABLE[MY_TABLE$TrainingDuration > 14,])

par(mfrow=c(2,2))
plot(modCannibalismSub)

summary(modCannibalismSub)


# Model 2sub
modCannibalism2Sub <- glm (CannibalizeYN ~ FTrtCode 
                        #+  Fcondition
                        , family = "binomial", data = MY_TABLE[MY_TABLE$MTrt == "AllRed" & MY_TABLE$TrainingDuration > 14,])

par(mfrow=c(2,2))
plot(modCannibalism2Sub)

summary(modCannibalism2Sub)


# Model 3sub

modCopSub <- glm (CopDuringVideo ~ FTrtCode * MTrtCode + MCarapaceWidth + Mcondition
               , family = "binomial", data = MY_TABLE[MY_TABLE$TrainingDuration > 14,])

par(mfrow=c(2,2))
plot(modCopSub)

summary(modCopSub) # significatif the more red the less copulations


# Model 4sub

modCop2Sub <- glm (CopDuringVideo ~ FTrtCode + MCarapaceWidth + Mcondition
                , family = "binomial", data = MY_TABLE[MY_TABLE$MTrt == "AllRed" & MY_TABLE$TrainingDuration > 14,])

par(mfrow=c(2,2))
plot(modCop2Sub)

summary(modCop2Sub)

}

{## Is the number of spiderlings different for any of the categories Ftrt*MTrt?
length(MY_TABLE$BroodSize[MY_TABLE$BroodSize == 0]) # 74

PivotSpiderling <- dcast(MY_TABLE, FTrt~MTrt, value.var="BroodSize", sum)
PivotSpiderling

summary(lm(BroodSize ~ FTrtCode* MTrtCode  , data = MY_TABLE))
}

{## Do females on the two diet treatments differ in final adult size or condition (in ways that might suggest that the presence of aversive prey reduces overall feeding rate)?

shapiro.test(MY_TABLE$Fcondition)
hist(MY_TABLE$Fcondition)
wilcox.test(MY_TABLE$Fcondition[MY_TABLE$FTrt == "RedPreference"],
            MY_TABLE$Fcondition[MY_TABLE$FTrt == "RedAverse"])
}




