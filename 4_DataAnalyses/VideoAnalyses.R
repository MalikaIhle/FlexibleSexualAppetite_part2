#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Video analyses FlexibleSexualAppetite part 2
#	 Start : 31 Jan 2019
#	 last modif : 19 Aug 2019
#	 commit: import all videos watched
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
  ## missing videos: FID = 233, 149, 497, 254
      ### FID = 233 and FID = 149 (with unmanipulated male 372) are entirely missing
      
      ### FID = 497, videoID = 175
      ### has the first 1.5 hours corrupted and the last 30 min watched. 
      ### live recording indicates that copulation occured in part 1.
      ### video should be excluded as we did not watch after copulation for the other videos.
  
      ### FID = 254, videoID = 264
      ### has the first 8 min watchedm then there is a missing part of about 20 min then the video starts again but was not watched
      ### should be excluded

    
  ## videos with 2 copulations
      ### FID = 553, videoID = 150
  
  
  ## Females with 2 copulations (one on video, one after video)
      ### FID = 524, videoID = 82
      ### FID = 538, videoID = 240
  
  
  ## Behav_Codes
  #  Table      Code             Meaning
  #  Courtship    0 NoAttackNoCopulation
  #  Courtship    1           Copulation
  #  Courtship   -1               Attack
  #  Attack       1                Lunge
  #  Attack       2              Grapple
  #  Attack       3               Consum
  
  
  ## time end
  # at first we were watching the whole video, unless the male was getting cannibalized
  # then (soon) we stopped watching videos after copulation occured
  # overall time stopped watching is either the time of attack if she consumed it, the time start of the copulation, 
  # or the time at which we stopped watching (after about 2 hours) if none of those two events occured
  
  
}

#rm(list = ls(all = TRUE))

{# packages
  library(lme4)
  library(stringr)
  library(dplyr)
  library(here)
}


#~~~ GET DATASET

{# load data
  
MY_TABLE <- read.csv(file = paste(here(),"3_ExtractedData/MY_TABLE.csv", sep="/"), header=TRUE, sep=",") # with 20 unmanipulated male tests
summary(MY_TABLE)

MY_TABLE_Videos <- read.csv(file = paste(here(),"3_ExtractedData/MY_TABLE_Videos.csv", sep="/"), header=TRUE, sep=",") # with 19 unmanipulated male tests videos
summary(MY_TABLE_Videos)

MY_TABLE_Videos$TrialDate <- as.Date(MY_TABLE_Videos$TrialDate)
MY_TABLE <- merge(MY_TABLE, MY_TABLE_Videos[,c('FID', 'CopDur')], by = 'FID', all.x = TRUE)
MY_TABLE_Videos <- merge(MY_TABLE_Videos, MY_TABLE[,c('FID', 'Fcondition')], by = 'FID', all.x = TRUE)

}

head(MY_TABLE) # breeding and trial data including 20 unmanipulated males
head(MY_TABLE_Videos) # Video data including 19 videos on unmanipulated males


#~~~ ANALYSES

## JoVE
{## For JoVE paper, need to compare the here 19 (for jove only had 14 watched at the time) unmanipulated males to males tested at the same time (20 unmanipulated males were tested but one video is missing)
## (i.e. at the end of the season - which may be a specific subset of males)

DatesTrialsUnmanipulated <- MY_TABLE_Videos$TrialDate[MY_TABLE_Videos$MTrt == "Unmanipulated"]
summary((MY_TABLE_Videos[MY_TABLE_Videos$TrialDate > (min(DatesTrialsUnmanipulated)-1),]))
summary(DatesTrialsUnmanipulated)
Unmanip <- unique(MY_TABLE_Videos$MID[MY_TABLE_Videos$MTrt == "Unmanipulated"])

MY_TABLE_Videos_End <- MY_TABLE_Videos[MY_TABLE_Videos$TrialDate >= "2018-08-01" & MY_TABLE_Videos$TrialDate <= "2018-09-30",]
MY_TABLE_Videos_End$MpaintedYN <- "1"
MY_TABLE_Videos_End$MpaintedYN[MY_TABLE_Videos_End$MTrt == "Unmanipulated"] <- "0"

table(MY_TABLE_Videos_End$MpaintedYN)
nrow(MY_TABLE_Videos_End)

shapiro.test(MY_TABLE_Videos_End$DelayLeaveDish)
shapiro.test(log(MY_TABLE_Videos_End$DelayLeaveDish+1))

modDelayLeaveDish_End <- lm(I(log(DelayLeaveDish+1))~ MpaintedYN, data = MY_TABLE_Videos_End)
summary(modDelayLeaveDish_End)#
summary(MY_TABLE_Videos_End$DelayLeaveDish) # they all excited the vials

shapiro.test(MY_TABLE_Videos_End$DelayFirstCourt)
shapiro.test(log(MY_TABLE_Videos_End$DelayFirstCourt+1))

modDelayCourt_End <- lm(I(log(DelayFirstCourt+1)) ~ MpaintedYN, data = MY_TABLE_Videos_End)
summary(modDelayCourt_End) # 
summary(MY_TABLE_Videos_End$DelayFirstCourt) # one (allgrey) was cannibalized before courting

shapiro.test(MY_TABLE_Videos_End$TotalCourtDur)
hist(MY_TABLE_Videos_End$TotalCourtDur)
hist(log(MY_TABLE_Videos_End$TotalCourtDur+1))
shapiro.test(log(MY_TABLE_Videos_End$TotalCourtDur+1))

modTotalCourtDur_End <- lm(log(TotalCourtDur+1)~ MpaintedYN ,data = MY_TABLE_Videos_End)
summary(modTotalCourtDur_End)#
summary(MY_TABLE_Videos_End$TotalCourtDur) # one (allgrey) was cannibalized before courting

shapiro.test(log(MY_TABLE_Videos_End$NaiveTotalCourtDur+1))
modNaiveTotalCourtDur_End <- lm(log(NaiveTotalCourtDur+1)~ MpaintedYN, data = MY_TABLE_Videos_End)
summary(modNaiveTotalCourtDur_End)#
summary(MY_TABLE_Videos_End$NaiveTotalCourtDur)# one (allgrey) was cannibalized before courting

table(droplevels(MY_TABLE_Videos_End$MTrt))

MY_TABLE_Videos_End$MID # on 02/06/2019
#[1] 18382 18433 18570 18457 18397 18459 18488 18364 18386 18398 18390 18438 18374 18421 18465 18555 18439 18569 18498 18447 18467 18571 18448 18448 18440 18583 18580
#[28] 18582 18451 18373 18466 18424
# on 19/08/2019
#[1] 18382 18537 18531 18433 18570 18457 18397 18368 18459 18488 18364 18386 18398 18390 18374 18579 18421 18465 18555 18471 18439 18569 18498
#[24] 18447 18467 18571 18448 18440 18583 18580 18582 18451 18373 18466 18485 18494 18424


}

## Descriptive
{
### Nb of males who didn't court
summary(MY_TABLE_Videos$NBCourt) # all males courted but the 4 that were eaten prior to starting to court
nrow(MY_TABLE_Videos) # 237

### Nb of videos where female attacked (or at least intended to)
summary(MY_TABLE_Videos$NbIntendedFAttacks)
length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$NbIntendedFAttacks > 0])/
  nrow(MY_TABLE_Videos)*100 # 74.2%

### average time watched
summary(MY_TABLE_Videos$TotalWatch)/60 # 68.1 min

### average delay to court
summary(MY_TABLE_Videos$DelayFirstCourt)/60 # 7.57 min

### average duration courting (out of duration watched)
MY_TABLE_Videos$CourtshipRate <- (MY_TABLE_Videos$TotalCourtDur/60)/(MY_TABLE_Videos$TotalWatch/60/60)
summary(MY_TABLE_Videos$CourtshipRate) # 29.1 min of coursthip per hour (!! videos may have stopped after 12 min if copulation occured so max isnt really 59 min per hour !!)
summary(MY_TABLE_Videos$TotalWatch/60) # between 0.66 and 133.5 min watched (the lowest being when attacked immediatly, then the econd lozest are when copulate very quickly)
MY_TABLE_Videos[MY_TABLE_Videos$CourtshipRate > 50 & !is.na(MY_TABLE_Videos$CourtshipRate),]
MY_TABLE_Videos[MY_TABLE_Videos$TotalWatch/60 <1 ,]

### link between cannibalism and copulation
nrow(MY_TABLE_Videos[MY_TABLE_Videos$CannibalizeYN == 1 & MY_TABLE_Videos$CopulateYN == 1,])#at least 57 females copulated (seen) then cannibalised the male
nrow(MY_TABLE_Videos[MY_TABLE_Videos$CannibalizeYN == 0 & MY_TABLE_Videos$CopulateYN == 1,])#93 female copulated without cannibalising the male within 2 days


}


## Preregistered


## Exploratory

### Does copulation length explain number of spiderlings?
{
### FID with spiderlings when CopulateYN = No
nrow(MY_TABLE[MY_TABLE$BroodSize > 0 & MY_TABLE$CopulateYN == 0,]) # 20 females had spiderlings but were not seen copulated
nrow(MY_TABLE[MY_TABLE$BroodSize > 0 ,]) # out of 147 females that had spiderlings

### FID with CopulateYN = Yes and CopDuringVid = No
nrow(MY_TABLE[MY_TABLE$CopDuringVideo == 0 & MY_TABLE$CopulateYN == 1,]) # 9 - where copulation was seen live after the video
nrow(MY_TABLE) # out of 221 females observed

### FID with CopulateYN = Yes and spiderlings = No
nrow(MY_TABLE[MY_TABLE$BroodSize == 0 & MY_TABLE$CopulateYN == 1,]) # 11 females were seen copulated but had no spiderlings
nrow(MY_TABLE[MY_TABLE$CopulateYN == 1,]) # out of 138 females that were seen copulating

### correlation between copulation duration (when observed) and number of spiderlings
head(MY_TABLE)
summary(MY_TABLE$CopDur) # 4 videos missing + 4 males recorded but who never courted as were eaten before
summary(MY_TABLE_Videos$CopDur) # 4 males eaten before ever courting
summary(MY_TABLE$BroodSize)

summary(MY_TABLE$CopDur[ MY_TABLE$CopDur > 0])
hist(MY_TABLE$CopDur[ MY_TABLE$CopDur > 0])
summary(MY_TABLE$BroodSize[ MY_TABLE$CopDur > 0])
hist(MY_TABLE$BroodSize[ MY_TABLE$CopDur > 0])

plot(BroodSize~ CopDur, data=MY_TABLE[MY_TABLE$CopDur > 0 ,])
summary(lm(BroodSize~ CopDur , data=MY_TABLE[ MY_TABLE$CopDur > 0 ,])) # ***
nrow(MY_TABLE[ !is.na(MY_TABLE$CopDur) & MY_TABLE$CopDur > 0 ,]) # 137

}

### is male courtship effort correlated to female body condition?
{head(MY_TABLE_Videos)
nrow(MY_TABLE_Videos[MY_TABLE_Videos$EatDuringVideo == 1,])#28
nrow(MY_TABLE_Videos[MY_TABLE_Videos$EatDuringVideo == 0,])#209
hist(MY_TABLE_Videos$TotalCourtDur)

summary(glm(TotalCourtDur~ Fcondition
            , offset = log(TotalWatch)
            , family = 'poisson'
            , data = MY_TABLE_Videos)) # ***
}

### does male courtship effort predict copulation likelihood?
{#### observation length vary greatly between videos, depending on when the copulation occured (the shorter TotalWatch, the earlier the copulation occured)
#### one cannot just do CopulateYN~TotalCourtDur or even CopulateYN~CourtshipRate
#### one need to use STAN as in https://ecoevorxiv.org/jq9n6/ with https://osf.io/kv3uc/ part S6.3 (although the dependent variable was a continuous trait not a binomial trait like here)

summary(glm(TotalCourtDur~ CopulateYN
            , offset = log(TotalWatch)
            , family = 'poisson'
            , data = MY_TABLE_Videos)) # ***

summary(glm(CopulateYN~ CourtshipRate
            , family = 'binomial'
            , data = MY_TABLE_Videos)) # ***

}

### Are copulation and attacks negatively correlated (while controlling for male courtship effort)?
{
  MY_TABLE_Videos$IntendedFAttacksNotFatalYN[MY_TABLE_Videos$NbIntendedFAttacksNotFatal > 0] <- 1
  MY_TABLE_Videos$IntendedFAttacksNotFatalYN[MY_TABLE_Videos$NbIntendedFAttacksNotFatal == 0] <- 0
  MY_TABLE_Videos$IntendedFAttacksNotFatalRate <- MY_TABLE_Videos$NbIntendedFAttacksNotFatal/(MY_TABLE_Videos$TotalWatch/60/60)
  
summary(glm(CopulateYN~ IntendedFAttacksNotFatalRate + CourtshipRate
    , family = 'binomial'
    , data = MY_TABLE_Videos)) 

summary(glm(NbIntendedFAttacksNotFatal~ CopulateYN + TotalCourtDur
            , offset = log(TotalWatch)
            , family = 'poisson'
            , data = MY_TABLE_Videos)) # ***

summary(glm(NbIntendedFAttacksNotFatal~ CopulateYN + CourtshipRate
            , offset = log(TotalWatch)
            , family = 'poisson'
            , data = MY_TABLE_Videos)) # ***

summary(glm(NbIntendedFAttacksNotFatal~ TotalCourtDur
            #, offset = log(TotalWatch) # because both the dependent and the independent variable are measured within the same time for each video
            , family = 'poisson'
            , data = MY_TABLE_Videos)) # ***

summary(glm(CopulateYN~ IntendedFAttacksNotFatalYN + CourtshipRate
            , family = 'binomial'
            , data = MY_TABLE_Videos)) 

}

### Is cannibalism predicted by attacks ?

summary(glm(CannibalizeYN~ IntendedFAttacksNotFatalRate
            , family = 'binomial'
            , data = MY_TABLE_Videos)) 

summary(MY_TABLE_Videos$NbIntendedFAttacksNotFatal)

hist(MY_TABLE_Videos$NbIntendedFAttacksNotFatal)


summary(glm(CannibalizeYN~ IntendedFAttacksNotFatalYN
            , family = 'binomial'
            , data = MY_TABLE_Videos)) 


