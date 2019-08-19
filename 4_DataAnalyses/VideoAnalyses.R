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

rm(list = ls(all = TRUE))

{# packages
  library(lme4)
  library(RODBC)
  library(stringr)
  library(dplyr)
  library(here)
}


#~~~ GET DATASET

{# load data
  
  MY_TABLE_all <- read.csv(file = paste(here(),"3_ExtractedData/MY_TABLE.csv", sep="/"), header=TRUE, sep=",") # with 20 unmanipulated male tests
  summary(MY_TABLE_all)
  
  MY_TABLE <- MY_TABLE_all[MY_TABLE_all$MTrt != "Unmanipulated",]
  summary(MY_TABLE)
  
  conDB1= odbcConnectAccess2007(paste(here(),"1_RawData/HabronatusPyrrithrix_DB.accdb", sep="/"))
  sqlTables(conDB1)
  Basic_Trials <- sqlFetch(conDB1, 'Basic_Trials')
  Behav_Female <- sqlFetch(conDB1, 'Behav_Female')
  close(conDB1)
  
conDB= odbcConnectAccess2007("1_RawData/VideoAnalyses_MaleTests_2018_BackEnd.accdb")
sqlTables(conDB)


Behav_Video_MaleTest <- sqlFetch(conDB, 'Behav_Video_MaleTest')
Behav_Female_Attacks <- sqlFetch(conDB, 'Behav_Female_Attacks')
Behav_Male_Courtships <- sqlFetch(conDB, 'Behav_Male_Courtships')

close(conDB)

## exclude two videos corrupeted and half watch (see remarks above)

Behav_Video_MaleTest <- subset(Behav_Video_MaleTest,VideoID != 175 & VideoID != 264)
Behav_Female_Attacks <- subset(Behav_Female_Attacks,VideoID != 175 & VideoID != 264)
Behav_Male_Courtships <- subset(Behav_Male_Courtships,VideoID != 175 & VideoID != 264)


}

{# functions

## read text column into time

ConvertToTime <- function(x){
  as.POSIXct(str_pad(x, 6, pad = "0"), format="%H%M%S") # this adds the date of today to every time... stupid but hard to get around and not important
}
  
ConvertTimeToSecs <- function(x){difftime(x, as.POSIXct(as.character('000000'), format="%H%M%S"), unit='secs')}

}

{# convert times, find TimeEnd, and calculate delays

Behav_Video_MaleTest <- mutate_at(Behav_Video_MaleTest,c('VideoTimeStart',
                                                         'TimeLeaveContainer',
                                                         'TimeStoppedWatching') ,ConvertToTime)


Behav_Female_Attacks <- mutate_at(Behav_Female_Attacks,'AttackTime',ConvertToTime)

Behav_Male_Courtships <- mutate_at(Behav_Male_Courtships,c('CourtshipStart',
                                                         'CourtshipEnd', 'TimeStartCop','TimeEndCop'),ConvertToTime)


# extract time of cannibalism and time of copulation, and compare to time stopped watching. see top script remarks for rules.
TimeEND <- merge(Behav_Video_MaleTest[,c('VideoID','TimeStoppedWatching')],Behav_Female_Attacks[Behav_Female_Attacks$AttackType == 3,c('VideoID','AttackTime')], all.x=TRUE)
Behav_Male_Courtships[Behav_Male_Courtships$VideoID == 150,]# FID 553 has two copulation during the video, we will consider until the second one
TimeEND <- merge(TimeEND,
                 rbind(Behav_Male_Courtships[Behav_Male_Courtships$FemaleResp == 1 & Behav_Male_Courtships$VideoID != 150 
                                             ,c('VideoID','TimeStartCop')],
                       Behav_Male_Courtships[Behav_Male_Courtships$CourtshipID == 3865,c('VideoID','TimeStartCop')])
                      , by = 'VideoID', all.x=TRUE)

for (i in 1:nrow(TimeEND)){
  if(is.na(TimeEND$AttackTime[i]) & is.na(TimeEND$TimeStartCop[i]))# if no cannibalism no copulation, we watched until 2h or video stopped
  {TimeEND$TimeEnd[i] <- as.character(TimeEND$TimeStoppedWatching[i])}
  if(!is.na(TimeEND$AttackTime[i]) &  is.na(TimeEND$TimeStartCop[i]))# if cannibalized (but no cop) we stopped watching then 
  {TimeEND$TimeEnd[i] <- as.character(TimeEND$AttackTime[i])}
  if(is.na(TimeEND$AttackTime[i]) &  !is.na(TimeEND$TimeStartCop[i]))# if copulated (but no cannibalism) we stopped watchign then (apart from first videos watched)
  {TimeEND$TimeEnd[i] <- as.character(TimeEND$TimeStartCop[i])}
  if(!is.na(TimeEND$AttackTime[i]) &  !is.na(TimeEND$TimeStartCop[i]))# if cannibalism occured after copulation, we stopped watching for courthsips at the copulation event
  {TimeEND$TimeEnd[i] <- as.character(TimeEND$TimeStartCop[i])}
}

TimeEND$TimeEnd <- as.POSIXct(TimeEND$TimeEnd)

# remove courtships watched after copulation (only few videos were watched longer initially)
Behav_Video_MaleTest <- merge(Behav_Video_MaleTest,TimeEND[,c('VideoID','TimeEnd')], all.x=TRUE)
Behav_Male_Courtships <- merge(Behav_Male_Courtships,TimeEND[,c('VideoID','TimeEnd')], all.x=TRUE)
TooLateCourtships <- Behav_Male_Courtships$CourtshipID[Behav_Male_Courtships$TimeEnd < Behav_Male_Courtships$CourtshipStart]
Behav_Male_Courtships <- Behav_Male_Courtships[!Behav_Male_Courtships$CourtshipID%in%TooLateCourtships,]

# calculate delays or durations
Behav_Video_MaleTest$DelayLeaveDish <- as.numeric(difftime(Behav_Video_MaleTest$TimeLeaveContainer, Behav_Video_MaleTest$VideoTimeStart, units='secs'))
Behav_Video_MaleTest$TotalWatch <- as.numeric(difftime(Behav_Video_MaleTest$TimeEnd, Behav_Video_MaleTest$VideoTimeStart, units='secs'))
Behav_Video_MaleTest$DelayLeaveDish[Behav_Video_MaleTest$DelayLeaveDish < 0] <- 0 # 5 NA = male never left dish (verified)
Behav_Male_Courtships$CourtDuration <- as.numeric(difftime(Behav_Male_Courtships$CourtshipEnd,Behav_Male_Courtships$CourtshipStart, units = 'secs'))

}

head(Behav_Video_MaleTest)
head(Behav_Female)
head(Behav_Female_Attacks)
head(Behav_Male_Courtships)

{# summarize type of interactions

{## female attacks 
### real attacks or consum
FAttacks <- Behav_Female_Attacks[Behav_Female_Attacks$AttackType == 2 | Behav_Female_Attacks$AttackType == 3,] %>% 
  group_by(VideoID) %>% 
  summarize(FirstFAttack = min(AttackTime),
            NbFAttacks = n())    
### all instances of female aggression (even missed attacks)
TotalIntendedFAttacks <- Behav_Female_Attacks %>% 
  group_by(VideoID) %>% 
  summarize(NbIntendedFAttacks = n())  

### consum male during video
Cannibalism <- Behav_Female_Attacks[Behav_Female_Attacks$AttackType == 3,c('VideoID', 'AttackTime')]
colnames(Cannibalism) <- c('VideoID','ConsumTime')

}
  
{### all courtships watched between time start and time consume or time copulation or time stop watching video if none of those occured
  ###### i.e. need to remove courtships watched after copulation (done above)
  ###### !FID 553 copulated twice (considered courtships up to the second copulation, done above)
  
AllCourts <- Behav_Male_Courtships %>%
  group_by(VideoID) %>% 
  summarize(NBCourt = n(),
            TotalCourtDur = sum(CourtDuration),
            FirstCourt = min(CourtshipStart))

}
  
{## keep only courtships before attacks from female
Behav_Male_Courtships <- merge(x=Behav_Male_Courtships,y=as.data.frame(FAttacks[c('VideoID','FirstFAttack')]), by="VideoID", all.x=TRUE)
AfterAttackCourtshipID <- Behav_Male_Courtships$CourtshipID[Behav_Male_Courtships$CourtshipStart > Behav_Male_Courtships$FirstFAttack & !is.na(Behav_Male_Courtships$FirstFAttack)]
NaiveBehav_Male_Courtships <- Behav_Male_Courtships[!Behav_Male_Courtships$CourtshipID %in% AfterAttackCourtshipID,]    
    
NaiveCourts <- NaiveBehav_Male_Courtships %>%
  group_by(VideoID) %>% 
  summarize(NaiveNBCourt = n(),
            NaiveTotalCourtDur = sum(CourtDuration),
            NaiveFirstCourt = min(CourtshipStart))

}
  
} 

FAttacks
TotalIntendedFAttacks
Cannibalism
AllCourts
NaiveCourts

{# Combine summaries

MY_TABLE_Videos <- Behav_Video_MaleTest[,c('VideoID','FID','MID','DelayLeaveDish','TotalWatch','VideoTimeStart')]
MY_TABLE_Videos <- arrange(MY_TABLE_Videos,VideoID)

MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= FAttacks[,c('FirstFAttack', 'NbFAttacks', 'VideoID')],by='VideoID', all.x=TRUE)
MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= TotalIntendedFAttacks[,c('NbIntendedFAttacks', 'VideoID')],by='VideoID', all.x=TRUE)
MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= Cannibalism[,c('ConsumTime', 'VideoID')],by='VideoID', all.x=TRUE)
MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= AllCourts[,c('NBCourt','TotalCourtDur', 'FirstCourt','VideoID')],by='VideoID', all.x=TRUE)
MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= NaiveCourts[,c('NaiveNBCourt','NaiveTotalCourtDur', 'NaiveFirstCourt','VideoID')],by='VideoID', all.x=TRUE)

summary(MY_TABLE_Videos) # 5 never left dish, 4 were eaten prior to ever court, 16 were attacked prior to ever court
MY_TABLE_Videos$NbFAttacks[is.na(MY_TABLE_Videos$NbFAttacks)] <- 0
MY_TABLE_Videos$NbIntendedFAttacks[is.na(MY_TABLE_Videos$NbIntendedFAttacks)] <- 0


MY_TABLE_Videos <- merge (x = MY_TABLE_Videos, y=  Behav_Female[Behav_Female$TestName == 'Male' & Behav_Female$FID > 18000
                                                                  ,c('FID','TrialDate', 'CopulateYN', 'CopDuringVideo', 'CannibalizeYN', 'EatDuringVideo')],
                          by = 'FID', all.x=TRUE)  # 241 FID - 4 videos missing = 237 videos

## Convert first times into delay from time start video, in seconds

MY_TABLE_Videos$DelayFirstFAttack <- as.numeric(difftime(MY_TABLE_Videos$FirstFAttack,MY_TABLE_Videos$VideoTimeStart, units = 'secs'))
MY_TABLE_Videos$DelayConsum <- as.numeric(difftime(MY_TABLE_Videos$ConsumTime,MY_TABLE_Videos$VideoTimeStart, units = 'secs'))
MY_TABLE_Videos$DelayFirstCourt <- as.numeric(difftime(MY_TABLE_Videos$FirstCourt,MY_TABLE_Videos$VideoTimeStart, units = 'secs'))
MY_TABLE_Videos$DelayNaiveFirstCourt <- as.numeric(difftime(MY_TABLE_Videos$NaiveFirstCourt,MY_TABLE_Videos$VideoTimeStart, units = 'secs'))

### remove times
MY_TABLE_Videos <- subset(MY_TABLE_Videos, select = - c(VideoTimeStart,FirstFAttack,ConsumTime,FirstCourt,NaiveFirstCourt))

## add M and F Trt
head(Basic_Trials)
summary(Basic_Trials)
MTRT <- Basic_Trials[Basic_Trials$Sex == 1 & Basic_Trials$Experiment == 'VirginMateChoice' & Basic_Trials$GroupName != 'Companion' & !is.na(Basic_Trials$GroupNumber)
                       ,c('Ind_ID','GroupName')] # remove companion male and painted male that were not allocated to female because of missed painting
colnames(MTRT) <- c('MID', 'MTrt')
FTRT <- Basic_Trials[Basic_Trials$Sex == 0  & Basic_Trials$Experiment == 'VirginMateChoice' 
                     ,c('Ind_ID','GroupName')] # this include all female trained even those that did not end up tested because killed after 3 months training without maturing
colnames(FTRT) <- c('FID', 'FTrt')

MY_TABLE_Videos <- merge(MY_TABLE_Videos,MTRT, all.x=TRUE)
MY_TABLE_Videos <- merge(MY_TABLE_Videos,FTRT, all.x=TRUE)

summary(MY_TABLE_Videos) # 237 videos

}

head(MY_TABLE_Videos)


#~~~ ANALYSES

{# JoVE
## For JoVE paper, need to compare the here 19 (for jove only had 14 watched at the time) unmanipulated males to males tested at the same time (20 unmanipulated males were tested but one video is missing)
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

modDelayLeaveDish_End <- lm(DelayLeaveDish~ MpaintedYN, data = MY_TABLE_Videos_End)
summary(modDelayLeaveDish_End)#
summary(MY_TABLE_Videos_End$DelayLeaveDish) # they all excited the vials

modDelayCourt_End <- lm(DelayFirstCourt ~ MpaintedYN, data = MY_TABLE_Videos_End)
summary(modDelayCourt_End) # 
summary(MY_TABLE_Videos_End$DelayFirstCourt) # one (allgrey) was cannibalized before courting

modTotalCourtDur_End <- lm(TotalCourtDur~ MpaintedYN ,data = MY_TABLE_Videos_End)
summary(modTotalCourtDur_End)#
summary(MY_TABLE_Videos_End$TotalCourtDur) # one (allgrey) was cannibalized before courting

modNaiveTotalCourtDur_End <- lm(NaiveTotalCourtDur~ MpaintedYN, data = MY_TABLE_Videos_End)
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



{## Descriptive

{### Nb of males who didn't court
summary(MY_TABLE_Videos$NBCourt) # all males courted but the 4 that were eaten prior to starting to court
nrow(MY_TABLE_Videos) # 237
  }


{### Nb of videos where female attacked (or at least intended to)
summary(MY_TABLE_Videos$NbIntendedFAttacks)
length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$NbIntendedFAttacks > 0])/
  nrow(MY_TABLE_Videos)*100 # 74.2%

} 

### average time watched
summary(MY_TABLE_Videos$TotalWatch)/60 # 68.1 min

### average delay to court
summary(MY_TABLE_Videos$DelayFirstCourt)/60 # 7.57 min

### average duration courting (out of duration watched)
MY_TABLE_Videos$CourtshipRate <- (MY_TABLE_Videos$TotalCourtDur/60)/(MY_TABLE_Videos$TotalWatch/60/60)
summary(MY_TABLE_Videos$CourtshipRate) # 29.5 min of coursthip per hour (!! videos may have stopped after 12 min if copulation occured so max isnt really 59 min per hour !!)
summary(MY_TABLE_Videos$TotalWatch/60) # between 0.6 and 133.5 min watched (the lowest being when attacked immediatly, then the econd lozest are when copulate very quickly)
MY_TABLE_Videos[MY_TABLE_Videos$CourtshipRate > 50 & !is.na(MY_TABLE_Videos$CourtshipRate),]
MY_TABLE_Videos[MY_TABLE_Videos$TotalWatch/60 <1 ,]

### Nb of courtships that lead to female attack
summary(Behav_Male_Courtships$FemaleResponse)
length(Behav_Male_Courtships$FemaleResponse[Behav_Male_Courtships$FemaleResponse == -1])/nrow(Behav_Male_Courtships)*100 #5.6%

}


# data correction for chronology in Behav_Courtships
## chronology within a courtship already corrected within the DB, here I check logic chronology among courtships

Behav_Male_Courtships_perVidID <- split(Behav_Male_Courtships, Behav_Male_Courtships$VideoID)
#x <- Behav_Male_Courtships_perVidID[['150']]
#x <- Behav_Male_Courtships_perVidID[[2]]

Behav_Male_Courtships_perVidID_fun <- function(x){
  x <- x[order(x$CourtshipID),]

  if (nrow(x) == 1) {return(0)}
  
  if (nrow(x) > 1)
 {
    x$InOrder <- c(x$CourtshipStart[-nrow(x)] < x$CourtshipStart[-1], TRUE)
    
    if ((nrow(x[x$InOrder == FALSE,]))==0) {return(0)}
    if ((nrow(x[x$InOrder == FALSE,]))>0)
      {
         if (nrow(x[x$InOrder == FALSE,]) == 1){return(x$CourtshipID[x$InOrder == FALSE])}
         if (nrow(x[x$InOrder == FALSE,]) > 1) {return(9999999)}
        }
  }
  

}

DisorderStart <- data.frame(VideoID = unique(Behav_Male_Courtships$VideoID),
           pbVidID = unlist(lapply(Behav_Male_Courtships_perVidID,Behav_Male_Courtships_perVidID_fun)))
DisorderStart[DisorderStart$pbVidID>0,] # video 160 chronologgy disrupted but verified and validated, video 164, 170, 189 possibly due to other corrections 

Behav_Male_Courtships_perVidID_fun1 <- function(x){
  x <- x[order(x$CourtshipID),]
  
  if (nrow(x) == 1) {return(0)}
  
  if (nrow(x) > 1)
  {
    x$InOrder <- c(x$CourtshipEnd[-nrow(x)] <= x$CourtshipStart[-1], TRUE)
    
    if ((nrow(x[x$InOrder == FALSE,]))==0) {return(0)}
    if ((nrow(x[x$InOrder == FALSE,]))>0)
    {
      if (nrow(x[x$InOrder == FALSE,]) == 1){return(x$CourtshipID[x$InOrder == FALSE])}
      if (nrow(x[x$InOrder == FALSE,]) > 1) {return(9999999)}
    }
  }
  
  
}

DisorderEnd <- data.frame(VideoID = unique(Behav_Male_Courtships$VideoID),
                            pbVidID = unlist(lapply(Behav_Male_Courtships_perVidID,Behav_Male_Courtships_perVidID_fun1)))
DisorderEnd[DisorderEnd$pbVidID>0,] # 





{## Does copulation length explain number of spiderlings?

### FID with spiderlings when CopulateYN = No
nrow(MY_TABLE[MY_TABLE$BroodSize > 0 & MY_TABLE$CopulateYN == 0,]) # 20 females had spiderlings but were not seen copulated
nrow(MY_TABLE[MY_TABLE$BroodSize > 0 ,]) # out of 147 females that had spiderlings

### FID with CopulateYN = Yes and CopDuringVid = No
nrow(MY_TABLE[MY_TABLE$CopDuringVideo == 0 & MY_TABLE$CopulateYN == 1,]) # 9 - where copulation was seen live after the video
nrow(MY_TABLE) # out of 221 females observed

### FID with CopulateYN = Yes and spiderlings = No
nrow(MY_TABLE[MY_TABLE$BroodSize == 0 & MY_TABLE$CopulateYN == 1,]) # 11 females were seen copulated but had no spiderlings
nrow(MY_TABLE[MY_TABLE$CopulateYN == 1,]) # out of 138 females that were seen copulating

}

