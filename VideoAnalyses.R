#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Video analyses FlexibleSexualAppetite part 2
#	 Start : 31 Jan 2019
#	 last modif : 
#	 commit: get the descriptive comparison between unmanipulated and painted males for JoVE paper
# !!!!! students need to correct courtships where FResp is empty
# !!!!!!!! students need to correct wrong chronology in courtship time start and end !
# # ! need to check for double copulations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
  ## missing videos: FID = 111, 233, 149, 497
  # 111, 233 and 149 are entirely missing
  # while 497 has the first 1.5 hours corrupted and the last 30 min watched. 
  # 497 live recording indicates that copulation occured in part 1.
  # 497 video should be excluded as we did not watch after copulation for the other videos.
  
  ## videos with 2 copulations
  # FID = 553, videoID = 150
  
  ## Females with 2 copulations (one on video, one after video)
  # FID 524
  
  
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
  # then we quickly stopped watching videos after copulation occured
  # overall time stopped watching is either the time of attack if she consumed it, the time start of the copulation, 
  # or the time at which we stopped watching (after about 2 hours) if none of those two events occured
  
  
}

rm(list = ls(all = TRUE))

{# packages
  library(lme4)
  library(RODBC)
  library(stringr)
  library(dplyr)
}


#~~~ GET DATASET

{# load data
  
conDB= odbcConnectAccess2007("RawData/VideoAnalyses_MaleTests_2018_BackEnd.accdb")
sqlTables(conDB)

Basic_Trials <- sqlFetch(conDB, 'Basic_Trials')

Behav_Video_MaleTest <- sqlFetch(conDB, 'Behav_Video_MaleTest')
Behav_Female <- sqlFetch(conDB, 'Behav_Female')
Behav_Female_Attacks <- sqlFetch(conDB, 'Behav_Female_Attacks')
Behav_Male_Courtships <- sqlFetch(conDB, 'Behav_Male_Courtships')

close(conDB)

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


### added !is.na for FemaleResp because data not cleaned yet and cells not all filled (students need to correct these)
# extract time of cannibalism and time of copulation, and compare to time stopped watching. see top script remarks for rules.
TimeEND <- merge(Behav_Video_MaleTest[,c('VideoID','TimeStoppedWatching')],Behav_Female_Attacks[Behav_Female_Attacks$AttackType == 3,c('VideoID','AttackTime')], all.x=TRUE)
TimeEND <- merge(TimeEND,Behav_Male_Courtships[!is.na(Behav_Male_Courtships$FemaleResp) & Behav_Male_Courtships$FemaleResp == 1,c('VideoID','TimeStartCop')], all.x=TRUE)

for (i in 1:nrow(TimeEND)){
  if(is.na(TimeEND$AttackTime[i]) & is.na(TimeEND$TimeStartCop[i]))
  {TimeEND$TimeEnd[i] <- as.character(TimeEND$TimeStoppedWatching[i])}
  if(!is.na(TimeEND$AttackTime[i]) &  is.na(TimeEND$TimeStartCop[i]))
  {TimeEND$TimeEnd[i] <- as.character(TimeEND$AttackTime[i])}
  if(is.na(TimeEND$AttackTime[i]) &  !is.na(TimeEND$TimeStartCop[i]))
  {TimeEND$TimeEnd[i] <- as.character(TimeEND$TimeStartCop[i])}
}

TimeEND$TimeEnd <- as.POSIXct(TimeEND$TimeEnd)

# remove courtships watched after copulation (only few videos were watched longer initially)
Behav_Video_MaleTest <- merge(Behav_Video_MaleTest,TimeEND[,c('VideoID','TimeEnd')], all.x=TRUE)
Behav_Male_Courtships <- merge(Behav_Male_Courtships,TimeEND[,c('VideoID','TimeEnd')], all.x=TRUE)
TooLateCourtships <- Behav_Male_Courtships$CourtshipID[Behav_Male_Courtships$TimeEnd < Behav_Male_Courtships$CourtshipStart]
      
      # to check
      # x <- merge(
      #   Behav_Male_Courtships[Behav_Male_Courtships$CourtshipStart>Behav_Male_Courtships$TimeEnd,c('VideoID', 'CourtshipID','CourtshipStart','TimeEnd')],
      #   Behav_Video_MaleTest[,c("VideoID", "FID")], all.x=TRUE)
      # y <- merge(x, Behav_Female[,c("FID",'CopDuringVideo','EatDuringVideo')], all.x=TRUE)
      # head(y)
      # y[y$CopDuringVideo == 0 & y$EatDuringVideo==0,]

Behav_Male_Courtships <- Behav_Male_Courtships[!Behav_Male_Courtships$CourtshipID%in%TooLateCourtships,]

# calculate delays or durations
Behav_Video_MaleTest$DelayLeaveDish <- as.numeric(difftime(Behav_Video_MaleTest$TimeLeaveContainer, Behav_Video_MaleTest$VideoTimeStart, units='secs'))
Behav_Video_MaleTest$TotalWatch <- as.numeric(difftime(Behav_Video_MaleTest$TimeEnd, Behav_Video_MaleTest$VideoTimeStart, units='secs'))
Behav_Video_MaleTest$DelayLeaveDish[Behav_Video_MaleTest$DelayLeaveDish < 0] <- 0
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
### all instances of female aggression (even missed attackes)
TotalIntendedFAttacks <- Behav_Female_Attacks %>% 
  group_by(VideoID) %>% 
  summarize(NbIntendedFAttacks = n())  

### consum male during video
Cannibalism <- Behav_Female_Attacks[Behav_Female_Attacks$AttackType == 3,c('VideoID', 'AttackTime')]
colnames(Cannibalism) <- c('VideoID','ConsumTime')

}
  
{### all courtships watched between time start and time consume or time copulation or time stop watching video if none of those occured
  ###### i.e. need to remove courtships watched after copulation
  ###### !FID 553 copulated twice
  
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

summary(MY_TABLE_Videos)
MY_TABLE_Videos <- MY_TABLE_Videos %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) # changes delay to leave dish, NbAttacks, NbCourt

MY_TABLE_Videos <- merge (x = MY_TABLE_Videos, y=  Behav_Female[,c('FID','TrialDate', 'CopulateYN', 'CopDuringVideo', 'CannibalizeYN', 'EatDuringVideo')],
                          by = 'FID', all.x=TRUE)  

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
MTRT <- Basic_Trials[Basic_Trials$Sex == 1,c('Ind_ID','GroupName')]
colnames(MTRT) <- c('MID', 'MTrt')
FTRT <- Basic_Trials[Basic_Trials$Sex == 0,c('Ind_ID','GroupName')]
colnames(FTRT) <- c('FID', 'FTrt')

MY_TABLE_Videos <- merge(MY_TABLE_Videos,MTRT, all.x=TRUE)
MY_TABLE_Videos <- merge(MY_TABLE_Videos,FTRT, all.x=TRUE)

summary(MY_TABLE_Videos)

}

head(MY_TABLE_Videos)


#~~~ ANALYSES

## For JoVE paper, need to compare the 14 unmanipulated males to males tested at the same time 
## (i.e. at the end of the season - which may be a specific subset of males)

DatesTrailsUnmanipulated <- MY_TABLE_Videos$TrialDate[MY_TABLE_Videos$MTrt == "Unmanipulated"]
summary((MY_TABLE_Videos[MY_TABLE_Videos$TrialDate > min(DatesTrailsUnmanipulated),]))
summary(DatesTrailsUnmanipulated)
Unmanip <- Basic_Trials$Ind_ID[Basic_Trials$GroupName == "Unmanipulated"]
Unmanip[!Unmanip%in%MY_TABLE_Videos$MID[MY_TABLE_Videos$MTrt == "Unmanipulated"]]


{## Preregistered
### comparison delay to court for both type of male in the valid tests (not excluded because one of the three spiders died for other reason than cannibalism)

modDelayCourt <- lm(DelayFirstCourt ~ relevel(MTrt,ref= "Unmanipulated") , data = MY_TABLE_Videos)
summary(modDelayCourt) # none of the painted categories are different from the unmanipulated. effect opposite expectation (unmanip take longer to court)
}

{## Exploratory
### are painted males behaving differently than unmanipulated males, or are they receiving more attacks from the female ?

modDelayNaiveCourtAllVideo <- lm(DelayNaiveFirstCourt ~ relevel(MTrt,ref= "Unmanipulated"), data = MY_TABLE_Videos)
summary(modDelayNaiveCourtAllVideo)# 


modDelayLeaveDish <- lm(DelayLeaveDish~ relevel(MTrt,ref= "Unmanipulated") ,data = MY_TABLE_Videos)
summary(modDelayLeaveDish)#


modNbFAttacks <- lm(NbFAttacks~ relevel(MTrt,ref= "Unmanipulated") ,data = MY_TABLE_Videos)
summary(modNbFAttacks)#

 
modTotalCourtDur <- lm(TotalCourtDur~ relevel(MTrt,ref= "Unmanipulated") ,data = MY_TABLE_Videos)
summary(modTotalCourtDur)#


modNaiveTotalCourtDur <- lm(NaiveTotalCourtDur~relevel(MTrt,ref= "Unmanipulated"), data = MY_TABLE_Videos)
summary(modNaiveTotalCourtDur)# 

}

{## Descriptive

{### Nb of males who didn't court
summary(MY_TABLE_Videos$NBCourt)
length(MY_TABLE_Videos$NBCourt[MY_TABLE_Videos$NBCourt == 0])/
  length(MY_TABLE_Videos$NBCourt)*100 # 
}

{### Nb of males who didn't court BUT not because they were eaten 
summary(MY_TABLE_Videos$NBCourt[MY_TABLE_Videos$EatDuringVideo == 1])
length(MY_TABLE_Videos$NBCourt[MY_TABLE_Videos$EatDuringVideo == 1])/
  length(MY_TABLE_Videos$NBCourt[MY_TABLE_Videos$EatDuringVideo == 0])*100 # 
MY_TABLE_Videos$MTrt[MY_TABLE_Videos$NBCourt == 0 & MY_TABLE_Videos$EatDuringVideo == 0]

}

{### Nb of videos where female attacked (or at least intended to)
summary(MY_TABLE_Videos$NbIntendedFAttacks)
length(MY_TABLE_Videos$NbIntendedFAttacks[MY_TABLE_Videos$NbIntendedFAttacks > 0])/
  length(MY_TABLE_Videos$NbIntendedFAttacks)*100 #

} 

### average time watched
summary(MY_TABLE_Videos$TotalWatch)/60 # in min

### average delay to court
summary(MY_TABLE_Videos$DelayFirstCourt)/60 # in min

### average duration courting (out of duration watched)
summary(60*(MY_TABLE_Videos$TotalCourtDur/60)/(MY_TABLE_Videos$TotalWatch/60)) # in min of coursthip per hour

### Nb of courtships that lead to female attack
summary(Behav_Male_Courtships$FemaleResponse)
length(Behav_Male_Courtships$FemaleResponse[Behav_Male_Courtships$FemaleResponse == -1])/nrow(Behav_Male_Courtships)*100 #



}





