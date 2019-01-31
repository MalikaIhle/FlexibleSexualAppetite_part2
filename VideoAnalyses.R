#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Video analyses FlexibleSexualAppetite part 2
#	 Start : 31 Jan 2019
#	 last modif : 
#	 commit: get the descriptive comparison between unmanipulated and painted males for JoVE paper
# !!!!! students need to correct courtships where FResp is empty
# !!!!!!!! students need to correct wrong chronology in courtship time start and end !
# ! should also check whether they updated timestopped watching if had initially ticked in progress (check if courtships or attacks occured after time stopped watching)
# ! need to check for double copulations
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
  
conDB= odbcConnectAccess2007("VideoAnalyses_MaleTests_2018_BackEnd.accdb")
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

# remove courtships watched after copulation (onyl few videos were watched longer initially)
Behav_Video_MaleTest <- merge(Behav_Video_MaleTest,TimeEND[,c('VideoID','TimeEnd')], all.x=TRUE)
Behav_Male_Courtships <- merge(Behav_Male_Courtships,TimeEND[,c('VideoID','TimeEnd')], all.x=TRUE)
TooLateCourtships <- Behav_Male_Courtships$CourtshipID[Behav_Male_Courtships$TimeEnd < Behav_Male_Courtships$CourtshipStart]
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

Behav_Male_Courtships[Behav_Male_Courtships$CourtshipStart>Behav_Male_Courtships$TimeEnd,c('VideoID', 'CourtshipID','CourtshipStart','TimeEnd')]

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

MY_TABLE <- Behav_Video_MaleTest[,c('VideoID','FID','MID','DelayLeaveDish','TotalWatch','VideoTimeStart')]
MY_TABLE <- arrange(MY_TABLE,VideoID)

MY_TABLE <- merge (x=MY_TABLE, y= FAttacks[,c('FirstFAttack', 'NbFAttacks', 'VideoID')],by='VideoID', all.x=TRUE)
MY_TABLE <- merge (x=MY_TABLE, y= TotalIntendedFAttacks[,c('NbIntendedFAttacks', 'VideoID')],by='VideoID', all.x=TRUE)
MY_TABLE <- merge (x=MY_TABLE, y= Cannibalism[,c('ConsumTime', 'VideoID')],by='VideoID', all.x=TRUE)
MY_TABLE <- merge (x=MY_TABLE, y= AllCourts[,c('NBCourt','TotalCourtDur', 'FirstCourt','VideoID')],by='VideoID', all.x=TRUE)
MY_TABLE <- merge (x=MY_TABLE, y= NaiveCourts[,c('NaiveNBCourt','NaiveTotalCourtDur', 'NaiveFirstCourt','VideoID')],by='VideoID', all.x=TRUE)

summary(MY_TABLE)
MY_TABLE <- MY_TABLE %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) # changes delay to leave dish, NbAttacks, NbCourt

MY_TABLE <- merge (x = MY_TABLE, y=  Behav_Female[,c('FID', 'CopulateYN', 'CopDuringVideo', 'CannibalizeYN', 'EatDuringVideo')],
                          by = 'FID', all.x=TRUE)  

## Convert first times into delay from time start video, in seconds

MY_TABLE$DelayFirstFAttack <- as.numeric(difftime(MY_TABLE$FirstFAttack,MY_TABLE$VideoTimeStart, units = 'secs'))
MY_TABLE$DelayConsum <- as.numeric(difftime(MY_TABLE$ConsumTime,MY_TABLE$VideoTimeStart, units = 'secs'))
MY_TABLE$DelayFirstCourt <- as.numeric(difftime(MY_TABLE$FirstCourt,MY_TABLE$VideoTimeStart, units = 'secs'))
MY_TABLE$DelayNaiveFirstCourt <- as.numeric(difftime(MY_TABLE$NaiveFirstCourt,MY_TABLE$VideoTimeStart, units = 'secs'))

### remove times
MY_TABLE <- subset(MY_TABLE, select = - c(VideoTimeStart,FirstFAttack,ConsumTime,FirstCourt,NaiveFirstCourt))

## add M and F Trt
head(Basic_Trials)
summary(Basic_Trials)
MTRT <- Basic_Trials[Basic_Trials$Sex == 1,c('Ind_ID','GroupName')]
colnames(MTRT) <- c('MID', 'MTrt')
FTRT <- Basic_Trials[Basic_Trials$Sex == 0,c('Ind_ID','GroupName')]
colnames(FTRT) <- c('FID', 'FTrt')

MY_TABLE <- merge(MY_TABLE,MTRT, all.x=TRUE)
MY_TABLE <- merge(MY_TABLE,FTRT, all.x=TRUE)

summary(MY_TABLE)

}

head(MY_TABLE)


#~~~ ANALYSES

{## Preregistered
### comparison delay to court for both type of male in the valid tests (not excluded because one of the three spiders died for other reason than cannibalism)

modDelayCourt <- lm(DelayFirstCourt ~ relevel(MTrt,ref= "Unmanipulated") , data = MY_TABLE)
summary(modDelayCourt) # none of the painted categories are different from the unmanipulated. effect opposite expectation (unmanip take longer to court)
}

{## Exploratory
### are painted males behaving differently than unmanipulated males, or are they receiving more attacks from the female ?

modDelayNaiveCourtAllVideo <- lm(DelayNaiveFirstCourt ~ relevel(MTrt,ref= "Unmanipulated"), data = MY_TABLE)
summary(modDelayNaiveCourtAllVideo)# 


modDelayLeaveDish <- lm(DelayLeaveDish~ relevel(MTrt,ref= "Unmanipulated") ,data = MY_TABLE)
summary(modDelayLeaveDish)#


modNbFAttacks <- lm(NbFAttacks~ relevel(MTrt,ref= "Unmanipulated") ,data = MY_TABLE)
summary(modNbFAttacks)#

 
modTotalCourtDur <- lm(TotalCourtDur~ relevel(MTrt,ref= "Unmanipulated") ,data = MY_TABLE)
summary(modTotalCourtDur)#


modNaiveTotalCourtDur <- lm(NaiveTotalCourtDur~relevel(MTrt,ref= "Unmanipulated"), data = MY_TABLE)
summary(modNaiveTotalCourtDur)# 

}

{## Descriptive

{### Nb of males who didn't court
summary(MY_TABLE$NBCourt)
length(MY_TABLE$NBCourt[MY_TABLE$NBCourt == 0])/
  length(MY_TABLE$NBCourt)*100 # 
}

{### Nb of males who didn't court BUT not because they were eaten 
summary(MY_TABLE$NBCourt[MY_TABLE$EatDuringVideo == 1])
length(MY_TABLE$NBCourt[MY_TABLE$EatDuringVideo == 1])/
  length(MY_TABLE$NBCourt[MY_TABLE$EatDuringVideo == 0])*100 # 
MY_TABLE$MTrt[MY_TABLE$NBCourt == 0 & MY_TABLE$EatDuringVideo == 0]

}

{### Nb of videos where female attacked (or at least intended to)
summary(MY_TABLE$NbIntendedFAttacks)
length(MY_TABLE$NbIntendedFAttacks[MY_TABLE$NbIntendedFAttacks > 0])/
  length(MY_TABLE$NbIntendedFAttacks)*100 #

} 

### average time watched
summary(MY_TABLE$TotalWatch)/60 # in min

### average delay to court
summary(MY_TABLE$DelayFirstCourt)/60 # in min

### average duration courting (out of duration watched)
summary(60*(MY_TABLE$TotalCourtDur/60)/(MY_TABLE$TotalWatch/60)) # in min of coursthip per hour

### Nb of courtships that lead to female attack
summary(Behav_Male_Courtships$FemaleResponse)
length(Behav_Male_Courtships$FemaleResponse[Behav_Male_Courtships$FemaleResponse == -1])/nrow(Behav_Male_Courtships)*100 #



}





