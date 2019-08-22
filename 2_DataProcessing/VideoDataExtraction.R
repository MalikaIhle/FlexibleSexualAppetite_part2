#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Video data extraction FlexibleSexualAppetite part 2
#	 Start : 20 Aug 2019
#	 last modif : 20 Aug 2019
#	 commit: import all videos watched, correct data and create summary statistic into one table
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
  Behav_Male_Courtships$CopDuration <- as.numeric(difftime(Behav_Male_Courtships$TimeEndCop,Behav_Male_Courtships$TimeStartCop, units = 'secs'))
  
  
  {# data correction for chronology in Behav_Courtships
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
    DisorderStart[DisorderStart$pbVidID>0,] # video 160, 170, 189 chronologgy disrupted but verified and validated, video 164 possibly due to other corrections 
    
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
    DisorderEnd[DisorderEnd$pbVidID>0,] # video 160, 170, 189 chronologgy disrupted but verified and validated, video 164 possibly due to other corrections 
    
  }
   
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
    
    ### all instances of female aggression (even missed attacks) without the fatal attack
    TotalIntendedFAttacksNotFatal <- Behav_Female_Attacks[Behav_Female_Attacks$AttackType == 1 | Behav_Female_Attacks$AttackType == 2,] %>% 
      group_by(VideoID) %>% 
      summarize(NbIntendedFAttacksNotFatal = n())     
    
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
                FirstCourt = min(CourtshipStart),
                CopDur = sum(CopDuration, na.rm = TRUE))
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
  
  ### Nb of courtships that lead to female attack
  summary(Behav_Male_Courtships$FemaleResponse)
  length(Behav_Male_Courtships$FemaleResponse[Behav_Male_Courtships$FemaleResponse == -1])/nrow(Behav_Male_Courtships)*100 #5.6%
  
  
  
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
  MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= TotalIntendedFAttacksNotFatal[,c('NbIntendedFAttacksNotFatal', 'VideoID')],by='VideoID', all.x=TRUE)
  MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= Cannibalism[,c('ConsumTime', 'VideoID')],by='VideoID', all.x=TRUE)
  MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= AllCourts[,c('NBCourt','TotalCourtDur', 'FirstCourt','CopDur', 'VideoID')],by='VideoID', all.x=TRUE)
  MY_TABLE_Videos <- merge (x=MY_TABLE_Videos, y= NaiveCourts[,c('NaiveNBCourt','NaiveTotalCourtDur', 'NaiveFirstCourt','VideoID')],by='VideoID', all.x=TRUE)
  
  summary(MY_TABLE_Videos) # 5 never left dish, 4 were eaten prior to ever court, 16 were attacked prior to ever court
  MY_TABLE_Videos$NbFAttacks[is.na(MY_TABLE_Videos$NbFAttacks)] <- 0
  MY_TABLE_Videos$NbIntendedFAttacks[is.na(MY_TABLE_Videos$NbIntendedFAttacks)] <- 0
  MY_TABLE_Videos$NbIntendedFAttacksNotFatal[is.na(MY_TABLE_Videos$NbIntendedFAttacksNotFatal)] <- 0
  
  
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

# write.csv(MY_TABLE_Videos, file = "3_ExtractedData/MY_TABLE_Videos.csv", row.names = FALSE)
# 20190820 first, add CopDur
# 20190822 add NbIntendedFAttacksNotFatal (to be able to correlate attacks with cannibalism excluding the fatal attack)
