#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data handling FlexibleSexualAppetite part 2
#	 Start : 04/25/2019 - it's my bday!
#	 last modif : 04/25/2019
#	 commit: data handling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
 
}


rm(list = ls(all = TRUE))


{# packages
  library(RODBC) # this require R AND ACCESS to run on 32 bits !
  library(here)
}

{# load data
  
  conDB= odbcConnectAccess2007(paste(here(),"1_RawData/HabronatusPyrrithrix_DB.accdb", sep="/"))
  
  Females <- sqlQuery(conDB, "
                      
SELECT Basic_Individuals.Ind_ID AS FID, Basic_Trials.GroupName AS FTrt, Behav_Female.TrialDate, Behav_Female.CopulateYN, Behav_Female.CopDuringVideo, Behav_Female.CannibalizeYN, Behav_Female.EatDuringVideo, Behav_Female.CannibalismTime, Behav_Female.CannibalismDate, Behav_Female.Remarks AS TestRemarks
FROM (Basic_Individuals LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID) LEFT JOIN Behav_Female ON Basic_Individuals.Ind_ID = Behav_Female.FID
WHERE (((Basic_Individuals.Sex)=0) AND ((Basic_Trials.Experiment)='VirginMateChoice') AND ((Behav_Female.TestName)='Male'));
                       
                      ")
  
  summary(Females)
  nrow(Females)
  
Males <- sqlQuery(conDB, "
  
  SELECT Basic_Individuals.Ind_ID AS MID, Basic_Trials.GroupName AS MTrt, Basic_Trials.GroupNumber AS FID, Basic_Trials.Remarks AS MalePaintingRemarks
  FROM Basic_Individuals LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID
  WHERE (((Basic_Individuals.Sex)=1) AND ((Basic_Trials.Experiment)='VirginMateChoice') AND ((Basic_Trials.GroupNumber) Is Not Null))
  
  ")

summary(Males)
nrow(Males)

Fitness <- sqlQuery(conDB, "

SELECT Behav_Female.FID, Behav_Female.TrialDateEnd, Breed_Clutches.EmergenceDate, Breed_Clutches.BroodSize, [Breed_Clutches]![LayDate]-[Behav_Female]![TrialDateEnd] AS DelaytoLay, Breed_Clutches.Remarks
FROM Basic_Individuals LEFT JOIN (Behav_Female LEFT JOIN Breed_Clutches ON Behav_Female.FID = Breed_Clutches.FID) ON Basic_Individuals.Ind_ID = Behav_Female.FID
WHERE (((Behav_Female.FID)>18000) AND ((Behav_Female.TestName)='Male') AND ((Breed_Clutches.ClutchNo) Is Null Or (Breed_Clutches.ClutchNo)=1))
ORDER BY Behav_Female.FID

")

summary(Fitness)
nrow(Fitness)

  
  close(conDB)
}


Females <- merge(Females, Males[Males$MTrt != "Companion",], by="FID")
nrow(Females)
summary(Females)

