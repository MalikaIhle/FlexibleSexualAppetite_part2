#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#	 Malika IHLE      malika_ihle@hotmail.fr
#	 Data handling FlexibleSexualAppetite part 2
#	 Start : 04/25/2019 - it's my bday!
#	 last modif : 04/25/2019
#	 commit: data handling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

{# Remarks
# FID 18401 has no weight (had a typo in the weight of both (before after) that could not be corrected)
# MID 18228 (paired with FID 18072) and MID 18390 (paired with FID 18478) have no weight
# companion males were IDed but not weighted (only measured at maturity like all spiders)
# FID 18417 disappeared before getting the opportunity to lay eggs - she was a female tested with an unmanipulated male
}


rm(list = ls(all = TRUE))


{# packages
  library(RODBC) # this require R AND ACCESS to run on 32 bits !
  library(here)
}

{# load data
  
  conDB= odbcConnectAccess2007(paste(here(),"1_RawData/HabronatusPyrrithrix_DB.accdb", sep="/"))
  
  Females <- sqlQuery(conDB, "
                      
SELECT Basic_Individuals.Ind_ID AS FID, Basic_Trials.GroupName AS FTrt, Basic_Trials.PeriodBeginDate, Behav_Female.TrialDate, Behav_Female.CopulateYN, Behav_Female.CopDuringVideo, Behav_Female.CannibalizeYN, Behav_Female.EatDuringVideo, Behav_Female.CannibalismTime, Behav_Female.CannibalismDate, Behav_Female.Remarks AS TestRemarks, Morph_Measurements.Mass AS FMass, Morph_Measurements_1.CarapaceWidth AS FCarapaceWidth
FROM (Basic_Individuals LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID) LEFT JOIN ((Behav_Female LEFT JOIN Morph_Measurements ON Behav_Female.FID = Morph_Measurements.Ind_ID) LEFT JOIN Morph_Measurements AS Morph_Measurements_1 ON Behav_Female.FID = Morph_Measurements_1.Ind_ID) ON Basic_Individuals.Ind_ID = Behav_Female.FID
                      WHERE (((Basic_Individuals.Sex)=0) AND ((Basic_Trials.Experiment)='VirginMateChoice') AND ((Behav_Female.TestName)='Male') AND ((Morph_Measurements.Occasion)='VirginMateChoice') AND ((Morph_Measurements_1.Occasion)='maturity'));
                      
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


PaintedMalesMeasurements <- sqlQuery(conDB, "

SELECT Mwidth.MID, Mwidth.MCarapaceWidth, MMass.MMass
FROM (
SELECT Basic_Individuals.Ind_ID AS MID, Morph_Measurements.CarapaceWidth AS MCarapaceWidth
                                     FROM (Basic_Individuals LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID) LEFT JOIN Morph_Measurements ON Basic_Trials.Ind_ID = Morph_Measurements.Ind_ID
                                     WHERE (((Basic_Trials.GroupName)<>'Companion') AND ((Basic_Trials.GroupNumber) Is Not Null) AND ((Basic_Individuals.Sex)=1) AND ((Basic_Trials.Experiment)='VirginMateChoice') AND ((Morph_Measurements.Occasion)='maturity'))
) AS Mwidth 

LEFT JOIN (
SELECT Basic_Trials.Ind_ID AS MID, Morph_Measurements.Mass AS MMass
                                     FROM (Basic_Individuals LEFT JOIN Basic_Trials ON Basic_Individuals.Ind_ID = Basic_Trials.Ind_ID) LEFT JOIN Morph_Measurements ON Basic_Trials.Ind_ID = Morph_Measurements.Ind_ID
                                     WHERE (((Basic_Trials.GroupName)<>'Companion') AND ((Basic_Individuals.Sex)=1) AND ((Basic_Trials.Experiment)='VirginMateChoice') AND ((Morph_Measurements.Occasion)='VirginMateChoice'))
) AS MMass 

ON Mwidth.MID = MMass.MID;

  ")


summary(PaintedMalesMeasurements)
nrow(PaintedMalesMeasurements)


Fitness <- sqlQuery(conDB, "

SELECT Behav_Female.FID, Behav_Female.TrialDateEnd, Breed_Clutches.EmergenceDate, Breed_Clutches.BroodSize, [Breed_Clutches]![LayDate]-[Behav_Female]![TrialDateEnd] AS DelaytoLay, Breed_Clutches.Remarks AS BroodRemarks
FROM Basic_Individuals LEFT JOIN (Behav_Female LEFT JOIN Breed_Clutches ON Behav_Female.FID = Breed_Clutches.FID) ON Basic_Individuals.Ind_ID = Behav_Female.FID
WHERE (((Behav_Female.FID)>18000) AND ((Behav_Female.TestName)='Male') AND ((Breed_Clutches.ClutchNo) Is Null Or (Breed_Clutches.ClutchNo)=1))
ORDER BY Behav_Female.FID

")


### Broodsize = 0 for those who didnt lay before december 2018 or laid infertile eggs (that did not lead to spiderlings)
### brood size = NA for 17417 who disappeared before getting the chance to lay

Fitness$BroodSize[is.na(Fitness$BroodSize)] <- 0
Fitness$BroodSize[Fitness$FID == 18417] <- NA

summary(Fitness)
nrow(Fitness)

  close(conDB)
}


{# combine and calculate data into MY_TABLE

CompanionMales <- Males[Males$MTrt == "Companion", c("MID","FID")]
colnames(CompanionMales) <- c('CompanionID', 'FID')

PaintedMales <- merge(Males[Males$MTrt != "Companion",], PaintedMalesMeasurements, by="MID")

MY_TABLE <- merge(Females, PaintedMales, by="FID")
MY_TABLE <- merge(MY_TABLE, CompanionMales, by="FID")
MY_TABLE <- merge(MY_TABLE, Fitness, by="FID")


## calculate condition
#### as per preregistration: if measurements were forgotten and cannot be done
#### (e.g. weight not measure immediately before the test),
#### the group average (e.g. among red averse females or
#### among males with red face and red pedipalps)
#### will be attributed to that individual

MY_TABLE[is.na(MY_TABLE$FMass),]
MY_TABLE$FMass[MY_TABLE$FID == 18401] <- mean(MY_TABLE$FMass[MY_TABLE$FTrt == 'RedAverse'], na.rm = TRUE)

MY_TABLE[is.na(MY_TABLE$MMass),]
MY_TABLE$MMass[MY_TABLE$MID == 18228] <- mean(MY_TABLE$MMass[MY_TABLE$MTrt == 'AllGrey'], na.rm = TRUE)
MY_TABLE$MMass[MY_TABLE$MID == 18390] <- mean(MY_TABLE$MMass[MY_TABLE$MTrt == 'Unmanipulated'], na.rm = TRUE)


MY_TABLE$Fcondition <- resid(lm(MY_TABLE$FMass~MY_TABLE$FCarapaceWidth))
MY_TABLE$Mcondition <- resid(lm(MY_TABLE$MMass~MY_TABLE$MCarapaceWidth))


## code FTrt and MTrt

#### as per preregistration

#### Female diet/training
##### red accustomed/preference group [code (relative to their preference for red) = +0.5]
##### red averse group [code (relative to their preference for red) = -0.5]

#### Male color manipulation 
##### AllRed: (face and pedipalps painted red [code (amount of red body parts) = 2] 
##### RedGrey: face painted red and pedipalps painted grey [code (amount of red body parts) = 1] 
##### AllGrey: face and pedipalps painted grey [code (amount of red body parts) = 0]
                                                                                                                        
MY_TABLE$FTrtCode[MY_TABLE$FTrt == "RedPreference"] <- 0.5
MY_TABLE$FTrtCode[MY_TABLE$FTrt == "RedAverse"] <- -0.5
MY_TABLE$MTrtCode[MY_TABLE$MTrt == "AllRed"] <- 2
MY_TABLE$MTrtCode[MY_TABLE$MTrt == "RedGrey"] <- 1
MY_TABLE$MTrtCode[MY_TABLE$MTrt == "AllGrey"] <- 0
MY_TABLE$MTrtCode[MY_TABLE$MTrt == "Unmanipulated"] <- NA



## Training duration
MY_TABLE$TrainingDuration <- as.numeric(MY_TABLE$TrialDate - MY_TABLE$PeriodBeginDate)

nrow(MY_TABLE)
summary(MY_TABLE)
}

head(MY_TABLE)

# write.csv(MY_TABLE, file = "3_ExtractedData/MY_TABLE.csv", row.names = FALSE)

# 20190627: add TrainingDuration, change brood size NA to 0 (but 18417 who disappeared before laying)


