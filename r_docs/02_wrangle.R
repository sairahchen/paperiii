

# load data
df <- read.csv("./data/22032023.csv")


# convert all colnames to lowercase
colnames(df) <- tolower(colnames(df))

# Create ID variable
df$myID <- 1:nrow(df)

# Clean variable names 

df$physicalActivity <- df$aktidag
df$height <- df$hoyde
df$weight <- df$vektana
df$smokingStatus <- as.factor(as.character(df$roykstat))
df$smokingStatus <- plyr::revalue(df$smokingStatus, c("1" = "Never", "2" = "Former", "3" = "Current"))
df$grMusli <- df$grfrubla
df$grYogurt <- df$gryoghur
df$energyIntake <- df$totkjoul
df$ageEntry <- df$startald
df$education <- df$skole
df$breastfeedMths <- df$amme
df$ageMenarche <- df$mensald
df$parity <- df$antbarn
df$breastfeeding <- df$amme

# format education
find_education <- function(dataframe){
  dataframe %>%
    mutate(education = as.factor(na_if(education, "")))
  

  
}


df <- find_education(df)

find_education_low_high <- function(dataframe){
  dataframe %>%
  mutate(educationLowHigh = case_when(education == "<=9" | education == "10-12" ~ "0-12 years",
                                                education == "13-16" | education == ">=17" ~ ">=13 years",
                                                TRUE ~ NA_character_)
  )
  
}
df <- find_education_low_high(df)

# format dates
df$dateDeath <- as.Date(df$doddt_dc, format= "%d/%m/%Y")
df$dateEmigration <- as.Date(df$emigdt, format = "%d/%m/%Y")
df$dateDiagnosis <- as.Date(df$diagdat, format = "%d/%m/%Y")
df$dateBirth <- as.Date(paste0("010719", df$faar),"%d%m%Y")
df$dateQuestionnaire <- as.Date(df$startdat, format="%d/%m/%Y")

find_age_at_questionnaire <- function(dataframe){
  dataframe %>%
    mutate(ageQuestionnaire = as.numeric(
      sprintf(
      as.numeric((dateQuestionnaire - dateBirth)/365.25),
      fmt = "%#.2f")
    )
    )
}

df <- find_age_at_questionnaire(df)

find_age_at_diagnosis <- function(dataframe){
  dataframe %>%
    mutate(ageDiagnosis = as.numeric(
      sprintf(
      as.numeric((dateDiagnosis - dateBirth)/365.25),
      fmt = "%#.2f")
    )
    )
}

df <- find_age_at_diagnosis(df)



find_lag_years <- function(dataframe){ # years between questionnaire and diagnosis
  dataframe %>%
    mutate(lagYears = ageDiagnosis - ageQuestionnaire)
  
}

df <- find_lag_years(df)



find_wave <- function(dataframe){
  dataframe %>%
    mutate(wave = case_when(as.numeric(format(dateQuestionnaire, format = "%Y")) <=  1998 ~ 1, # 1996-1998
                            as.numeric(format(dateQuestionnaire, format = "%Y")) <=  2006 ~ 2, # 2002-2006
                            as.numeric(format(dateQuestionnaire, format = "%Y")) <=  2014 ~ 3, # 2010-2014
                            TRUE ~ NA_real_),
           yearQuestionnaire = as.numeric(format(dateQuestionnaire, format = "%Y")))
}

df <- find_wave(df)



find_follow_up_time_days <- function(dataframe){

  dataframe %>%
    
    mutate(dateExit = pmin(as.Date("2020-12-31"), 
                                    dateDeath,
                                    dateEmigration,
                                    na.rm = TRUE),
         followUpTimeDays = as.numeric(dateExit - dateDiagnosis),
         followUpTimeDaysX1Year = followUpTimeDays - 365.25,
         followUpTimeDaysX3Year = followUpTimeDays - 1095.75,
         followUpTimeYears = followUpTimeDays/365.25,
         followUpTimeYears = followUpTimeDays/365.25,
         q_to_exit_years = as.numeric(dateExit - dateQuestionnaire)/365.25)
}

df <- find_follow_up_time_days(df)



find_age_exit <- function(dataframe){
  dataframe %>%
    mutate(ageExit = ageDiagnosis + (as.numeric(followUpTimeDays)/365.25))
}

df <- find_age_exit(df)


find_cause_death <- function(dataframe){
  dataframe %>%
    mutate(causeDeath = case_when(sjmisc::is_empty(dc, first.only = FALSE) == TRUE ~ NA_character_,
                                  TRUE ~ dc)
    )
}

df <- find_cause_death(df)


# Exclusions ----

# Exclude death before or same month as cancer diagnosis
df <- df %>%
  filter(dateDeath>dateDiagnosis | is.na(dateDeath))

# Exclude emig before or same month as cancer diagnosis
df <- df %>%
  filter(dateEmigration>dateDiagnosis | is.na(dateEmigration))

#df <- df %>%
#  filter(dateDeath > dateDiagnosis | is.na(dateDeath))
# Exclude cancer diagnosis before questionnaire (prevalent cancer) -  should be non excluded in dataset

df <- df %>%
  filter(dateDiagnosis > dateQuestionnaire)

#      Excluded if have a cause of death but no date of death

exclusions <- function(dataframe){
  filter(dataframe, !(is.na(causeDeath) & !is.na(dateDeath)))
}

df <- exclusions(df)





# physical activity ----

# (dichotomize PA scale for descriptive purposes)

find_active <- function(dataframe) {
  dataframe %>%
    mutate(active = case_when(physicalActivity >= 6 ~ "Active (>=6)",
                              physicalActivity < 6 ~ "Inactive (<6)",
                              TRUE ~ NA_character_))

}



df <- find_active(df)


# HLI physical activity score 

score_physical_activity <- function(physicalActivity){
  a <- cut(physicalActivity, breaks = (
    quantile(physicalActivity, probs= seq(0,1,0.2), na.rm=TRUE)),
    labels = c("0", "1", "2", "3", "4"), include.lowest = TRUE, right = FALSE)
  
  return(a)
}

df$physicalActivityScore <- score_physical_activity(df$physicalActivity)

# BMI ----


# Calculate BMI

findBMI <- function(dataframe){
  dataframe %>%
    mutate(bmi = vektana/(hoyde/100)^2)
}

df <- findBMI(df) 


# HLI BMI score 

score_bmi <- function(bmi){
  a <- case_when(bmi >= 30 ~ 0,
                 bmi > 27 ~ 1,
                 bmi > 25 ~ 2,
                 bmi > 23 ~ 3,
                 bmi > 0 ~ 4,
                 TRUE ~ NA_real_
  )
  a <- as.factor(as.character(a))
  return(a)
}

df$bmiScore <- score_bmi(df$bmi)

# Smoking ----

# Find smoking intensity at age intervals
# Only applies to early questionnaires (S14-S26)

merge_smoking_intensity_age_intervals <- function(dataframe){
  return(
    dataframe %>%
      mutate(
        smokingIntensity1519 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant1,
          TRUE ~ roykant1519),
        smokingIntensity2029 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant2,
          TRUE ~ roykant2029),
        smokingIntensity3039 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant3,
          TRUE ~ roykant3039),
        smokingIntensity4049 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant4,
          TRUE ~ roykant4049),
        smokingIntensity5059 = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant5,
          TRUE ~ roykant50mm),
        smokingIntensity60plus = case_when(
          serienr >= 14 & serienr <= 26 ~ roykant6,
          TRUE ~ roykant50mm))
  )
}

# Find smoking intensity at current age
#   0 = 0, 1 = 1-4, 2 = 5-9, 3 = 10-14, 4 = 15-19, 5 = 20-24, 6 = 25+
#   All smoking intensity variables throughout series are coded the same


find_smoking_intensity <- function(dataframe){
  return (dataframe %>% 
            mutate(
              smokingIntensity = case_when(
                smokingStatus == "Current" ~ case_when(
                  serienr == 28 | serienr == 29 ~ roykar2,
                  serienr == 32 | serienr == 33 ~ roykar1,
                  serienr == 38 | serienr == 39 | serienr == 42 ~ roksist5,
                  serienr == 46 ~ roksist8,
                  serienr == 47 | serienr == 48 ~ roykar2,
                  ageQuestionnaire >= 65 ~ smokingIntensity60plus,
                  ageQuestionnaire >= 55 ~ smokingIntensity5059,
                  ageQuestionnaire >= 45 ~ smokingIntensity4049,
                  ageQuestionnaire >= 35 ~ smokingIntensity3039,
                  ageQuestionnaire >= 25 ~ smokingIntensity2029,
                  ageQuestionnaire >= 15 ~ smokingIntensity1519), 
              TRUE ~ NA_integer_
              )
            )
  )
}  

df <- merge_smoking_intensity_age_intervals(df)
df <- find_smoking_intensity(df)
  


# Years since smoking cessation

years_since_smoking_cessation <- function(dataframe){
  return(dataframe %>%
           mutate(
             yearsSinceSmokingCessation = case_when(
               smokingStatus == "Former" ~ ageQuestionnaire - sistald,
               TRUE ~ NA_real_)
           ))
}

df <- years_since_smoking_cessation(df)


# HLI smoking score

score_smoking <- function(dataframe){
  dataframe %>%
    mutate(smokingScore = case_when(smokingStatus == "Never" ~ "4",
                                    smokingStatus == "Former" & yearsSinceSmokingCessation > 10 ~ "3",
                                    smokingStatus == "Former" & yearsSinceSmokingCessation <= 10 ~ "2",
                                    smokingStatus == "Current" & smokingIntensity <= 3 ~ "1",
                                    smokingStatus == "Current" & smokingIntensity > 3 ~ "0",
                 TRUE ~ NA_character_) %>%
             forcats::fct_relevel(c("0", "1", "2", "3", "4")
             )
    )

}

df <- score_smoking(df)

# Alcohol ----

# Finetune missing on alcohol from alkogr

calculateGrAlcohol <- function(dataframe){
  return(dataframe %>%
           mutate(gramsAlcohol = case_when(
             avhold == 1 & # avhold=1 is "not sober"
               is.na(dataframe$olglass) &
               is.na(dataframe$vinglass) &
               is.na(dataframe$drinker) ~ NA_real_,#Missing on alcohol if declared not sober and have not filled in frequency questions
             TRUE ~ alkogr
           )))
}  

df <- calculateGrAlcohol(df)

# HLI alcohol score

score_alcohol <- function(daily_grams_alcohol_intake){
  a <- case_when(daily_grams_alcohol_intake > 20 ~ 0,
                 daily_grams_alcohol_intake > 10 ~ 1,
                 daily_grams_alcohol_intake > 5 ~ 2,
                 daily_grams_alcohol_intake > 0 ~ 3,
                 daily_grams_alcohol_intake == 0 ~ 4,
                 TRUE ~ NA_real_)
  return(as.factor(a))
}

df$alcoholScore <- score_alcohol(df$gramsAlcohol)

# Diet ----

# Finetune missing

calculateGrWholeGrainBread <- function(dataframe){
  return(
    dataframe %>%
      mutate(gramsWholeGrainBread = case_when(
        is.na(fqgrbrod) &
          is.na(fqknbrod) &
          is.na(brodfin) ~ NA_real_,
        TRUE ~ grgrbrod)
      ))
}

df <- calculateGrWholeGrainBread(df)

calculateGrFruit <- function(dataframe){
  return(
    dataframe %>% 
      mutate(gramsFruit = case_when(
        is.na(fqeplepa) &
          is.na(fqappels) &
          is.na(fqbanan) &
          is.na(fqanfruk) ~ NA_real_,
        TRUE ~ (grfrukt #+ ifelse(
          #is.na(grjordbar), 
          #0, 
          #grjordbar)
        ))
      )
  )
}

df <- calculateGrFruit(df)

calculateGrVegetable <- function(dataframe){
  return(
    dataframe %>%
      mutate(gramsVegetable = case_when(
        is.na(fqgulrot) &
          is.na(fqkaal) &
          is.na(fqbrokko)&
          is.na(fqsalat) &
          is.na(fqgrblan) &
          is.na(fqkalrot)&
          is.na(fqgrsak) ~ NA_real_,
        TRUE ~ grgrsak
      )))
}

df <- calculateGrVegetable(df)

calculateGrRedMeat <- function(dataframe){
  return(
    dataframe %>%
      mutate(gramsRedMeat = case_when(
        is.na(fqsteik) &
          is.na(fqkotele) &
          is.na(fqbiff) &
          is.na(fqkjkake) &#
          is.na(fqpolse) &#
          is.na(fqlapska) &
          is.na(fqpizza) &
          is.na(fqkyllin) &
          is.na(fqkjot) ~ NA_real_,
        TRUE ~ grrenkjo
      ))
  )
}


df <- calculateGrRedMeat(df)

calculateGrProcessedMeat <- function(dataframe){
  return(
    dataframe %>%
      mutate(gramsProcessedMeat = case_when(
        is.na(fqsteik) &
          is.na(fqkotele) &
          is.na(fqbiff) &
          is.na(fqkjkake) &
          is.na(fqpolse) &
          is.na(fqlapska) &
          is.na(fqpizza) &
          is.na(fqkyllin) &
          is.na(fqkjot) ~ NA_real_,
        TRUE ~ (grkjkake + # as per Parr, 2013
                  ifelse(
                    is.na(grpolse), 
                    0, 
                    grpolse) + 
                  grkjotpa)
      ))
  )
}



df <- calculateGrProcessedMeat(df)

calculateGrMilk <- function(dataframe){
  return(
    dataframe %>% 
      mutate(gramsMilk = case_when(
        is.na(fqhemelk) &
          is.na(fqlemelk) &
          is.na(fqskmelk) &
          is.na(fqdmelk) ~ NA_real_,
        TRUE ~ grmelk
      ))
  )
}

df <- calculateGrMilk(df)

calculateGrCheese <- function(dataframe){
  return(
    dataframe %>%
      mutate(gramsCheese = case_when(
        is.na(fqsyltet) & 
          is.na(fqbrostf) &
          is.na(fqbrostm) &
          is.na(fqkvostf) &
          is.na(fqkvostm) &
          is.na(fqkjotpa) ~ NA_real_,
        TRUE ~ grbrostf + grbrostm + grkvostf + grkvostm
      ))
  )
}

df <- calculateGrCheese(df)

# create wholegrain variable
df$gramsWholegrain <- df$gramsWholeGrainBread + df$grfrubla

# create dairy variable
df$gramsDairy <- df$gramsMilk + df$gramsCheese + df$gryoghur


# nutrient density

find_nutrient_densities <- function(dataframe){
  a <- dataframe$totkjoul/1000
  dataframe %>% 
    mutate(
      ndWholeGrain = gramsWholegrain/a,
      ndVegetable = gramsVegetable/a,
      ndFruit = gramsFruit/a,
      ndDairy = gramsDairy/a,
      ndRedMeat = gramsRedMeat/a,
      ndProcessedMeat = gramsProcessedMeat/a)
  
}

df <- find_nutrient_densities(df)

# diet score full
score_diet_full <- function(wholegrain,
                               vegetable,
                               fruit,
                               dairy,
                               red_meat,
                               processed_meat){ 
  a <- cut(wholegrain, breaks = c(
    quantile(wholegrain, probs =seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  a <- as.numeric(as.character(a))
  b <- cut(vegetable, breaks = c(
    quantile(vegetable, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  b <- as.numeric(as.character(b))
  c <- cut(fruit, breaks = c(
    quantile(fruit, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  c <- as.numeric(as.character(c))
  d <- cut(dairy, breaks = c(
    quantile(dairy, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  d <- as.numeric(as.character(d))
  e <- cut(red_meat, breaks = c(
    quantile(red_meat, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  e <- as.numeric(as.character(e))
  f <- cut(processed_meat, breaks = c(
    quantile(processed_meat, probs = seq(0,1, 0.25), na.rm=TRUE)),
    labels = c("3","2", "1", "0"), include.lowest = TRUE, right = FALSE)
  f <- as.numeric(as.character(f))
  g <- a+b+c+d+e+f
  return(g)}

df$dietScoreFull <- score_diet_full(df$ndWholeGrain,
                                    df$ndVegetable,
                                    df$ndFruit,
                                    df$ndDairy,
                                    df$ndRedMeat,
                                    df$ndProcessedMeat)

# HLI diet

score_diet <- function(dataframe){
  dataframe %>%
    mutate(dietScore = cut(dietScoreFull, breaks = c(
      quantile(dietScoreFull, probs = seq(0, 1, 0.2), na.rm=TRUE)),
      labels = c("0", "1", "2", "3", "4"), include.lowest = TRUE, right = FALSE)
    )
    
    
}


df <- score_diet(df)

# HLI score ----

# Continuous HLI score and reduced HLI scores
score_hli <- function(dataframe){
  dataframe %>%
    mutate(hliScore = 
             as.numeric(as.character(physicalActivityScore)) +
             as.numeric(as.character(bmiScore)) +
             as.numeric(as.character(smokingScore)) +
             as.numeric(as.character(alcoholScore)) +
             as.numeric(as.character(dietScore)),
           hliScoreNoPhy = hliScore - as.numeric(as.character(physicalActivityScore)),
           hliScoreNoBmi = hliScore - as.numeric(as.character(bmiScore)),
           hliScoreNoSmo = hliScore - as.numeric(as.character(smokingScore)),
           hliScoreNoAlc = hliScore - as.numeric(as.character(alcoholScore)),
           hliScoreNoDie = hliScore - as.numeric(as.character(dietScore))
    )

}

df <- score_hli(df)

# Categorical HLI score will be created after splitting into cancer case types due to different percentiles







# covariates ----

# menopausal status

find_menopause_status <- function(dataframe){
  dataframe %>%
    mutate(menopausalStatus = case_when(klimald >= ageDiagnosis ~ "Premenopausal",
                                        klimald < ageDiagnosis ~ "Postmenopausal",
                                        is.na(klimald) & ageDiagnosis >= 53 ~ "Postmenopausal",
                                        is.na(klimald) & ageDiagnosis < 53 ~ "Premenopausal"
                                        ),
           ageMenopause = ifelse(is.na(klimald),
                                 53,
                                 klimald)
    )
  
  
}

df <- find_menopause_status(df)


# breastfeeding

categorize_breastfeeding <- function(dataframe) {
  dataframe %>%
    mutate(breastfeedingCategories = case_when(breastfeeding == 0 ~ "None",
                                               breastfeeding > 0 & breastfeeding <= 12 ~ "<= 12 months",
                                               breastfeeding>12 ~ ">12 months"))
}

df <- categorize_breastfeeding(df)

# Hormone replacement therapy status

find_hrt_status <- function(dataframe){
  dataframe %>%
    mutate(hrtStatus = case_when(htgroup == 1 ~ "Never", # never
                                    htgroup == 2 ~ "Former", # former
                                    htgroup == 3 ~ "Current", # current
                                    TRUE ~ "Never") %>%
             forcats::fct_relevel(c("Never", "Former", "Current")
                                  )
           )
}

df <- find_hrt_status(df)

# Oral contraceptive use

find_oc_ever_use <- function(dataframe) {
  dataframe %>% 
    mutate(ocEverUse = case_when(ppille == 1 ~ "Never", # Never
                                    ppille == 0 ~ "Ever", # Ever
                                    is.na(ppille) ~ "Ever") %>%
             forcats::fct_relevel(c("Never", "Ever")
                                  )
           )
}

df <- find_oc_ever_use(df)

# Parity

categorize_parity <- function(dataframe){
  dataframe %>%
    mutate(parityCategories = case_when(antbarn == 0 ~ "0",
                                    antbarn == 1 ~ "1",
                                    antbarn == 2 ~ "1",
                                    antbarn > 2 ~ "2") %>%
             forcats::fct_relevel(c("0", "1", "2")
             )
    )
}

df <- categorize_parity(df)

# Family history of breast cancer in first degree relative

find_family_history_bc <- function(dataframe){
  dataframe %>%
    mutate(familyHistBC = case_when((mor==0 |
                                       mor == 3 |
                                       datter == 0 |
                                       soster == 0 |
                                       soster ==3) ~ "Yes", # 1 is yes
                                    (is.na(mor) &
                                       is.na(datter) &
                                       is.na(soster)) ~ "No", # is no
                                    TRUE ~ "No") %>%
             forcats::fct_relevel(c("No", "Yes")))
  
}

df <- find_family_history_bc(df)

# Cancer registry treatment variables

findTreatmentVars <- function(dataframe){
  dataframe %>%
    mutate(chemotherapy = case_when(kjemoterapi == "0" ~ "No",
                                    kjemoterapi == "1"~ "Yes",
                                    kjemoterapi == "9" ~ "Unknown",
                                    kjemoterapi == "." ~ ".",
                                    kjemoterapi == "" ~ "Blank",
                                    TRUE ~ NA_character_),
           radiationTherapy = case_when(straalebehandling == "0" ~ "No",
                                        straalebehandling == "1" ~ "Yes",
                                        straalebehandling == "9" ~ "Unknown",
                                        TRUE ~ NA_character_),
           otherTreatment = case_when(annen_behandling == "" ~ "Blank",
                                      annen_behandling == "0" ~ "No",
                                      annen_behandling == "1" ~ "Yes",
                                      annen_behandling == "2" ~ "Stem cell transplant",
                                      annen_behandling == "3" ~ "Hyperthermia",
                                      annen_behandling == "4" ~ "Immune modulatory therapy",
                                      annen_behandling == "9" ~ "Unknown",
                                      annen_behandling == "N" ~ "No",
                               TRUE ~ NA_character_),
           hormonalTherapy = case_when(hormonterapi == "0" ~ "No",
                                       hormonterapi == "1" ~ "Yes",
                                       hormonterapi == "9" ~ "Unknown",
                                       hormonterapi == "." ~ ".",
                                       hormonterapi == "" ~ "[Blank]",
                                       TRUE ~ NA_character_))
}

df <- findTreatmentVars(df)


# Cancer status ----
  
# Make breast, colon, rectal, and lung cancer status 
  
  findBreastCancerStatus <- function(dataframe){
    dataframe %>%
      mutate(statusBreast = ifelse(dataframe$icd10_gr=="C50"|
                                     dataframe$icd10_gr=="C500"|
                                     dataframe$icd10_gr=="C509",
                                   TRUE,
                                   FALSE))
  }

df <- findBreastCancerStatus(df)

findColonCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusColon = ifelse(dataframe$icd10_gr == "C18"|
                                  dataframe$icd10_gr == "C180"|
                                  dataframe$icd10_gr == "C181" |
                                  dataframe$icd10_gr == "C182" |
                                  dataframe$icd10_gr == "C183" |
                                  dataframe$icd10_gr == "C184" |
                                  dataframe$icd10_gr == "C185" |
                                  dataframe$icd10_gr == "C186" |
                                  dataframe$icd10_gr == "C187" |
                                  dataframe$icd10_gr == "C188" |
                                  dataframe$icd10_gr == "C189",
                                TRUE,
                                FALSE)
    )
}

df <- findColonCancerStatus(df)


findRectalCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusRectal = ifelse(dataframe$icd10_gr =="C19"| # N=0
                                   dataframe$icd10_gr == "C199" |
                                   dataframe$icd10_gr == "C20" | # N=1
                                   dataframe$icd10_gr == "C209",
                                 TRUE,
                                 FALSE)
    )
}

df <- findRectalCancerStatus(df)

findColorectalCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusColorectal = ifelse(dataframe$statusColon == TRUE |
                                       dataframe$statusRectal == TRUE,
                                     TRUE,
                                     FALSE
    ))
}

df <- findColorectalCancerStatus(df)

findLungCancerStatus <- function(dataframe){
  dataframe%>%
    mutate(statusLung = ifelse(dataframe$icd10_gr=="C340" |
                                 dataframe$icd10_gr=="C341" |
                                 dataframe$icd10_gr=="C342" |
                                 dataframe$icd10_gr=="C343" |
                                 dataframe$icd10_gr=="C348" |
                                 dataframe$icd10_gr=="C349" ,
                               TRUE, #true if event occurs
                               FALSE #false if censored
    ))
}

df <- findLungCancerStatus(df)


# small cell LC (SCLC)
findLungCancerSubtypes <- function(dataframe){
  dataframe %>% 
    mutate(statusSCLC = case_when(substr(hist, 1, 5) == "80413" ~ TRUE,
                                  substr(hist, 1, 5) == "80423" ~ TRUE,
                                  substr(hist, 1, 5) == "80433" ~ TRUE,
                                  substr(hist, 1, 5) == "80443" ~ TRUE,
                                  substr(hist, 1, 5) == "80453" ~ TRUE,
                                  TRUE ~ FALSE),
           
           
           
           statusNSCLC = case_when(statusSCLC == FALSE ~ TRUE,
                                   TRUE ~ FALSE
                                   )
           )}
                                   
      

df <- findLungCancerSubtypes(df)


# Variable coding breast, colorectal, or lung cancer cases

find_case_type <- function(dataframe){
  dataframe %>%
    mutate(caseType = as.factor(case_when(statusBreast == T ~ "Breast",
                                statusColorectal == T ~ "Colorectal",
                                statusLung == T ~ "Lung",
                                TRUE ~ NA_character_))
    )
}

df <- find_case_type(df)

# Causes of death

find_cause_death <- function(dataframe){
  dataframe %>%
    mutate(breastDeath = case_when(grepl("C50", causeDeath) ~ 1,
                                   TRUE ~ 0),
           colorectalDeath = case_when(grepl("C18", causeDeath) ~ 1,
                                       grepl("C19", causeDeath) ~ 1,
                                       grepl("C20", causeDeath) ~ 1,
                                       TRUE ~ 0), 
           rectalDeath = case_when(grepl("C19", causeDeath) ~ 1,
                                   grepl("C20", causeDeath) ~ 1,
                                   TRUE ~ 0),
           colonDeath = case_when(grepl("C18", causeDeath) ~ 1,
                                  TRUE ~ 0),
           lungDeath = case_when(grepl("C34", causeDeath) ~ 1,
                                 TRUE ~ 0),
           otherDeath = case_when(!grepl("C", causeDeath) ~ 1,
                                  TRUE ~ 0),
           totalDeath = case_when(!is.na(causeDeath) ~ 1,
                                  TRUE ~ 0)
    )
}


df <- find_cause_death(df)


# Fine and Gray status
  # 0: censored
  # 1: outcome of interest
  # 2: outcomes that compete with 1

find_cr_death <- function(dataframe){
  dataframe %>%
    mutate(breastDeathCr = case_when(grepl("C50", causeDeath) ~ 1,
                                     is.na(causeDeath) ~ 0,
                                     TRUE ~ 2),
           colorectalDeathCr = case_when(grepl("C18", causeDeath) |
                                           grepl("C19", causeDeath) |
                                           grepl("C20", causeDeath) ~ 1,
                                         is.na(causeDeath) ~ 0,
                                         TRUE ~ 2), 
           rectalDeathCr = case_when(grepl("C19", causeDeath) |
                                       grepl("C20", causeDeath) ~ 1,
                                     is.na(causeDeath) ~ 0,
                                     TRUE ~ 2),
           colonDeathCr = case_when(grepl("C18", causeDeath) ~ 1,
                                    is.na(causeDeath) ~ 0,
                                    TRUE ~ 2),
           lungDeathCr = case_when(grepl("C34", causeDeath) ~ 1,
                                   is.na(causeDeath) ~ 0,
                                   TRUE ~ 2)
    )
}

df <- find_cr_death(df)

df <- find_cause_death(df)
# If cause of death is the same as cancer diagnosis
find_cancer_specific_death <- function(dataframe){
  dataframe %>%
    mutate(cancerSpecificDeath = case_when(
      statusBreast == TRUE & breastDeath == 1 ~ 1,
      statusColorectal == TRUE & colorectalDeath == 1 ~ 1,
      statusLung == TRUE & lungDeath == 1 ~ 1,
      TRUE ~ 0
    ))
  
}

df <- find_cancer_specific_death(df)

# Make breast, colon, rectal, colorectal, and lung cancer survival cohorts

breast <- filter(df, statusBreast==T)
colon <- filter(df, statusColon==T)
rectal <- filter(df, statusRectal==T)
colorectal <- filter(df, statusColorectal==T)
lung <- filter(df, statusLung==T)
sclc <- filter(df, statusSCLC == T)
nsclc <- filter(df, statusLung ==T & statusNSCLC ==T)


# HLI score - categorical

find_hli_categories <- function(dataframe){
  dataframe %>%
    mutate(hliCategories = 
      cut(hliScore, breaks = c(
        quantile(hliScore, probs = seq(0, 1, 0.25), na.rm=TRUE)), 
        include.lowest = TRUE, right = FALSE),
      hliCategories5 = 
        cut(hliScore, breaks = c(
          quantile(hliScore, probs = seq(0, 1, 0.2), na.rm=TRUE)), 
          include.lowest = TRUE, right = FALSE),
      hliTertiles = cut(hliScore, breaks = c(
        quantile(hliScore, probs = seq(0, 1, 0.33333), na.rm=TRUE)), 
        include.lowest = TRUE, right = FALSE))
  
    
}

find_hli_categories_levels <- function(dataframe){
  
  dataframe %>%
    mutate(hliCategoriesLevels = factor(hliCategories, labels = 1:4),
           hliTertilesLevels = factor(hliTertiles, labels = 1:3)
    )
  
}

breast <- find_hli_categories(breast)
colorectal <- find_hli_categories(colorectal)
lung <- find_hli_categories(lung)
colon <- find_hli_categories(colon)
rectal <- find_hli_categories(rectal)

breast <- find_hli_categories_levels(breast)
colorectal <- find_hli_categories_levels(colorectal)
lung <- find_hli_categories_levels(lung)
colon <- find_hli_categories_levels(colon)
rectal <- find_hli_categories_levels(rectal)




# Staging variables ----

# Breast cancer staged with TNM

find_anatomic_stage_tnm <- function(dataframe){
  dataframe %>%
    mutate(tnmAnatomicStage = case_when(stage == "I" ~ "I",
                                        stage == "IA" ~ "I",
                                        stage == "IB" ~ "I",
                                        stage == "II" ~ "II",
                                        stage == "IIA" ~ "II",
                                        stage == "IIB" ~ "II",
                                        stage == "III" ~ "III",
                                        stage == "IIIA" ~ "III",
                                        stage == "IIIB" ~ "III",
                                        stage == "IIIC" ~ "III",
                                        stage == "IV" ~ "IV",
                                        stage == "Ukjent" ~ "Unknown",
                                        stage == "100" ~ "I",
                                        stage == "210" ~ "II",
                                        stage == "999" ~ "Unknown",
                                        TRUE ~ NA_character_)
                                        )
    
    

    
}

breast <- find_anatomic_stage_tnm(breast)

# Colorectal and lung cancer staged from metastase codes into SEER-stage
# refer to https://metadata.kreftregisteret.no/variables/detail/109?tabIndex=1


find_seer_stage <- function(dataframe){
  dataframe %>%
    mutate(seerStage = case_when(meta == "0" | meta == "8" ~ "Localized",
                                 meta == "1" | meta == "5" | meta == "6" ~ "Regional",
                                 meta == "2" | meta == "3" | meta == "4" ~ "Distant",
                                 meta == "7" | meta == "9" ~ "Unknown",
                                 TRUE ~ NA_character_) %>%
             forcats::fct_relevel(c("Localized", "Regional", "Distant", "Unknown")
                                  )
           )  # Should not be any missings
    
}
breast <- find_seer_stage(breast)
colorectal <- find_seer_stage(colorectal)
lung <- find_seer_stage(lung)





# ER/PR/HER2 status ----

# Careful with dichotomization of ER and PR due to changing cutoffs
# See document describing receptor status variables from Cancer Registry

find_hormone_receptor_status <- function(dataframe){
  dataframe %>%
    mutate(estrogenRec = case_when(er == 1 ~ "Neg",
                                   er == 98 ~ NA_character_,
                                   is.na(er) ~ NA_character_,
                                   TRUE ~ "Pos"),
           progesteroneRec = case_when(pr == 1 ~ "Neg",
                                       pr == 12645 ~ "Neg",
                                       pr == 98 ~ NA_character_,
                                       is.na(pr) ~ NA_character_,
                                       TRUE ~ "Pos"),
           her2Expression = case_when(her2 == 0 ~ "Pos",
                                      her2 == 1 ~ "Neg",
                                      her2 == 98 ~ NA_character_,
                                      TRUE ~ NA_character_)
    )
           
}

breast <- find_hormone_receptor_status(breast)
df <- find_hormone_receptor_status(df)

# Molecular subtype

find_molecular_subtype <- function(dataframe) {
  dataframe %>%
    mutate(erPos_prNeg = case_when(
      estrogenRec == "Pos" &
        progesteroneRec == "Neg" ~ 1,
      TRUE ~ NA_real_),
      tripleNeg = case_when( # too few cases of triple negative to analyse
        estrogenRec == "Neg" &
          progesteroneRec == "Neg" &
          her2Expression == "Neg" ~ 1,
        TRUE ~ NA_real_),
      her2Enrich = case_when(
        estrogenRec == "Neg" &
          progesteroneRec == "Neg" &
          her2Expression == "Pos" ~ 1,
        TRUE ~ NA_real_)
    )
}

breast <- find_molecular_subtype(breast)
df <-  find_molecular_subtype(df)


# left censor breast cancer diagnosed before 2005

breast2005 <- breast %>% filter(dateDiagnosis >= "2005-01-01")

# find number of prevalent cases
table(breast$dateDiagnosis <= breast$dateQuestionnaire)
table(colorectal$dateDiagnosis <= colorectal$dateQuestionnaire)
table(lung$dateDiagnosis <= lung$dateQuestionnaire)

# convert all character vars to factor vars

breast <- breast %>% mutate_if(is.character, as.factor)
colorectal <- colorectal %>% mutate_if(is.character, as.factor)
lung <- lung %>% mutate_if(is.character, as.factor)

# bind df back together 

df <- bind_rows(breast, colorectal, lung)

# left truncated follow-up cohorts --> exclude cases where diagnosis is close to date of questionnaire

exclude_followUpTime <- function(dataframe, daysExcluded){
  dataframe %>%
    filter(followUpTimeDays > daysExcluded) 
}

breastX1Yr <- exclude_followUpTime(breast, 365.25)
breastX3Yr <- exclude_followUpTime(breast, 1095.75)
colorectalX1Yr <- exclude_followUpTime(colorectal, 365.25)
colorectalX3Yr <- exclude_followUpTime(colorectal, 1095.75)
lungX1Yr <- exclude_followUpTime(lung, 365.25)
lungX3Yr <- exclude_followUpTime(lung, 1095.75)

# stop inclusion of new cases less than 3 years before end of study 

breastX3YrNewCases <- breast %>% filter(dateDiagnosis <= "2017-12-31")
colorectalX3YrNewCases <- colorectal %>% filter(dateDiagnosis <= "2017-12-31")
lungX3YrNewCases <- lung %>% filter(dateDiagnosis <= "2017-12-31")

# create binary prediagnostic interval (lag years)

binary_lag_years <- function(dataframe){
  dataframe %>%
    mutate(lagYearsMedian = as.factor(case_when(lagYears < median(lagYears) ~ "short",
                                      lagYears >= median(lagYears) ~ "long",
                                      TRUE ~ NA_character_)))
}

breast <- binary_lag_years(breast)
colorectal <- binary_lag_years(colorectal)
lung <- binary_lag_years(lung)
