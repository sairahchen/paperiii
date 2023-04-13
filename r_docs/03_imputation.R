#-----------------------
# Title: Imputation for survival paper

# Description: Imputation of three datasets: breast, colorectal, and lung cancer cases

# Date: 06.07.2022
#----------------------

# Create dataframe with only variables in analysis / predictors of those variables ----
# include all variables that appear in the complete data model, including the outcome
# include variables related to nonresponse
# include variables that explain a significant amount of variance - helps reduce incertainty of imputations and identified by their correlation with target variable
# remove variables from above two points that have too many missing within the subgroup of incomplete cases


select_general_variables <- function(dataframe){
  return(
    dataframe %>%
      select(myID, # Take out of predictor matrix
             ageDiagnosis, # complete
             education, # incomplete
             wave, # complete
             weight, # incomplete
             height, # incomplete
             physicalActivityScore, #incomplete
             bmi, # will not impute on BMI score level because bmi is already a transformation
             smokingStatus, # incomplete
             smokingScore, # incomplete
             alcoholScore, # incomplete
             dietScore, # incomplete
             energyIntake, #incomplete
             lagYears, #complete
             totalDeath, #complete
             followUpTimeDays # complete
             
      )
  )
}




# Breast cancer specific variables

select_breast_variables <- function(dataframe){
  return(
    dataframe %>%
      select(myID, # complete
             hrtStatus, # complete
             ocEverUse, # complete
             parity, # complete
             breastfeeding, # complete
             ageMenarche, # 
             menopausalStatus, # complete
             familyHistBC, # complete
             tnmAnatomicStage, # complete with unknowns
             breastDeath # complete
             )
  )
}


# Colorectal and lung cancer specific variables

select_crc_variables <- function(dataframe){
  return(
    dataframe %>%
      select(myID, # complete
             seerStage, # complete with unknowns
             colorectalDeath # complete
             )
  )
}

select_lung_variables <- function(dataframe){
  return(
    dataframe %>%
      select(myID, # complete
             seerStage, # complete with unknowns
             lungDeath # complete
             
      )
  )
}

# Generating imputation datasets for analytical samples

breastTotalMortality <- merge(select_general_variables(breast), select_breast_variables(breast), by = "myID")
breastCancerMortality <- merge(select_general_variables(breast), select_breast_variables(breast), by = "myID")

colorectalTotalMortality <- merge(select_general_variables(colorectal), select_crc_variables(colorectal), by = "myID")
colorectalCancerMortality <- merge(select_general_variables(colorectal), select_crc_variables(colorectal), by = "myID")

lungTotalMortality <- merge(select_general_variables(lung), select_lung_variables(lung), by = "myID")
lungCancerMortality <- merge(select_general_variables(lung), select_lung_variables(lung), by = "myID")

# Find Nelson Aalen estimator


breastTotalMortality$nelsonAalen <- nelsonaalen(breastTotalMortality, followUpTimeDays, totalDeath)
breastCancerMortality$nelsonAalen <- nelsonaalen(breastCancerMortality, followUpTimeDays, breastDeath)

colorectalTotalMortality$nelsonAalen <- nelsonaalen(colorectalCancerMortality, followUpTimeDays, totalDeath)
colorectalCancerMortality$nelsonAalen <- nelsonaalen(colorectalCancerMortality, followUpTimeDays, colorectalDeath)

lungTotalMortality$nelsonAalen <- nelsonaalen(lungTotalMortality, followUpTimeDays, totalDeath)
lungCancerMortality$nelsonAalen <- nelsonaalen(lungCancerMortality, followUpTimeDays, lungDeath)







p_missing <- unlist(lapply(breastTotalMortality, function(x) sum(is.na(x))))/nrow(breastTotalMortality)
sort(p_missing[p_missing > 0], decreasing = TRUE)
sapply(breastTotalMortality, function(x) sum(is.na(x)))



# imputation ----


imputeToMids100 <- function(imputationDataframe){
  initialise <- mice(imputationDataframe, maxit = 0)
  
  
  predM <- initialise$predictorMatrix # rows are incomplete target variables, column var is predictor

  
  
  # pmm is default
  meth <- initialise$method
  ordinal <- c("smokingStatus",
               "physicalActivityScore",
               "smokingScore",
               "alcoholScore",
               "dietScore",
               "education")
  #    meth[binary] = "logreg" / all binary vars are complete
  meth[ordinal] = "polr"
  
  
  meth["bmi"] <- "~I(weight/(height/100)^2)" # passive imputation
  predM[c("weight", "height"), "bmi"] <- 0 # break the feedback loop that can produce absurd imputations
  predM[, c("myID")] <- 0
  imputedMids <- mice(imputationDataframe, method=meth, predictorMatrix = predM, m=100, maxit=10, print=TRUE)
  #TODO make a plot for imputedMids to check convergence
  
  return(imputedMids)
}

imputedMids100BreastTotalMortality <- imputeToMids100(breastTotalMortality)
imputedMids100BreastCancerMortality <- imputeToMids100(breastCancerMortality)
imputedMids100ColorectalTotalMortality <- imputeToMids100(colorectalTotalMortality)
imputedMids100ColorectalCancerMortality <- imputeToMids100(colorectalCancerMortality)
imputedMids100LungTotalMortality <- imputeToMids100(lungTotalMortality)
imputedMids100LungCancerMortality <- imputeToMids100(lungCancerMortality)



saveRDS(imputedMids100BreastTotalMortality, file="./data/imputedMids100_breast_total_mortality.rds")
saveRDS(imputedMids100BreastCancerMortality, file="./data/imputedMids100_breast_cancer_mortality.rds")
saveRDS(imputedMids100ColorectalTotalMortality, file="./data/imputedMids100_colorectal_total_mortality.rds")
saveRDS(imputedMids100ColorectalCancerMortality, file="./data/imputedMids100_colorectal_cancer_mortality.rds")
saveRDS(imputedMids100LungTotalMortality, file="./data/imputedMids100_lung_total_mortality.rds")
saveRDS(imputedMids100LungCancerMortality, file="./data/imputedMids100_lung_cancer_mortality.rds")

# Check if there are missing left after imputation

sapply(complete(imputedMids100BreastTotalMortality), function(x) sum(is.na(x)))

plot(imputedMids100BreastTotalMortality)
savePlot(filename = "./output/plot", type = "jpeg")