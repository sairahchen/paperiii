
# Load imputed data ----

imputedMids100BreastTotalMortality <- readRDS(file="./data/imputedMids100_breast_total_mortality_27032023.rds")
imputedMids100BreastCancerMortality <- readRDS(file="./data/imputedMids100_breast_cancer_mortality.rds")
imputedMids100ColorectalTotalMortality <- readRDS(file="./data/imputedMids100_colorectal_total_mortality.rds")
imputedMids100ColorectalCancerMortality <- readRDS(file="./data/imputedMids100_colorectal_cancer_mortality.rds")
imputedMids100LungTotalMortality <- readRDS(file="./data/imputedMids100_lung_total_mortality.rds")
imputedMids100LungTotalMortality <- readRDS(file="./data/imputedMids100_lung_cancer_mortality.rds")


# Merge imputed datasets with variables that were not included in the imputation ----

convertMidsToLongMerged <- function(imputedMids, originalDataframe){
  long <- complete(imputedMids, "long", include=TRUE)
  imputedMergedLong <- left_join(long, originalDataframe[, c("myID", 
                                                                  setdiff(
                                                                    colnames(originalDataframe),
                                                                    colnames(long)
                                                                  ))], by = "myID")
  return(imputedMergedLong)
}

imputedLong100BreastTotalMortality <- convertMidsToLongMerged(imputedMids100BreastTotalMortality, breast)
imputedLong100BreastCancerMortality <- convertMidsToLongMerged(imputedMids100BreastCancerMortality, breast)
imputedLong100ColorectalTotalMortality <- convertMidsToLongMerged(imputedMids100ColorectalTotalMortality, colorectal)
imputedLong100ColorectalCancerMortality <- convertMidsToLongMerged(imputedMids100ColorectalCancerMortality, colorectal)
imputedLong100LungTotalMortality <- convertMidsToLongMerged(imputedMids100LungTotalMortality, lung)
imputedLong100LungCancerMortality <- convertMidsToLongMerged(imputedMids100LungTotalMortality, lung)

# Tranformations ----

## HLI score ----
find_HLI_score <- function(dataframe){
  dataframe %>%
    mutate(bmiScore = case_when(bmi >= 30 ~ 0,
                                bmi > 27 ~ 1,
                                bmi > 25 ~ 2,
                                bmi > 23 ~ 3,
                                bmi > 0 ~ 4,
                                TRUE ~ NA_real_
                                ),
           hliScore = as.numeric(as.character(physicalActivityScore)) +
                      as.numeric(as.character(bmiScore)) +
                      as.numeric(as.character(smokingScore)) +
                      as.numeric(as.character(alcoholScore)) +
                      as.numeric(as.character(dietScore)),
           hliScoreNoPhy = (hliScore - as.numeric(as.character(physicalActivityScore)))*(5/4), # HLI-excluding-single-factor-scores will be standardised to 20pts
           hliScoreNoBmi = (hliScore - as.numeric(as.character(bmiScore)))*(5/4),
           hliScoreNoSmo = (hliScore - as.numeric(as.character(smokingScore)))*(5/4),
           hliScoreNoAlc = (hliScore - as.numeric(as.character(alcoholScore)))*(5/4),
           hliScoreNoDie = (hliScore - as.numeric(as.character(dietScore)))*(5/4)
           )
}
breast <- find_HLI_score(breast)
imputedLong100BreastTotalMortality <- find_HLI_score(imputedLong100BreastTotalMortality)
imputedLong100BreastCancerMortality <- find_HLI_score(imputedLong100BreastCancerMortality)
imputedLong100ColorectalTotalMortality <- find_HLI_score(imputedLong100ColorectalTotalMortality)
imputedLong100ColorectalCancerMortality <- find_HLI_score(imputedLong100ColorectalCancerMortality)
imputedLong100LungTotalMortality <- find_HLI_score(imputedLong100LungTotalMortality)
imputedLong100LungCancerMortality <- find_HLI_score(imputedLong100LungCancerMortality)

## HLI categories ----

find_hli_categories <- function(dataframe){
  dataframe %>%
    mutate(hliCategories = 
             cut(hliScore, breaks = c(
               quantile(hliScore, probs = seq(0, 1, 0.25), na.rm=TRUE)), 
               include.lowest = TRUE, right = FALSE))
  
  
}

imputedLong100BreastTotalMortality <- find_hli_categories(imputedLong100BreastTotalMortality)
imputedLong100BreastCancerMortality <- find_hli_categories(imputedLong100BreastCancerMortality)
imputedLong100ColorectalTotalMortality <- find_hli_categories(imputedLong100ColorectalTotalMortality)
imputedLong100ColorectalCancerMortality <- find_hli_categories(imputedLong100ColorectalCancerMortality)
imputedLong100LungTotalMortality <- find_hli_categories(imputedLong100LungTotalMortality)
imputedLong100LungCancerMortality <- find_hli_categories(imputedLong100LungCancerMortality)


## Label HLI categories from 1-4 so coding across datasets is transferable ----

find_hli_categories_levels <- function(dataframe){
  
  dataframe %>%
    mutate(hliCategoriesLevels = factor(hliCategories, labels = 1:4)
    )
  
}



# Quartiles have not changed between complete case and imputed datasets

imputedLong100BreastTotalMortality <- find_hli_categories_levels(imputedLong100BreastTotalMortality)
imputedLong100BreastCancerMortality <- find_hli_categories_levels(imputedLong100BreastCancerMortality)
imputedLong100ColorectalTotalMortality <- find_hli_categories_levels(imputedLong100ColorectalTotalMortality)
imputedLong100ColorectalCancerMortality <- find_hli_categories_levels(imputedLong100ColorectalCancerMortality)
imputedLong100LungTotalMortality <- find_hli_categories_levels(imputedLong100LungTotalMortality)
imputedLong100LungCancerMortality <- find_hli_categories_levels(imputedLong100LungCancerMortality)

# Create breast all cause and bc mortality diagnosed 2005 and after dataframes


imputedLong100BreastTotalMortality2005 <- imputedLong100BreastTotalMortality %>% 
  filter(dateDiagnosis >= "2005-01-01")

imputedLong100BreastCancerMortality2005 <- imputedLong100BreastTotalMortality %>% 
  filter(dateDiagnosis >= "2005-01-01")

# Create dataframes on pre/postmenopausal status for breast cancer

imputedLong100BreastTotalMortalityPostmeno <- imputedLong100BreastTotalMortality %>%
  filter(ageDiagnosis >= ageMenopause)
imputedLong100BreastCancerMortalityPostmeno <- imputedLong100BreastCancerMortality %>%
  filter(ageDiagnosis >= ageMenopause)

imputedLong100BreastTotalMortalityPremeno <- imputedLong100BreastTotalMortality %>%
  filter(ageDiagnosis < ageMenopause)
imputedLong100BreastCancerMortalityPremeno <- imputedLong100BreastCancerMortality %>%
  filter(ageDiagnosis < ageMenopause)


# Create dataframes for colon and rectal cancers
imputedLong100ColonTotalMortality <- imputedLong100ColorectalTotalMortality %>%
  filter(statusColon ==T)
imputedLong100ColonCancerMortality <- imputedLong100ColorectalCancerMortality %>%
  filter(statusColon ==T)
imputedLong100RectalTotalMortality <- imputedLong100ColorectalTotalMortality %>%
  filter(statusRectal == T)
imputedLong100RectalCancerMortality <- imputedLong100ColorectalCancerMortality %>%
  filter(statusRectal == T)

# Create dataframes for SCLC and NSCLC lung cancer

imputedLong100sclcTotalMortality <- imputedLong100LungTotalMortality %>%
  filter(statusSCLC == T)
imputedLong100sclcCancerMortality <- imputedLong100LungCancerMortality %>%
  filter(statusSCLC == T)

imputedLong100NsclcTotalMortality <- imputedLong100LungTotalMortality %>%
  filter(statusNSCLC == T)
imputedLong100NsclcCancerMortality <- imputedLong100LungCancerMortality %>%
  filter(statusNSCLC == T)

# left truncated follow-up cohorts --> exclude cases where diagnosis is close to date of questionnaire

exclude_followUpTime <- function(dataframe, daysExcluded){
  dataframe %>%
    filter(followUpTimeDays > daysExcluded)
}

breastX1YrTotalM <- exclude_followUpTime(imputedLong100BreastTotalMortality, 365.25)
breastX1YrCancerM <- exclude_followUpTime(imputedLong100BreastCancerMortality, 365.25)
breastX3YrTotalM <- exclude_followUpTime(imputedLong100BreastTotalMortality, 1095.75)
breastX3YrCancerM <- exclude_followUpTime(imputedLong100BreastCancerMortality, 1095.75)
colorectalX1YrTotalM <- exclude_followUpTime(imputedLong100ColorectalTotalMortality, 365.25)
colorectalX1YrCancerM <- exclude_followUpTime(imputedLong100ColorectalCancerMortality, 365.25)
colorectalX3YrTotalM <- exclude_followUpTime(imputedLong100ColorectalTotalMortality, 1095.75)
colorectalX3YrCancerM <- exclude_followUpTime(imputedLong100ColorectalCancerMortality, 1095.75)
lungX1YrTotalM <- exclude_followUpTime(imputedLong100LungTotalMortality, 365.25)
lungX1YrCancerM <- exclude_followUpTime(imputedLong100LungCancerMortality, 365.25)
lungX3YrTotalM <- exclude_followUpTime(imputedLong100LungTotalMortality, 1095.75)
lungX3YrCancerM <- exclude_followUpTime(imputedLong100LungCancerMortality, 1095.75)

# stop introducing new cases 3 years before end of study period (Dec 2020) 

breastX3YrNewCasesMi <- imputedLong100BreastTotalMortality %>% filter(dateDiagnosis <= "2017-12-31")
colorectalX3YrNewCasesMi <- imputedLong100ColorectalTotalMortality %>% filter(dateDiagnosis <= "2017-12-31")
lungX3YrNewCasesMi <- imputedLong100LungTotalMortality %>% filter(dateDiagnosis <= "2017-12-31")



# Create dataframes for age before 63 and 63+

imputedLong100BreastTotalMortalityYoung <- imputedLong100BreastTotalMortality %>%
  filter(ageDiagnosis <63)
imputedLong100BreastTotalMortalityOld <- imputedLong100BreastTotalMortality %>%
  filter(ageDiagnosis >= 63)
imputedLong100BreastCancerMortalityYoung <- imputedLong100BreastCancerMortality %>%
  filter(ageDiagnosis <63)
imputedLong100BreastCancerMortalityOld <- imputedLong100BreastCancerMortality %>%
  filter(ageDiagnosis >= 63)

imputedLong100ColorectalTotalMortalityYoung <- imputedLong100ColorectalTotalMortality %>%
  filter(ageDiagnosis <68)
imputedLong100ColorectalTotalMortalityOld <- imputedLong100ColorectalTotalMortality %>%
  filter(ageDiagnosis >= 68)
imputedLong100ColorectalCancerMortalityYoung <- imputedLong100ColorectalCancerMortality %>%
  filter(ageDiagnosis <68)
imputedLong100ColorectalCancerMortalityOld <- imputedLong100ColorectalCancerMortality %>%
  filter(ageDiagnosis >= 68)

imputedLong100LungTotalMortalityYoung <- imputedLong100LungTotalMortality %>%
  filter(ageDiagnosis <68)
imputedLong100LungTotalMortalityOld <- imputedLong100LungTotalMortality %>%
  filter(ageDiagnosis >= 68)
imputedLong100LungCancerMortalityYoung <- imputedLong100LungCancerMortality %>%
  filter(ageDiagnosis <68)
imputedLong100LungCancerMortalityOld <- imputedLong100LungCancerMortality %>%
  filter(ageDiagnosis >= 68)

# Create dataframes for prediagnostic interval shorter and longer than median 
imputedLong100BreastTotalMortalityPrediagShort <- imputedLong100BreastTotalMortality %>%
  filter(lagYearsMedian == "short")
imputedLong100BreastTotalMortalityPrediagLong <- imputedLong100BreastTotalMortality %>%
  filter(lagYearsMedian == "long")
imputedLong100BreastCancerMortalityPrediagShort <- imputedLong100BreastCancerMortality %>%
  filter(lagYearsMedian == "short")
imputedLong100BreastCancerMortalityPrediagLong <- imputedLong100BreastCancerMortality %>%
  filter(lagYearsMedian == "long")

imputedLong100ColorectalTotalMortalityPrediagShort <- imputedLong100ColorectalTotalMortality %>%
  filter(lagYearsMedian == "short")
imputedLong100ColorectalTotalMortalityPrediagLong <- imputedLong100ColorectalTotalMortality %>%
  filter(lagYearsMedian == "long")
imputedLong100ColorectalCancerMortalityPrediagShort <- imputedLong100ColorectalCancerMortality %>%
  filter(lagYearsMedian == "short")
imputedLong100ColorectalCancerMortalityPrediagLong <- imputedLong100ColorectalCancerMortality %>%
  filter(lagYearsMedian == "long")

imputedLong100LungTotalMortalityPrediagShort <- imputedLong100LungTotalMortality %>%
  filter(lagYearsMedian == "short")
imputedLong100LungTotalMortalityPrediagLong <- imputedLong100LungTotalMortality %>%
  filter(lagYearsMedian == "long")
imputedLong100LungCancerMortalityPrediagShort <- imputedLong100LungCancerMortality %>%
  filter(lagYearsMedian == "short")
imputedLong100LungCancerMortalityPrediagLong <- imputedLong100LungCancerMortality %>%
  filter(lagYearsMedian == "long")


#imputedMids100BreastTotalMortality <- as.mids(imputedLong100BreastTotalMortality)
