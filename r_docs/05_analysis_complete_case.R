


source("./r_docs/office-template-code.R")




#################################
# Updating text/tabular results
#################################


result_prep_tpl <- function(result){
  result$table <- cbind(broom::tidy(result,conf.int=TRUE)) # Make a tidy result table
  result$table %<>% mutate(hrci = sprintf("%0.2f (%0.2f, %0.2f)", exp(estimate), exp(conf.low), exp(conf.high))) 
  result$name <- result$model$fullname # Model name (unique)
  return(result)
}


# Converts result table to a vector of named values
vectorize_result <- function(result){
  result$table %<>% 
    mutate(across(everything(), as.character)) %>% 
    tidyr::pivot_longer(cols = !matches("term"), 
                        names_to = "stat", 
                        values_to = "value") %>%
    mutate(names = paste0(result$name,".", term, ".", stat))
  
  nameNEvent <- paste0(result$name, ".", "nevent")
  
  result_vector = c(result$table$value, result$nevent)  
  names(result_vector) <- c(result$table$names, nameNEvent)
  
  return(result_vector)
}



# Take multiple results and convert values to a single named vector
make_template_values <- function(results){
  results %>% `names<-`(NULL) %>% lapply(vectorize_result) %>% unlist
}





base_model = list(
  model = TRUE,
  filter = NULL,
  outcome_str = NULL,
  predictors = c("ageDiagnosis", "education", "height", "strata(wave)"),
  names = NULL
)

templates <- list(
  #### Outcomes ####
  o.totalDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDays, totalDeath)"
    m$names %<>% c("tD")
    return(m)
  },
  o.breastDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDays,breastDeath)"
    m$names %<>% c("bcD")
    return(m)
  },
  o.colorectalDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDays,colorectalDeath)"
    m$names %<>% c("crcD")
    return(m)
  },
  o.lungDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDays,lungDeath)"
    m$names %<>% c("lcD")
    return(m)
  },
  o.exclude1YearTotalDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDaysX1Year,totalDeath)"
    m$names %<>% c("x1yTd")
    return(m)
  },
  o.exclude1YearBreastDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDaysX1Year,breastDeath)"
    m$names %<>% c("x1ybcD")
    return(m)
  },
  o.exclude1YearColorectalDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDaysX1Year,colorectalDeath)"
    m$names %<>% c("x1ycrcD")
    return(m)
  },
  o.exclude1YearLungDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDaysX1Year,lungDeath)"
    m$names %<>% c("x1ylcD")
    return(m)
  },
  o.exclude3YearTotalDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDaysX3Year,totalDeath)"
    m$names %<>% c("x3yTd")
    return(m)
  },
  o.exclude3YearBreastDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDaysX3Year,breastDeath)"
    m$names %<>% c("x3ybcD")
    return(m)
  },
  o.exclude3YearColorectalDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDaysX3Year,colorectalDeath)"
    m$names %<>% c("x3ycrcD")
    return(m)
  },
  o.exclude3YearLungDeath = function(m){
    m$outcome_str <- "Surv(followUpTimeDaysX3Year,lungDeath)"
    m$names %<>% c("x3ylcD")
    return(m)
  },
  
  
  
  
  #### Dataset ####
  d.bccohort = function(m){
    m$names %<>% c("bc.")
    return(m)
  },
  d.bccohort2005 = function(m){
    m$names %<>% c("bc2005.")
    return(m)
  },
  
  d.crccohort = function(m){
    m$names %<>% c("crc.")
    return(m)
  },
  d.lccohort = function(m){
    m$names %<>% c("lc.")
    return(m)
  },
  
  #### Covariates ####
  c.hliContinuous = function(m){
    m$predictors %<>% c("hliScore") 
    return(m)
  },
  c.hliCategories = function(m){
    m$predictors %<>% c("hliCategoriesLevels") 
    return(m)
  },
  c.hliTertiles = function(m){
    m$predictors %<>% c("hliTertilesLevels")
    m$names %<>% c("hliTert")
    return(m)
  },
  c.hliNoPhy = function(m){
    m$predictors %<>% c("hliScoreNoPhy")
    m$names %<>% c(".noPhy")
    return(m)
  },
  c.hliNoBmi = function(m){
    m$predictors %<>% c("hliScoreNoBmi") 
    m$names %<>% c(".noBmi")
    return(m)
  },
  c.hliNoSmo = function(m){
    m$predictors %<>% c("hliScoreNoSmo") 
    m$names %<>% c(".noSmo")
    return(m)
  },
  c.hliNoAlc = function(m){
    m$predictors %<>% c("hliScoreNoAlc")
    m$names %<>% c(".noAlc")
    return(m)
  },
  c.hliNoDie = function(m){
    m$predictors %<>% c("hliScoreNoDie") 
    m$names %<>% c(".noDie")
    return(m)
  },
  c.tnmstage = function(m){
    m$predictors %<>% c("strata(tnmAnatomicStage)") # Used for breast cancer
    return(m)
  },
  c.seerstage = function(m){
    m$predictors %<>% c("strata(seerStage)") # Used for colorectal and lung
    return(m)
  },
  c.bcvars = function(m){
    m$predictors %<>% c("menopausalStatus", "ageMenarche", "hrtStatus", "ocEverUse", "familyHistBC", "parity", "breastfeeding")
    return(m)
  },
  c.molecularsubtype = function(m){
    m$predictors %<>% c("estrogenRec", "progesteroneRec", "her2Expression")
    return(m)
  },
  c.her2 = function(m){
    m$predictors %<>% c("her2Expression")
    return(m)
  },
  c.prophaz = function(m){
    m$predictors %>% c("followUpTimeDays*hliCategories")
    return(m)
  },
  c.lagtime = function(m){
    m$predictors %>% c("lagYears", "lagYears:hliScore")
    return(m)
  },
  c.hormonerec = function(m){
    m$predictors %>% c("hliScore:hormoneRec")
    return(m)
  },

  
  #### Filters ####
  f.all = function(m){
    m$filter = function(d){d}
    return(m)
  },
  f.tnmI = function(m){
    m$filter <- function(d){d %>% filter(tnmAnatomicStage == "I")}
    m$names %<>% c("tnmI")
    return(m)
  },
  f.tnmII = function(m){
    m$filter <- function(d){d %>% filter(tnmAnatomicStage == "II")}
    m$names %<>% c("tnmII")
    return(m)
  },
  f.tnmIII = function(m){
    m$filter <- function(d){d %>% filter(tnmAnatomicStage == "III")}
    m$names %<>% c("tnmIII")
    return(m)
  },
  f.tnmIV = function(m){
    m$filter <- function(d){d %>% filter(tnmAnatomicStage == "IV")}
    m$names %<>% c("tnmIV")
    return(m)
  },
  f.tnmUnknown = function(m){
    m$filter <- function(d){d %>% filter(tnmAnatomicStage == "Unknown")}
    m$names %<>% c("tnmUnknown")
    return(m)
  },
  f.seerLocalized = function(m){
    m$filter <- function(d){d %>% filter(seerStage == "Localized")}
    m$names %<>% c("seerLoc")
    return(m)
  },
  f.seerRegional = function(m){
    m$filter <- function(d){d %>% filter(seerStage == "Regional")}
    m$names %<>% c("seerReg")
    return(m)
  },
  f.seerDistant = function(m){
    m$filter <- function(d){d %>% filter(seerStage == "Distant")}
    m$names %<>% c("seerDis")
    return(m)
  },
  f.seerUnknown = function(m){
    m$filter <- function(d){d %>% filter(seerStage == "Unknown")}
    m$names %<>% c("seerUnk")
    return(m)
  },
  f.exclude1Year = function(m){
    m$filter <- function(d){d %>% filter(
      (dateDeath > (dateDiagnosis + 365.25)) | is.na(dateDeath) )}
    m$names %<>% c("x1y")
    return(m)
  },
  f.exclude3Year = function(m){
    m$filter <- function(d){d %>% filter(
      (dateDeath > (dateDiagnosis + 1095.75)) | is.na(dateDeath) )}
    m$names %<>% c("x3y")
    return(m)
  },
  f.dbefore2005 = function(m){
    m$filter <- function(d){d %>% filter(dateDiagnosis < "2005-01-01")}
    m$names %<>% c("pre2005")
    return(m)
  },
  f.dafter2005 = function(m){
    m$filter <- function(d){d %>% filter(dateDiagnosis >= "2005-01-01")}
    m$names %<>% c("post2005")
    return(m)
  },
  f.ernegprneg = function(m){
    m$filter <- function(d){d %>% filter(estrogenRec == "Neg" & progesteroneRec == "Neg")}
    m$names %<>% c("er-pr-")
    return(m)
  },
  f.ernegprpos = function(m){
    m$filter <- function(d){d %>% filter(estrogenRec == "Neg" & progesteroneRec == "Pos")}
    m$names %<>% c("er-pr+")
    return(m)
  },
  f.erposprneg = function(m){
    m$filter <- function(d){d %>% filter(estrogenRec == "Pos" & progesteroneRec == "Neg")}
    m$names %<>% c("er+pr-")
    return(m)
  },
  f.erposprpos = function(m){
    m$filter <- function(d){d %>% filter(estrogenRec == "Pos" & progesteroneRec == "Pos")}
    m$names %<>% c("er+pr+")
    return(m)
  }
  ,
  f.prediag5 = function(m){
    m$filter <- function(d){d %>% filter(lagYears >= 5)}
    m$names %<>% c("prediag5")
    return(m)
  },
  f.qtoexit5 = function(m){
    m$filter <- function(d){d %>% filter(q_to_exit_years >= 5)}
    m$names %<>% c("qtoexit5")
    return(m)
  }
  
)



#### Breast cancer models ####

# HLI continuous
breast_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.breastDeath")]) %>% 
    serial_apply(templates[c("c.hliContinuous", "c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates[c("f.all",
                             "f.dbefore2005", 
                             "f.dafter2005")]) 

breast_results <- breast_models %>% lapply(run_model, dataset=breast)
breast_results %<>% lapply(result_prep_tpl)
breast_result_values <- make_template_values(breast_results) # apply to a list of models

breast_results_stop3yr <- breast_models %>% lapply(run_model, dataset=breastX3YrNewCases)
breast_results_stop3yr %<>% lapply(result_prep_tpl)
breast_result_values_stop3yr <- make_template_values(breast_results_stop3yr) # apply to a list of models



breast_X1Yr_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort")]) %>%
  parallel_apply(templates[c( 
                             "o.exclude1YearTotalDeath", "o.exclude1YearBreastDeath"
 )]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates[c(
                             "f.exclude1Year")]) 

breast_X1Yr_results <- breast_X1Yr_models %>% lapply(run_model, dataset=breastX1Yr)
breast_X1Yr_results %<>% lapply(result_prep_tpl)
breast_X1Yr_result_values <- make_template_values(breast_X1Yr_results)

breast_X3Yr_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort")]) %>%
  parallel_apply(templates[c( 
    "o.exclude3YearTotalDeath", "o.exclude3YearBreastDeath"
  )]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates[c(
    "f.exclude3Year")]) 

breast_X3Yr_results <- breast_X3Yr_models %>% lapply(run_model, dataset=breastX3Yr)
breast_X3Yr_results %<>% lapply(result_prep_tpl)
breast_X3Yr__result_values <- make_template_values(breast_X3Yr_results)




# adjusted for hormone receptor

# HLI continuous
breast_fa_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort2005")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.breastDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", 
                           "c.tnmstage", 
                           "c.bcvars",
                           "c.molecularsubtype")]) %>% 
  parallel_apply(templates[c( 
    "f.all")]) 

breast_fa_results <- breast_fa_models %>% lapply(run_model, dataset=breast2005)


breast_fa_results %<>% lapply(result_prep_tpl)

breast_fa_result_values <- make_template_values(breast_fa_results) 

# subgroup analysis for hormone receptor


# HLI continuous
breast_hr_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort2005")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.breastDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", 
                           "c.tnmstage", 
                           "c.bcvars",
                           "c.her2")]) %>% 
  parallel_apply(templates[c( 
                             #"f.ernegprneg",
                             #"f.ernegprpos",
                             #"f.erposprneg",
                             "f.erposprpos")]) 

breast_hr_results <- breast_hr_models %>% lapply(run_model, dataset=breast2005)


breast_hr_results %<>% lapply(result_prep_tpl)

breast_hr_result_values <- make_template_values(breast_hr_results) 

# HLI categorical

breast_cat_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.breastDeath")]) %>% 
  serial_apply(templates[c("c.hliCategories", "c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates[c("f.all", "f.prediag5", "f.qtoexit5")]) 

breast_cat_results <- breast_cat_models %>% lapply(run_model, dataset=breast)
breast_cat_results %<>% lapply(result_prep_tpl)
breast_cat_result_values <- make_template_values(breast_cat_results)
names(breast_cat_result_values) <- paste0("cat.", names(breast_cat_result_values))


breast_tert_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.breastDeath")]) %>% 
  serial_apply(templates[c("c.hliTertiles", "c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates[c("f.all", "f.prediag5", "f.qtoexit5")]) 

breast_tert_results <- breast_tert_models %>% lapply(run_model, dataset=breast)
breast_tert_results %<>% lapply(result_prep_tpl)
breast_tert_result_values <- make_template_values(breast_tert_results)
names(breast_tert_result_values) <- paste0("cat.", names(breast_tert_result_values))


breast_x1y_cat_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort")]) %>%
  parallel_apply(templates[c("o.exclude1YearTotalDeath", "o.exclude1YearBreastDeath")]) %>% 
  serial_apply(templates[c("c.hliCategories", "c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates[c("f.exclude1Year")]) 

breast_x1y_cat_results <- breast_x1y_cat_models %>% lapply(run_model, dataset=breast)
breast_x1y_cat_results %<>% lapply(result_prep_tpl)
breast_x1y_cat_result_values <- make_template_values(breast_x1y_cat_results)
names(breast_x1y_cat_result_values) <- paste0("cat.", names(breast_x1y_cat_result_values))


# Subgroup analysis by TNM stage (HLI continuous)

breast_tnm_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.breastDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.bcvars", "f.all")]) %>% 
  parallel_apply(templates[c("f.tnmI", "f.tnmII", "f.tnmIII", "f.tnmIV", "f.tnmUnknown")]) 

breast_tnm_results <- breast_tnm_models %>% lapply(run_model, dataset=breast)


breast_tnm_results %<>% lapply(result_prep_tpl)

breast_tnm_result_values <- make_template_values(breast_tnm_results)



#### Colorectal cancer models ####

# HLI continuous
colorectal_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.crccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.colorectalDeath"
                             )]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.all")]) 

colorectal_results <- colorectal_models %>% lapply(run_model, dataset=colorectal)
colorectal_results %<>% lapply(result_prep_tpl)
colorectal_result_values <- make_template_values(colorectal_results) # apply to a list of models

colorectal_results_stop3Yr <- colorectal_models %>% lapply(run_model, dataset=colorectalX3YrNewCases)
colorectal_results_stop3Yr %<>% lapply(result_prep_tpl)
colorectal_result_values_stop3Yr <- make_template_values(colorectal_results_stop3Yr) # apply to a list of models



colorectal_X1Yr_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.crccohort")]) %>%
  parallel_apply(templates[c(
                             "o.exclude1YearTotalDeath", "o.exclude1YearColorectalDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.exclude1Year")]) 

colorectal_X1Yr_results <- colorectal_X1Yr_models %>% lapply(run_model, dataset=colorectalX1Yr)
colorectal_X1Yr_results %<>% lapply(result_prep_tpl)
colorectal_X1Yr_result_values <- make_template_values(colorectal_X1Yr_results) 



colorectal_X3Yr_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.crccohort")]) %>%
  parallel_apply(templates[c(
    "o.exclude3YearTotalDeath", "o.exclude3YearColorectalDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.exclude3Year")]) 

colorectal_X3Yr_results <- colorectal_X3Yr_models %>% lapply(run_model, dataset=colorectalX3Yr)
colorectal_X3Yr_results %<>% lapply(result_prep_tpl)
colorectal_X3Yr_result_values <- make_template_values(colorectal_X3Yr_results) 

# HLI categorical
colorectal_cat_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.crccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.colorectalDeath")]) %>% 
  serial_apply(templates[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.all", "f.prediag5", "f.qtoexit5")]) 

colorectal_cat_results <- colorectal_cat_models %>% lapply(run_model, dataset=colorectal)
colorectal_cat_results %<>% lapply(result_prep_tpl)
colorectal_cat_result_values <- make_template_values(colorectal_cat_results)
names(colorectal_cat_result_values) <- paste0("cat.", names(colorectal_cat_result_values))


colorectal_tert_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.crccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.colorectalDeath")]) %>% 
  serial_apply(templates[c("c.hliTertiles", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.all", "f.prediag5", "f.qtoexit5")]) 

colorectal_tert_results <- colorectal_tert_models %>% lapply(run_model, dataset=colorectal)
colorectal_tert_results %<>% lapply(result_prep_tpl)
colorectal_tert_result_values <- make_template_values(colorectal_tert_results)
names(colorectal_tert_result_values) <- paste0("cat.", names(colorectal_tert_result_values))


colorectal_x1y_cat_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.crccohort")]) %>%
  parallel_apply(templates[c("o.exclude1YearTotalDeath", "o.exclude1YearColorectalDeath")]) %>% 
  serial_apply(templates[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.exclude1Year")]) 

colorectal_x1y_cat_results <- colorectal_x1y_cat_models %>% lapply(run_model, dataset=colorectal)
colorectal_x1y_cat_results %<>% lapply(result_prep_tpl)
colorectal_x1y_cat_result_values <- make_template_values(colorectal_x1y_cat_results)
names(colorectal_x1y_cat_result_values) <- paste0("cat.", names(colorectal_x1y_cat_result_values))

# Subgroup analysis by SEER stage

colorectal_seer_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.crccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.colorectalDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", "f.all")]) %>% 
  parallel_apply(templates[c("f.seerLocalized", "f.seerRegional", "f.seerDistant", "f.seerUnknown")]) 

colorectal_seer_results <- colorectal_seer_models %>% lapply(run_model, dataset=colorectal)


colorectal_seer_results %<>% lapply(result_prep_tpl)

colorectal_seer_result_values <- make_template_values(colorectal_seer_results)

#### Lung cancer models ####

# HLI continuous

lung_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.lccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.lungDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.all")]) 

lung_results <- lung_models %>% lapply(run_model, dataset=lung)
lung_results %<>% lapply(result_prep_tpl)
lung_result_values <- make_template_values(lung_results) # apply to a list of models


lung_results_stop3Yr <- lung_models %>% lapply(run_model, dataset=lungX3YrNewCases)
lung_results_stop3Yr %<>% lapply(result_prep_tpl)
lung_result_values_stop3Yr <- make_template_values(lung_results_stop3Yr) # apply to a list of models


lung_X1Yr_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.lccohort")]) %>%
  parallel_apply(templates[c( 
                             "o.exclude1YearTotalDeath", "o.exclude1YearLungDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.exclude1Year")]) 

lung_X1Yr_results <- lung_X1Yr_models %>% lapply(run_model, dataset=lungX1Yr)
lung_X1Yr_results %<>% lapply(result_prep_tpl)
lung_X1Yr_result_values <- make_template_values(lung_X1Yr_results)


lung_X3Yr_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.lccohort")]) %>%
  parallel_apply(templates[c( 
    "o.exclude3YearTotalDeath", "o.exclude3YearLungDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.exclude3Year")]) 

lung_X3Yr_results <- lung_X3Yr_models %>% lapply(run_model, dataset=lungX3Yr)
lung_X3Yr_results %<>% lapply(result_prep_tpl)
lung_X3Yr_result_values <- make_template_values(lung_X3Yr_results)



# HLI categorical

lung_cat_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.lccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.lungDeath")]) %>% 
  serial_apply(templates[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.all", "f.prediag5", "f.qtoexit5")]) 

lung_cat_results <- lung_cat_models %>% lapply(run_model, dataset=lung)
lung_cat_results %<>% lapply(result_prep_tpl)
lung_cat_result_values <- make_template_values(lung_cat_results)
names(lung_cat_result_values) <- paste0("cat.", names(lung_cat_result_values))

lung_tert_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.lccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.lungDeath")]) %>% 
  serial_apply(templates[c("c.hliTertiles", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.all", "f.prediag5", "f.qtoexit5")]) 

lung_tert_results <- lung_tert_models %>% lapply(run_model, dataset=lung)
lung_tert_results %<>% lapply(result_prep_tpl)
lung_tert_result_values <- make_template_values(lung_tert_results)
names(lung_tert_result_values) <- paste0("cat.", names(lung_tert_result_values))

lung_x1y_cat_models <- list(base_model) %>% 
  parallel_apply(templates[c("o.exclude1YearTotalDeath", "o.exclude1YearLungDeath")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.lungDeath")]) %>% 
  serial_apply(templates[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates[c("f.exclude1Year")]) 

lung_x1y_cat_results <- lung_x1y_cat_models %>% lapply(run_model, dataset=lung)
lung_x1y_cat_results %<>% lapply(result_prep_tpl)
lung_x1y_cat_result_values <- make_template_values(lung_x1y_cat_results)
names(lung_x1y_cat_result_values) <- paste0("cat.", names(lung_x1y_cat_result_values))



# Subgroup analysis by SEER stage

lung_seer_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.lccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.lungDeath")]) %>% 
  serial_apply(templates[c("c.hliContinuous", "f.all")]) %>% 
  parallel_apply(templates[c("f.seerLocalized", "f.seerRegional", "f.seerDistant", "f.seerUnknown")]) 

lung_seer_results <- lung_seer_models %>% lapply(run_model, dataset=lung)


lung_seer_results %<>% lapply(result_prep_tpl)

lung_seer_result_values <- make_template_values(lung_seer_results)

#### All result values ####


all_result_values <- c(breast_result_values,
                       breast_cat_result_values,
                       breast_tnm_result_values,
                       breast_hr_result_values,
                       breast_fa_result_values,
                       colorectal_result_values,
                       colorectal_cat_result_values,
                       colorectal_seer_result_values,
                       lung_result_values,
                       lung_cat_result_values,
                       lung_seer_result_values,
                       breast_X1Yr_result_values,
                       breast_X3Yr__result_values,
                       colorectal_X1Yr_result_values,
                       colorectal_X3Yr_result_values,
                       lung_X1Yr_result_values,
                       lung_X3Yr_result_values,
                       breast_tert_result_values,
                       colorectal_tert_result_values,
                       lung_tert_result_values)



updateOfficeTemplate("./output/table_4_template.docx", "./output/table_4.docx", update_values = all_result_values)
updateOfficeTemplate("./output/table_6_template.docx", "./output/table_6.docx", update_values = all_result_values)
updateOfficeTemplate("./output/table_14_template.docx", "./output/table_14.docx", update_values = all_result_values)

# HLI excluding single factors ----

breast_excluding_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.bccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.breastDeath")]) %>% 
  parallel_apply(templates[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  serial_apply(templates[c("c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates[c("f.all")])

breast_excluding_results <- breast_excluding_models %>% lapply(run_model, dataset=breast)
breast_excluding_results %<>% lapply(result_prep_tpl)
breast_excluding_result_values <- make_template_values(breast_excluding_results)

colorectal_excluding_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.crccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.colorectalDeath")]) %>% 
  parallel_apply(templates[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  serial_apply(templates[c("c.seerstage")]) %>% 
  parallel_apply(templates[c("f.all")])

colorectal_excluding_results <- colorectal_excluding_models %>% lapply(run_model, dataset=colorectal)
colorectal_excluding_results %<>% lapply(result_prep_tpl)
colorectal_excluding_results_values <- make_template_values(colorectal_excluding_results)

lung_excluding_models <- list(base_model) %>% 
  parallel_apply(templates[c("d.lccohort")]) %>%
  parallel_apply(templates[c("o.totalDeath", "o.lungDeath")]) %>% 
  parallel_apply(templates[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  serial_apply(templates[c("c.seerstage")]) %>% 
  parallel_apply(templates[c("f.all")])

lung_excluding_results <- lung_excluding_models %>% lapply(run_model, dataset=lung)
lung_excluding_results %<>% lapply(result_prep_tpl)
lung_excluding_results_values <- make_template_values(lung_excluding_results)


excluding_results_values <- c(breast_excluding_result_values,
                              colorectal_excluding_results_values,
                              lung_excluding_results_values)
updateOfficeTemplate("./output/table_9_template.docx", "./output/table_9.docx", update_values = excluding_results_values)




excluding_time_cat_values <- c(breast_x1y_cat_result_values,
                               colorectal_x1y_cat_result_values,
                               lung_x1y_cat_result_values,
                               breast_cat_result_values,
                               colorectal_cat_result_values,
                               lung_cat_result_values
)
                               
updateOfficeTemplate("./output/table_12_template.docx", "./output/table_12.docx", update_values = excluding_time_cat_values)

# Competing risks ----

library(cmprsk)
source("./r_docs/cmprsk_add_on.R")

covarsmatrix_bc <- cbind(breast$hliScore,
                        breast$ageDiagnosis,
                        factor2ind(breast$tnmAnatomicStage, "I"),
                        factor2ind(breast$menopausalStatus),
                        breast$ageMenarche,
                        factor2ind(breast$hrtStatus),
                        factor2ind(breast$ocEverUse),
                        factor2ind(breast$familyHistBC),
                        breast$parity
                        )

bcD.cmprsk <- cmprsk::crr(breast$followUpTimeDays, breast$breastDeathCr, failcode = 1, covarsmatrix_bc)
summary(bcD.cmprsk)

covarsmatrix_crc <- cbind(colorectal$hliScore,
                          colorectal$ageDiagnosis,
                          factor2ind(colorectal$seerStage, "Localized")
)

crcD.cmprsk <- cmprsk::crr(colorectal$followUpTimeDays, colorectal$colorectalDeathCr, failcode = 1, covarsmatrix_crc)
summary(crcD.cmprsk)


covarsmatrix_lc <- cbind(lung$hliScore,
                         lung$ageDiagnosis,
                         factor2ind(lung$seerStage, "Localized")
)

lcD.cmprsk <- cmprsk::crr(lung$followUpTimeDays, lung$lungDeathCr, failcode = 1, covarsmatrix_lc)
summary(lcD.cmprsk)


# Median follow-up time

survtime.bc.tD <- quantile(prodlim(Hist(followUpTimeDays, totalDeath)~1,data=breast,reverse=TRUE))
survtime.bc.bcD <- quantile(prodlim(Hist(followUpTimeDays, breastDeath)~1,data=breast,reverse=TRUE))
survtime.crc.tD <- quantile(prodlim(Hist(followUpTimeDays, totalDeath)~1,data=colorectal,reverse=TRUE))
survtime.crc.crcD <- quantile(prodlim(Hist(followUpTimeDays, colorectalDeath)~1,data=colorectal,reverse=TRUE))
survtime.lc.tD <- quantile(prodlim(Hist(followUpTimeDays, totalDeath)~1,data=lung,reverse=TRUE))
survtime.lc.lcD <- quantile(prodlim(Hist(followUpTimeDays, lungDeath)~1,data=lung,reverse=TRUE))

medsurvyears <- function(prodlimObj){
  medsurvdays <- prodlimObj$quantiles.survival$quantile[3]
  print(medsurvdays/365.25)
}

medsurvyears(survtime.bc.tD)
medsurvyears(survtime.bc.bcD)
medsurvyears(survtime.crc.tD)
medsurvyears(survtime.crc.crcD)
medsurvyears(survtime.lc.tD)
medsurvyears(survtime.lc.lcD)


# Testing interaction for lag time

bc_td_intx <- coxph(Surv(followUpTimeDays, totalDeath) ~ hliScore +
        ageDiagnosis +
        education +
        height +
        strata(wave) +
        strata(tnmAnatomicStage) +
        menopausalStatus +
        ageMenarche +
        hrtStatus +
        ocEverUse +
        familyHistBC +
        parity +
        breastfeeding +
        hliScore*lagYearsMedian,
      data = breast
)

bc_td <- coxph(Surv(followUpTimeDays, totalDeath) ~ hliScore +
        ageDiagnosis +
        height +
        education +
        strata(wave) +
        strata(tnmAnatomicStage) +
        menopausalStatus +
        ageMenarche +
        hrtStatus +
        ocEverUse +
        familyHistBC +
        parity +
        breastfeeding ,
      data = breast
)

anova( bc_td, bc_td_intx)

bc_bcd_intx <- coxph(Surv(followUpTimeDays, breastDeath) ~ hliScore +
                      ageDiagnosis +
                      education +
                       height +
                      strata(wave) +
                      strata(tnmAnatomicStage) +
                      menopausalStatus +
                      ageMenarche +
                      hrtStatus +
                      ocEverUse +
                      familyHistBC +
                      parity +
                      breastfeeding +
                      hliScore*lagYears,
                    data = breast
)

bc_bcd <- coxph(Surv(followUpTimeDays, breastDeath) ~ hliScore +
                 ageDiagnosis +
                 education +
                  height +
                 strata(wave) +
                 strata(tnmAnatomicStage) +
                 menopausalStatus +
                 ageMenarche +
                 hrtStatus +
                 ocEverUse +
                 familyHistBC +
                 parity +
                 breastfeeding,
               data = breast
)

anova( bc_bcd, bc_bcd_intx)


crc_td_intx <- coxph(Surv(followUpTimeDays, totalDeath) ~ hliScore +
        ageDiagnosis +
        education +
          height +
        strata(wave) +
        strata(seerStage) +
        hliScore*lagYears,
      data = colorectal
)
crc_td <- coxph(Surv(followUpTimeDays, totalDeath) ~ hliScore +
        ageDiagnosis +
        education +
          height +
        strata(wave) +
        strata(seerStage),
      data = colorectal
)

anova(crc_td_intx, crc_td)

crc_crcd_intx <- coxph(Surv(followUpTimeDays, colorectalDeath) ~ hliScore +
        ageDiagnosis +
        education +
          height +
        strata(wave) +
        strata(seerStage) +
        hliScore*lagYears,
      data = colorectal
)

crc_crcd <- coxph(Surv(followUpTimeDays, colorectalDeath) ~ hliScore +
        ageDiagnosis +
        education +
          height +
        strata(wave) +
        strata(seerStage),
      data = colorectal
)

anova(crc_crcd_intx, crc_crcd)

lc_td_intx <- coxph(Surv(followUpTimeDays, totalDeath) ~ hliScore +
        ageDiagnosis +
        education +
          height +
        strata(wave) +
        strata(seerStage) +
        hliScore*lagYears,
      data = lung
)
lc_td <- coxph(Surv(followUpTimeDays, totalDeath) ~ hliScore +
        ageDiagnosis +
        education +
          height +
        strata(wave) +
        strata(seerStage),
      data = lung
)

anova(lc_td_intx, lc_td)

lc_lcd_intx <- coxph(Surv(followUpTimeDays, lungDeath) ~ hliScore +
        ageDiagnosis +
        education +
          height +
        strata(wave) +
        strata(seerStage) +
        hliScore*lagYears,
      data = lung
) 
lc_lcd <- coxph(Surv(followUpTimeDays, lungDeath) ~ hliScore +
        ageDiagnosis +
        education +
          height +
        strata(wave) +
        strata(seerStage),
      data = lung
) 

anova(lc_lcd_intx, lc_lcd)

# Complete case datasets

breast_complete <- function(dataframe) {
  dataframe %>%
  select(hliScore,
         hliCategories,
         ageDiagnosis, 
         education, 
         height, 
         wave, 
         tnmAnatomicStage, 
         menopausalStatus, 
         ageMenarche, 
         hrtStatus, 
         ocEverUse, 
         familyHistBC, 
         parity, 
         breastfeeding,
         totalDeath,
         breastDeath,
         followUpTimeDays,
         followUpTimeDaysX1Year,
         followUpTimeDaysX3Year,
         dateDiagnosis,
         dateQuestionnaire,
         wave,
         lagYears) %>%
  na.omit()
}

breastComplete <- breast_complete(breast)
breastX1YrComplete <- breast_complete(breastX1Yr)
breastX3YrComplete <- breast_complete(breastX3Yr)
breastX3YrNewCasesComplete <- breast_complete(breastX3YrNewCases)

colorectal_complete <- function(dataframe) {
  dataframe %>%
  select(hliScore,
         hliCategories,
         ageDiagnosis, 
         education, 
         height, 
         wave, 
         seerStage,
         totalDeath,
         colorectalDeath,
         followUpTimeDays,
         followUpTimeDaysX1Year,
         followUpTimeDaysX3Year,
         dateDiagnosis,
         dateQuestionnaire,
         wave,
         lagYears) %>%
  na.omit()
}

colorectalComplete <- colorectal_complete(colorectal)
colorectalX1YrComplete <- colorectal_complete(colorectalX1Yr)
colorectalX3YrComplete <- colorectal_complete(colorectalX3Yr)
colorectalX3YrNewCasesComplete <- colorectal_complete(colorectalX3YrNewCases)


lung_complete <- function(dataframe){
  dataframe %>%
  select(hliScore,
         hliCategories,
         ageDiagnosis, 
         education, 
         height, 
         wave, 
         seerStage,
         totalDeath,
         lungDeath,
         followUpTimeDays,
         followUpTimeDaysX1Year,
         followUpTimeDaysX3Year,
         dateDiagnosis,
         dateQuestionnaire,
         wave,
         lagYears) %>%
  na.omit()
}
lungComplete <- lung_complete(lung)
lungX1YrComplete <- lung_complete(lungX1Yr)
lungX3YrComplete <- lung_complete(lungX3Yr)
lungX3YrNewCasesComplete <- lung_complete(lungX3YrNewCases)

 
exclude_followUpTime <- function(dataframe, daysExcluded){
  dataframe %>%
    filter(followUpTimeDays > daysExcluded)
}

breastCompleteX1Yr <- exclude_followUpTime(breastComplete, 365.25)
breastCompleteX3Yr <- exclude_followUpTime(breastComplete, 1095.75)

colorectalCompleteX1Yr <- exclude_followUpTime(colorectalComplete, 365.25)
colorectalCompleteX3Yr <- exclude_followUpTime(colorectalComplete, 730.5)

lungCompleteX1Yr <- exclude_followUpTime(lungComplete, 365.25)
lungCompleteX3Yr <- exclude_followUpTime(lungComplete, 730.5)


# Stop inclusion of cases 3 years before end of study period

breastX3YrNewCasesComplete <- breastComplete %>% filter(dateDiagnosis <= "2017-12-31")
colorectalX3YrNewCasesComplete <- colorectalComplete %>% filter(dateDiagnosis <= "2017-12-31")
lungX3YrNewCasesComplete <- lungComplete %>% filter(dateDiagnosis <= "2017-12-31")




## Testing stage interaction ----


bc.td <- coxph(Surv(followUpTimeDays, totalDeath) ~ ageDiagnosis + 
                 hliScore + 
                 tnmAnatomicStage + 
                 education + 
                 height + 
                 strata(wave) + 
                 menopausalStatus + 
                 ageMenarche + 
                 hrtStatus + 
                 ocEverUse + 
                 familyHistBC + 
                 parity + 
                 breastfeeding, 
               data = breast)

bc.td.intx <- coxph(Surv(followUpTimeDays, totalDeath) ~ ageDiagnosis + 
                      hliScore:tnmAnatomicStage + 
                      tnmAnatomicStage +
                      education + 
                      height + 
                      strata(wave) + 
                      menopausalStatus + 
                      ageMenarche + 
                      hrtStatus + 
                      ocEverUse + 
                      familyHistBC + 
                      parity + 
                      breastfeeding, 
                    data = breast)

anova(bc.td, bc.td.intx)


bc.bcd <- coxph(Surv(followUpTimeDays, breastDeath) ~ ageDiagnosis + 
                 hliScore + 
                 strata(tnmAnatomicStage) + 
                 education + 
                 height + 
                 strata(wave) + 
                 menopausalStatus + 
                 ageMenarche + 
                 hrtStatus + 
                 ocEverUse + 
                 familyHistBC + 
                 parity + 
                 breastfeeding, 
               data = breast)

bc.bcd.intx <- coxph(Surv(followUpTimeDays, breastDeath) ~ ageDiagnosis + 
                      hliScore:strata(tnmAnatomicStage) + 
                      strata(tnmAnatomicStage) +
                      education + 
                      height + 
                      strata(wave) + 
                      menopausalStatus + 
                      ageMenarche + 
                      hrtStatus + 
                      ocEverUse + 
                      familyHistBC + 
                      parity + 
                      breastfeeding, 
                    data = breast)

anova(bc.bcd, bc.bcd.intx)



crc.td <- coxph(Surv(followUpTimeDays, totalDeath) ~ ageDiagnosis +
                  hliScore +
                  education +
                  height +
                  strata(wave) +
                  strata(seerStage),
                data=colorectal)

crc.td.intx <- coxph(Surv(followUpTimeDays, totalDeath) ~ ageDiagnosis +
                  hliScore +
                  education +
                  height +
                  strata(wave) +
                  strata(seerStage)+
                  hliScore:strata(seerStage),
                data=colorectal)

anova(crc.td, crc.td.intx)



lc.td <- coxph(Surv(followUpTimeDays, totalDeath) ~ ageDiagnosis +
                  hliScore +
                  education +
                  height +
                  strata(wave) +
                  strata(seerStage),
                data=lung)

lc.td.intx <- coxph(Surv(followUpTimeDays, totalDeath) ~ ageDiagnosis +
                       hliScore +
                       education +
                       height +
                       strata(wave) +
                       strata(seerStage)+
                       hliScore:strata(seerStage),
                     data=lung)

anova(lc.td, lc.td.intx)


# RCS ----

## breast

ddist <- datadist(breastComplete)
options(datadist="ddist")
bc_td_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ rcs(hliScore, 5) + 
                          ageDiagnosis + 
                          education + 
                          height +
                          menopausalStatus +
                          ageMenarche + 
                          hrtStatus +
                          ocEverUse +
                          familyHistBC +
                          parity +
                          breastfeeding +
                          strat(wave) +
                          strat(tnmAnatomicStage), 
                        data=breastComplete)

bc_td_rcs_plot <- ggplot(rms:::Predict(bc_td_rcs_model, hliScore=seq(1, 20),
                                        fun=exp),
                          xlab= "Healthy Lifestyle Index score",
                          ylab="Hazard Ratio") + 
  ggtitle("BC, all-cause, 5 knots")


bc_bcd_rcs_model <- cph(Surv(followUpTimeDays, breastDeath) ~ rcs(hliScore, 5) + 
                            ageDiagnosis + 
                            education + 
                            height +
                          menopausalStatus +
                          ageMenarche + 
                          hrtStatus +
                          ocEverUse +
                          familyHistBC +
                          parity +
                          breastfeeding +
                          strat(wave) +
                          strat(tnmAnatomicStage), 
                          data=breastComplete)

bc_bcd_rcs_plot <- ggplot(rms:::Predict(bc_bcd_rcs_model, hliScore=seq(1, 20),
                                          fun=exp),
                            xlab= "Healthy Lifestyle Index score",
                            ylab="Hazard Ratio") + 
  ggtitle("BC, cause-specific death, 5 knots")

## colorectal
ddist <- datadist(colorectalComplete)
options(datadist="ddist")
crc_td_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ rcs(hliScore, 5) + 
                           ageDiagnosis + 
                           education + 
                           height +
                           strat(wave) +
                           strat(seerStage), 
                         data=colorectalComplete)

crc_td_rcs_plot <- ggplot(rms:::Predict(crc_td_rcs_model, hliScore=seq(1, 20),
                                         fun=exp),
                           xlab= "Healthy Lifestyle Index score",
                           ylab="Hazard Ratio") + 
  ggtitle("CRC, all-cause, 5 knots")


crc_crcd_rcs_model <- cph(Surv(followUpTimeDays, colorectalDeath) ~ rcs(hliScore, 5) + 
                            ageDiagnosis + 
                            education + 
                            height +
                            strat(wave) +
                            strat(seerStage), 
                          data=colorectalComplete)

crc_crcd_rcs_plot <- ggplot(rms:::Predict(crc_crcd_rcs_model, hliScore=seq(1, 20),
                                          fun=exp),
                            xlab= "Healthy Lifestyle Index score",
                            ylab="Hazard Ratio") + 
  ggtitle("CRC, cause-specific death, 5 knots")

## lung
ddist <- datadist(lungComplete)
options(datadist="ddist")
lung_td_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ rcs(hliScore, 5) + 
                           ageDiagnosis + 
                           education + 
                           height +
                           strat(wave) +
                           strat(seerStage), 
                         data=lungComplete)

lung_td_rcs_plot <- ggplot(rms:::Predict(lung_td_rcs_model, hliScore=seq(1, 20),
                                  fun=exp),
                    xlab= "Healthy Lifestyle Index score",
                    ylab="Hazard Ratio") + 
  ggtitle("LC, all-cause, 5 knots")


lung_lcd_rcs_model <- cph(Surv(followUpTimeDays, lungDeath) ~ rcs(hliScore, 3) + 
                           ageDiagnosis + 
                           education + 
                           height +
                           strat(wave) +
                           strat(seerStage), 
                         data=lungComplete)

lung_lcd_rcs_plot <- ggplot(rms:::Predict(lung_lcd_rcs_model, hliScore=seq(1, 20),
                                         fun=exp),
                           xlab= "Healthy Lifestyle Index score",
                           ylab="Hazard Ratio") + 
  ggtitle("LC, cause-specific death, 5 knots")


# RCS with interaction ----
## using ::interactionRCS

## Interaction with prediagnostic interval

# breast cancer 

bc_td_lagintx_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ hliScore*rcs(lagYears, 3) + 
                         ageDiagnosis + 
                         education + 
                         height +
                         menopausalStatus +
                         ageMenarche + 
                         hrtStatus +
                         ocEverUse +
                         familyHistBC +
                         parity +
                         breastfeeding +
                         strat(wave) +
                         strat(tnmAnatomicStage), 
                       data=breast)
bc_td_lagintx_rcs_delta <- intEST(var2values = c(0:25),
                              model = bc_td_lagintx_rcs_model,
                              data = breast, var1 = "hliScore", var2 = "lagYears",
                              ci=TRUE, conf = 0.95, ci.method = "delta")


bc_td_lagintx_rcs_plot <- ggplot(bc_td_lagintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", linewidth=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Prediagnostic interval (years)", y = "HR all-cause mortality", title = "Breast cancer cases") +
  scale_x_continuous(breaks = seq(0, 25, by = 2), limits = c(0,24)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 



# check if loglinear model is a better fit compared to rcs ----
bc_td_lagintx_loglin_model <- cph(Surv(followUpTimeDays, totalDeath) ~ hliScore*lagYears + 
                                 ageDiagnosis + 
                                 education + 
                                 height +
                                 menopausalStatus +
                                 ageMenarche + 
                                 hrtStatus +
                                 ocEverUse +
                                 familyHistBC +
                                 parity +
                                 breastfeeding +
                                 strat(wave) +
                                 strat(tnmAnatomicStage) , 
                               data=breast)


AIC(bc_td_lagintx_loglin_model)
AIC(bc_td_lagintx_rcs_model)


bc_bcd_lagintx_rcs_model <- cph(Surv(followUpTimeDays, breastDeath) ~ hliScore*rcs(lagYears, 3) + 
                                 ageDiagnosis + 
                                 education + 
                                 height +
                                 menopausalStatus +
                                 ageMenarche + 
                                 hrtStatus +
                                 ocEverUse +
                                 familyHistBC +
                                 parity +
                                 breastfeeding +
                                 strat(wave) +
                                 strat(tnmAnatomicStage), 
                               data=breast)
bc_bcd_lagintx_rcs_delta <- intEST(var2values = c(0:25),
                                  model = bc_bcd_lagintx_rcs_model,
                                  data = breast, var1 = "hliScore", var2 = "lagYears",
                                  ci=TRUE, conf = 0.95, ci.method = "delta")


bc_bcd_lagintx_rcs_plot <- ggplot(bc_bcd_lagintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", linewidth=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Prediagnostic interval (years)", y = "HR breast cancer mortality", title = "Breast cancer cases") +
  scale_x_continuous(breaks = seq(0, 25, by = 2), limits = c(0,24)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 

# colorectal cancer

crc_td_lagintx_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ hliScore*rcs(lagYears, 3) + 
                          ageDiagnosis + 
                          education + 
                          height +
                          strat(wave) +
                          strat(seerStage), 
                        data=colorectal)

crc_td_lagintx_rcs_delta <- intEST(var2values = c(0:25),
                                   model = crc_td_lagintx_rcs_model,
                                   data = colorectal, var1 = "hliScore", var2 = "lagYears",
                                   ci=TRUE, conf = 0.95, ci.method = "delta")
crc_td_lagintx_rcs_plot <- ggplot(crc_td_lagintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) +
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", linewidth=0.75) +
    theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Prediagnostic interval (years)", y = "HR all-cause mortality", title = "Colorectal cancer cases") +
  scale_x_continuous(breaks = seq(0, 25, by = 2), limits = c(0,24)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 


crc_crcd_lagintx_rcs_model <- cph(Surv(followUpTimeDays, colorectalDeath) ~ hliScore*rcs(lagYears, 3) + 
                                  ageDiagnosis + 
                                  education + 
                                  height +
                                  strat(wave) +
                                  strat(seerStage), 
                                data=colorectal)

crc_crcd_lagintx_rcs_delta <- intEST(var2values = c(0:25),
                                   model = crc_crcd_lagintx_rcs_model,
                                   data = colorectal, var1 = "hliScore", var2 = "lagYears",
                                   ci=TRUE, conf = 0.95, ci.method = "delta")
crc_crcd_lagintx_rcs_plot <- ggplot(crc_crcd_lagintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", crc_td_lagintx_rcs_plot=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Prediagnostic interval (years)", y = "HR colorectal cancer mortality", title = "Colorectal cancer cases") +
  scale_x_continuous(breaks = seq(0, 25, by = 2), limits = c(0,24)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 


# lung cancer
lc_td_lagintx_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ hliScore*rcs(lagYears, 3) + 
                                  ageDiagnosis + 
                                  education + 
                                  height +
                                  strat(wave) +
                                  strat(seerStage), 
                                data=lung)

lc_td_lagintx_rcs_delta <- intEST(var2values = c(0:25),
                                   model = lc_td_lagintx_rcs_model,
                                   data = lung, var1 = "hliScore", var2 = "lagYears",
                                   ci=TRUE, conf = 0.95, ci.method = "delta")
lc_td_lagintx_rcs_plot <- ggplot(lc_td_lagintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", linewidth=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Prediagnostic interval (years)", y = "HR all-cause mortality", title = "Lung cancer cases") +
  scale_x_continuous(breaks = seq(0, 25, by = 2), limits = c(0,24)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 



lc_lcd_lagintx_rcs_model <- cph(Surv(followUpTimeDays, lungDeath) ~ hliScore*rcs(lagYears, 3) + 
                                 ageDiagnosis + 
                                 education + 
                                 height +
                                 strat(wave) +
                                 strat(seerStage), 
                               data=lung)

lc_lcd_lagintx_rcs_delta <- intEST(var2values = c(0:25),
                                  model = lc_lcd_lagintx_rcs_model,
                                  data = lung, var1 = "hliScore", var2 = "lagYears",
                                  ci=TRUE, conf = 0.95, ci.method = "delta")
lc_lcd_lagintx_rcs_plot <- ggplot(lc_lcd_lagintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", linewidth=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Prediagnostic interval (years)", y = "HR lung cancer mortality", title = "Lung cancer cases") +
  scale_x_continuous(breaks = seq(0, 25, by = 2), limits = c(0,24)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 


lagintx_panel_plot <- ggarrange(bc_td_lagintx_rcs_plot,
                                bc_bcd_lagintx_rcs_plot,
                                crc_td_lagintx_rcs_plot,
                                crc_crcd_lagintx_rcs_plot,
                                lc_td_lagintx_rcs_plot,
                                lc_lcd_lagintx_rcs_plot,
                                ncol = 2, nrow = 3)
png("./output/lagintx_panel_plot.png", units = "in", width = 10, height = 12, res = 1000)
annotate_figure(lagintx_panel_plot, top = text_grob(face = "bold", "Associations for HLI score across levels of prediagnostic interval"),
                bottom = text_grob(x = 0.05, just = "left", "\nAssociation between HLI score and mortality (all-cause, cause-specific) modelled with multiplicative interaction by \nprediagnostic interval. Prediagnostic interval was modelled with restricted cubic splines with three knots."))
dev.off()




## Interaction with q_to_exit days

# breast cancer 

bc_td_exitintx_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ hliScore*rcs(q_to_exit_years,3) + 
                                 ageDiagnosis + 
                                 education + 
                                 height +
                                 menopausalStatus +
                                 ageMenarche + 
                                 hrtStatus +
                                 ocEverUse +
                                 familyHistBC +
                                 parity +
                                 breastfeeding +
                                 strat(wave) +
                                 strat(tnmAnatomicStage), 
                               data=breast)
bc_td_exitintx_rcs_delta <- intEST(var2values = c(1:25),
                                  model = bc_td_exitintx_rcs_model,
                                  data = breast, var1 = "hliScore", var2 = "q_to_exit_years",
                                  ci=TRUE, conf = 0.95, ci.method = "delta")


bc_td_exitintx_rcs_plot <- ggplot(bc_td_exitintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", size=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Questionnaire to exit (years)", y = "HR all-cause mortality", title = "Breast cancer cases") +
  scale_x_continuous(breaks = seq(1, 25, by = 2), limits = c(1,25)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 



bc_bcd_exitintx_rcs_model <- cph(Surv(followUpTimeDays, breastDeath) ~ hliScore*rcs(q_to_exit_years, 3) + 
                                  ageDiagnosis + 
                                  education + 
                                  height +
                                  menopausalStatus +
                                  ageMenarche + 
                                  hrtStatus +
                                  ocEverUse +
                                  familyHistBC +
                                  parity +
                                  breastfeeding +
                                  strat(wave) +
                                  strat(tnmAnatomicStage), 
                                data=breast)
bc_bcd_exitintx_rcs_delta <- intEST(var2values = c(1:25),
                                   model = bc_bcd_exitintx_rcs_model,
                                   data = breast, var1 = "hliScore", var2 = "q_to_exit_years",
                                   ci=TRUE, conf = 0.95, ci.method = "delta")


bc_bcd_exitintx_rcs_plot <- ggplot(bc_bcd_exitintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", size=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Questionnaire to exit (years)", y = "HR breast cancer mortality", title = "Breast cancer cases") +
  scale_x_continuous(breaks = seq(1, 25, by = 2), limits = c(1,25)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 

# colorectal cancer

crc_td_exitintx_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ hliScore*rcs(q_to_exit_years, 3) + 
                                  ageDiagnosis + 
                                  education + 
                                  height +
                                  strat(wave) +
                                  strat(seerStage), 
                                data=colorectal)

crc_td_exitintx_rcs_delta <- intEST(var2values = c(1:25),
                                   model = crc_td_exitintx_rcs_model,
                                   data = colorectal, var1 = "hliScore", var2 = "q_to_exit_years",
                                   ci=TRUE, conf = 0.95, ci.method = "delta")
crc_td_exitintx_rcs_plot <- ggplot(crc_td_exitintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) +
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", size=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Questionnaire to exit (years)", y = "HR all-cause mortality", title = "Colorectal cancer cases") +
  scale_x_continuous(breaks = seq(1, 25, by = 2), limits = c(1,25)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 


crc_crcd_exitintx_rcs_model <- cph(Surv(followUpTimeDays, colorectalDeath) ~ hliScore*rcs(q_to_exit_years, 3) + 
                                    ageDiagnosis + 
                                    education + 
                                    height +
                                    strat(wave) +
                                    strat(seerStage), 
                                  data=colorectal)

crc_crcd_exitintx_rcs_delta <- intEST(var2values = c(1:25),
                                     model = crc_crcd_exitintx_rcs_model,
                                     data = colorectal, var1 = "hliScore", var2 = "q_to_exit_years",
                                     ci=TRUE, conf = 0.95, ci.method = "delta")
crc_crcd_exitintx_rcs_plot <- ggplot(crc_crcd_exitintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", size=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Questionnaire to exit (years)", y = "HR colorectal cancer mortality", title = "Colorectal cancer cases") +
  scale_x_continuous(breaks = seq(1, 25, by = 2), limits = c(1,25)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 


# lung cancer
lc_td_exitintx_rcs_model <- cph(Surv(followUpTimeDays, totalDeath) ~ hliScore*rcs(q_to_exit_years, 3) + 
                                 ageDiagnosis + 
                                 education + 
                                 height +
                                 strat(wave) +
                                 strat(seerStage), 
                               data=lung)

lc_td_exitintx_rcs_delta <- intEST(var2values = c(1:25),
                                  model = lc_td_exitintx_rcs_model,
                                  data = lung, var1 = "hliScore", var2 = "q_to_exit_years",
                                  ci=TRUE, conf = 0.95, ci.method = "delta")
lc_td_exitintx_rcs_plot <- ggplot(lc_td_exitintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", size=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Questionnaire to exit (years)", y = "HR all-cause mortality", title = "Lung cancer cases") +
  scale_x_continuous(breaks = seq(1, 25, by = 2), limits = c(1,25)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 



lc_lcd_exitintx_rcs_model <- cph(Surv(followUpTimeDays, lungDeath) ~ hliScore*rcs(q_to_exit_years, 3) + 
                                  ageDiagnosis + 
                                  education + 
                                  height +
                                  strat(wave) +
                                  strat(seerStage), 
                                data=lung)

lc_lcd_exitintx_rcs_delta <- intEST(var2values = c(1:25),
                                   model = lc_lcd_exitintx_rcs_model,
                                   data = lung, var1 = "hliScore", var2 = "q_to_exit_years",
                                   ci=TRUE, conf = 0.95, ci.method = "delta")
lc_lcd_exitintx_rcs_plot <- ggplot(lc_lcd_exitintx_rcs_delta, aes(x = Value, y = HR)) + 
  geom_line(linewidth = 0.75) + 
  geom_hline(yintercept=1, colour = "grey", linetype="dashed", size=0.75) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor=element_blank(), plot.title = element_text(size=12)) +
  geom_ribbon(aes(ymin = CI_L, ymax = CI_U), alpha= 0.1) +
  labs(x = "Questionnaire to exit (years)", y = "HR lung cancer mortality", title = "Lung cancer cases") +
  scale_x_continuous(breaks = seq(1, 25, by = 2), limits = c(1,25)) +
  scale_y_continuous(breaks = scales::breaks_width(0.05)) 


exitintx_panel_plot <- ggarrange(bc_td_exitintx_rcs_plot,
                                bc_bcd_exitintx_rcs_plot,
                                crc_td_exitintx_rcs_plot,
                                crc_crcd_exitintx_rcs_plot,
                                lc_td_exitintx_rcs_plot,
                                lc_lcd_exitintx_rcs_plot,
                                ncol = 2, nrow = 3)
png("./output/exitintx_panel_plot.png", units = "in", width = 10, height = 12, res = 1000)
annotate_figure(exitintx_panel_plot, top = text_grob(face = "bold", "Associations for HLI score across levels of questionnaire to exit interval"),
                bottom = text_grob(x = 0.05, just = "left", "\nAssociation between HLI score and mortality (all-cause, cause-specific) modelled with multiplicative interaction by \nquestionnaire to exit interval. Questionnaire to exit interval was modelled with restricted cubic splines with three knots."))
dev.off()


