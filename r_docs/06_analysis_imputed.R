
#### Imputed analysis ####

source("./r_docs/office-template-code.R")





# Templating model ----

## Base template ----

base_model = list(
  model = TRUE,
  filter = NULL,
  outcome_str = NULL,
  predictors = c("ageDiagnosis", "education", "height", "strata(wave)"),
  names = NULL
)




templates_mi <- list(
## Outcomes ----
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
    m$outcome_str <- "Surv(followUpTimeDays, colorectalDeath)"
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

  
  
  
## MI data  ----
  mi.bccohort = function(m){
    m$names %<>% c("bc")
    return(m)
  },
  mi.crccohort = function(m){
    m$names %<>% c("crc")
    return(m)
  },
mi.cccohort = function(m){
  m$names %<>% c("cc")
  return(m)
},
mi.rccohort = function(m){
  m$names %<>% c("rc")
  return(m)
},
  mi.lccohort = function(m){
    m$names %<>% c("lc")
    return(m)
  },  
mi.sclccohort = function(m){
  m$names %<>% c("sclc")
  return(m)
},  
mi.nsclccohort = function(m){
  m$names %<>% c("nsclc")
  return(m)
},  

  mi.bccohort.2005 = function(m){
    m$names %<>% c("bc2005")
    return(m)
  },  
  mi.bccohort.ms = function(m){
    m$names %<>% c("bc2005ms")
    return(m)
  }, 
  mi.bccohort.pm = function(m){
    m$names %<>% c("bc.pm")
    return(m)
  },  
mi.bccohort.prem = function(m){
  m$names %<>% c("bc.prem")
  return(m)
}, 
mi.bccohort.young = function(m){
  m$names %<>% c("bc.young")
  return(m)
},  
mi.bccohort.old = function(m){
  m$names %<>% c("bc.old")
  return(m)
},  
mi.crccohort.young = function(m){
  m$names %<>% c("crc.young")
  return(m)
},  
mi.crccohort.old = function(m){
  m$names %<>% c("crc.old")
  return(m)
},  
mi.lccohort.young = function(m){
  m$names %<>% c("lc.young")
  return(m)
},  
mi.lccohort.old = function(m){
  m$names %<>% c("lc.old")
  return(m)
},  


## Covariates ----

c.hliContinuous = function(m){
    m$predictors %<>% c("hliScore") 
    return(m)
  },
  c.hliCategories = function(m){
    m$predictors %<>% c("hliCategoriesLevels")
    m$names %<>% c("cat")
    
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
  c.bcvarspostmeno = function(m){
   m$predictors %<>% c("ageMenarche", "hrtStatus", "ocEverUse", "familyHistBC", "parity", "breastfeeding")
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
c.physcore = function(m){
  m$predictors %<>% c("physicalActivityScore") 
  m$names %<>% c(".physcore")
  return(m)
},
c.bmiscore = function(m){
  m$predictors %<>% c("bmiScore") 
  m$names %<>% c(".bmiscore")
  return(m)
},
c.smoscore = function(m){
  m$predictors %<>% c("smokingScore") 
  m$names %<>% c(".smoscore")
  return(m)
},
c.alcscore = function(m){
  m$predictors %<>% c("alcoholScore") 
  m$names %<>% c(".alcscore")
  return(m)
},
c.diescore = function(m){
  m$predictors %<>% c("dietScore") 
  m$names %<>% c(".diescore")
  return(m)
},
c.prophaz = function(m){
  m$predictors %<>% c("hliCategories*log(followUpTimeDays)") 
  m$names %<>% c("prophaz")
  return(m)
},
c.lagyears = function(m){
  m$predictors %<>% c("lagYears*hliScore") 
  m$names %<>% c(".lag")
  return(m)
},
c.agediagnosis = function(m){
  m$predictors %<>% c("ageDiagnosis") 
  return(m)
},

  
## Filters ----
### TNM stages
  f.all = function(m){
    m$filter = function(d){d}
    return(m)
  },
f.tnmI = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("tnmI")
  return(m)
},
f.tnmII = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("tnmII")
  return(m)
},
f.tnmIII = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("tnmIII")
  return(m)
},
f.tnmIV = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("tnmIV")
  return(m)
},
f.tnmUnknown = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("tnmUnk")
  return(m)
},
### SEER stages
f.seerLocalized = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("seerLoc")
  return(m)
},
f.seerRegional = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("seerReg")
  return(m)
},
f.seerDistant = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("seerDis")
  return(m)
},
f.seerUnknown = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("seerUnk")
  return(m)
},
### Hormone receptor status
f.erpos = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("erpos")
  return(m)
},
f.erneg = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("erneg")
  return(m)
},  
f.erposprneg = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("erposprneg")
  return(m)
},
f.tripleneg = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("tripleneg")
  return(m)
},
f.her2enrich = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("her2enrich")
  return(m)
},
f.prediagshort = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("short")
  return(m)
},
f.prediaglong = function(m){
  m$filter <- function(d){d}
  m$names %<>% c("long")
  return(m)
}
)



# Functions for running cox and formatting results vector ----

run_model_mi <- function(model, dataset){
  message("Running model: ", model$fullname)
  formula_str = paste(model$outcome_str , "~", paste0(model$predictors, collapse=" + "))
  list_results <- dataset %>% group_by(.imp) %>% do(model = coxph(as.formula(formula_str), data = . )) %>%
    model$filter() %>% 
    as.list() 
  pooled_results <- list_results %>% .[[-1]] %>% pool() %>%
    summary(conf.int=TRUE)
  return(list(table = pooled_results,
              model = model))
  
}

vectorize_pooled_results <- function(model_results){ 
  result_table <-  model_results$table %>%
    mutate(hazr = sprintf(exp(estimate), fmt = "%#.2f"),
           lci = sprintf(exp(estimate - 1.96*std.error), fmt = "%#.2f"),
           uci = sprintf(exp(estimate + 1.96*std.error), fmt = "%#.2f"),
           hrci = paste0(hazr, "(", lci, "-", uci, ")"),
           across(everything(), as.character)) %>% 
    tidyr::pivot_longer(cols = !matches("term"), 
                        names_to = "stat", 
                        values_to = "value") %>%
    mutate(names = paste0(model_results$model$fullname, ".", term, ".", stat))
  
  
  result_vector = result_table$value
  names(result_vector) <- result_table$names
  return(result_vector)
  
}

run_model_no_format <- function(model, dataset){
  message("Running model: ", model$fullname)
  formula_str = paste(model$outcome_str , "~", paste0(model$predictors, collapse=" + "))
  list_results <- dataset %>% group_by(.imp) %>% do(model = coxph(as.formula(formula_str), data = . )) %>%
    model$filter() %>% 
    as.list() 

  
}



# Breast cancer models ----
## Total mortality ----
### HLI continuous 
bc_td_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_results_mi <-  bc_td_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortality)
bc_td_results_mi %<>% lapply(vectorize_pooled_results)



bc_td_x1Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_x1Yr_results_mi <-  bc_td_x1Yr_model_mi %>% lapply(run_model_mi, dataset= breastX1YrTotalM)
bc_td_x1Yr_results_mi %<>% lapply(vectorize_pooled_results)


bc_td_x3Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude3YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_x3Yr_results_mi <-  bc_td_x3Yr_model_mi %>% lapply(run_model_mi, dataset= breastX3YrTotalM)
bc_td_x3Yr_results_mi %<>% lapply(vectorize_pooled_results)

bc_td_x3YrNew_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_x3YrNew_results_mi <-  bc_td_x3YrNew_model_mi %>% lapply(run_model_mi, dataset= breastX3YrNewCasesMi)
bc_td_x3YrNew_results_mi %<>% lapply(vectorize_pooled_results)

  # HLI categorical
bc_td_cat_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_cat_results_mi <- bc_td_cat_model_mi %>% lapply(run_model_mi, dataset=imputedLong100BreastTotalMortality)
bc_td_cat_results_mi %<>% lapply(vectorize_pooled_results)

bc_td_cat_x1Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_cat_x1Yr_results_mi <-  bc_td_cat_x1Yr_model_mi %>% lapply(run_model_mi, dataset= breastX1YrTotalM)
bc_td_cat_x1Yr_results_mi %<>% lapply(vectorize_pooled_results)


## Breast cancer mortality ----
### HLI continuous 
bc_bcd_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_bcd_results_mi <-  bc_bcd_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortality)
bc_bcd_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_x1Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearBreastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_bcd_x1Yr_results_mi <-  bc_bcd_x1Yr_model_mi %>% lapply(run_model_mi, dataset= breastX1YrCancerM)
bc_bcd_x1Yr_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_x3Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude3YearBreastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_bcd_x3Yr_results_mi <-  bc_bcd_x3Yr_model_mi %>% lapply(run_model_mi, dataset= breastX3YrCancerM)
bc_bcd_x3Yr_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_x3YrNew_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_bcd_x3YrNew_results_mi <-  bc_bcd_x3YrNew_model_mi %>% lapply(run_model_mi, dataset= breastX3YrNewCasesMi)
bc_bcd_x3YrNew_results_mi %<>% lapply(vectorize_pooled_results)

### HLI categorical 
  bc_bcd_cat_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_bcd_cat_results_mi <- bc_bcd_cat_model_mi %>% lapply(run_model_mi, dataset=imputedLong100BreastCancerMortality)
bc_bcd_cat_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_cat_x1Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearBreastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_bcd_cat_x1Yr_results_mi <-  bc_bcd_cat_x1Yr_model_mi %>% lapply(run_model_mi, dataset= breastX1YrCancerM)
bc_bcd_cat_x1Yr_results_mi %<>% lapply(vectorize_pooled_results)
# Colorectal cancer models ----

## Total mortality ----
### HLI continuous 
crc_td_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_td_results_mi <-  crc_td_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalTotalMortality)
crc_td_results_mi %<>% lapply(vectorize_pooled_results)


crc_td_x1Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_td_x1Yr_results_mi <-  crc_td_x1Yr_model_mi %>% lapply(run_model_mi, dataset= colorectalX1YrTotalM)
crc_td_x1Yr_results_mi %<>% lapply(vectorize_pooled_results)


crc_td_x3Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude3YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_td_x3Yr_results_mi <-  crc_td_x3Yr_model_mi %>% lapply(run_model_mi, dataset= colorectalX3YrTotalM)
crc_td_x3Yr_results_mi %<>% lapply(vectorize_pooled_results)

crc_td_x3YrNew_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_td_x3YrNew_results_mi <-  crc_td_x3YrNew_model_mi %>% lapply(run_model_mi, dataset= colorectalX3YrNewCasesMi)
crc_td_x3YrNew_results_mi %<>% lapply(vectorize_pooled_results)

#### HLI continuous colon and rectal

cc_td_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.cccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


cc_td_results_mi <-  cc_td_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColonTotalMortality)
cc_td_results_mi %<>% lapply(vectorize_pooled_results)

rc_td_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.rccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


rc_td_results_mi <-  rc_td_model_mi %>% lapply(run_model_mi, dataset= imputedLong100RectalTotalMortality)
rc_td_results_mi %<>% lapply(vectorize_pooled_results)


### HLI categorical 
crc_td_cat_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_td_cat_results_mi <- crc_td_cat_model_mi %>% lapply(run_model_mi, dataset=imputedLong100ColorectalTotalMortality)
crc_td_cat_results_mi %<>% lapply(vectorize_pooled_results)

crc_td_cat_x1y_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_td_cat_x1y_results_mi <- crc_td_cat_x1y_model_mi %>% lapply(run_model_mi, dataset=colorectalX1YrTotalM)
crc_td_cat_x1y_results_mi %<>% lapply(vectorize_pooled_results)


## Colorectal cancer mortality ----

### HLI continuous 
crc_crcd_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_crcd_results_mi <-  crc_crcd_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalCancerMortality)
crc_crcd_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_x1Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearColorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_crcd_x1Yr_results_mi <-  crc_crcd_x1Yr_model_mi %>% lapply(run_model_mi, dataset= colorectalX1YrCancerM)
crc_crcd_x1Yr_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_x3Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude3YearColorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_crcd_x3Yr_results_mi <-  crc_crcd_x3Yr_model_mi %>% lapply(run_model_mi, dataset= colorectalX3YrCancerM)
crc_crcd_x3Yr_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_x3YrNew_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_crcd_x3YrNew_results_mi <-  crc_crcd_x3YrNew_model_mi %>% lapply(run_model_mi, dataset= colorectalX3YrNewCasesMi)
crc_crcd_x3YrNew_results_mi %<>% lapply(vectorize_pooled_results)

#### colon and rectal

cc_crcd_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.cccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


cc_crcd_results_mi <-  cc_crcd_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColonCancerMortality)
cc_crcd_results_mi %<>% lapply(vectorize_pooled_results)

rc_crcd_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.rccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


rc_crcd_results_mi <-  rc_crcd_model_mi %>% lapply(run_model_mi, dataset= imputedLong100RectalCancerMortality)
rc_crcd_results_mi %<>% lapply(vectorize_pooled_results)


### HLI categorical 
crc_crcd_cat_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_crcd_cat_results_mi <- crc_crcd_cat_model_mi %>% lapply(run_model_mi, dataset=imputedLong100ColorectalCancerMortality)
crc_crcd_cat_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_cat_x1y_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearColorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_crcd_cat_x1y_results_mi <- crc_crcd_cat_x1y_model_mi %>% lapply(run_model_mi, dataset=colorectalX1YrCancerM)
crc_crcd_cat_x1y_results_mi %<>% lapply(vectorize_pooled_results)

# Lung cancer models ----
## Total mortality ----
### HLI continuous 
lc_td_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_td_results_mi <- lc_td_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungTotalMortality)
lc_td_results_mi %<>% lapply(vectorize_pooled_results)

lc_td_x1Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_td_x1Yr_results_mi <- lc_td_x1Yr_model_mi %>% lapply(run_model_mi, dataset= lungX1YrTotalM)
lc_td_x1Yr_results_mi %<>% lapply(vectorize_pooled_results)

lc_td_x3Yr_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude3YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_td_x3Yr_results_mi <- lc_td_x3Yr_model_mi %>% lapply(run_model_mi, dataset= lungX3YrTotalM)
lc_td_x3Yr_results_mi %<>% lapply(vectorize_pooled_results)

lc_td_x3YrNew_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_td_x3YrNew_results_mi <- lc_td_x3YrNew_model_mi %>% lapply(run_model_mi, dataset= lungX3YrNewCasesMi)
lc_td_x3YrNew_results_mi %<>% lapply(vectorize_pooled_results)

#### HLI continuous SCLC and NSCLC

sclc_td_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.sclccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


sclc_td_results_mi <- sclc_td_model_mi %>% lapply(run_model_mi, dataset= imputedLong100sclcTotalMortality)
sclc_td_results_mi %<>% lapply(vectorize_pooled_results)

nsclc_td_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.nsclccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


nsclc_td_results_mi <- nsclc_td_model_mi %>% lapply(run_model_mi, dataset= imputedLong100NsclcTotalMortality)
nsclc_td_results_mi %<>% lapply(vectorize_pooled_results)

### HLI categorical 
lc_td_cat_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_td_cat_results_mi <- lc_td_cat_model_mi %>% lapply(run_model_mi, dataset=imputedLong100LungTotalMortality)
lc_td_cat_results_mi %<>% lapply(vectorize_pooled_results)

lc_td_cat_x1y_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearTotalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_td_cat_x1y_results_mi <- lc_td_cat_x1y_model_mi %>% lapply(run_model_mi, dataset=lungX1YrTotalM)
lc_td_cat_x1y_results_mi %<>% lapply(vectorize_pooled_results)

## Lung cancer mortality ----

### HLI continuous 
lc_lcd_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_lcd_results_mi <- lc_lcd_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungCancerMortality)
lc_lcd_results_mi %<>% lapply(vectorize_pooled_results)


lc_lcd_x1Yr_model <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearLungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_lcd_x1Yr_results_mi <- lc_lcd_x1Yr_model %>% lapply(run_model_mi, dataset= lungX1YrCancerM)
lc_lcd_x1Yr_results_mi %<>% lapply(vectorize_pooled_results)

lc_lcd_x3Yr_model <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude3YearLungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_lcd_x3Yr_results_mi <- lc_lcd_x3Yr_model %>% lapply(run_model_mi, dataset= lungX3YrCancerM)
lc_lcd_x3Yr_results_mi %<>% lapply(vectorize_pooled_results)

lc_lcd_x3YrNew_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_lcd_x3YrNew_results_mi <- lc_lcd_x3YrNew_model_mi %>% lapply(run_model_mi, dataset= lungX3YrNewCasesMi)
lc_lcd_x3YrNew_results_mi %<>% lapply(vectorize_pooled_results)

#### HLI continuous SCLC and NSCLC

sclc_lcd_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.sclccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


sclc_lcd_results_mi <- sclc_lcd_model_mi %>% lapply(run_model_mi, dataset= imputedLong100sclcCancerMortality)
sclc_lcd_results_mi %<>% lapply(vectorize_pooled_results)

nsclc_lcd_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.nsclccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


nsclc_lcd_results_mi <- nsclc_lcd_model_mi %>% lapply(run_model_mi, dataset= imputedLong100NsclcCancerMortality)
nsclc_lcd_results_mi %<>% lapply(vectorize_pooled_results)


### HLI categorical 
lc_lcd_cat_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_lcd_cat_results_mi <- lc_lcd_cat_model_mi %>% lapply(run_model_mi, dataset=imputedLong100LungCancerMortality)
lc_lcd_cat_results_mi %<>% lapply(vectorize_pooled_results)

lc_lcd_cat_x1y_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.exclude1YearLungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliCategories", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_lcd_cat_x1y_results_mi <- lc_lcd_cat_x1y_model_mi %>% lapply(run_model_mi, dataset=lungX1YrTotalM)
lc_lcd_cat_x1y_results_mi %<>% lapply(vectorize_pooled_results)
# Stage subgroup analysis ----

## Breast cancer ----

### Total mortality

#### TNM I
bc_td_tnmI <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmI")]) 

bc_td_tnmI_results_mi <- bc_td_tnmI %>% lapply(run_model_mi, dataset= (imputedLong100BreastTotalMortality %>% 
                                                                     filter(tnmAnatomicStage == "I")) )
bc_td_tnmI_results_mi %<>% lapply(vectorize_pooled_results)

#### TNM II

bc_td_tnmII <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmII")]) 


bc_td_tnmII_results_mi <- bc_td_tnmII %>% lapply(run_model_mi, dataset= (imputedLong100BreastTotalMortality %>% 
                                                                      filter(tnmAnatomicStage == "II")) )
bc_td_tnmII_results_mi %<>% lapply(vectorize_pooled_results)

#### TNM III

bc_td_tnmIII <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmIII")]) 


bc_td_tnmIII_results_mi <- bc_td_tnmIII %>% lapply(run_model_mi, dataset= (imputedLong100BreastTotalMortality %>% 
                                                                           filter(tnmAnatomicStage == "III")) )
bc_td_tnmIII_results_mi %<>% lapply(vectorize_pooled_results)

#### TNMIV
bc_td_tnmIV <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmIV")]) 


bc_td_tnmIV_results_mi <- bc_td_tnmIV %>% lapply(run_model_mi, dataset= (imputedLong100BreastTotalMortality %>% 
                                                                           filter(tnmAnatomicStage == "IV")) )
bc_td_tnmIV_results_mi %<>% lapply(vectorize_pooled_results)

#### TNM Unknown

bc_td_tnmUnk <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmUnknown")]) 


bc_td_tnmUnk_results_mi <- bc_td_tnmUnk %>% lapply(run_model_mi, dataset= (imputedLong100BreastTotalMortality %>% 
                                                                           filter(tnmAnatomicStage == "Unknown")) )
bc_td_tnmUnk_results_mi %<>% lapply(vectorize_pooled_results)


### Breast cancer mortality

#### TNM I
bc_bcd_tnmI <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmI")]) 

bc_bcd_tnmI_results_mi <- bc_bcd_tnmI %>% lapply(run_model_mi, dataset= (imputedLong100BreastCancerMortality %>% 
                                                                         filter(tnmAnatomicStage == "I")) )
bc_bcd_tnmI_results_mi %<>% lapply(vectorize_pooled_results)

#### TNM II

bc_bcd_tnmII <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmII")]) 


bc_bcd_tnmII_results_mi <- bc_bcd_tnmII %>% lapply(run_model_mi, dataset= (imputedLong100BreastCancerMortality %>% 
                                                                           filter(tnmAnatomicStage == "II")) )
bc_bcd_tnmII_results_mi %<>% lapply(vectorize_pooled_results)

#### TNM III

bc_bcd_tnmIII <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmIII")]) 


bc_bcd_tnmIII_results_mi <- bc_bcd_tnmIII %>% lapply(run_model_mi, dataset= (imputedLong100BreastCancerMortality %>% 
                                                                             filter(tnmAnatomicStage == "III")) )
bc_bcd_tnmIII_results_mi %<>% lapply(vectorize_pooled_results)

#### TNMIV
bc_bcd_tnmIV <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmIV")]) 


bc_bcd_tnmIV_results_mi <- bc_bcd_tnmIV %>% lapply(run_model_mi, dataset= (imputedLong100BreastCancerMortality %>% 
                                                                           filter(tnmAnatomicStage == "IV")) )
bc_bcd_tnmIV_results_mi %<>% lapply(vectorize_pooled_results)

#### TNM Unknown

bc_bcd_tnmUnk <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.tnmUnknown")]) 


bc_bcd_tnmUnk_results_mi <- bc_bcd_tnmUnk %>% lapply(run_model_mi, dataset= (imputedLong100BreastCancerMortality %>% 
                                                                             filter(tnmAnatomicStage == "Unknown")) )
bc_bcd_tnmUnk_results_mi %<>% lapply(vectorize_pooled_results)

## Colorectal cancer ----

### Total mortality

#### SEER localized
crc_td_seerLoc_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerLocalized")]) 


crc_td_seerLoc_results_mi <-  crc_td_seerLoc_model_mi %>% lapply(run_model_mi, (dataset= imputedLong100ColorectalTotalMortality %>%
                                                                   filter(seerStage == "Localized")))
crc_td_seerLoc_results_mi %<>% lapply(vectorize_pooled_results)

#### SEER regional
crc_td_seerReg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerRegional")]) 


crc_td_seerReg_results_mi <-  crc_td_seerReg_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100ColorectalTotalMortality %>%
                                                                   filter(seerStage == "Regional")))
crc_td_seerReg_results_mi %<>% lapply(vectorize_pooled_results)

#### SEER distant
crc_td_seerDis_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerDistant")]) 


crc_td_seerDis_results_mi <-  crc_td_seerDis_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100ColorectalTotalMortality %>%
                                                                   filter(seerStage == "Distant")))
crc_td_seerDis_results_mi %<>% lapply(vectorize_pooled_results)


#### SEER unknown
crc_td_seerUnk_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerUnknown")]) 


crc_td_seerUnk_results_mi <-  crc_td_seerUnk_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100ColorectalTotalMortality %>%
                                                                   filter(seerStage == "Unknown")))
crc_td_seerUnk_results_mi %<>% lapply(vectorize_pooled_results)

### Colorectal cancer mortality

#### SEER Localized
crc_crcd_seerLoc_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerLocalized")]) 


crc_crcd_seerLoc_results_mi <-  crc_crcd_seerLoc_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100ColorectalCancerMortality %>%
                                                                   filter(seerStage == "Localized")))
crc_crcd_seerLoc_results_mi %<>% lapply(vectorize_pooled_results)

#### SEER Regional
crc_crcd_seerReg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerRegional")]) 


crc_crcd_seerReg_results_mi <-  crc_crcd_seerReg_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100ColorectalCancerMortality %>%
                                                                       filter(seerStage == "Regional")))
crc_crcd_seerReg_results_mi %<>% lapply(vectorize_pooled_results)
#### SEER Distant
crc_crcd_seerDis_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerDistant")]) 


crc_crcd_seerDis_results_mi <-  crc_crcd_seerDis_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100ColorectalCancerMortality %>%
                                                                       filter(seerStage == "Distant")))
crc_crcd_seerDis_results_mi %<>% lapply(vectorize_pooled_results)
#### SEER Unknown
crc_crcd_seerUnk_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerUnknown")]) 


crc_crcd_seerUnk_results_mi <-  crc_crcd_seerUnk_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100ColorectalCancerMortality %>%
                                                                       filter(seerStage == "Unknown")))
crc_crcd_seerUnk_results_mi %<>% lapply(vectorize_pooled_results)
## Lung cancer ----

### Total mortality
#### SEER localized
lc_td_seerLoc_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerLocalized")]) 


lc_td_seerLoc_results_mi <-  lc_td_seerLoc_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100LungTotalMortality %>%
                                                                   filter(seerStage == "Localized")))
lc_td_seerLoc_results_mi %<>% lapply(vectorize_pooled_results)

#### SEER regional
lc_td_seerReg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerRegional")]) 


lc_td_seerReg_results_mi <-  lc_td_seerReg_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100LungTotalMortality %>%
                                                                   filter(seerStage == "Regional")))
lc_td_seerReg_results_mi %<>% lapply(vectorize_pooled_results)

#### SEER distant
lc_td_seerDis_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerDistant")]) 


lc_td_seerDis_results_mi <-  lc_td_seerDis_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100LungTotalMortality %>%
                                                                   filter(seerStage == "Distant")))
lc_td_seerDis_results_mi %<>% lapply(vectorize_pooled_results)


#### SEER unknown
lc_td_seerUnk_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerUnknown")]) 


lc_td_seerUnk_results_mi <-  lc_td_seerUnk_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100LungTotalMortality %>%
                                                                   filter(seerStage == "Unknown")))
lc_td_seerUnk_results_mi %<>% lapply(vectorize_pooled_results)


### Lung cancer mortality
#### SEER localized
lc_lcd_seerLoc_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerLocalized")]) 


lc_lcd_seerLoc_results_mi <-  lc_lcd_seerLoc_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100LungCancerMortality %>%
                                                                 filter(seerStage == "Localized")))
lc_lcd_seerLoc_results_mi %<>% lapply(vectorize_pooled_results)

#### SEER regional
lc_lcd_seerReg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerRegional")]) 


lc_lcd_seerReg_results_mi <-  lc_lcd_seerReg_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100LungCancerMortality %>%
                                                                 filter(seerStage == "Regional")))
lc_lcd_seerReg_results_mi %<>% lapply(vectorize_pooled_results)

#### SEER distant
lc_lcd_seerDis_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerDistant")]) 


lc_lcd_seerDis_results_mi <-  lc_lcd_seerDis_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100LungCancerMortality %>%
                                                                 filter(seerStage == "Distant")))
lc_lcd_seerDis_results_mi %<>% lapply(vectorize_pooled_results)


#### SEER unknown
lc_lcd_seerUnk_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous")]) %>% 
  parallel_apply(templates_mi[c("f.seerUnknown")]) 


lc_lcd_seerUnk_results_mi <-  lc_lcd_seerUnk_model_mi %>% lapply(run_model_mi, dataset= (imputedLong100LungCancerMortality %>%
                                                                 filter(seerStage == "Unknown")))
lc_lcd_seerUnk_results_mi %<>% lapply(vectorize_pooled_results)

# Hormone receptor status subgroup analysis----
## Breast cancer only, adjust for molecular subtype only include cases diagnosed after 2005

## all cause mortality ----

bc_td_ms_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.ms")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage", "c.molecularsubtype")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_ms_results_mi <-  bc_td_ms_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortality2005) 
bc_td_ms_results_mi %<>% lapply(vectorize_pooled_results)



## breast cancer mortality
bc_bcd_ms_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.ms")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 

bc_bcd_ms_results_mi <-  bc_bcd_ms_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortality2005) 
bc_bcd_ms_results_mi %<>% lapply(vectorize_pooled_results)

## Molecular subtype, diagnosed after 2005 breast cancer


### ER+ 
bc_td_erpos_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.erpos")]) 


bc_td_erpos_results_mi <-  bc_td_erpos_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortality2005 %>%
                                                           filter(estrogenRec == "Pos")) 
bc_td_erpos_results_mi %<>% lapply(vectorize_pooled_results)


bc_bcd_erpos_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.erpos")]) 


bc_bcd_erpos_results_mi <-  bc_bcd_erpos_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortality2005 %>%
                                                                       filter(estrogenRec == "Pos")) 
bc_bcd_erpos_results_mi %<>% lapply(vectorize_pooled_results)

### ER-

bc_td_erneg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.erneg")]) 


bc_td_erneg_results_mi <-  bc_td_erneg_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortality2005 %>%
                                                                       filter(estrogenRec == "Neg")) 
bc_td_erneg_results_mi %<>% lapply(vectorize_pooled_results)



bc_bcd_erneg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.erneg")]) 


bc_bcd_erneg_results_mi <-  bc_bcd_erneg_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortality2005 %>%
                                                                     filter(estrogenRec == "Neg")) 
bc_bcd_erneg_results_mi %<>% lapply(vectorize_pooled_results)

### ER+ and PR-
bc_td_erposprneg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.erposprneg")]) 


bc_td_erposprneg_results_mi <-  bc_td_erposprneg_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortality2005 %>%
                                                                       filter(erPos_prNeg == 1)) 
bc_td_erposprneg_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_erposprneg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.erposprneg")]) 


bc_bcd_erposprneg_results_mi <-  bc_bcd_erposprneg_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortality2005 %>%
                                                                       filter(erPos_prNeg == 1)) 
bc_bcd_erposprneg_results_mi %<>% lapply(vectorize_pooled_results)

### ER-/PR-/HER-

bc_td_tripleneg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.tripleneg")]) 


bc_td_tripleneg_results_mi <-  bc_td_tripleneg_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortality2005 %>%
                                                                     filter(tripleNeg == 1)) 
bc_td_tripleneg_results_mi %<>% lapply(vectorize_pooled_results)


bc_bcd_tripleneg_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.tripleneg")]) 


bc_bcd_tripleneg_results_mi <-  bc_bcd_tripleneg_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortality2005 %>%
                                                                         filter(tripleNeg == 1)) 
bc_bcd_tripleneg_results_mi %<>% lapply(vectorize_pooled_results)

### ER-/PR-/HER+

bc_td_her2enrich_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.her2enrich")]) 


bc_td_her2enrich_results_mi <-  bc_td_her2enrich_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortality2005 %>%
                                                                     filter(her2Enrich == 1)) 
bc_td_her2enrich_results_mi %<>% lapply(vectorize_pooled_results)


bc_bcd_her2enrich_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.her2enrich")]) 


bc_bcd_her2enrich_results_mi <-  bc_bcd_her2enrich_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortality2005 %>%
                                                                       filter(her2Enrich == 1)) 
bc_bcd_her2enrich_results_mi %<>% lapply(vectorize_pooled_results)



# Include cases diagnosies 2005 and after as sensitivity analysis for the molecular subtype results (models are not adjusted for molecular subtype)
## all cause mortality ----

bc_td_2005_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.2005")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_2005_results_mi <-  bc_td_2005_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortality2005) 
bc_td_2005_results_mi %<>% lapply(vectorize_pooled_results)

## breast cancer mortality
bc_bcd_2005_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.2005")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 

bc_bcd_2005_results_mi <-  bc_bcd_2005_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortality2005) 
bc_bcd_2005_results_mi %<>% lapply(vectorize_pooled_results)

# By menopausal status breast cancer 

## all cause mortality ----

bc_td_pm_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.pm")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvarspostmeno", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_pm_results_mi <-  bc_td_pm_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortalityPostmeno) 
bc_td_pm_results_mi %<>% lapply(vectorize_pooled_results)

bc_td_prem_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.prem")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvarspostmeno", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_prem_results_mi <-  bc_td_prem_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortalityPremeno) 
bc_td_prem_results_mi %<>% lapply(vectorize_pooled_results)

## breast cancer mortality
bc_bcd_pm_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.pm")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvarspostmeno", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 

bc_bcd_pm_results_mi <-  bc_bcd_pm_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortalityPostmeno) 
bc_bcd_pm_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_prem_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.prem")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvarspostmeno", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 

bc_bcd_prem_results_mi <-  bc_bcd_prem_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortalityPremeno) 
bc_bcd_prem_results_mi %<>% lapply(vectorize_pooled_results)


# Subgroups by age at diagnosis (< 63, >= 63)
    # breast
bc_td_young_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.young")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous","c.bcvars", "c.tnmstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_young_results_mi <-  bc_td_young_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortalityYoung) 
bc_td_young_results_mi %<>% lapply(vectorize_pooled_results)

bc_td_old_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.old")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_td_old_results_mi <-  bc_td_old_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortalityOld) 
bc_td_old_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_young_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.young")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous","c.bcvars", "c.tnmstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_bcd_young_results_mi <-  bc_bcd_young_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortalityYoung) 
bc_bcd_young_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_old_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort.old")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


bc_bcd_old_results_mi <-  bc_bcd_old_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortalityOld) 
bc_bcd_old_results_mi %<>% lapply(vectorize_pooled_results)

    # colorectal

crc_td_young_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort.young")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_td_young_results_mi <-  crc_td_young_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortalityYoung) 
crc_td_young_results_mi %<>% lapply(vectorize_pooled_results)

crc_td_old_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort.old")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_td_old_results_mi <-  crc_td_old_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalTotalMortalityOld) 
crc_td_old_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_young_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort.young")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_crcd_young_results_mi <-  crc_crcd_young_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalCancerMortalityYoung) 
crc_crcd_young_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_old_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort.old")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


crc_crcd_old_results_mi <-  crc_crcd_old_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalCancerMortalityOld) 
crc_crcd_old_results_mi %<>% lapply(vectorize_pooled_results)

# lung

lc_td_young_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort.young")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_td_young_results_mi <-  lc_td_young_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungTotalMortalityYoung) 
lc_td_young_results_mi %<>% lapply(vectorize_pooled_results)

lc_td_old_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort.old")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_td_old_results_mi <-  lc_td_old_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungTotalMortalityOld) 
lc_td_old_results_mi %<>% lapply(vectorize_pooled_results)

lc_lcd_young_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort.young")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_lcd_young_results_mi <-  lc_lcd_young_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungCancerMortalityYoung) 
lc_lcd_young_results_mi %<>% lapply(vectorize_pooled_results)

lc_lcd_old_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort.old")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage", "c.agediagnosis")]) %>% 
  parallel_apply(templates_mi[c("f.all")]) 


lc_lcd_old_results_mi <-  lc_lcd_old_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungCancerMortalityOld) 
lc_lcd_old_results_mi %<>% lapply(vectorize_pooled_results)




# UPDATE TEMPLATE ----


all_result_values_mi <- c(bc_td_results_mi,
                          bc_td_cat_results_mi,
                          bc_bcd_results_mi,
                          bc_bcd_cat_results_mi,
                          crc_td_results_mi,
                          crc_td_cat_results_mi,
                          crc_crcd_results_mi,
                          crc_crcd_cat_results_mi,
                          lc_td_results_mi,
                          lc_td_cat_results_mi,
                          lc_lcd_results_mi,
                          lc_lcd_cat_results_mi,
                          bc_td_tnmI_results_mi,
                          bc_td_tnmII_results_mi,
                          bc_td_tnmIII_results_mi,
                          bc_td_tnmIV_results_mi,
                          bc_td_tnmUnk_results_mi,
                          bc_bcd_tnmI_results_mi,
                          bc_bcd_tnmII_results_mi,
                          bc_bcd_tnmIII_results_mi,
                          bc_bcd_tnmIV_results_mi,
                          bc_bcd_tnmUnk_results_mi,
                          bc_td_ms_results_mi,
                          bc_bcd_ms_results_mi,
                          bc_td_2005_results_mi,
                          bc_bcd_2005_results_mi,
                          bc_td_pm_results_mi,
                          bc_bcd_pm_results_mi,
                          bc_td_prem_results_mi,
                          bc_bcd_prem_results_mi,
                          crc_td_seerLoc_results_mi,
                          crc_td_seerReg_results_mi,
                          crc_td_seerDis_results_mi,
                          crc_td_seerUnk_results_mi,
                          crc_crcd_seerLoc_results_mi,
                          crc_crcd_seerReg_results_mi,
                          crc_crcd_seerDis_results_mi,
                          crc_crcd_seerUnk_results_mi,
                          cc_td_results_mi,
                          rc_td_results_mi,
                          cc_crcd_results_mi,
                          rc_crcd_results_mi,
                          lc_td_seerLoc_results_mi,
                          lc_td_seerReg_results_mi,
                          lc_td_seerDis_results_mi,
                          lc_td_seerUnk_results_mi,
                          lc_lcd_seerLoc_results_mi,
                          lc_lcd_seerReg_results_mi,
                          lc_lcd_seerDis_results_mi,
                          lc_lcd_seerUnk_results_mi,
                          sclc_td_results_mi,
                          nsclc_td_results_mi,
                          sclc_lcd_results_mi,
                          nsclc_lcd_results_mi,
                          bc_td_prediag_short_results_mi,
                          bc_bcd_prediag_short_results_mi,
                          crcc_td_prediag_short_results_mi,
                          crcc_crcd_prediag_short_results_mi,
                          lc_td_prediag_short_results_mi,
                          lc_lcd_prediag_short_results_mi
                          
                          
                          
                          ) %>% 
  unlist()

updateOfficeTemplate("./output/table_5_template.docx", "./output/table_5.docx", update_values = all_result_values_mi)
updateOfficeTemplate("./output/table_7_template.docx", "./output/table_7.docx", update_values = all_result_values_mi)


excluding_years_result_values <- c(
                                   bc_td_x1Yr_results_mi,
                                   bc_td_x3Yr_results_mi,
                                   crc_td_x1Yr_results_mi,
                                   crc_td_x3Yr_results_mi,
                                   lc_td_x1Yr_results_mi,
                                   lc_td_x3Yr_results_mi,
                                   bc_bcd_x1Yr_results_mi,
                                   bc_bcd_x3Yr_results_mi,
                                   crc_crcd_x1Yr_results_mi,
                                   crc_crcd_x3Yr_results_mi,
                                   lc_lcd_x1Yr_results_mi,
                                   lc_lcd_x3Yr_results_mi) %>%
  unlist()

cc_mi_result_values <- c(all_result_values, excluding_years_result_values)
updateOfficeTemplate("./output/table_8_template.docx", "./output/table_8.docx", update_values = cc_mi_result_values) #sensitivity analysis, left truncation


# Excluding 1 year follow-up categorical estimates

excluding_1yr_categorical_mi <- c(bc_td_cat_x1Yr_model_results_mi,
                                  bc_bcd_cat_x1Yr_results_mi,
                                  crc_td_cat_x1y_results_mi,
                                  crc_crcd_cat_x1y_results_mi,
                                  lc_td_cat_x1y_results_mi,
                                  lc_lcd_cat_x1y_results_mi
  
) %>%
  unlist()


updateOfficeTemplate("./output/table_11_template.docx", "./output/table_11.docx", update_values = excluding_1yr_categorical_mi) 


# Sensitivity analysis removing one factor at a time from the HLI


bc_td_excluding_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  parallel_apply(templates_mi[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  parallel_apply(templates_mi[c("c.physcore", "c.bmiscore", "c.smoscore", "c.alcscore", "c.diescore")]) %>%
  serial_apply(templates_mi[c("c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.all")])

bc_td_excluding_results_mi <- bc_td_excluding_model_mi %>% lapply(run_model_mi, dataset=imputedLong100BreastTotalMortality)
bc_td_excluding_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_excluding_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  parallel_apply(templates_mi[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  parallel_apply(templates_mi[c("c.physcore", "c.bmiscore", "c.smoscore", "c.alcscore", "c.diescore")]) %>%
  serial_apply(templates_mi[c("c.tnmstage", "c.bcvars")]) %>% 
  parallel_apply(templates_mi[c("f.all")])

bc_bcd_excluding_results_mi <- bc_bcd_excluding_model_mi %>% lapply(run_model_mi, dataset=imputedLong100BreastCancerMortality)
bc_bcd_excluding_results_mi %<>% lapply(vectorize_pooled_results)

crc_td_excluding_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  parallel_apply(templates_mi[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  parallel_apply(templates_mi[c("c.physcore", "c.bmiscore", "c.smoscore", "c.alcscore", "c.diescore")]) %>%
  serial_apply(templates_mi[c("c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")])

crc_td_excluding_results_mi <- crc_td_excluding_model_mi %>% lapply(run_model_mi, dataset=imputedLong100ColorectalTotalMortality)
crc_td_excluding_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_excluding_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  parallel_apply(templates_mi[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  parallel_apply(templates_mi[c("c.physcore", "c.bmiscore", "c.smoscore", "c.alcscore", "c.diescore")]) %>%
  serial_apply(templates_mi[c("c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")])

crc_crcd_excluding_results_mi <- crc_crcd_excluding_model_mi %>% lapply(run_model_mi, dataset=imputedLong100ColorectalCancerMortality)
crc_crcd_excluding_results_mi %<>% lapply(vectorize_pooled_results)

lc_td_excluding_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  parallel_apply(templates_mi[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  parallel_apply(templates_mi[c("c.physcore", "c.bmiscore", "c.smoscore", "c.alcscore", "c.diescore")]) %>%
  serial_apply(templates_mi[c("c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")])

lc_td_excluding_results_mi <- lc_td_excluding_model_mi %>% lapply(run_model_mi, dataset=imputedLong100LungTotalMortality)
lc_td_excluding_results_mi %<>% lapply(vectorize_pooled_results)

lc_lcd_excluding_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  parallel_apply(templates_mi[c("c.hliNoPhy", "c.hliNoBmi", "c.hliNoSmo", "c.hliNoAlc", "c.hliNoDie")]) %>%
  parallel_apply(templates_mi[c("c.physcore", "c.bmiscore", "c.smoscore", "c.alcscore", "c.diescore")]) %>%
  serial_apply(templates_mi[c("c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.all")])

lc_lcd_excluding_results_mi <- lc_lcd_excluding_model_mi %>% lapply(run_model_mi, dataset=imputedLong100LungCancerMortality)
lc_lcd_excluding_results_mi %<>% lapply(vectorize_pooled_results)

excluding_results_values_mi <- c(bc_td_excluding_results_mi,
                                 bc_bcd_excluding_results_mi,
                                 crc_td_excluding_results_mi,
                                 crc_crcd_excluding_results_mi,
                                 lc_td_excluding_results_mi,
                                 lc_lcd_excluding_results_mi) %>%
  unlist()

updateOfficeTemplate("./output/table_10_template.docx", "./output/table_10.docx", update_values = excluding_results_values_mi)


# Prediagnostic interval subgroup analysis
## Short and Long groups are divided by median prediagnostic interval within each BC, CRC, and LC

bc_td_prediag_short_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediagshort")]) 

bc_td_prediag_short_results_mi <-  bc_td_prediag_short_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortalityPrediagShort)
bc_td_prediag_short_results_mi %<>% lapply(vectorize_pooled_results)

bc_td_prediag_long_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediaglong")]) 

bc_td_prediag_long_results_mi <-  bc_td_prediag_long_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastTotalMortalityPrediagLong)
bc_td_prediag_long_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_prediag_short_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediagshort")]) 

bc_bcd_prediag_short_results_mi <-  bc_bcd_prediag_short_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortalityPrediagShort)
bc_bcd_prediag_short_results_mi %<>% lapply(vectorize_pooled_results)

bc_bcd_prediag_long_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.bccohort")]) %>%
  parallel_apply(templates_mi[c("o.breastDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.bcvars", "c.tnmstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediaglong")]) 


bc_bcd_prediag_long_results_mi <-  bc_bcd_prediag_long_model_mi %>% lapply(run_model_mi, dataset= imputedLong100BreastCancerMortalityPrediagLong)
bc_bcd_prediag_long_results_mi %<>% lapply(vectorize_pooled_results)



crc_td_prediag_short_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediagshort")]) 


crc_td_prediag_short_results_mi <-  crc_td_prediag_short_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalTotalMortalityPrediagShort)
crc_td_prediag_short_results_mi %<>% lapply(vectorize_pooled_results)

crc_td_prediag_long_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediaglong")]) 


crc_td_prediag_long_results_mi <-  crc_td_prediag_long_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalTotalMortalityPrediagLong)
crc_td_prediag_long_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_prediag_short_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediagshort")]) 


crc_crcd_prediag_short_results_mi <-  crc_crcd_prediag_short_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalCancerMortalityPrediagShort)
crc_crcd_prediag_short_results_mi %<>% lapply(vectorize_pooled_results)

crc_crcd_prediag_long_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.crccohort")]) %>%
  parallel_apply(templates_mi[c("o.colorectalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediaglong")]) 


crc_crcd_prediag_long_results_mi <-  crc_crcd_prediag_long_model_mi %>% lapply(run_model_mi, dataset= imputedLong100ColorectalCancerMortalityPrediagLong)
crc_crcd_prediag_long_results_mi %<>% lapply(vectorize_pooled_results)


lc_td_prediag_short_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediagshort")]) 


lc_td_prediag_short_results_mi <- lc_td_prediag_short_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungTotalMortalityPrediagShort)
lc_td_prediag_short_results_mi %<>% lapply(vectorize_pooled_results)

lc_td_prediag_long_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.totalDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediaglong")]) 


lc_td_prediag_long_results_mi <- lc_td_prediag_long_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungTotalMortalityPrediagLong)
lc_td_prediag_long_results_mi %<>% lapply(vectorize_pooled_results)

lc_lcd_prediag_short_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediagshort")]) 


lc_lcd_prediag_short_results_mi <- lc_lcd_prediag_short_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungCancerMortalityPrediagShort)
lc_lcd_prediag_short_results_mi %<>% lapply(vectorize_pooled_results)

lc_lcd_prediag_long_model_mi <- list(base_model) %>% 
  parallel_apply(templates_mi[c("mi.lccohort")]) %>%
  parallel_apply(templates_mi[c("o.lungDeath")]) %>% 
  serial_apply(templates_mi[c("c.hliContinuous", "c.seerstage")]) %>% 
  parallel_apply(templates_mi[c("f.prediaglong")]) 


lc_lcd_prediag_long_results_mi <- lc_lcd_prediag_long_model_mi %>% lapply(run_model_mi, dataset= imputedLong100LungCancerMortalityPrediagLong)
lc_lcd_prediag_long_results_mi %<>% lapply(vectorize_pooled_results)


all_result_values_prediag_mi <- c(bc_td_prediag_short_results_mi,
                                  bc_td_prediag_long_results_mi,
                          bc_bcd_prediag_short_results_mi,
                          bc_bcd_prediag_long_results_mi,
                          crc_td_prediag_short_results_mi,
                          crc_td_prediag_long_results_mi,
                          crc_crcd_prediag_short_results_mi,
                          crc_crcd_prediag_long_results_mi,
                          lc_td_prediag_short_results_mi,
                          lc_td_prediag_long_results_mi,
                          lc_lcd_prediag_short_results_mi,
                          lc_lcd_prediag_long_results_mi
                          
                          
                          
) %>% 
  unlist()

updateOfficeTemplate("./output/table_13_template.docx", "./output/table_13.docx", update_values = all_result_values_prediag_mi)


# Competing risks ----

# Making a covariate matrix (not necessary when using tidycmprsk::crr)

make_covars_matrix_cr <- function(dataframe){
  stageMatrix <- factor2ind(dataframe$seerStage, "Localized")
  continuousCovars <- dataframe %>%
    select(.imp, hliScore, ageDiagnosis)
  covars <- cbind(stageMatrix, continuousCovars)
  splitCovarsList <- covars %>% group_by(.imp) %>% group_split() 
  covarsList <- splitCovarsList %>% lapply(. %>% select(!.imp))
  
  return(covarsList)
}

a <- make_covars_matrix_cr(imputedLong100LungTotalMortality)


# Modeling competing risk in mi data 

# Breast
bcD.cmprsk.mi <- imputedLong100BreastTotalMortality %>% group_by(.imp) %>% 
  do(model = tidycmprsk::crr(Surv(followUpTimeDays, as.factor(as.character(breastDeathCr))) ~ 
                               hliScore +
                               education +
                               height +
                               strata(wave) +
                               ageDiagnosis +
                               tnmAnatomicStage +
                               menopausalStatus +
                               ageMenarche +
                               hrtStatus +
                               ocEverUse +
                               familyHistBC +
                               parity +
                               breastfeeding, 
                             data = ., 
                             failcode = 1)) %>% 
  as.list() %>% 
  .[[-1]] %>% 
  pool() %>%
  summary(conf.int=TRUE) %>%
  mutate(hazr = exp(estimate),
         lci = exp(`2.5 %`),
         uci = exp(`97.5 %`))

# Colorectal
crcD.cmprsk.mi <- imputedLong100ColorectalTotalMortality %>% group_by(.imp) %>% 
  do(model = tidycmprsk::crr(Surv(followUpTimeDays, as.factor(as.character(colorectalDeathCr))) ~ 
                               hliScore +
                               education +
                               height +
                               strata(wave) +
                               ageDiagnosis +
                               seerStage, 
                             data = ., 
                             failcode = 1)) %>% 
  as.list() %>% 
  .[[-1]] %>% 
  pool() %>%
  summary(conf.int=TRUE) %>%
  mutate(hazr = exp(estimate),
         lci = exp(`2.5 %`),
         uci = exp(`97.5 %`))

# Lung
lcD.cmprsk.mi <- imputedLong100LungTotalMortality %>% group_by(.imp) %>% 
  do(model = tidycmprsk::crr(Surv(followUpTimeDays, as.factor(as.character(lungDeathCr))) ~ 
                               hliScore +
                               education +
                               height +
                               strata(wave) +
                               ageDiagnosis +
                               seerStage, 
                             data = ., 
                             failcode = 1)) %>% 
  as.list() %>% 
  .[[-1]] %>% 
  pool() %>%
  summary(conf.int=TRUE) %>%
  mutate(hazr = exp(estimate),
         lci = exp(`2.5 %`),
         uci = exp(`97.5 %`))


# Restricted subic splines

ddist <- datadist(casesIncluded)
options(datadist="ddist")
coxspline5Lung <- cph(Surv(ageEntry, ageExit, statusLung) ~ rcs(HLIScore, 5) + education, data=casesIncluded, x=TRUE, y=TRUE)

plotLung5 <- ggplot(rms:::Predict(coxspline5Lung, HLIScore=seq(1, 20),
                                  fun=exp),
                    xlab= "Healthy Lifestyle Index score",
                    ylab="Hazard Ratio") + 
  ggtitle("5 knots")

d <- imputedLong100ColorectalTotalMortality
ddist <- datadist(d)
options(datadist="ddist")

a <- imputedLong100ColorectalTotalMortality %>% group_by(.imp) %>%
  do(model = cph(Surv(followUpTimeDays, totalDeath)~ rcs(hliScore, 3) + ageDiagnosis,
                 data = .,
                 x=T,y=T)) %>%
  as.list() %>%
  .[[-1]] %>%
  pool()

b <- fit.mult.impute(Surv(followUpTimeDays, totalDeath) ~ rcs(hliScore, 3) + ageDiagnosis,
                      fitter = cph, xtrans = imputedLong100ColorectalTotalMortality,
                      data = colorectal, x = TRUE, y = TRUE)

plot.a <- ggplot(rms::Predict(a,
                              hliScore = seq(1,20),
                              fun=exp),
                 xlab="HLI score",
                 ylab = "Hazard Ratio")

# ---- Test prediagnostic interval interaction ----

a <-   imputedLong100BreastTotalMortality %>% group_by(.imp) %>% do(model = coxph(Surv(followUpTimeDays, totalDeath) ~ 
                                                                               #hliScore + # exclude HLI score to parameterize differently to obtain HLI score estimates in short and long lag years
                                                                               lagYearsMedian + 
                                                                               hliScore:lagYearsMedian +
                                                                               ageDiagnosis, 
                                                                             data = . )) %>%

  as.list() %>% .[[-1]] %>% pool() %>%
  summary(conf.int=TRUE) %>%
    mutate(hazr = sprintf(exp(estimate), fmt = "%#.3f"),
         lci = sprintf(exp(estimate - 1.96*std.error), fmt = "%#.3f"),
         uci = sprintf(exp(estimate + 1.96*std.error), fmt = "%#.3f"),
         hrci = paste0(hazr, "(", lci, "-", uci, ")"))



# test interaction with pool.compare

## need to use with() to obtain class mira models

bc_td_simple_mira <- with(imputedMids100BreastTotalMortality, coxph(Surv(followUpTimeDays, totalDeath) ~ 
                                                                 hliScore +
                                                                 ageDiagnosis
                                                               )
                     )

bc_td_intx_mira <- with(imputedMids100BreastTotalMortality, coxph(Surv(followUpTimeDays, totalDeath) ~ 
                                                                 hliScore +
                                                                 ageDiagnosis +
                                                               education
                                                               
)
)

D3(bc_td_intx_mira, bc_td_simple_mira) 

# test interaction with ::miceafter

install.packages("miceafter")
library(miceafter)

filtered <- imputedLong100BreastTotalMortality %>% filter(.imp != 0)

fit0 <- coxph(Surv(followUpTimeDays, totalDeath) ~ 
                hliScore +
                ageDiagnosis)
fit1 <- coxph(Surv(followUpTimeDays, totalDeath) ~ 
                hliScore +
                ageDiagnosis +
                education)
pool_D4(filtered, 100, .imp)
