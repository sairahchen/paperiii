# Flexible parametric survival analysis

# Lung cancer cox models did not meet PH assumption for age at diagnosis and global test


# Create lung dataframe with no missings on relevant covariates


lungComplete <- lung[complete.cases(lung %>% select(hliScore,
                                  hliCategoriesLevels,
                                  ageDiagnosis,
                                  height,
                                  education,
                                  seerStage,
                                  wave,
                                  totalDeath,
                                  lungDeath)),
]


# Regular cox 

cox.lung.td <- coxph(Surv(followUpTimeDays, lungDeath==1) ~ 
                          hliScore +
                          ageDiagnosis +
                          height +
                          education +
                          strata(seerStage) +
                          strata(wave),
                        data=lung)

cox.lung.td <- rms::cph(Surv(followUpTimeDays, totalDeath==1) ~ 
                          hliScore +
                          rcs(ageDiagnosis, 3) +
                          height +
                          education +
                          seerStage +
                          wave,
                        data=lungComplete)

cox.zph(cox.lung.td) # Test weighted residuals for PH

# Fit parametric survival model with natural splines 
lung.td.fpsa <- stpm2(Surv(followUpTimeDays, totalDeath==1) ~ 
                hliScore +
                ageDiagnosis +
                height +
                education +
                seerStage +
                wave,
                data=lungComplete, df=4)
summary(lung.td.fpsa)
eform(lung.td.fpsa)

# Know from PH test in cox model that age at diagnosis and seer stage have non-proportional hazards
# Model age diagnosis and seer stage with rcs, creating a more complex model
lung.td.fpsa.tvc <- stpm2(Surv(followUpTimeDays, totalDeath==1) ~ 
                        hliScore +
                        ageDiagnosis +
                        height +
                        education +
                        seerStage +
                        wave,
                      data=lungComplete, df=4,
                      tvc = list(ageDiagnosis=2, seerStage=2))
eform(lung.td.fpsa.tvc)


# Compared complex model to nested model

anova(lung.td.fpsa, lung.td.fpsa.tvc)


lung.lcd.fpsa <- stpm2(Surv(followUpTimeDays, lungDeath==1) ~ 
                        hliScore +
                        ageDiagnosis +
                        height +
                        education +
                        seerStage +
                        wave,
                      data=lungComplete, df=4)
summary(lung.lcd.fpsa)
eform(lung.lcd.fpsa)

# Imputed data
list_results <- imputedLong20LungTotalMortality %>% 
  group_by(.imp) %>% 
  do(model = stpm2(Surv(followUpTimeDays, lungDeath==1) ~ 
                     hliScore +
                     ageDiagnosis +
                     height +
                     education +
                     seerStage +
                     wave, data = . )) %>%
                                                                                          
  as.list() 
pooled_results <- list_results %>% .[[-1]] %>% pool() %>%
  summary(conf.int=TRUE)
pooled_results <- pooled_results %>%
  mutate(hazr = sprintf(exp(estimate), fmt = "%#.2f"),
         lci = sprintf(exp(estimate - 1.96*std.error), fmt = "%#.2f"),
         uci = sprintf(exp(estimate + 1.96*std.error), fmt = "%#.2f"),
         hrci = paste0(hazr, "(", lci, "-", uci, ")"))


