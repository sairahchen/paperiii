table(lung$hliScore, lung$hliCategories)

lungComplete$hliCategories <- factor(lungComplete$hliCategories, levels = c("[2,9)","[9,11)","[11,13)","[13,20]"))

ddist <- datadist(lungComplete)
options(datadist="ddist")
lung_lcd_lin_model <- cph(Surv(followUpTimeDays, lungDeath) ~ hliCategories + 
                            ageDiagnosis + 
                            education + 
                            height +
                            strat(wave) +
                            strat(seerStage), 
                          data=lungComplete)
summary(lung_lcd_lin_model)

lung_lcd_lin_model_coxph <- coxph(Surv(followUpTimeDays, lungDeath) ~ hliCategories + 
                                    ageDiagnosis + 
                                    education + 
                                    height +
                                    strata(wave) +
                                    strata(seerStage), 
                                  data=lungComplete)
summary(lung_lcd_lin_model_coxph)

ddist <- datadist(lungComplete)
options(datadist="ddist")
ddist$limits["Adjust to", "hliScore"] <- 2
lung_lcd_rcs_model <- cph(Surv(followUpTimeDays, lungDeath) ~ rcs(hliScore, 4) + 
                            ageDiagnosis + 
                            education + 
                            height +
                            strat(wave) +
                            strat(seerStage), 
                          data=lungComplete)

lung_lcd_rcs_plot <- ggplot(rms:::Predict(lung_lcd_rcs_model, hliScore=seq(4, 20),
                                          ref.zero = TRUE,
                                          fun=exp),
                            xlab= "Healthy Lifestyle Index score",
                            ylab="Hazard Ratio") + 
  ggtitle("LC, cause-specific death, 5 knots")

