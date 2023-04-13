# Kaplan Meier


km_bc_td <- ggsurvplot(
  fit = survfit(Surv(followUpTimeDays, totalDeath) ~ hliCategories, data = breast), 
  xlab = "Days since diagnosis", 
  ylab = "Overall survival probability",
  pval = TRUE, conf.int = TRUE,
  risk.table = TRUE, # Add risk table
  risk.table.col = "strata", # Change risk table color by groups
  linetype = "strata", # Change line type by groups
  surv.median.line = "hv",# Specify median survival
  title="Kaplan-Meier curves to estimate survival function of \nHealthy Lifestyle Index quartiles"
  )

ggsave("./output/km_bc_td.tiff", print(km_bc_td), width = 5, height= 5, units = "cm")

# Flexible parametric

library(rstpm2)

# Omit NAs in relevant variables
br <- breast[!(is.na(breast$hliCategoriesLevels)), ]
br <- br[!(is.na(br$totalDeath)), ]
br <- br[!(is.na(br$followUpTimeDays)), ]

# Fit survival model with natural splines 
fit <- stpm2(Surv(followUpTimeDays, totalDeath==1)~ hliCategoriesLevels, data=br, df=4)
summary(fit)

# Retreive HRs and 95% CIs
eform(fit)

# Predict for plot
predHliLevels <- predict(fit, newdata=data.frame(hliCategoriesLevels=c("1")), type ="hazard", grid=TRUE, full=TRUE, se.fit=TRUE)
predHliLevels <- predict(fit, newdata=data.frame(hliCategoriesLevels=c("1", "4")), type ="hazard", grid=TRUE, full=TRUE, se.fit=TRUE)
predHliLevels <- transform(predHliLevels,
                           HLI=factor(hliCategoriesLevels,
                                      labels=c("Q1 HLI 3-9 (Unhealthiest)","Q4 HLI 15-20 (Healthiest)")))  

# Plot hazards for lowest and highest HLI group in an overlaying plot
layered_hz_plot <- ggplot(predHliLevels,
                          aes(x=followUpTimeDays,y=Estimate,ymin=lower,ymax=upper,fill=HLI)) +
  xlab("Time since diagnosis (days)") +
  ylab("Hazard") +
  geom_ribbon(alpha=0.6) +
  geom_line() +
  labs(title = "Hazards for total mortality among breast cancer survivors for 1st and 4th HLI quartiles",
       caption = "Hazard functions for the association between HLI and total mortality over days since diagnosis. \nObtained from flexible parametric survival model using restricted cubic splines with 2 internal knots. \n This model is unadjusted" 
  )
ggsave("./output/hz_bc_td.tiff", width = 20, height = 15, units = "cm")
