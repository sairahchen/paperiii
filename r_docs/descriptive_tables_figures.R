# Table 1



# Characteristics at questionnaire

table_1 <- 
  df %>% 
  select(caseType, 
         ageQuestionnaire,
         active, 
         bmi, 
         smokingStatus,
         gramsAlcohol,
         dietScore,
         hliScore,
         hliTertilesLevels,
         educationLowHigh,
         menopausalStatus,
         hrtStatus,
         ocEverUse,
         parityCategories,
         breastfeedingCategories,
         familyHistBC
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = caseType,                                               # stratify entire table by outcome
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      caseType ~ "Case type", 
      ageQuestionnaire ~ "Age at questionnaire (years)",
      active ~ "Physical activity",
      bmi ~ "Body mass index",
      smokingStatus ~ "Smoking status",
      gramsAlcohol ~ "Alcohol intake (grams/day)",
      dietScore ~ "Diet score (0-4)",
      hliScore ~ "HLI score",
      hliTertilesLevels ~ "HLI tertiles",
      educationLowHigh ~ "Education (years)",
      menopausalStatus ~ "Menopausal status",
      hrtStatus ~ "Hormone replacement therapy status",
      ocEverUse ~ "Oral contraceptive use",
      parityCategories ~ "Parity",
      breastfeedingCategories ~ "Breastfeeding",
      familyHistBC ~ "Family history breast cancer"),
    missing_text = "Missing"    # how missing values should display 
    ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



table_1 %>%
  as_flex_table() %>%
  add_header_row(values = c("", "Cancer case type"), colwidths = c(1, 3)) %>%
  add_header_lines(values = "Table 1. Prediagnosis characteristics of the study sample according to cancer case type, Norwegian women and cancer study, N = 9286") %>%
  #bold(bold = TRUE, i = 2, part = "header") %>%
  footnote(i = 28, j = 1, 
           value = as_paragraph("Cutoffs for healthy lifestyle index score categories were determined by quartiles for breast, colorectal, and lung cancer survival cohorts, separately. Quartile 1: breast 3-9, colorectal 3-9, lung 2-8; Quartile 2: breast 10-12, colorectal 10-12, lung 9-10; Quartile 3: breast 13-14, colorectal 13-14, lung 11-12; Quartile 4: breast 15-20, colorectal 15-20, lung 13-20"),
           ref_symbols = "2",
           part = "body") %>%
  #span_header() %>%
  flextable::save_as_docx(path="./output/table_1.docx")





# Characteristics at diagnosis


table_2 <- 
  df %>% 
  select(caseType, 
         ageDiagnosis,
         lagYears,
         tnmAnatomicStage, 
         seerStage,
         estrogenRec,
         progesteroneRec,
         her2Expression
  
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = caseType,                                               # stratify entire table by outcome
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0,0)
                  ), # number of decimals
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      caseType ~ "Case type", 
      ageDiagnosis ~ "Age at diagnosis (years)",
      lagYears ~ "Years between baseline and diagnosis",
      tnmAnatomicStage ~ "TNM stage at diagnosis",
      seerStage ~ "SEER stage",
      estrogenRec ~ "Estrogen receptor status",
      progesteroneRec ~ "Progesterone receptor status",
      her2Expression ~ "HER2 expression"
      
),
    missing_text = "Missing"    # how missing values should display 
  ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



table_2 %>%
  as_flex_table() %>%
  add_header_row(values = c("", "Cancer case type"), colwidths = c(1, 3)) %>%
  add_header_lines(values = "Table 2. Cancer diagnosis characteristics of the study sample according to cancer case type, Norwegian women and cancer study, N = 9286") %>%
  #bold(bold = TRUE, i = 2, part = "header") %>%
  footnote(i = 3, j = 1, 
           value = as_paragraph("TNM stage only available for breast cancer from the Cancer Registry of Norway"),
           ref_symbols = "3",
           part = "body") %>%
  footnote(i = 2, j = 1, 
           value = as_paragraph("Years between questionnaire and cancer diagnosis"),
           ref_symbols = "2",
           part = "body") %>%

  #span_header() %>%
  flextable::save_as_docx(path="./output/table_2.docx")

# Kaplan Meier curves

## Breast cancer survivors
fit_breast <- survfit(Surv(followUpTimeYears, totalDeath) ~ hliTertiles, data = breast)
km_breast <- ggsurvplot(fit_breast,
                        xlab = "Years since diagnosis",
                        legend.labs = c("0-10", "11-14", "15-20"),
                        legend.title = "HLI tertile",
                        linetype = c("dotted", "solid", "dashed"),
                        palette = c("black", "black", "black"),
                        censor=F)
  #guides(colour = guide_legend(nrow = 2))




## Colorectal cancer survivors
fit_colorectal <- survfit(Surv(followUpTimeYears, totalDeath) ~ hliTertiles, data = colorectal)
km_colorectal <- ggsurvplot(fit_colorectal,
                            xlab = "Years since diagnosis",
                            legend.labs = c("0-10", "11-14", "15-20"),
                            legend.title = "HLI tertile",
                            linetype = c("dotted", "solid", "dashed"),
                            palette = c("black", "black", "black"),                            
                            censor=F) 
  #guides(colour = guide_legend(nrow = 2))
                            


## Lung cancer survivors
fit_lung <- survfit(Surv(followUpTimeYears, totalDeath) ~ hliTertiles, data = lung)
km_lung <- ggsurvplot(fit_lung,
                      xlab = "Years since diagnosis",
                      legend.labs = c("0-9", "10-12", "13-20"),
                      legend.title = "HLI quartile",
                      linetype = c("dotted", "solid", "dashed"),
                      palette = c("black", "black", "black"),
                      censor=F) 
  #guides(colour = guide_legend(nrow = 2))
  #labs(tag="C. Lung")


ggarrange(km_breast$plot, km_colorectal$plot, km_lung$plot,
          ncol=3,
          widths = c(1,1,1))
ggsave("./output/figure_1.jpeg",
       width = 32, height = 15, units = "cm") # play with the width and height to get it right. Almost there...:)





ggsave("./output/submission/figure_1.tiff", 
       plot = km,
       width = 15, 
       height= 7)

# facet plots by stage
fit_lung_stage <- survfit(Surv(followUpTimeYears, totalDeath) ~ hliCategories + seerStage, data = lung)
km_lung_stage <- ggsurvplot(fit_lung_stage)
km_lung_stage <- km_lung_stage$plot + theme_bw()+facet_wrap(~seerStage)
ggsave("./output/km_lung_stage.tiff",
       plot = km_lung_stage,
       width = 20, height = 20)

#----

# Survival outcomes

survival_table_5_10 <- function(dataframe){
  
  survival_table <- tbl_survfit(
    list(
      survfit(Surv(followUpTimeDays, totalDeath) ~ 1, dataframe),
      survfit(Surv(followUpTimeDays, totalDeath) ~ hliTertilesLevels, dataframe)
    ),
    times = c(1826.25,3652.5),
    label = hliTertilesLevels ~ "HLI quartile",
    label_header = "{time/365.25} Year"
  )
  
  
}

breast_survival_table_5_10 <- survival_table_5_10(breast)
colorectal_survival_table_5_10 <- survival_table_5_10(colorectal)
lung_survival_table_5_10 <- survival_table_5_10(lung)


stacked_survival_table <- tbl_stack(list(breast_survival_table_5_10, 
                                         colorectal_survival_table_5_10,
                                         lung_survival_table_5_10),
                                         group_header = c("Breast", "Colorectal", "Lung"))


flextable <- stacked_survival_table %>%
  as_flex_table() %>%
 set_header_labels(groupname_col = "Cancer type") %>%
  flextable::save_as_docx(path="./output/table_3.docx")


survival_table_5 <- function(dataframe){
  
  survival_table <- tbl_survfit(
    list(
      survfit(Surv(followUpTimeDays, totalDeath) ~ 1, dataframe),
      survfit(Surv(followUpTimeDays, totalDeath) ~ hliTertilesLevels, dataframe)
    ),
    times = c(1826.25),
    label = hliTertilesLevels ~ "HLI quartile",
    label_header = "{time/365.25} Year"
  ) 
  
}

breast_survival_table_5 <- survival_table_5(breast)
colorectal_survival_table_5 <- survival_table_5(colorectal)
lung_survival_table_5 <- survival_table_5(lung)

merged_survival_tables <- tbl_merge(tbls=list(breast_survival_table_5, colorectal_survival_table_5, lung_survival_table_5),
                                    tab_spanner = c("Breast", "Colorectal", "Lung")) %>% 
  as_hux_table() %>% 
  slice(-1, -2) %>% 
  as_flextable() %>% 
  autofit() %>%
  as_raster(webshot = "webshot2")





# --
# try making single tables

breast_survival_table <- as_hux_table(breast_survival_table_5) %>%
  slice(-1,-2) %>%
  as_flextable() %>%
  autofit() %>%
  as_raster(webshot = "webshot2")

colorectal_survival_table <- as_hux_table(colorectal_survival_table_5) %>%
  slice(-1,-2) %>%
  as_flextable() %>%
  autofit() %>%
  as_raster(webshot = "webshot2")

lung_survival_table <- as_hux_table(lung_survival_table_5) %>%
  slice(-1,-2) %>%
  as_flextable() %>%
  autofit() %>%
  as_raster(webshot = "webshot2")

gg_surv_table_breast <- ggplot() +
  theme_void() + 
  annotation_custom(rasterGrob(breast_survival_table))
gg_surv_table_colorectal <- ggplot() +
  theme_void() + 
  annotation_custom(rasterGrob(colorectal_survival_table))
gg_surv_table_lung <- ggplot() +
  theme_void() + 
  annotation_custom(rasterGrob(lung_survival_table))


# Check descriptives with each HLI quartile grouped by case type

# breast

table_bc <- 
  breast %>% 
  select(
         ageQuestionnaire,
         active, 
         bmi, 
         smokingStatus,
         gramsAlcohol,
         dietScore,
         hliScore,
         hliCategories,
         educationLowHigh,
         menopausalStatus,
         hrtStatus,
         ocEverUse,
         parityCategories,
         breastfeedingCategories,
         familyHistBC
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = hliCategories,                                               # stratify entire table by quartile
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
       
      ageQuestionnaire ~ "Age at questionnaire (years)",
      active ~ "Physical activity",
      bmi ~ "Body mass index",
      smokingStatus ~ "Smoking status",
      gramsAlcohol ~ "Alcohol intake (grams/day)",
      dietScore ~ "Diet score (0-4)",
      hliScore ~ "HLI score",
      hliCategories ~ "HLI quartiles",
      educationLowHigh ~ "Education (years)",
      menopausalStatus ~ "Menopausal status",
      hrtStatus ~ "Hormone replacement therapy status",
      ocEverUse ~ "Oral contraceptive use",
      parityCategories ~ "Parity",
      breastfeedingCategories ~ "Breastfeeding",
      familyHistBC ~ "Family history breast cancer"),
    missing_text = "Missing"    # how missing values should display 
  ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



table_bc %>%
  as_flex_table() %>%
  add_header_row(values = c("", "HLI Quartiles"), colwidths = c(1, 4)) %>%
  add_header_lines(values = "Table 1. Prediagnosis characteristics of women diagnosed with breast cancer") %>%
  #span_header() %>%
  flextable::save_as_docx(path="./output/table_bc.docx")

# colorectal

table_crc <- 
  colorectal %>% 
  select(
    ageQuestionnaire,
    active, 
    bmi, 
    smokingStatus,
    gramsAlcohol,
    dietScore,
    hliScore,
    hliCategories,
    educationLowHigh,
    menopausalStatus,
    hrtStatus,
    ocEverUse,
    parityCategories,
    breastfeedingCategories,
    familyHistBC
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = hliCategories,                                               # stratify entire table by quartile
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      
      ageQuestionnaire ~ "Age at questionnaire (years)",
      active ~ "Physical activity",
      bmi ~ "Body mass index",
      smokingStatus ~ "Smoking status",
      gramsAlcohol ~ "Alcohol intake (grams/day)",
      dietScore ~ "Diet score (0-4)",
      hliScore ~ "HLI score",
      hliCategories ~ "HLI quartiles",
      educationLowHigh ~ "Education (years)",
      menopausalStatus ~ "Menopausal status",
      hrtStatus ~ "Hormone replacement therapy status",
      ocEverUse ~ "Oral contraceptive use",
      parityCategories ~ "Parity",
      breastfeedingCategories ~ "Breastfeeding",
      familyHistBC ~ "Family history breast cancer"),
    missing_text = "Missing"    # how missing values should display 
  ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



table_crc %>%
  as_flex_table() %>%
  add_header_row(values = c("", "HLI Quartiles"), colwidths = c(1, 4)) %>%
  add_header_lines(values = "Table 1. Prediagnosis characteristics of women diagnosed with colorectal cancer") %>%
  #span_header() %>%
  flextable::save_as_docx(path="./output/table_crc.docx")


# lung

table_lc <- 
  lung %>% 
  select(
    ageQuestionnaire,
    active, 
    bmi, 
    smokingStatus,
    gramsAlcohol,
    dietScore,
    hliScore,
    hliCategories,
    lagYears,
    educationLowHigh,
    menopausalStatus,
    hrtStatus,
    ocEverUse,
    parityCategories,
    breastfeedingCategories,
    familyHistBC
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = hliCategories,                                               # stratify entire table by quartile
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      
      ageQuestionnaire ~ "Age at questionnaire (years)",
      active ~ "Physical activity",
      bmi ~ "Body mass index",
      smokingStatus ~ "Smoking status",
      gramsAlcohol ~ "Alcohol intake (grams/day)",
      dietScore ~ "Diet score (0-4)",
      hliScore ~ "HLI score",
      hliCategories ~ "HLI quartiles",
      lagYears ~ "Prediagnostic interval",
      educationLowHigh ~ "Education (years)",
      menopausalStatus ~ "Menopausal status",
      hrtStatus ~ "Hormone replacement therapy status",
      ocEverUse ~ "Oral contraceptive use",
      parityCategories ~ "Parity",
      breastfeedingCategories ~ "Breastfeeding",
      familyHistBC ~ "Family history breast cancer"),
    missing_text = "Missing"    # how missing values should display 
  ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



table_lc %>%
  as_flex_table() %>%
  add_header_row(values = c("", "HLI Quartiles"), colwidths = c(1, 4)) %>%
  add_header_lines(values = "Table 1. Prediagnosis characteristics of women diagnosed with lung cancer") %>%
  #span_header() %>%
  flextable::save_as_docx(path="./output/table_lc.docx")

