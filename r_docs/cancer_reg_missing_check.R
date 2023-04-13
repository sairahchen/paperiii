# Hormone receptor status data check


erMissing <- lapply(split(breast, cut(breast$dateDiagnosis, "1 year")),
                    function(x) sum(is.na(x["estrogenRec"]))) %>% unlist()
erMissing <- data.frame(names(erMissing), erMissing)
rownames(erMissing) <- 1:nrow(erMissing)
erMissing$yearDiagnosis <- year(erMissing$names.erMissing.)
plot(erMissing$yearDiagnosis, erMissing$erMissing)


prMissing <- lapply(split(breast, cut(breast$dateDiagnosis, "1 year")),
                    function(x) sum(is.na(x["progesteroneRec"]))) %>% unlist()
prMissing <- data.frame(names(prMissing), prMissing)
rownames(prMissing) <- 1:nrow(prMissing)
prMissing$yearDiagnosis <- year(prMissing$names.prMissing.)
plot(prMissing$yearDiagnosis, prMissing$prMissing)

her2Missing <- lapply(split(breast, cut(breast$dateDiagnosis, "1 year")),
                    function(x) sum(is.na(x["her2Expression"]))) %>% unlist()
her2Missing <- data.frame(names(her2Missing), her2Missing)
rownames(her2Missing) <- 1:nrow(her2Missing)
her2Missing$yearDiagnosis <- year(her2Missing$names.her2Missing.)
plot(her2Missing$yearDiagnosis, her2Missing$her2Missing)

# Treatment variables

## Plots of treatment x calendar year


chemo_yes_plot <- function(dataframe){
chemoYes <- lapply(split(dataframe, cut(dataframe$dateDiagnosis, "1 year")),
                       function(x) sum(x$chemotherapy == "Yes")/count(x)) %>% unlist()
chemoYes <- data.frame(names(chemoYes), chemoYes)
rownames(chemoYes) <- 1:nrow(chemoYes)
chemoYes$yearDiagnosis <- year(chemoYes$names.chemoYes)
plot(chemoYes$yearDiagnosis, chemoYes$chemoYes,
     main= deparse(substitute(dataframe)))

}

chemo_yes_plot(breast)
chemo_yes_plot(colorectal)
chemo_yes_plot(lung)




rad_yes_plot <- function(dataframe){
radYes <- lapply(split(dataframe, cut(dataframe$dateDiagnosis, "1 year")),
                   function(x) sum(x$radiationTherapy == "Yes")/count(x)) %>% unlist()
radYes <- data.frame(names(radYes), radYes)
rownames(radYes) <- 1:nrow(radYes)
radYes$yearDiagnosis <- year(radYes$names.radYes)
plot(radYes$yearDiagnosis, radYes$radYes,
     main= deparse(substitute(dataframe)))
}

rad_yes_plot(breast)
rad_yes_plot(colorectal)
rad_yes_plot(lung)


horm_yes_plot <- function(dataframe){
hormYes <- lapply(split(dataframe, cut(dataframe$dateDiagnosis, "1 year")),
                 function(x) sum(x$hormonalTherapy == "Yes")/count(x)) %>% unlist()
hormYes <- data.frame(names(hormYes), hormYes)
rownames(hormYes) <- 1:nrow(hormYes)
hormYes$yearDiagnosis <- year(hormYes$names.hormYes)
plot(hormYes$yearDiagnosis, hormYes$hormYes,
     main= deparse(substitute(dataframe)))
}

horm_yes_plot(breast)
horm_yes_plot(colorectal)
horm_yes_plot(lung)


oth_yes_plot <- function(dataframe){
othYes <- lapply(split(dataframe, cut(dataframe$dateDiagnosis, "1 year")),
                  function(x) sum(x$otherTreatment == "Yes")/count(x)) %>% unlist()
othYes <- data.frame(names(othYes), othYes)
rownames(othYes) <- 1:nrow(othYes)
othYes$yearDiagnosis <- year(othYes$names.othYes)
plot(othYes$yearDiagnosis, othYes$othYes,
     main= deparse(substitute(dataframe)))
}

oth_yes_plot(breast)
oth_yes_plot(colorectal)
oth_yes_plot(lung)



treatment_table <- 
  df %>% 
  select(caseType, 
         chemotherapy,
         radiationTherapy,
         hormonalTherapy,
         otherTreatment
         
  ) %>% # keep only columns of interest
  tbl_summary(     
    by = caseType,                                               # stratify entire table by outcome
    statistic = list(all_continuous() ~ "{mean} ({sd})",        # stats and format for continuous columns
                     all_categorical() ~ "{n} ({p}%)"),   # stats and format for categorical columns
    digits = all_continuous() ~ 1,                              # rounding for continuous columns
    type   = all_categorical() ~ "categorical",                 # force all categorical levels to display
    label  = list(                                              # display labels for column names
      caseType ~ "Case type", 
      chemotherapy ~ "Chemotherapy",
      radiationTherapy ~ "Radiation",
      hormonalTherapy ~ "Hormonal therapy",
      otherTreatment ~ "Other treatment"),
    missing_text = "Missing"    # how missing values should display 
  ) %>%
  #modify_spanning_header(all_stat_cols() ~ "**Cancer case type**") %>%
  modify_footnote(all_stat_cols() ~ "Mean (sd) for continuous and frequency (%) for categorical characteristics") %>%
  bold_labels() 



treatment_table %>%
  as_flex_table() %>%
  add_header_row(values = c("", "Cancer case type"), colwidths = c(1, 3)) %>%
  add_header_lines(values = "Table 1. Cancer treatments according to cancer case type, Norwegian women and cancer study, N = 9287") %>%
  bold(bold = TRUE, i = 2, part = "header") %>%
  flextable::save_as_docx(path="./output/table_treatment.docx")
