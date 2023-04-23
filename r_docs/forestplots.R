# Forest plot ----

source("./r_docs/06_analysis_imputed.R")

# Figure 3 - Main results

## Create style for forest plot
fpstyle <- fpShapesGp(
  box = list(
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red")
    
  )
)

# set graphic element design
own <- fpTxtGp()
own # see parameters
own <- fpTxtGp(label=gpar(cex=3.00),
               ticks = gpar(cex=2.00)) # change tabletext font

## Create tibble for forest plot
fpdata <- tibble(
  coef = c(0.94, # all cause mortality bc
           0.97, # cancer mortality bc
           0.97, # all cause mortality crc
           0.98, # cancer mortality crc
           1.00, # all cause mortality lc
           1.00 # cancer mortality lc
  ),
  low = c(0.92,
          0.94,
          0.95,
          0.96,
          0.98,
          0.98

          ),
  high = c(0.97,
           1.00,
           1.00,
           1.01,
           1.02,
           1.03),
  survival_cohort = c(
    "Breast cancer",
    "",
    "Colorectal cancer",
    "",
    "Lung cancer",
    ""
    ),
  endpoint = c(
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality"
  ),

  n_deaths = c(
    "912",
    "509",
    "902",
    "679",
    "1094",
    "961"),
  hr = c(
    "0.94 (0.92-0.97)",
    "0.97 (0.94-1.00)",
    "0.97 (0.95-1.00)",
    "0.98 (0.95-1.01)",
    "1.00 (0.98-1.02)",
    "1.00 (0.98-1.03)"
))


fpdata |>
  forestplot(labeltext = c(survival_cohort, n_deaths, hr),
             mean = coef,
             lower = low,
             upper = high,
             boxsize = 0.2,
             zero = 1,
             xticks = c(0.92,0.96,1.00,1.04),
             graphwidth = unit(16, "cm"),
             shapes_gp = fpstyle,
             txt_gp = own,
             lwd.ci = 2) |>

  fp_add_header(
                survival_cohort = c("Case type"),
                n_deaths = c("N deaths"),
                hr = c("HR (95% CI)")) |>
  fp_set_style(align = "l")

dev.copy(jpeg, "./output/submission/figure_2a.jpeg", units="in", width = 20, height = 8, res=800)
dev.off()

# Figure 4 - subgroup by cancer stage

## Specify style

fpstyle <- fpShapesGp(
  box = list(
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red"),
    gpar(fill = "black"),
    gpar(fill = "red")
  )
)


# set graphic element design
own <- fpTxtGp()
own # see parameters
own <- fpTxtGp(label=gpar(cex=3.00),
               ticks = gpar(cex=2.00)) # change tabletext font

## Create tibble for forest plot


fpdata <- tibble(
  coef = c(0.93, # I all cause
           0.99, # I bc 
           0.95, # II all cause 
           0.97, # II bc
           1.00, # III all cause 
           0.98, # III bc
           0.93, # IV all cause
           0.97, # IV bc
           
           0.93, # Local all cause
           0.91, # Local crc
           0.98, # Regional all cause
           0.97, # Regional crc
           0.99, # Distant all cause
           0.99, # Distant crc
           
           0.94, # Local all cause
           0.96, # Local lc
           1.01, # Regional all cause
           1.01, # Regional lc
           1.00, # Distant all cause
           1.00 # Distant lc
           

  ),
  low = c(0.89,
          0.91,
          0.91,
          0.93,
          0.93,
          0.90,
          0.86,
          0.90,
          
          0.86,
          0.76,
          0.94,
          0.93,
          0.96,
          0.96,
          
          0.87,
          0.87,
          0.97,
          0.96,
          0.98,
          0.98
         
          
  ),
  high = c(0.98,
           1.07,
           0.98,
           1.02,
           1.08,
           1.06,
           1.01,
           1.06,
           
           1.01,
           1.09,
           1.01,
           1.02,
           1.02,
           1.03,
           
           1.01,
           1.05,
           1.06,
           1.06,
           1.03,
           1.03
           ),
  survival_cohort = c(
    "Breast cancer",
    "",
    "",
    "",
    "",
    "",
    "",
    "",

    "Colorectal cancer",
    "",
    "",
    "",
    "",
    "",

    "Lung cancer",
    "",
    "",
    "",
    "",
    ""
  ),
  stage = c(
    "I",
    "",
    "II",
    "",
    "III",
    "",
    "IV",
    "",

    "Localised",
    "",
    "Regional",
    "",
    "Distant",
    "",

    "Localised",
    "",
    "Regional",
    "",
    "Distant",
    ""
  ),
  endpoint = c(
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality",

    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality",
    
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality"
  ),
  n_deaths = c(
    "254",
    "71",
    "379",
    "224",
    "110",
    "85",
    "105",
    "98",

    "81",
    "14",
    "352",
    "235",
    "432",
    "410",

    "99",
    "58",
    "304",
    "260",
    "622",
    "591"),
  hr = c(
    "0.93 (0.89-0.98)",
    "0.99 (0.91-1.07)",
    "0.95 (0.91-0.98)",
    "0.97 (0.93-1.02)",
    "1.00 (0.93-1.08)",
    "0.98 (0.90-1.06)",
    "0.93 (0.86-1.01)",
    "0.97 (0.89-1.05)",

    "0.93 (0.86-1.01)",
    "0.91 (0.76-1.09)",
    "0.98 (0.94-1.01)",
    "0.97 (0.93-1.02)",
    "0.99 (0.96-1.02)",
    "0.99 (0.96-1.03)",
    
    "0.94 (0.87-1.01)",
    "0.96 (0.87-1.05)",
    "1.01 (0.97-1.06)",
    "1.01 (0.96-1.06)",
    "1.00 (0.98-1.03)",
    "1.00 (0.98-1.03)")
)

fpdata |>
  forestplot(labeltext = c(survival_cohort, stage, n_deaths, hr),
             mean = coef,
             lower = low,
             upper = high,
             boxsize = 0.3,
             zero = 1,
             xticks = c(0.74, 0.84, 0.94, 1.04, 1.14
                       ),
             graphwidth = unit(20, "cm"),
             shapes_gp = fpstyle,
             txt_gp = own,
             lwd.ci = 2.0) |>
  
  fp_add_header(
    survival_cohort = c("Case type"),
    stage = c("Stage"),
    n_deaths = c("N deaths"),
    hr = c("HR (95% CI)")) |>
  fp_set_style(align = "l")

dev.copy(tiff, "./output/submission/figure_2b.tiff", width = 2000, height = 1600)
dev.off()

dev.copy(jpeg, "./output/submission/figure_2b.jpeg", units="in", width = 28, height = 20, res=600)
dev.off()


# Figure 5 - Breast cancer subgroups

## Create style for forest plot
fpstyle <- fpShapesGp(
  box = list(
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue"),
    gpar(fill = "black"),
    gpar(fill = "blue")
    
  )
)

# set graphic element design
own <- fpTxtGp()
own # see parameters
own <- fpTxtGp(label=gpar(cex=1.00),
               ticks = gpar(cex=0.75)) # change tabletext font

## Create tibble for forest plot
fpdata <- tibble(
  coef = c(0.95, # all-cause
           0.97, # cancer
           0.92, # all cause mortality crc
           0.95, # cancer mortality crc
           0.93, # all cause mortality lc
           0.94 # cancer mortality lc
  ),
  low = c(0.92,
          0.93,
          0.89,
          0.91,
          0.90,
          0.89
          
  ),
  high = c(0.97,
           1.00,
           0.95,
           0.99,
           0.97,
           0.99),
  subgroup = c(
    "Postmenopausal",
    "",
    "ER/PR adjusted",
    "",
    "ER+/PR+",
    ""
  ),

  endpoint = c(
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality",
    "All-cause mortality",
    "Cancer-specific mortality"
  ),
  n_deaths = c(
    "812",
    "443",
    "509",
    "275",
    "477",
    "257"),
  hr = c(
    "0.95 (0.92-0.97)",
    "0.97 (0.93-1.00)",
    "0.92 (0.89-0.95)",
    "0.95 (0.91-0.99)",
    "0.93 (0.90-0.97)",
    "0.94 (0.89-0.99)")
)


fpdata |>
  forestplot(labeltext = c(subgroup, n_deaths, hr),
             mean = coef,
             lower = low,
             upper = high,
             boxsize = 0.2,
             lineheight = unit(1, "cm"),
             zero = 1,
             xticks = c(0.92,0.96,0.98,1.00,1.02,1.04),
             graphwidth = unit(12, "cm"),
             shapes_gp = fpstyle,
             txt_gp = own) |>
  
  fp_add_header(
    subgroup = c("Subgroup"),
    n_deaths = c("N deaths"),
    hr = c("HR (95% CI)")) 

dev.copy(tiff, "./output/figure_5.tiff", width = 1000, height = 200)
dev.off()
