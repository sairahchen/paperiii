library(stringr)
library(dplyr)
library(png)
library(zip)

#### MODEL TEMPLATING ####

#### Concepts ####

# MODEL: a list object with the following elements
#  model - a logical vector indicating the list is a model
#  predictors - character vector of model covariates
#  outcome_str - string containing Cox Surv() object specification
#  names - character vector of short text strings which will be concatenated to produce the model identifier/name

# MODEL TEMPLATE: a function that takes a model and returns a modified model
#  - Usually adds to, or changes existing model elements (see above).
#  - Usually adds an addition short string to the "names" element

# RESULT OBJECT: an augmented regression fit object with additional elements
#  - original fit object
#  - model specification
#  - additional result table with term names, point estimates, confidence intervals

# POSTPROCESSING: a user-supplied/applied function that adds to the result table whatever 
# formatted columns the user desires, derived from existing columns.

# RESULT VECTOR: named character vector of formatted results derived from the result tables

# RESULT INPUT TEMPLATE: an Excel .xlsx spreadsheet with formatted tables
#   whose cells contain the string identifiers which correspond to
#   the names of values in the RESULT VECTOR.

# RESULT OUTPUT TEMPLATE: an Excel .xlsx spreadsheet with formatted tables
#   whose cells contain the corresponding values substituted from th
#   RESULT VECTOR.

#### Template helper functions ####

# Applies templates to models sequentially
serial_apply <- function(models, templates){
  
  assertthat::assert_that( is.list(models)    )
  assertthat::assert_that( is.list(templates) )
  
  for(template in templates){
    assertthat::assert_that( is.function(template) )
    models <- lapply(models, template)
  }
  models %<>% update_model_names
  return(models)
  
}

# Applies M templates to N models in parallel
# Outputs a list of M*N models
parallel_apply <- function(models, templates){
  
  assertthat::assert_that( is.list(templates)    )
  assertthat::assert_that( is.list(models)       )
  assertthat::assert_that( is.null(models$model) )
  
  models <- lapply(models, 
                   function(model, templates){  
                     lapply(templates, function(model, template){
                       model %<>% template 
                     }, model=model)
                   }, templates=templates) %>% 
    unlist(recursive = FALSE)
  
  models %<>% update_model_names
  return(models)
  
}

# Updates model names from "names" element in each model
# Used after templates are applied.
update_model_names <- function(models, sep = ""){
  
  assertthat::assert_that( is.list(models) )
  
  models %<>% lapply(function(m){m$fullname = paste0(m$names, collapse=sep); m }) 
  names(models) <- lapply(models, function(m){ paste0(m$names, collapse=sep)}) %>% unlist
  return(models)
  
}

# Runs a model on a dataset ; formats result table
run_model <- function(model, dataset){
  message("Running model: ", model$fullname)
  formula_str = paste( model$outcome_str , "~", paste0(model$predictors, collapse=" + "))
  fit <- coxph(as.formula(formula_str), data = dataset %>% model$filter() )
  fit$model <- model # Required
  fit.ci <- fit %>%  confint(.) %>% data.frame
  names(fit.ci) <- c("lcl", "ucl")
  fit$table <- cbind(broom::tidy(fit), fit.ci ) # Required
  fit
}



 #### OUTPUT TEMPLATING ####


updateOfficeTemplate <- function(template_file, output_file, update_values, images=F){
  
  if(length(update_values) < 1) stop("No update values specified!")
  
  if(is.null(names(update_values))) stop("Update values are unnamed. Value names are required!")
  
  if(any(names(update_values) == "")) stop(
    sprintf("Update values must ALL have names! Missing names at positions: %s", paste0(which(names(update_values) == ""), collapse=", "))
  )
  
  if(any(duplicated(names(update_values)))){
   dup_idx <- which(duplicated(names(update_values)))
   dup_idx <- which(names(update_values) %in% names(update_values)[dup_idx]) 
   for(i in dup_idx){
     message(sprintf("Result: %s -> %s", names(update_values)[i], update_values[i])) 
   }
   stop("Duplicate result value names detected!")
  }
  
  if(!file.exists(template_file)) {
    stop(sprintf('Template file "%s" does not exist!\n', template_file))
  }
  
  template_file_type <- case_when(
    str_ends(tolower(template_file), ".docx") ~ "DOCX", # Word Document
    str_ends(tolower(template_file), ".xlsx") ~ "XLSX", # Excel Spreadsheet
    str_ends(tolower(template_file), ".pptx") ~ "PPTX", # PowerPoint Presentation
    TRUE ~ "unknown"
  )
  
  output_file_type <- case_when(
    str_ends(tolower(output_file), ".docx") ~ "DOCX", # Word Document
    str_ends(tolower(output_file), ".xlsx") ~ "XLSX", # Excel Spreadsheet
    str_ends(tolower(output_file), ".pptx") ~ "PPTX", # PowerPoint Presentation
    TRUE ~ "unknown"
  )
  
  if(template_file_type == "unknown") {
    warning(sprintf('Template file "%s" does not have a .docx, .xlsx, or .pptx extension. Unexpected behavior may occur.\n', template_file))
  }
  
  if(template_file_type != output_file_type) {
    stop(sprintf('File extensions for template and output files do not match!\nTemplate file: "%s"\n  Output file: "%s"\n', template_file, output_file))
  }
  message("") 
  message("Loading template file: ", appendLF=FALSE)
  cat(basename(template_file))
  message(" ... ", appendLF = FALSE)
  
       orig_dir <- getwd() # Save the current working directory
  base_temp_dir <- tempdir() # Get the temporary directory
       temp_dir <- tempfile(pattern = "dir")
output_zip_file <- file.path(base_temp_dir, basename(output_file))
       dir.create(temp_dir, showWarnings = TRUE)
  if(!dir.exists(temp_dir)){ stop("Unable to create temp_dir: ", temp_dir)}
       
  unzip(zipfile = template_file, exdir = temp_dir)
  print(list.files(temp_dir))
  
  message("done.")
  
  if(images == T){
    
    template_files <- list.files(path = temp_dir, pattern = "*.png$", recursive = T)
    
    message("Updating labelled template images:")
    
    mfn <- max(nchar(template_files))
    
    any_updated = FALSE
    for(filepath in template_files){
      
      
      file_label <- getPNGlabel(file.path(temp_dir, filepath))
      
      message(sprintf(paste0("%",mfn,"s: %s"), filepath, file_label), appendLF = FALSE)
      
      updated = FALSE
      if(file_label %in% names(update_values)){
        file.copy(from=update_values[file_label], to=file.path(temp_dir, filepath), overwrite=TRUE)
        message(sprintf(" <- %s", update_values[file_label]))
        any_updated = updated = TRUE
      }
      if(!updated) { 
        message("")
      } 
      
    }
    
    if(!any_updated) {  warning("No matching labelled images found in template file!") }
     
  } else if(images == F) {
    template_files <- list.files(path = temp_dir, pattern = "*.xml$", recursive = T)
      
    mtf <- max(nchar(template_files))
    mun <- max(nchar(names(update_values)))
    muv <- max(nchar(update_values))
    
    message("Updating template result names with values:")
    
    any_updated = FALSE
    for(filepath in template_files){
      
      fn = file(file.path(temp_dir, filepath), "r")
      data <- readLines(con=fn, warn=FALSE)
      close(fn)
    
      message(sprintf(paste0("%",mtf,"s:"), filepath ), appendLF = FALSE)
      
      updated = FALSE
      for( result in names(update_values) ){
        if(any(str_detect(data, result))){
          any_updated = updated = TRUE
          cat(sprintf(paste0("\n          %",mun,"s -> %",muv,"s"), result, update_values[[result]]))
          data = str_replace_all(data, result, update_values[[result]])
        }
      }
      if(!updated) { 
        message(" No matches.")
      } else {
        message("")
        fn = file(file.path(temp_dir, filepath), "w")
        writeLines(data, fn)
        close(fn)
      }
      
    }
    
    
    if(!any_updated) {  warning("No result names found in template file!") }
      
    
  } else {
    stop("Weird value for 'image' argument. Should be TRUE or FALSE.")
  }
  
  
  setwd(temp_dir)
  message("Reconstituting template file ... ", appendLF = FALSE)
  #zip(output_zip_file, files = list.files(), extras = "-q")
  zip::zip(output_zip_file, files = list.files(), mode="mirror")
  if(!file.exists(output_zip_file)) { stop("Internal zip file was not created: ", output_zip_file)}
  message("done.")
  message("    Creating new output file: ", appendLF = FALSE)
  cat(basename(output_file))
  message(" ... ", appendLF=FALSE)
  setwd(orig_dir)
  file.copy(output_zip_file, output_file, overwrite = TRUE)
  if(!file.exists(output_file)) { stop("Output zip file was not copied successfully: ", output_file)}
  message("done.")
  unlink(output_zip_file)
  unlink(temp_dir, recursive = TRUE)
  
} 


labelPNG <- function(filename, label){
  r <- readPNG(filename, info=TRUE)
  if(!is.null(attr(r, "info")) && "text" %in% names(attr(r, "info"))){
    if("label" %in% names(attr(r, "info")[["text"]])){
      attr(r, "info")[["text"]]["label"] <- label 
    } else {
      attr(r, "info")[["text"]] <- c(label = label, attr(r, "info")[["text"]])
    }
    writePNG(r, target = filename, text = attr(r, "info")[["text"]])
  } else {
    writePNG(r, target = filename, text = c(label = label))
  }
}


getPNGlabel <- function(filename){
  r <- readPNG(filename, info=TRUE)
  if(!is.null(attr(r, "info")) && 
     "text" %in% names(attr(r, "info")) &&
     "label" %in% names(attr(r, "info")[["text"]])
     ){
    return(attr(r, "info")[["text"]]["label"])
  } else {
    return("")
  }
}


