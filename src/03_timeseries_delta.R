library(metR)
library(ggplot2)
library(data.table)
library(lubridate)
library(rcdo)


models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", 
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4")

threshold <- "percentile"
percentiles <- 100 - c(0.1, 0.5,  1, 2, 2.5, 5, 7.5, 10)


# Compare with -10 degrees
#   Sum 1 over years 
#   Ensamble mean --> time series of frequency of events

file_list <- c(Sys.glob("~/t-fall/data/cmip/100km/deltat_day_*ssp585*"),
               Sys.glob("~/t-fall/data/cmip/100km/deltat_day_*hist*"))

future::plan(future::multisession, workers = 4)
furrr::future_map(percentiles, function(p) {
  purrr::map(file_list, function(f) {
    
    meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
    
    model <- meta[[1]][["model"]]
    member <- meta[[1]][["member"]]
    experiment <- meta[[1]][["scenario"]]
    
    message(paste0("processing ", basename(f)))
    
    if (!(model %in% models_in_gadi)) {
      message(paste0("skiping ", model))
      return(f)
    }
    
    outfile <- paste0("~/t-drop-trends/data/temp/100km/", "deltap", p, "_year_", model, "_", experiment, "_", member, "_20150101-21001231.nc")
    dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
    
    write(paste0("processing ", basename(outfile)), file = "~/log", append = TRUE)
    
    if (file.exists(outfile)) {
      return(outfile)
    }
    
    if (threshold == "constant") {
      
      cdo_lec(f, c = -10) |>
        cdo_yearsum() |> 
        cdo_execute(outfile, options = "-L")
      
    } else if (threshold == "percentile") {
      
      threshold_file <- Sys.glob(here::here(paste0("data/temp/percentiles/deltat_", model, "_historical_*_p", p, "_1950-1979.nc")))
      
      if (length(threshold_file) == 0 || !file.exists(threshold_file)) {
        return(paste0("no percentile calcualted for this model ", model))
      }
      
      message(paste0("threshold: ", basename(threshold_file)))
      
      cdo_le(f, threshold_file) |> 
        # cdo_seassum() |>
        cdo_yearsum() |>
        cdo_execute(outfile, options = "-L")
      
    } else {  # land vs sea threshold
      
      cdo_le(f,  "~/t-fall/data/cmip/threshold_l10s04_cmip.nc") |> 
        # cdo_yearsum() |> 
        cdo_seassum() |>
        cdo_execute(outfile, options = "-L")
      
    }
    
  })
  
})
