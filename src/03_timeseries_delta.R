library(metR)
library(ggplot2)
library(data.table)
library(lubridate)
library(rcdo)


models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", 
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4")

threshold <- "percentile"
# percentiles <- 100 - c(0.1, 0.5,  1, 2, 2.5, 5, 7.5, 10)
percentiles <- 100 - c(2.5)


# Compare with -10 degrees
#   Sum 1 over years 
#   Ensamble mean --> time series of frequency of events

file_list <- c(Sys.glob("/g/data/w40/pc2687/cf/data/old/100km/deltat_day_*ssp585*"),
               Sys.glob("/g/data/w40/pc2687/cf/data/old/100km/deltat_day_*hist*"))

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
    
    outfile1 <- paste0("~/t-drop-trends/data/temp/100km/", "deltap", p, "_year_", model, "_", experiment, "_", member, "_20150101-21001231_DJF.nc")
    outfile2 <- paste0("~/t-drop-trends/data/temp/100km/", "deltap", p, "_year_", model, "_", experiment, "_", member, "_20150101-21001231_JJA.nc")
    dir.create(dirname(outfile1), showWarnings = FALSE, recursive = TRUE)
    
    write(paste0("processing ", basename(outfile1)), file = "~/log", append = TRUE)
    
    # if (file.exists(outfile1)) {
    #   return(outfile1)
    # }
    
    if (threshold == "constant") {
      
      cdo_lec(f, c = -10) |>
        cdo_yearsum() |> 
        cdo_execute(outfile, options = "-L")
      
    } else if (threshold == "percentile") {
      
      threshold_file1 <- Sys.glob(here::here(paste0("t-drop-trends/data/temp/percentiles/deltat_", model, "_historical_*_p", p, "_1950-1979_DJF.nc")))
      threshold_file2 <- Sys.glob(here::here(paste0("t-drop-trends/data/temp/percentiles/deltat_", model, "_historical_*_p", p, "_1950-1979_JJA.nc")))
      
      if (length(threshold_file1) == 0 || !file.exists(threshold_file1)) {
        return(paste0("no percentile calcualted for this model ", model))
      }
      
      message(paste0("threshold: ", basename(threshold_file1)))
      
      cdo_selseason(f, "DJF") |> 
        cdo_le(threshold_file1) |> 
        cdo_yearsum() |>
        cdo_execute(outfile1, options = "-L")
      
      cdo_selseason(f, "JJA") |> 
        cdo_le(threshold_file2) |> 
        cdo_seassum() |>
        cdo_execute(outfile2, options = "-L")

      
    } else {  # land vs sea threshold
      
      cdo_le(f,  "~/t-fall/data/cmip/threshold_l10s04_cmip.nc") |> 
        # cdo_yearsum() |> 
        cdo_seassum() |>
        cdo_execute(outfile, options = "-L")
      
    }
    
  })
  
})
