## Histogram only for deltaT < p99

library(data.table)
library(lubridate)
library(rcdo)

file_list <- Sys.glob("~/t-fall/data/cmip/100km/deltat_day_*")
p <- 99
bins <- paste(c("-inf", seq(-25, 0, 0.5), "inf"), sep = ",", collapse = ",")

future::plan(future::multisession, workers = 2)
furrr::future_map(file_list, function(f) {
# purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
  
  model <- meta[[1]][["model"]]
  member <- meta[[1]][["member"]]
  experiment <- meta[[1]][["scenario"]]
  
  message(paste0("processing ", basename(f)))
  
  outfile <- paste0("~/t-fall/data/cmip/histograms/", "histp99_", model, "_", experiment, "_", member, ".nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(outfile)) {
    return(outfile)
  }
  
  threshold_file <- Sys.glob(paste0("~/t-fall/data/cmip/percentiles/deltat_", model, "_historical_*_p", p, "_1979-2014.nc"))
  
  if (length(threshold_file) == 0 || !file.exists(threshold_file)) {
    return(paste0("no percentile calcualted for this model ", model))
  }
  
  message(paste0("threshold: ", basename(threshold_file)))
  
  if (experiment == "historical") {
    
   subset <-  cdo_seldate(f, startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |> 
     cdo_execute(options = "-L")
   
   cdo_le(subset, threshold_file) |> 
      cdo_setctomiss(c = 0) |> 
      cdo_mul(subset)  |> 
      cdo_histcount(bounds = bins) |> 
      cdo_execute(outfile, options = "-L")
    
  } else {
    
    subset <-  cdo_seldate(f, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
      cdo_execute(options = "-L")
    
    cdo_le(subset, threshold_file) |> 
      cdo_setctomiss(c = 0) |> 
      cdo_mul(subset)  |> 
      cdo_histcount(bounds = bins) |> 
      cdo_execute(outfile, options = "-L")
  }
})