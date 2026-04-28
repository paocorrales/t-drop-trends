library(data.table)
library(lubridate)
library(rcdo)

bins <- paste(c("-inf", seq(-20, 0, 0.5), "inf"), sep = ",", collapse = ",")

file_list <- Sys.glob("~/t-fall/data/cmip/100km/deltat_day_*")

#future::plan(future::multisession, workers = 4)
#furrr::future_map(file_list, function(f) {
purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
  
  model <- meta[[1]][["model"]]
  member <- meta[[1]][["member"]]
  experiment <- meta[[1]][["scenario"]]
  
  write(paste0("processing ", basename(f)), file = "~/log", append = TRUE)
  message(paste0("processing ", basename(f)))
  
  outfile <- paste0("~/t-fall/data/cmip/histograms/", "histcount_", model, "_", experiment, "_", member, ".nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(outfile)) {
    return(outfile)
  }
  
  if (experiment == "historical") {
    
    cdo_seldate(f, startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |> 
      cdo_histcount(bounds = bins) |> 
      cdo_execute(outfile, options = "-L")
    
  } else {
    
    cdo_seldate(f, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
      cdo_histcount(bounds = bins) |> 
      cdo_execute(outfile, options = "-L")
  }
  
})
