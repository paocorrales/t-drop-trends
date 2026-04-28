library(data.table)
library(lubridate)
library(rcdo)
purrr::walk(Sys.glob(here::here("R/*")), source)

# For each model/experiment
# 
file_list_cmip <- readr::read_rds(here::here("data/cmip_file_list_scenario.rds")) |>
  _[nominal_resolution == "100 km"]

# Merge and regrid files and calculate deltat

grid <- "/g/data/oi10/replicas/CMIP6/ScenarioMIP/BCC/BCC-CSM2-MR/ssp245/r1i1p1f1/day/tasmax/gn/v20190318/tasmax_day_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_20150101-20391231.nc"

purrr::map(1:nrow(file_list_cmip), function(m) {
# future::plan(future::multisession, workers = 4)
# furrr::future_map(1:nrow(file_list_cmip), function(m) {
  
  model <- file_list_cmip$source_id[m]
  var <- file_list_cmip$variable_id[m]
  member <- file_list_cmip$member_id[m]
  experiment <- file_list_cmip$experiment_id[m]
  grid_label <- file_list_cmip$grid_label[m]
  
  files <- file_list_cmip$file_list[[m]]
  
  file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |> 
    _[, merge := fifelse(inidate >= 1850 | enddate >= 2015, TRUE, FALSE)] |> 
    _[merge == TRUE]
  
  message(paste0(".........", m, ".........."))
  
  outfile <- here::here(paste0("data/temp/100km/", "deltat_day_", model, "_", experiment, "_", member, "_", grid_label, "_20150101-21001231.nc"))
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(outfile)) {
    return(outfile)
  }
  
  write(paste0("processing ", basename(outfile)), file = "~/log", append = TRUE)
  message(paste0("processing ", basename(outfile)))
  
  if (nrow(file_list) == 1) {
    
    regridded <- cdo_seldate(file_list$path, startdate = "1850-01-01T00:00:00", enddate = "2014-12-31T23:00:00") |> 
      cdo_remapbil(grid = grid) |> 
      cdo_execute(options = "-L")
    
  } else {
    
    regridded <- cdo_mergetime(file_list$path) |> 
      cdo_seldate(startdate = "1850-01-01T00:00:00", enddate = "2014-12-31T23:00:00") |>
      cdo_remapbil(grid = grid) |>
      cdo_execute(options = "-L")
    
  }
  
  cdo_deltat(regridded) |> 
    cdo_shifttime(shiftValue = "-1days") |> 
    cdo_execute(outfile, options = "-L")
  
})


   
