library(data.table)
library(lubridate)
library(rcdo)
purrr::walk(Sys.glob(here::here("R/*")), source)
Sys.setenv(SKIP_SAME_TIME=1)

# For each model/experiment
# 
file_list_cmip <- readr::read_rds(here::here("data/cmip/cmip_file_list_historical_uv.rds")) |> 
  _[n_files != 0]
   # _[experiment_id == "ssp585"]

# Merge and regrid files 

grid <- "/g/data/oi10/replicas/CMIP6/ScenarioMIP/BCC/BCC-CSM2-MR/ssp245/r1i1p1f1/day/ua/gn/v20190318/ua_day_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_20150101-20171231.nc"

# purrr::map(1:nrow(file_list_cmip), function(m) {
future::plan(future::multisession, workers = 2)
furrr::future_map(1:nrow(file_list_cmip), function(m) {
  
  model <- file_list_cmip$source_id[m]
  var <- file_list_cmip$variable_id[m]
  member <- file_list_cmip$member_id[m]
  experiment <- file_list_cmip$experiment_id[m]
  grid_label <- file_list_cmip$grid_label[m]
  
  files <- file_list_cmip$file_list[[m]]
  
  file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |>
    _[, merge := fifelse(as.numeric(inidate >= 1950) & as.numeric(enddate) <= 21003112, TRUE, FALSE)] |>
    _[merge == TRUE]
  
  file_list
  message(paste0(".........", m, ".........."))
  
  outfile <- here::here(paste0("data/temp/", var, "/", var, "_day_", model, "_", experiment, "_", member, "_", grid_label, "_20150101-21001231.nc"))
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(outfile)) {
    
    write(paste0("skipping ", basename(outfile)), file = "~/log", append = TRUE)
    message(paste0("skippig ", basename(outfile)))
    return(outfile)
  }
  
  write(paste0("processing ", basename(outfile)), file = "~/log", append = TRUE)
  message(paste0("processing ", basename(outfile)))
  
  if (nrow(file_list) == 1) {

    regridded <- cdo_sellevel(file_list$path, 25000) |> 
      cdo_del29feb() |> 
      cdo_remapbil(grid = grid) |> 
      cdo_execute(options = "-L")
    
    ab <- cdo_trend(regridded, ofile1 = tempfile(), ofile2 = tempfile()) |> 
      cdo_execute(options = "-L")
    
    detrended <- cdo_subtrend(regridded, ab[1], ab[2]) |> 
      cdo_execute(options = "-L")
    
    detrended |> 
      cdo_highpass(61) |> 
      cdo_execute(outfile, options = "-L")
    
  } else {
    
    regridded <- cdo_mergetime(file_list$path) |> 
      cdo_sellevel(25000) |> 
      cdo_del29feb() |> 
      cdo_remapbil(grid = grid) |> 
      cdo_execute(options = "-L")
    
    ab <- cdo_trend(regridded, ofile1 = tempfile(), ofile2 = tempfile()) |> 
      cdo_execute(options = "-L")
    
    detrended <- cdo_subtrend(regridded, ab[1], ab[2]) |> 
      cdo_execute(options = "-L")
    
    detrended |> 
      cdo_highpass(61) |> 
      cdo_execute(outfile, options = "-L")

  }
  
  
})

