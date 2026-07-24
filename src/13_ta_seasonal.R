library(data.table)
library(lubridate)
library(rcdo)
purrr::walk(Sys.glob(here::here("R/*")), source)

models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", 
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4")

file_list_cmip <- rbind(readr::read_rds("data/cmip_file_list_historical.rds"),
                        readr::read_rds("data/cmip_file_list_scenario.rds"))|>
  _[source_id %in% models_in_gadi] |> 
  _[, let(path = paste0("/g/data/oi10/replicas/CMIP6/", activity_drs, "/", institution_id, "/",
                        source_id, "/",
                        experiment_id, "/",
                        member_id, "/day/", 
                        "ta/*/*/*"))] |> 
  # unique(by = "path") |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, let(n_files = length(file_list[[1]])), by = path] |> 
  _[, let(inidate = NULL, enddate = NULL)] |> 
  _[, let(path = fifelse(n_files == 0, paste0("/scratch/gb02/pc2687/CMIP6/", activity_drs, "/", institution_id, "/",
                                              source_id, "/",
                                              experiment_id, "/",
                                              member_id, "/day/", 
                                              "ta/*/*/*.nc"), path))] |> 
  _[, let(file_list = list(Sys.glob(path))), by = path] |> 
  _[, c("inidate", "enddate") := extract_range(file_list[[1]]), by = path] |> 
  _[, let(inidate = ymd(inidate),
          enddate = ymd(enddate))] |> 
  _[, let( in_gadi = !((inidate >= ymd(18500101) | is.na(inidate)) | (enddate <= ymd(21001231) | is.na(enddate))))]

# Merge and regrid files and calculate deltat

grid <- "/g/data/oi10/replicas/CMIP6/ScenarioMIP/BCC/BCC-CSM2-MR/ssp245/r1i1p1f1/day/tasmax/gn/v20190318/tasmax_day_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_20150101-20391231.nc"

purrr::map(c(1:20), function(m) {
  # future::plan(future::multisession, workers = 4)
  # furrr::future_map(1:nrow(file_list_cmip), function(m) {
  
  model <- file_list_cmip$source_id[m]
  var <- "ta"
  member <- file_list_cmip$member_id[m]
  experiment <- file_list_cmip$experiment_id[m]
  grid_label <- file_list_cmip$grid_label[m]
  
  files <- file_list_cmip$file_list[[m]]
  
  message(paste0(".........", m, ".........."))
  
  outfile1 <- paste0("/g/data/gb02/pc2687/cf/data/cmip/", var, "/", var, "_", model, "_", experiment, "_DJF.nc")
  outfile2 <- paste0("/g/data/gb02/pc2687/cf/data/cmip/", var, "/", var, "_", model, "_", experiment, "_JJA.nc")
  dir.create(dirname(outfile1), showWarnings = FALSE, recursive = TRUE)
  
  if (file_list_cmip$n_files[m] == 0) {
    return(outfile1)
  }
  
  if (file.exists(outfile1)) {
    return(outfile1)
  }
  
  write(paste0("processing ", basename(outfile1)), file = "~/log", append = TRUE)
  message(paste0("processing ", basename(outfile1)))
  
  if (experiment == "historical") {
    
    file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |>
      _[, merge := fifelse(inidate >= 1970 | enddate >= 2000, TRUE, FALSE)] |> 
      _[merge == TRUE]
    
    message(file_list$path)
    
    cdo_mergetime(file_list$path) |> 
      cdo_seldate(startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |> 
      cdo_sellevel("85000,70000,25000") |> 
      cdo_selseason("DJF") |> 
      cdo_timmean() |> 
      cdo_remapbil(grid = grid) |> 
      cdo_execute(outfile1, options = "-L")
    
    cdo_mergetime(file_list$path) |> 
      cdo_seldate(startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |> 
      cdo_sellevel("85000,70000,25000") |> 
      cdo_selseason("JJA") |> 
      cdo_timmean() |> 
      cdo_remapbil(grid = grid) |> 
      cdo_execute(outfile2, options = "-L")
    
  } else {
    
    file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |> 
      _[, merge := fifelse(inidate >= 2075 | enddate >= 2100, TRUE, FALSE)] |> 
      _[merge == TRUE]
    
    message(file_list$path)
    
    cdo_mergetime(file_list$path) |> 
      cdo_seldate(startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
      cdo_sellevel("85000,70000,25000") |>
      cdo_selseason("DJF") |> 
      cdo_timmean() |> 
      cdo_remapnn(grid = grid) |> 
      cdo_execute(outfile1, options = "-L")  
    
    cdo_mergetime(file_list$path) |> 
      cdo_seldate(startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
      cdo_sellevel("85000,70000,25000") |>
      cdo_selseason("JJA") |> 
      cdo_timmean() |> 
      cdo_remapnn(grid = grid) |> 
      cdo_execute(outfile2, options = "-L")  
  }
  
})
