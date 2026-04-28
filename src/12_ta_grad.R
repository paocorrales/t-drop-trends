library(data.table)
library(lubridate)
library(rcdo)
purrr::walk(Sys.glob("~/t-fall/R/*"), source)

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

lon <- cdo_expr(grid, "'lon=clon(tasmax)'") |> 
  cdo_execute(options = "-L")
lat <- cdo_expr(grid, "'lat=clat(tasmax)'") |> 
  cdo_execute(options = "-L")
coslat <- cdo_expr(lat, "'coslat=cos(lat_2*3.14159265359/180)'") |> 
  cdo_execute(options = "-L")

# purrr::map(1:nrow(file_list_cmip), function(m) {
  future::plan(future::multisession, workers = 2)
  furrr::future_map(1:nrow(file_list_cmip), function(m) {
  
  model <- file_list_cmip$source_id[m]
  var <- "ta"
  member <- file_list_cmip$member_id[m]
  experiment <- file_list_cmip$experiment_id[m]
  grid_label <- file_list_cmip$grid_label[m]
  
  files <- file_list_cmip$file_list[[m]]
  
  message(paste0(".........", m, ".........."))
  
  outfile <- paste0("~/t-drop-trends/data/cmip/", var, "/", var, "_day_", model, "_", experiment, "_", member, "_", grid_label, "_20150101-21001231.nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  # if (file.exists(outfile)) {
  #   return(outfile)
  # }
  
  write(paste0("processing ", basename(outfile)), file = "~/log", append = TRUE)
  message(paste0("processing ", basename(outfile)))
  
  if (experiment == "historical") {
    
    file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |> 
      _[, let(grid = basename(dirname(dirname(path))))] |> 
      _[, merge := fifelse(inidate >= 1970 | enddate >= 2000, TRUE, FALSE)] |> 
      _[merge == TRUE & grid == unique(grid)[1]]
    
    temp <- cdo_mergetime(file_list$path) |> 
      cdo_seldate(startdate = "1980-01-01T00:00:00", enddate = "2000-12-31T23:00:00") |> 
      cdo_sellevel("85000") |>
      # cdo_timmean() |> 
      cdo_remapbil(grid = grid) |> 
      cdo_execute(options = "-L")
    
  } else {
    file_list <- data.table(path = files, purrr::map_df(files, extract_range)) |> 
      _[, let(grid = basename(dirname(dirname(path))))] |> 
      _[, merge := fifelse(inidate >= 2070 | enddate >= 2100 , TRUE, FALSE)] |>
      _[merge == TRUE & grid == unique(grid)[1]]
    
    if (length(file_list$path) == 1) {
      # cdo_mergetime() |> 
      temp <- cdo_seldate(file_list$path, startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
        cdo_sellevel("85000") |>
        # cdo_timmean() |> 
        cdo_remapbil(grid = grid) |> 
        cdo_execute(options = "-L") 
    } else {
      temp <- cdo_mergetime(file_list$path) |> 
        cdo_seldate(startdate = "2080-01-01T00:00:00", enddate = "2100-12-31T23:00:00") |> 
        cdo_sellevel("85000") |>
        # cdo_timmean() |> 
        cdo_remapbil(grid = grid) |> 
        cdo_execute(options = "-L")  
    }
  }
  
  dx <- cdo_shiftx(lon, nshift = 1) |> 
    cdo_sub(ifile1 = lon) |> 
    cdo_mul(coslat) |> 
    cdo_mulc(c = pi/(180*6371000)) |> 
    cdo_execute(options = "-L")
  
  dy <- cdo_shifty(lat, nshift = 1) |>
    cdo_sub(ifile1 = lat) |>
    # cdo_mul(coslat) |>
    cdo_mulc(c = pi/(180*6371000)) |>
    cdo_execute(options = "-L")

  dtemp_dx <- cdo_shiftx(temp, nshift = 1, cyclic = "cyclic") |> 
    cdo_sub(ifile1 = temp) |> 
    cdo_div(dx) |> 
    cdo_execute(paste0("data/temp/grad/d", var, "-dx_day_", model, "_", experiment, ".nc"), options = "-L")
  
  dtemp_dy <- cdo_shifty(temp, nshift = 1) |>
    cdo_sub(ifile1 = temp) |>
    cdo_div(dy) |>
    cdo_execute(paste0("data/temp/grad/d", var, "-dy_day_", model, "_", experiment, ".nc"), options = "-L")
  
})