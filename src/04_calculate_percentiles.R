library(data.table)
library(lubridate)
library(rcdo)
purrr::walk(Sys.glob(here::here("R/*")), source)

## Distribution of deltaT
## Calculate percentiles for historical experiments, period 1950-2000
## Compare percentile with experiment and keep those values
## Calculate frequency of each grid point?


models_in_gadi <- c("CMCC-ESM2", "EC-Earth3", "INM-CM4-8", "INM-CM5-0", 
                    "MPI-ESM1-2-HR", "NorESM2-MM", "EC-Earth3-CC", "EC-Earth3-Veg",
                    "EC-Earth3-Veg-LR", "GFDL-CM4")

file_list <- Sys.glob("/g/data/w40/pc2687/cf/data/old/100km/deltat_*historical*")
# percentiles <- c(0.1, 0.5,  1, 2, 2.5, 5, 7.5, 10)
percentiles <- 2.5

future::plan(future::multisession, workers = 4)
furrr::future_map(percentiles, function(percentile) {
  
  purrr::map(file_list, function(f) {
    
    meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
    
    model <- meta[[1]][["model"]]
    member <- meta[[1]][["member"]]
    experiment <- meta[[1]][["scenario"]]
    
    if (!(model %in% models_in_gadi)) {
      message(paste0("skiping ", model))
      return(f)
    }
    
    outfile1 <- paste0("~/t-drop-trends/data/temp/percentiles/", "deltat_", model, "_", experiment, "_", member, "_p", 100 - percentile, "_1950-1979_DJF.nc")
    outfile2 <- paste0("~/t-drop-trends/data/temp/percentiles/", "deltat_", model, "_", experiment, "_", member, "_p", 100 - percentile, "_1950-1979_JJA.nc")
    dir.create(dirname(outfile1), showWarnings = FALSE, recursive = TRUE)
    
    if (file.exists(outfile1)) {
      message(paste0("skiping ", basename(f)))
      return(outfile1)
    }
    message(paste0("processing ", basename(f)))
    
    period <- cdo_seldate(f, startdate = "1950-01-01T00:00:00", enddate = "1979-12-31T23:00:00") |>
      cdo_selseason("DJF") |> 
      cdo_execute(options = "-L")
    
    cdo_timpctl(period, cdo_timmin(period), cdo_timmax(period),  p = percentile) |> 
      cdo_execute(outfile1, options = "-L")
    
    period <- cdo_seldate(f, startdate = "1950-01-01T00:00:00", enddate = "1979-12-31T23:00:00") |>
      cdo_selseason("JJA") |> 
      cdo_execute(options = "-L")
    
    cdo_timpctl(period, cdo_timmin(period), cdo_timmax(period),  p = percentile) |> 
      cdo_execute(outfile2, options = "-L")
    
  })
  
})


# file_list <- Sys.glob("~/t-fall/data/cmip/percentiles//deltat_*historical*")

# future::plan(future::multisession, workers = 4)
# furrr::future_map(file_list[17:28], function(f) {
# purrr::map(file_list, function(f) {
#   
#   meta <- unglue::unglue(basename(f), "deltat_{model}_{scenario}_{member}_p90_1950-2000.nc")
#   
#   model <- meta[[1]][["model"]]
#   member <- meta[[1]][["member"]]
#   experiment <- meta[[1]][["scenario"]]
#   
#   message(paste0("processing ", basename(f)))
#   
#   outfile <- paste0("~/t-fall/data/cmip/delta10/", "deltap975_year_", model, "_", experiment, "_", member, "_1979-2014.nc")
#   dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
#   
#   if (file.exists(outfile)) {
#     return(outfile)
#   }
#   
#   varfile <- Sys.glob(paste0("~/t-fall/data/cmip/100km/deltat_day_" , model, "_", experiment, "_", member, "*"))
#   
#   cdo_le(varfile, f) |> 
#     cdo_setctomiss(c = 0) |> 
#     cdo_mul(varfile) |> 
#     cdo_execute(outfile, options = "-L")
#   
# })


## Distribution of deltaT for a historical simulation


# years <- seq(1961, 2014)
# nc <- ncdf4:::nc_open("~/t-fall/data/cmip/extrems/deltat_AWI-CM-1-1-MR_historical_r1i1p1f1_p90_1950-2000.nc")
# mask <- ReadNetCDF(nc, vars = "tasmax",
#                    subset = list(time = c("2000-01-01"),
#                                  lat = list(-60:-20, 20:60))) |> 
#   _[, let(land = MaskLand(lon, lat),
#           region = fifelse(lat < 0, "SH", "NH"),
#           time = NULL,
#           tasmax = NULL)] |> 
#   setkey(lon, lat)
# 
# purrr::map(years, function(y) {
#   
#   message(y)
#   
#   # tictoc::tic()
#   a <- ReadNetCDF(nc, vars = "tasmax",
#                   subset = list(time = c(paste0(y, "-01-01"), paste0(y, "-12-31")),
#                                 lat = list(-60:-20, 20:60))) |> 
#     setkey(lon, lat)
#   
#   mask[a] |> 
#     _[!is.na(tasmax) & !is.na(region)] |> 
#     _[, let(deltat = cut_round(tasmax, breaks = seq(-30, 0, 0.5)),
#             land = fifelse(land == TRUE, "Land", "Sea"))] |> 
#     _[, .N, by = .(deltat, land, region, year(time))]
#   
#   # tictoc::toc()
# }) |> 
#   rbindlist() |> 
#   fwrite(x = _, file = "~/t-fall/data/cmip/freq_AWI-CM-1-1-MR_historical_r1i1p1f1.csv", append = TRUE)

