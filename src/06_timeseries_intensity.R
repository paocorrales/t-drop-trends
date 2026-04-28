library(data.table)
library(lubridate)
library(rcdo)

file_list <- Sys.glob("~/t-fall/data/cmip/100km/deltat_day_*")

future::plan(future::multisession, workers = 2)
furrr::future_map(file_list, function(f) {
# purrr::map(file_list, function(f) {
  
  meta <- unglue::unglue(basename(f), "deltat_day_{model}_{scenario}_{member}_{grid}_20150101-21001231.nc")
  
  model <- meta[[1]][["model"]]
  member <- meta[[1]][["member"]]
  experiment <- meta[[1]][["scenario"]]
  
  write(paste0("processing ", basename(f)), file = "~/log", append = TRUE)
  message(paste0("processing ", basename(f)))
  
  outfile <- paste0("~/t-fall/data/cmip/intensity/", "deltat_", model, "_", experiment, "_", member, "_max5.nc")
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(outfile)) {
    return(outfile)
  }
  
  percentile <- cdo_yearpctl(f, cdo_yearmin(f), cdo_yearmax(f), p = 1.37) |> 
    cdo_execute(options = "-L")
  
  cdo_yearsub(f, percentile) |>
    cdo_lec(c = 0) |>
    cdo_setctomiss(c = 0) |>
    cdo_mul(f) |>
    cdo_yearmean() |>
    cdo_execute(outfile, options = "-L")
  
})





