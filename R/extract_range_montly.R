extract_range_monthly <- function(file_list) {
  
  if (length(file_list) == 0) {
    
    return(data.table::data.table(inidate = NA_character_, enddate = NA_character_))
  }
  
  # tasmax_day_FGOALS-g3_ssp370_r1i1p1f1_gn_21000101-21001231.nc
  unglue::unglue_data(basename(file_list), "{var}_{freq}_{model}_{experiment}_{member}_{grid}_{inidate}-{enddate}.nc") |> 
    data.table::setDT() |> 
    _[, .(inidate = min(inidate),
          enddate = max(enddate))]
  
}
