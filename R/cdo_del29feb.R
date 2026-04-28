cdo_del29feb <- function(ifile, ofile = NULL) {
  rcdo::cdo_operator("del29feb", params = NULL, 1, 1) |>
    rcdo::cdo(
      input = list(ifile),
      params = NULL,
      output = ofile
    )
}