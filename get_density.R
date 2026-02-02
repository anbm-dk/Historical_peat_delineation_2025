# Function to calculate point densities

get_density <- function(datxy, sig) {
  require(spatstat.geom)
  dens_out <- ppp(
    datxy$UTMX,
    datxy$UTMY,
    c(441000, 894000),
    c(6049000, 6403000)
  ) %>%
    density(
      sigma = sig,
      at = "points",
      leaveoneout = FALSE
    )
  
  attributes(dens_out) <- NULL
  
  return(dens_out)
}