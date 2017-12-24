
### sd.from.radius ################################################################################
# Description:
#   Get standard deviation required to achieve desired radius.
# Input variable:
#   radius  desired radius
# Output variable:
#   sd  standard deviation
sd.from.radius <- function(radius) {
  return((radius^2)/4)
}

### radius.from.area ##############################################################################
radius.from.area <- function(area) {
  return(sqrt(area/pi))
}


