units_convertion <- function(variable, x){
  if(variable == "temp" || variable == "temperature" || variable == "tas"){
    x - 273.15  # convert from Kelvin to Celsius
  } else if(variable == "pr" || variable == "precipitation"){
    x * 60 * 60 * 24  # convert to mm/day
  } else if(variable == "rh" || variable == "relative_humidity" || variable == "hurs"){
    x  # no modification
  } else {
    warning(paste("Unknown variable:", variable, "— returning unconverted."))
    x
  }
}