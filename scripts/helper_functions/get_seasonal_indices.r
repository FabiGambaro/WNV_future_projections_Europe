get_seasonal_indices <- function(year = NA, year_start = NA, year_end = NA, start_year_raster = 1901) {
  winter_indices <- c()
  spring_indices <- c()
  summer_indices <- c()
  fall_indices   <- c()
  
  if (!is.na(year)) {
    # single year case
    #message("indices on single year")
    month_start <- (year - start_year_raster) * 12 + 1  # Jan of year
    
    # winter
    if (year == start_year_raster) {
      winter_indices <- c(month_start, month_start + 1)  # Jan, Feb
    } else {
      winter_indices <- c(month_start - 1, month_start, month_start + 1)  # Dec prev year, Jan, Feb
    }
    # other seasons
    spring_indices <- month_start + 2:4 # Mar, Apr, May
    summer_indices <- month_start + 5:7 # Jun, Jul, Aug
    fall_indices   <- month_start + 8:10 # Sep, Oct, Nov
    
  } else {
    # year range case
    #message("indices for year range")
    years <- year_start:year_end
    
    for (yr in years) {
      month_start <- (yr - start_year_raster) * 12 + 1
      
      # winter
      if (yr == start_year_raster) {
        winter_indices <- c(winter_indices, month_start, month_start + 1)
      } else {
        winter_indices <- c(winter_indices, month_start - 1, month_start, month_start + 1)
      }
      
      # other seasons
      spring_indices <- c(spring_indices, month_start + 2:4)
      summer_indices <- c(summer_indices, month_start + 5:7)
      fall_indices   <- c(fall_indices,   month_start + 8:10)
    }
  }
  
  return(list(
    winter = sort(unique(winter_indices)),
    spring = sort(unique(spring_indices)),
    summer = sort(unique(summer_indices)),
    fall   = sort(unique(fall_indices))
  ))
}
