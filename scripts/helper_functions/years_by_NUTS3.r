##Function modified from KS##
#This function aggregates years of presence for each NUTS3 region into consecutive year ranges. 
#If there is a gap of 10 years or more between observations, a new range is started. 
#Single-year observations are kept as is.

#require(purrr)
#years_by_nuts = function(df) {
#  df %>%
#    group_by(nutsM_code) %>%
#    summarize(years = list(sort(unique(year))), .groups = "drop") %>%
#    rowwise() %>%
#    mutate(
#      groups = list({
#        y = years
#        print(paste("Processing ID:", nutsM_code))
#        print(paste("All years:", paste(y, collapse = ", ")))
#        
#        if (length(y) == 1) {
#          print("Single year")
#          list(y)
#        } else {
#          breaks = c(0, which(diff(y) >= 10), length(y))
#          print(paste("Year gap positions:", paste(breaks, collapse = ", ")))
#          lapply(seq_along(breaks[-1]), function(i) y[(breaks[i] + 1):breaks[i + 1]])
#        }
#      })
#    ) %>%
#    unnest(groups) %>%
#    mutate(n_years = lengths(groups),
#           year= ifelse(n_years == 1, groups, NA_integer_),
#           year_start= ifelse(n_years > 1, map_int(groups, min), NA_integer_),
#           year_end=ifelse(n_years > 1, map_int(groups, max), NA_integer_)) %>%
#    select(nutsM_code, year, year_start, year_end) %>%{
#      .
#      }
#}
#

library(dplyr)

years_by_nuts = function(df, gap = 10){
  df %>%
    filter(!is.na(year)) %>%
    distinct(nutsM_code, year) %>%
    arrange(nutsM_code, year) %>%
    group_by(nutsM_code) %>%
    mutate(cluster = cumsum(c(TRUE, diff(year) >= gap))) %>%
    group_by(nutsM_code, cluster) %>%
    dplyr::summarize(n_years = n(), start = min(year), end = max(year), .groups = "drop") %>%
    transmute(nutsM_code, year=ifelse(n_years == 1, start, NA), year_start=ifelse(n_years > 1, start, NA),
              year_end= ifelse(n_years > 1, end, NA))
}