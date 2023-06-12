#' @name make_AT_device_summary
#'
#' @title Make a summary of AtmoTube data
#'
#' @param data a data frame with AtmoTube data merged with device list
#' data

#' @description Create a statistical summary of AtmoTube data merged
#' with device list data
#'
#' @author Neil E. Klepeis
#'
#' @return a tibble
#'
#' @details
#'
# ----------------------------------------

make_AT_device_summary <- function(data) {

  data %>%
  select(`Date (GMT)`, did, pid, `PM2.5 (ug/m3)`) %>%
  mutate(Time = with_tz(`Date (GMT)`, tz="America/Los_Angeles")) %>%
  group_by(did, pid) %>%
  summarize(
    NumberOfPoints = n(),
    firstTime = format(min(Time,na.rm=TRUE), "%m/%d/%y %H:%M %Z"),
    lastTime = format(max(Time,na.rm=TRUE), "%m/%d/%y %H:%M %Z"),
    maxPM2.5 = max(`PM2.5 (ug/m3)`,na.rm=TRUE),
    minPM2.5 = min(`PM2.5 (ug/m3)`,na.rm=TRUE)
  )
}
