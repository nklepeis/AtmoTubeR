#' @name get_AT_from_device_list
#'
#' @title Get Daily AtmoTube Sensor Data Based on a list of devices
#'
#' @param devices a data from containing cols for "mac"=MacID, "key"=Access Key,
#' "did"=device ID, and "pid" = Participant or Person ID.
#'
#' @param start_date staring containg starting date in "YYYY-MM-DD" format
#' @param end_date string containg ending date in "YYYY-MM-DD" format
#'
#' @description Retrieve daily AtmoTube Air Monitor sensor data using a list
#' of devices and their associated MacID and Access Key
#'
#' @author Neil E. Klepeis
#'
#' @return a tibble containing rectangular AtmoTube data with one
#' record per time point and multiple columns containing sensor data
#' streams
#'
#' @details This function is a front-end to get_AT_cloud_data_multi
#' but takes a list of device and conveniently returns data for one or
#' more devices for every date in between 'start' and 'end' dates.
#'
# ----------------------------------------

get_AT_from_device_list <- function(devices, start_date, end_date) {

  mytime <- format(Sys.time(), "at%H%M%S")

  dates <- format(seq.POSIXt(as_datetime(start_date),
                             as_datetime(end_date), by="1 day"),
                  format="%Y-%m-%d")

  data <-  get_AT_cloud_data_multi(key=unique(devices$key),
                            macs=devices$mac,
                            dates=dates)
  if (NROW(data))
    data %>%
    left_join(devices %>% select(mac, did, pid),
              by=join_by(Mac == mac)) %>%
    select(Mac, did, pid, everything())
  else NULL


}
