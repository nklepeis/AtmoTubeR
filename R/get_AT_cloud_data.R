#' @name get_AT_cloud_data
#'
#' @title Get AtmoTube Sensor Data via Cloud API
#'
#' @param key string containing API access key (obtained from AtmoTube)
#' @param mac MAC address for the device
#' @param limit maximum number of records to return
#' @param offset skip records
#' @param order "asc" or "desc" time order of records
#' @param format "csv" or "json"
#' @param date filter by date, e.g., "2022-10-21"
#' @param ... additional arguments passed to "fromJSON" function
#'
#' @description Retrieve AtmoTube Air Monitor sensor data using the Cloud API
#'
#' @author Neil E. Klepeis
#'
#' @return a tibble containing rectangular AtmoTube data with one
#' record per time point and multiple columns containing sensor data
#' streams (\code{format="csv"}) OR a list representing a JSON object containing
#' the sensor data and various flags (\code{format="json"})
#'
#' @details The AtmoTube Cloud API contains features to retrieve
#' air sensor data for a single monitor with a specified limit to the number
#' of records and the time-order of the records.
#'
#' For the AtmoTube PRO, sensor data streams include particulate matter in
#' PM1, PM2.5, and PM10 ranges (ug/m3), VOC's (ppm),
#' temperature (degrees C), humidity (%), atmospheric pressure (mbar),
#' latitude, and longitude.
#'
#' ### Parameters
#'
#' * mac - MAC address of Atmotube device
#' * order - Time descending "desc" or ascending "asc"
#' * format - "json" or "csv"
#' * offset - records to skip for pagination, min of 0
#' * date - date filter, enter date, e.g., "2022-10-1"
#' * limit - maximum number of records to return, up to 1000
#'
#' @examples
#'
#' key <- "190b3980-d505-11eb-b8bc-0242ac130003"
#' mac <- "DF:21:2A:AE:9A:17"
#' x <- get_AT_cloud_data(key, mac)
#'
#' # https://api.atmotube.com/api/v1/data?api_key=190b3980-d505-11eb-b8bc-0242ac130003&mac=DF:21:2A:AE:9A:17&limit=199999&offset=0&order=desc
#'
#----------------------------------------------------



get_AT_cloud_data <- function(key, mac, limit=1000, offset=0, format="csv",
                        order="desc", date=NULL, ...) {

  baseURL <- "https://api.atmotube.com/api/v1/data?"

  url <- paste0(baseURL, "api_key=", key, "&mac=", mac,
                "&limit=", limit, "&offset=", offset, "&order=", order,
                "&format=",format)
  if (!is.null(date)) url <- paste0(url, "&date=", date)

  cat("Reading from URL:\n", url)

  if (format=="json")
    jsonlite::fromJSON(url, ...)
    #mutate(time = as_datetime(time))
  else
    read_delim(url, delim=";") #%>%
      #mutate(
       # `Date (GMT)` = as_datetime(
        #  `Date (GMT)`,
        #   format="%Y/%m/%d %H:%M",
        #   tz="UTC"
        #  )
      #)



}
