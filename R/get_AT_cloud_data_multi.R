#' @name get_AT_cloud_data_multi
#'
#' @title Get AtmoTube Sensor Data via Cloud API for Multiple Devices
#' Over a range of specified days
#'
#' @param key string containing API access key (obtained from AtmoTube)
#' @param macs MAC addresses for the devices
#' @param dates dates for which to retrieve data
#' @param limit maximum number of records to return, max 1000
#'
#' @description Retrieve AtmoTube Air Monitor sensor data using the Cloud API
#'
#' @author Neil E. Klepeis
#'
#' @return a tibble containing rectangular AtmoTube data with one
#' record per time point and multiple columns containing sensor data
#' streams
#'
#' @details The AtmoTube Cloud API contains features to retrieve
#' air sensor data in CSV format for multiple monitors with a specified limit to the number
#' of records and the time-order of the records.
#'
#' Sets offset to 0 and order to "asc".
#'
#' For the AtmoTube PRO, sensor data streams include particulate matter in
#' PM1, PM2.5, and PM10 ranges (ug/m3), VOC's (ppm),
#' temperature (degrees C), humidity (%), atmospheric pressure (mbar),
#' latitude, and longitude.
#'
#' ### Parameters
#'
#' * macs - MAC addresses of 1 or more Atmotube devices
#' * order - Time descending "desc" or ascending "asc"
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



get_AT_cloud_data_multi <- function(key, macs, dates=as.character(Sys.Date()),
                                    limit=1000, format="csv") {

  data <- tibble()

  for (m in macs) {

    for (d in dates) {

      cat("\nQuerying  MAC = ", m, "   Date = ", d, "\n\n")

      r1 <- tryCatch({

        get_AT_cloud_data(key=key, mac=m, limit=limit, offset=0,
                          format="csv", order="asc", date=d)

        }, error =  function(err) {
          print(err)
          NULL
        }
      )

      if (!is.null(r1)) {
        if (NROW(r1) & any(grepl("Date", names(r1)))) {
          print(r1)
          data <- data %>%
            bind_rows(
              r1 %>% mutate(Mac=m) %>%
                select(Mac, everything())
            )
        }
      }

    }

  }

  data



}
