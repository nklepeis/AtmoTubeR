#' @name build_AT_url
#'
#' @title Create URL's for downloading AT data from the cloud
#'
#' @param x data frame with cols for "key" and "mac"
#' @param dates what dates to retrieve format is, e.g., "2022-10-21"
#' @param limit maximum number of records to return per date
#'
#' @description Create API URL's for retrieving AtmoTube data
#'
#' @author Neil E. Klepeis
#'
#' @return a vector of character strings containing urls
#'
# -------------------------------

build_AT_url <- function (x, dates=as.character(Sys.Date()), limit=1000) {

  for (i in 1:NROW(x))

    for (d in dates) {

      #print(x[i,])

      baseURL <- "https://api.atmotube.com/api/v1/data?"

      url <- paste0(baseURL, "api_key=", x[i,"key"], "&mac=", x[i,"mac"],
                    "&limit=", limit, "&offset=0&order=asc",
                    "&format=csv")

      if (!is.null(d)) url <- paste0(url, "&date=", as.character(d))

      cat(url,"\n")

    }

}
