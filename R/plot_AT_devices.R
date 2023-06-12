#' @name plot_AT_devices
#'
#' @title Plot AtmoTube data merged with device info
#'
#' @param data a data frame with AtmoTube data merged with device list
#' data

#' @description Create a ggplot object of AtmoTube data merged with device info
#'
#' @author Neil E. Klepeis
#'
#' @return a tibble
#'
#' @details
#'
# ----------------------------------------

plot_AT_devices <- function(data, cols='PM2.5 (ug/m3)',
                            id.cols="Mac",
                            tz="America/Los_Angeles",
                            by="1 day", date_breaks="6 hour") {

  drange <- range(date(with_tz(data[['Date (GMT)']], tz=)))

  ncols <- length(seq.POSIXt(as_datetime(drange[1],tz=tz),
                             as_datetime(drange[2], tz=tz), by=by))

  plot_AT_streams(data, id.cols=id.cols, cols=cols,
                  numcols=ncols,
                  scales="free", date_breaks=date_breaks,
                  pad=TRUE)


}
