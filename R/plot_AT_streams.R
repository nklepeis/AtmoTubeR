#' @name plot_AT_streams
#'
#' @title Plot AtmoTube sensor streams
#'
#' @description Plot streams downloaded using the AtmoTube API
#'
#' @param data a tibble of AtmoTube data from Cloud API
#' @param cols character vector of columns names corresponding to sensor response streams to plot.
#' Names of responses in the columns to plot, defaults to PM and VOC columns.
#' @param id.cols names of device-ID columns to use in faceting the plot,
#' e.g., Mac address or site name., defaults to "Mac"
#' @param time.col name of column containing the time
#' @param by, string giving the time window for faceting, e.g., "1 day", "1 month",
#' default to "1 day"
#' @param tz the time zone
#' @param facet.response facet by the response var
#' @param area plot area
#' @param step plot steps
#' @param line plot lines
#' @param ... other arguments to plot_streams_ggplot
#'
#' @author Neil E. Klepeis
#'
#' @details Converts a raw wide file downloaded using the AtmoTube API to a
#' standard long format with Time, Response, and Value columns.  id.cols contains
#' extra index columns to use in faceting, defaults to "Mac" for the device
#' MAC address.
#'
# --------------------------------------------------------------------------

## TODO.  Plot an interactive map showing where the
##  AtmoTube was located by day/hour/etc and a calender plot
##  showing concentrations by day/hour/etc

plot_AT_streams <- function(data,
                    cols=c("PM1,","PM2.5,","PM10,", "VOC"),
                    id.cols = "Mac",
                    time.col = "Date (GMT)",
                    by="1 day", tz = "America/Los_Angeles",
                    facet.response = TRUE,
                    area=FALSE, step=FALSE, line=TRUE,
                     ...) {

  data <- data %>%
    select(all_of(time.col), starts_with(id.cols), starts_with(cols)) %>%
        pivot_longer(
          cols=starts_with(cols),
          names_to="Response",
          values_to="Value"
        ) %>%
    rename(Time = all_of(time.col)) %>%
    mutate(Time = with_tz(Time, tz=tz))


  cat("AT Data to plot:\n")
  print(data)

  plot_streams_ggplot(data, area=area, step=step, line=line,
                      calendar = FALSE,
                      facet.response=facet.response,
                       ...) +
    theme_streams_dark()

}
