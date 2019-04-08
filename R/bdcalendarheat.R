#' Calendar heat map of biodiversity data
#'
#' Produces a heat map {\url{https://en.wikipedia.org/wiki/Heat_map}}
#' representing the distribution of records in time.
#'
#' The calendar heat map is a matrix-like plot where each cell represents a
#' unique date, and the color the cell is painted with shows the amount of
#' records that have that particular date. Rows are weekdays and columns are
#' week numbers, each year having its own "panel".
#'
#' @import sqldf
#' @importFrom stats na.omit
#' @param indf input data frame containing biodiversity data set
#' @param title title custom title for the plot
#' @examples
#' \dontrun{
#' bd_calendarheat(inat)
#' }
#' @family Temporal visualizations
#' @export
bd_calendarheat <- function(indf = NA, title = NA) {
  if (is.na(title)) {
    title <- "number of records"
  }
  indf$Date_collected <- as.Date(indf$Date_collected, "%Y-%m-%d")
  dat <-
    sqldf::sqldf("select Date_collected, count(*) as recs from indf group by Date_collected")
  dat <- na.omit(dat)
  year <- as.numeric(strftime(as.Date(dat$Date_collected, na.rm = T), format = "%Y"))
  current_year <-
    as.numeric(strftime(as.Date(Sys.Date()), format = "%Y"))
  if (max(year) > current_year) {
    dat <- dat[which(year <= current_year), ]
  }
  year <- as.numeric(strftime(as.Date(dat$Date_collected, na.rm = T), format = "%Y"))
  if (max(year) - min(year) > 6) {
    dat <- dat[which(year > (max(year) - 6)), ]
  }
  calendar_heat(dat$Date_collected, dat$recs, varname = title)
}
