#' An xtsmatt Function
#'
#' This function allows you to calculate year over year % change for an xts object.
#' @param x The name of your xts object.
#' @param merge If TRUE, merges orginal xts object with newly calculated object. If FALSE, returns just the calculated matrix.
#' @keywords yoy, year over year
#' @export
#' @examples
#' yoy(myxts_obj, merge = TRUE)
#'
#'
yoy <- function(x, merge=FALSE) {

  library(xts)
  yr_aggr <- apply.yearly(x, colSums)
  colnames(yr_aggr) <- paste0(colnames(yr_aggr), "___yearly.value")
  z <- as.list(yr_aggr)
  yr_yoy <- lapply(z, function(x){((x - lag(x)) / x) * 100 })
  yr_yoy_matrix <- do.call(cbind, yr_yoy)
  cx <- colnames(x)
  new_col_names <- paste0(cx, "___per.change.yoy")
  colnames(yr_yoy_matrix) <- new_col_names

  #Determine if original data should be returned along with calculated data (yoy)
  if(merge) {
    yr_yoy_matrix <- merge(yr_aggr, yr_yoy_matrix)
  } else {
    yr_yoy_matrix
  }
}
