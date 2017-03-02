#' An xtsmatt Function
#'
#' This function allows you to calculate week over week % change from the previous year for an xts object.
#' @param x The name of your xts object.
#' @param merge If TRUE, merges orginal xts object with newly calculated object. If FALSE, returns just the calculated matrix.
#' @keywords wow, week over week
#' @export
#' @examples
#' wow(myxts_obj, merge = TRUE)
#'
#'
wow <- function(x, merge=FALSE) {

  library(xts)
  wk_aggr <- apply.weekly(x, colSums)
  colnames(wk_aggr) <- paste0(colnames(wk_aggr), "___weekly.value")
  z <- as.list(wk_aggr)
  wk_wow <- lapply(z, function(x){((x - lag(x, 4)) / x) * 100 })
  wk_wow_matrix <- do.call(cbind, wk_wow)
  cx <- colnames(x)
  new_col_names <- paste0(cx, "___per.change.wow")
  colnames(wk_wow_matrix) <- new_col_names

  #Determine if original data should be returned along with calculated data (wow)
  if(merge) {
    wk_wow_matrix <- merge(wk_aggr, wk_wow_matrix)
  } else {
    wk_wow_matrix
  }
}
