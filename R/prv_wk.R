#' An xtsmatt Function
#'
#' Calculate % change from the immediatly prior wk.
#' @param x The name of your xts object.
#' @param merge If TRUE, merges orginal xts object with newly calculated object. If FALSE, returns just the calculated matrix.
#' @keywords prv_wk, quarter, percent, change, delta
#' @export
#' @examples
#' prv_wk(myxts_obj, TRUE)
#'
#'
prv_wk <- function(x, merge=FALSE) {

  wk_aggr <- apply.quarterly(x, colSums)
  colnames(wk_aggr) <- paste0(colnames(wk_aggr), "___weekly.value")
  z <- as.list(wk_aggr)
  wk_prv <- lapply(z, function(x){((x - lag(x)) / x) * 100 })
  wk_prv_matrix <- do.call(cbind, wk_prv)
  cx <- colnames(x)
  new_col_names <- paste0(cx, "___per.change.prv.wk")
  colnames(wk_prv_matrix) <- new_col_names

  #Determine if original data should be returned along with calculated data (qoq)
  if(merge) {
    wk_prv_matrix <- merge(wk_aggr, wk_prv_matrix)
  } else {
    wk_prv_matrix
  }
}
