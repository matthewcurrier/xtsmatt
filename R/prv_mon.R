#' An xtsmatt Function
#'
#' Calculate % change from the immediatly prior month.
#' @param x The name of your xts object.
#' @param merge If TRUE, merges orginal xts object with newly calculated object. If FALSE, returns just the calculated matrix.
#' @keywords prv_mon, month, percent, change, delta
#' @export
#' @examples
#' prv_mon(myxts_obj, TRUE)
#'
#'
prv_mon <- function(x, merge=FALSE) {

  mon_aggr <- apply.quarterly(x, colSums)
  colnames(mon_aggr) <- paste0(colnames(mon_aggr), "___monthly.value")
  z <- as.list(mon_aggr)
  mon_prv <- lapply(z, function(x){((x - lag(x)) / x) * 100 })
  mon_prv_matrix <- do.call(cbind, mon_prv)
  cx <- colnames(x)
  new_col_names <- paste0(cx, "___per.change.prv.mon")
  colnames(mon_prv_matrix) <- new_col_names

  #Determine if original data should be returned along with calculated data (qoq)
  if(merge) {
    mon_prv_matrix <- merge(mon_aggr, mon_prv_matrix)
  } else {
    mon_prv_matrix
  }
}
