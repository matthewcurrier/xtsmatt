#' An xtsmatt Function
#'
#' Calculate % change from the immediatly prior qtr.
#' @param x The name of your xts object.
#' @param merge If TRUE, merges orginal xts object with newly calculated object. If FALSE, returns just the calculated matrix.
#' @keywords prv_qtr, quarter, percent, change, delta
#' @export
#' @examples
#' prv_qtr(myxts_obj, TRUE)
#'
#'
prv_qtr <- function(x, merge=FALSE) {

  qtr_aggr <- apply.quarterly(x, colSums)
  colnames(qtr_aggr) <- paste0(colnames(qtr_aggr), "___quarterly.value")
  z <- as.list(qtr_aggr)
  qtr_prv <- lapply(z, function(x){((x - lag(x)) / x) * 100 })
  qtr_prv_matrix <- do.call(cbind, qtr_prv)
  cx <- colnames(x)
  new_col_names <- paste0(cx, "___per.change.prv.qtr")
  colnames(qtr_prv_matrix) <- new_col_names

  #Determine if original data should be returned along with calculated data (qoq)
  if(merge) {
    qtr_prv_matrix <- merge(qtr_aggr, qtr_prv_matrix)
  } else {
    qtr_prv_matrix
  }
}
