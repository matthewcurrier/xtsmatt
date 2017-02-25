#' An xtsmatt Function
#'
#' This function allows you to calculate quarter over quarter % change for an xts object.
#' @param x The name of your xts object.
#' @param merge If TRUE, merges orginal xts object with newly calculated object. If FALSE, returns just the calculated matrix.
#' @keywords qoq, quarter
#' @export
#' @examples
#' qoq(myxts_obj, TRUE)
#'
#'
qoq <- function(x, merge=FALSE) {
  library(xts)

  qtr_aggr <- apply.quarterly(x, colSums)
  colnames(qtr_aggr) <- paste0(colnames(qtr_aggr), "___quarterly.value")
  z <- as.list(qtr_aggr)
  qtr_qoq <- lapply(z, function(x){((x - lag(x, 4)) / x) * 100 })
  qtr_qoq_matrix <- do.call(cbind, qtr_qoq)
  cx <- colnames(x)
  new_col_names <- paste0(cx, "___per.change.qoq")
  colnames(qtr_qoq_matrix) <- new_col_names

  #Determine if original data should be returned along with calculated data (qoq)
  if(merge) {
    qtr_qoq_matrix <- merge(qtr_aggr, qtr_qoq_matrix)
  } else {
    qtr_qoq_matrix
  }
}
