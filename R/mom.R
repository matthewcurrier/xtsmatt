#' An xtsmatt Function
#'
#' This function allows you to calculate month over month % change for an xts object.
#' @param x The name of your xts object.
#' @param merge If TRUE, merges orginal xts object with newly calculated object. If FALSE, returns just the calculated matrix.
#' @keywords mom, month over month
#' @export
#' @examples
#' mom(myxts_obj, merge = TRUE)
#'
#'
mom <- function(x, merge=FALSE) {

  library(xts)
  mo_aggr <- apply.monthly(x, colSums)
  colnames(mo_aggr) <- paste0(colnames(mo_aggr), "___monthly.value")
  z <- as.list(mo_aggr)
  mo_mom <- lapply(z, function(x){((x - lag(x, 4)) / x) * 100 })
  mo_mom_matrix <- do.call(cbind, mo_mom)
  cx <- colnames(x)
  new_col_names <- paste0(cx, "___per.change.mom")
  colnames(mo_mom_matrix) <- new_col_names

  #Determine if original data should be returned along with calculated data (mom)
  if(merge) {
    mo_mom_matrix <- merge(mo_aggr, mo_mom_matrix)
  } else {
    mo_mom_matrix
  }
}
