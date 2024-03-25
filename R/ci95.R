#' Format Confidence Intervals
#'
#' This function formats confidence intervals with specified precision.
#'
#' @param sumtable A numeric vector of length 3 containing the estimate and its lower and upper confidence limits.
#' @param my.digit Integer; the number of decimal places to use (default is 1).
#' @param table.aspercent Logical; if TRUE, treats `sumtable` values as percentages (default is FALSE).
#' @param print.aspercent Logical; if TRUE, prints output as percentages (default is TRUE).
#'
#' @return A character string representing the formatted confidence interval.
#' The returned string is in the format of "estimate (lower limit-upper limit)".
#' For example, a return value of "50.0 (45.0-55.0)" indicates an estimate of 50.0
#' with a 95% confidence interval ranging from 45.0 to 55.0. If `table.aspercent`
#' and `print.aspercent` are both set to TRUE, the estimate and confidence limits
#' are expressed as percentages, facilitating easy interpretation of the interval
#' as a percentage range.
#'
#' @examples
#' ci95(c(50, 45, 55))
#' ci95(c(0.5, 0.45, 0.55), my.digit = 2, table.aspercent = TRUE, print.aspercent = TRUE)
#'
#' @export
ci95 <- function(sumtable, my.digit=1,table.aspercent=FALSE, print.aspercent=TRUE){
  if(!(table.aspercent*print.aspercent)){
    sumtable <- sumtable*100
  }
  sumtable1 <- format(round(sumtable, my.digit), nsmall = my.digit, justify="left", trim=TRUE)
  res <- paste(sumtable1[1], " (", sumtable1[2], "-", sumtable1[3], ")", sep="")
  return(res)
}

##############################################################################################

