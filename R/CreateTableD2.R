#' Compare Two Binary Diagnostic Tests in Clinical Studies
#'
#' This function compares the diagnostic performance between two modalities on the same subject within clinical studies. It generates formatted tables displaying diagnostic outcomes for sensitivity, specificity, accuracy, positive predictive value (PPV), negative predictive value (NPV), and area under the curve (AUC), providing a clear and comprehensive comparison directly through the R console.
#'
#' @param x A data frame containing the diagnostic test outcomes and the true disease status.
#' @param my.printlayout Logical; if TRUE, prints the result table to the console and possibly saves it to a file.
#'
#' @return A list containing three data frames: `Diseased`, `Non-diseased`, and `Comparison`.
#' - `Diseased`: A data frame showing the contingency table for diseased cases based on the two diagnostic tests.
#'   It contains the counts of true positives, false negatives, false positives, and true negatives for the first diagnostic test compared to the second.
#' - `Non-diseased`: A data frame showing the contingency table for non-diseased cases based on the two diagnostic tests.
#'   Similar to `Diseased`, it contains counts of true negatives, false positives, false negatives, and true positives.
#' - `Comparison`: A data frame summarizing the diagnostic performance metrics (sensitivity, specificity, accuracy, PPV, NPV, and AUC) for each modality, along with the p-values from statistical tests comparing the two modalities.
#'   Each row represents a different metric, with columns for the estimated value of the first modality, the estimated value of the second modality, and the p-value assessing the difference between the two.
#' This structure allows for a comprehensive overview of the comparative diagnostic performance of the two tests, facilitating easy interpretation and analysis.
#'
#' @importFrom dplyr %>% mutate filter case_when
#' @importFrom epiR epi.tests
#' @importFrom irr kappa2
#' @importFrom pROC roc roc.test
#' @importFrom stats mcnemar.test pchisq
#' @export
#'
#' @examples
#' # Assuming that data1, data2, data3, and data4 are available
#' # and contain columns `y1`, `y2`, and `d`
#' # where `y1` and `y2` are the outcomes of the two diagnostic tests,
#' # and `d` is the true disease status.
#' \donttest{
#' data(data1)
#' data(data2)
#' data(data3)
#' data(data4)
#'
#' # Checking the structure of one of the datasets
#' str(data1)
#'
#' # Creating tables using CreateTableD2 function for each dataset
#' CreateTableD2(data1)
#' CreateTableD2(data2)
#' CreateTableD2(data3)
#' CreateTableD2(data4)
#' }


# Main function to create table for two binary diagnostic tests
CreateTableD2 <- function(x, my.printlayout = TRUE){

  # Define d
  d <- x[["d"]]

  # Load necessary packages
  lapply(c("dplyr", "epiR", "irr", "pROC"), loadPackage)

  # Process data for accuracy calculation
  x.a <- x %>%
    dplyr::mutate(y1.acc = case_when(
      y1 == 0 & d == 0 ~ 1,
      y1 == 1 & d == 1 ~ 1,
      y1 == 1 & d == 0 ~ 0,
      y1 == 0 & d == 1 ~ 0
    )) %>%
    dplyr::mutate(y2.acc = case_when(
      y2 == 0 & d == 0 ~ 1,
      y2 == 1 & d == 1 ~ 1,
      y2 == 1 & d == 0 ~ 0,
      y2 == 0 & d == 1 ~ 0
    ))

  # Filter data for diseased and non-diseased
  x.p <- x %>% filter(d == 1)
  x.n <- x %>% filter(d == 0)

  # Create contingency tables
  dat1 <- as.data.frame.matrix(table(x$y1, x$d))
  dat2 <- as.data.frame.matrix(table(x$y2, x$d))

  # Check for dimension errors
  if (dim(dat1)[1] > 2 | dim(dat1)[2] > 2 | dim(dat2)[1] > 2 | dim(dat2)[2] > 2) {
    stop("Error in dimensions: out of bounds in diagnostic criteria")
  }

  # Categorize test results into intervals for y1, y2, and d
  intervals_y1 <- cut(x$y1, breaks=c(-1,0,1))
  intervals_y2 <- cut(x$y2, breaks=c(-1,0,1))
  intervals_d <- cut(x$d, breaks=c(-1,0,1))

  # Create contingency tables for y1 and y2 against d
  dat01 <- table(intervals_y1, intervals_d)
  dat02 <- table(intervals_y2, intervals_d)

  # Reformat tables and set column and row names for diagnostic criteria
  # Perform diagnostic tests on y1 data and summarize results
  dat1 <- dat01[2:1,2:1]
  colnames(dat1) <- c("Dis+","Dis-")
  rownames(dat1) <- c("Test+","Test-")

  rval1 <- epi.tests(dat1, conf.level = 0.95)
  s.rval1 <- summary(rval1)
  rownames(s.rval1) <- s.rval1$statistic

  # Repeat the process for y2 data
  dat2 <- dat02[2:1,2:1]
  colnames(dat2) <- c("Dis+","Dis-")
  rownames(dat2) <- c("Test+","Test-")

  rval2 <- epi.tests(dat2, conf.level = 0.95)
  s.rval2 <- summary(rval2)
  rownames(s.rval2) <- s.rval2$statistic

  # Calculate the Area Under Curve (AUC) for both y1 and y2
  roc1 <- roc(d ~ y1, data = x, ci = TRUE)
  roc2 <- roc(d ~ y2, data = x, ci = TRUE)

  # Format the confidence intervals for AUC
  ci1 <- matrix(split(roc1$ci, "")[[1]], ncol=3)
  ci2 <- matrix(split(roc2$ci, "")[[1]], ncol=3)
  colnames(ci1) <- c("lower", "est", "upper")
  rownames(ci1) <- c("auc")
  colnames(ci2) <- c("lower", "est", "upper")
  rownames(ci2) <- c("auc")

  # Compile se, sp, acc, ppv, npv and auc
  rt1 <- rbind(s.rval1["se",], s.rval1["sp",], s.rval1["diag.ac",], s.rval1["pv.pos",], s.rval1["pv.neg",])
  rt1 <- rt1[, c("est", "lower", "upper")]
  rt1 <- rbind(rt1, ci1)

  rt2 <- rbind(s.rval2["se",], s.rval2["sp",], s.rval2["diag.ac",], s.rval2["pv.pos",], s.rval2["pv.neg",])
  rt2 <- rt2[, c("est", "lower", "upper")]
  rt2 <- rbind(rt2, ci2)

  # Apply the ci95 function to format the ci
  dt1 <- apply(rt1, 1, ci95)
  dt2 <- apply(rt2, 1, ci95)

  # For Hypothesis test sensitivity, specificity, accuracy...
  intervals_xpy1 <- cut(x.p$y1, breaks=c(-1,0,1))
  intervals_xpy2 <- cut(x.p$y2, breaks=c(-1,0,1))

  intervals_xny1 <- cut(x.n$y1, breaks=c(-1,0,1))
  intervals_xny2 <- cut(x.n$y2, breaks=c(-1,0,1))

  intervals_xay1acc <- cut(x.a$y1.acc, breaks=c(-1,0,1))
  intervals_xay2acc <- cut(x.a$y2.acc, breaks=c(-1,0,1))

  dat.sn <- table(intervals_xpy1, intervals_xpy2)[2:1,2:1]
  dat.sp <- table(intervals_xny1, intervals_xny2)[2:1,2:1]
  dat.acc <- table(intervals_xay1acc, intervals_xay2acc)[2:1,2:1]

  # For Hypothesis test PPV and NPV...
  s11 <- dat.sn[1,1]; s10 <- dat.sn[1,2]; s01 <- dat.sn[2,1]; s00 <- dat.sn[2,2]
  r11 <- dat.sp[1,1]; r10 <- dat.sp[1,2]; r01 <- dat.sp[2,1]; r00 <- dat.sp[2,2]
  ss <- s11 + s10 + s01 + s00;  rr <- r11 + r10 + r01 + r00
  n11 <- s11 + r11; n10 <- s10 + r10; n01 <- s01 + r01; n00 <- s00 + r00
  n <- n11 + n10 + n01 + n00

  prev <- ss / n; qrev <- 1 - prev
  Se1 <- (s11 + s10) / ss;  Se2 <- (s11 + s01) / ss;  Sp1 <- (r01 + r00) / rr;  Sp2 <- (r10 + r00) / rr

  PPV1 <- (prev * Se1) / (prev * Se1 + qrev * (1 - Sp1))
  PPV2 <- (prev * Se2) / (prev * Se2 + qrev * (1 - Sp2))
  NPV1 <- (qrev * Sp1) / (prev * (1 - Se1) + qrev * Sp1)
  NPV2 <- (qrev * Sp2) / (prev * (1 - Se2) + qrev * Sp2)

  PPVp <- (2 * s11 + s10 + s01) / (2 * n11 + n10 + n01)
  NPVp <- (2 * r00 + r01 + r10) / (2 * n00 + n01 + n10)

  CPPVp <- (s11 * (1 - PPVp)^2 + r11 * PPVp^2) / (2 * n11 + n10 + n01)
  CNPVp <- (s00 * NPVp^2 + r00 * (1 - NPVp)^2) / (2 * n00 + n01 + n10)

  T1 <- (PPV1 - PPV2)^2 / ((PPVp * (1 - PPVp) - 2 * CPPVp) * ((1 / (n10 + n11)) + (1 / (n01 + n11))))
  T2 <- (NPV1 - NPV2)^2 / ((NPVp * (1 - NPVp) - 2 * CNPVp) * ((1 / (n01 + n00)) + (1 / (n10 + n00))))

  # p-values of the comparison
  p.vs <- rbind(
    mcnemar.test(dat.sn)$p.value,
    mcnemar.test(dat.sp)$p.value,
    mcnemar.test(dat.acc)$p.value,
    1 - pchisq(T1, 1),
    1 - pchisq(T2, 1),
    roc.test(roc1, roc2)$p.value
  )
  dt3 <- apply(p.vs, 1, printp)

  # Create the final result table
  diseased_df <- as.data.frame.matrix(dat.sn)
  colnames(diseased_df) <- c("test1.pos.", "test1.neg.")
  rownames(diseased_df) <- c("test2.pos.", "test2.neg.")

  non_diseased_df <- as.data.frame.matrix(dat.sp)
  colnames(non_diseased_df) <- c("test1.pos.", "test1.neg.")
  rownames(non_diseased_df) <- c("test2.pos.", "test2.neg.")

  comparison_df <- data.frame(
    `Modality1` = c(dt1),
    `Modality2` = c(dt2),
    `P-value` = c(dt3)
  )
  colnames(comparison_df) <- c("Modality 1", "Modality 2", "P-value")
  rownames(comparison_df) <- c("se", "sp", "accuracy", "ppv", "npv", "auc")


  # Returns the final result as a list
  results_list <- list(
    Diseased = diseased_df,
    `Non-diseased` = non_diseased_df,
    Comparison = comparison_df
  )

  if(my.printlayout) {
    print(results_list)
  } else {
    return(results_list)
  }
}
