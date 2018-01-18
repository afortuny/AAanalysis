#' validate_csv
#'
#' @param df R dataframe to validate
#' @param progress_report is used for logging
#'
#' @return None, stops the process if there is any problem with data detected
#'
#' @author Vadim Zayakin <vadims.zajakins@@accenture.com>
#'
validate_csv <- function(df, progress_report) {
  lapply(
    df,
    function(x) {
      lapply(
        x,
        function(y) {
          if (grepl('\n', y, fixed = TRUE)) {
            progress_report$error('Input data contains illegal symbols (new line inside text field)')
            stop()
          }
          NA
        }
      )
      NA
    }
  )
  NA
}
