#'
#' report_process_factory.R
#'
#' Function that print, according to the severity line
#'
#' @author Vadim Zayakin <vadims.zajakins@@accenture.com>
#' @author Fortuny Sicart, Alan <Alan.FortunySicart@@adidas-group.com>
#'
#' @param prod_division A string refering to the product division
#' @param severity_level An integer specifing the threshold of severity (1 =Error,2=Warning, 3=Info,4=Trace,5=Debug)
#'
#' @return The progress of the code showing messages to the desired severity level
#'

report_progress_factory <-
  function(prod_division = c(),
           severity_level = 3) {
    severity_names <- c("Error",
                        "Warning",
                        "Info",
                        "Trace",
                        "Debug")

    # the following function prints the product division on each line output by the code

    do_print_line <- function(severity, line) {
      print(paste0(
        paste(rep(' ', length(prod_division)), collapse = ""),
        "[",
        paste(prod_division, collapse = "]["),
        "]",
        " - ",
        severity_names[severity],
        ":",
        " ",
        line
      ))
    }


    # Do print only prints those lines with severity equal or higher than the one defined

    do_print <- function(severity, text) {
      if (severity <= severity_level) {
        if (typeof(text) == "character")
          do_print_line(severity, text)
        else
          for (line in utils::capture.output(text)) {
            do_print_line(severity, line)
          }
      }
    }

    list(
      error = function(...)
        do_print(1, ...),
      warn  = function(...)
        do_print(2, ...),
      info  = function(...)
        do_print(3, ...),
      trace = function(...)
        do_print(4, ...),
      debug = function(...)
        do_print(5, ...),

      subarea = function(sub_area, key = NA) {
        name <- c(key, sub_area)
        report_progress_factory(c(prod_division, paste(name[!is.na(name)], collapse = '=')))
      }
    )
  }
