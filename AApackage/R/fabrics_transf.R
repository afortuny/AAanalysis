#' Fabrics Transformation
#'
#' It manipulates the fabric variable (which is a character variable) and obtains the upper and lower limit of it. This function also isolates
#' all possible levels of fabric available in the examined dataset of articles.
#'
#' @param data_group input dataset
#' @param fabric A text string indicating the column name of fabric
#' @author Nikolaos Kapodistrias <n.kapodistrias@@accenture.com>
#' @author Ilektra Kocheila <ilektra.kocheila@@accenture.com>
#' @return a dataset including unique fabric values and the upper limits of each fabric category
#' @export
#' @examples
#' \dontrun{
#' #Run the function
#' fabric_limits<-fabrics_transf(data_mart_assort_ARP_clean_v0=data_mart_assort_ARP_clean_v0)
#'}


fabrics_transf <- function(data_group,
                           fabric="fabric")
{
  #find unique fabric values
  fabric_limits <-
    as.data.frame(unique(data_group[,fabric]))
  names(fabric_limits) <- "fabric_list"

  #get the upper limits of each fabric level

  #get the upper levels for cases with "-"
  last <- function(x)
  {
    g <- gregexpr("-", x, fixed = TRUE)
    loc <- g[[1]]
    substrRight <- function(x, n) {
      substr(x, nchar(x) - n + 1, nchar(x))
    }
    last_char <- substrRight(x, nchar(x) - loc[length(loc)])
    rbind(x, last_char)
  }

  fabric_limits <- as.data.frame(t(apply(fabric_limits, 1, last)))
  names(fabric_limits)[1] <- "fabric"
  names(fabric_limits)[2] <- "lim"
  fabric_limits$lim <- as.character(fabric_limits$lim)

  #get the upper levels from <
  fabric_limits$lim[substr(fabric_limits$lim, 1, 1) == "<"] <-
    as.numeric(gsub("<", "", fabric_limits$lim[substr(fabric_limits$lim, 1, 1) ==
                                                 "<"]))

  #get the upper levels from >
  if (length(fabric_limits$lim[substr(fabric_limits$lim, 1, 1) == ">"]) <=1) {
    fabric_limits$lim[substr(fabric_limits$lim, 1, 1) == ">"] <- Inf
    fabric_limits$lim <- as.numeric(fabric_limits$lim)
  } else {
    fabric_limits$lim <- as.numeric(fabric_limits$lim)
  }

  return(fabric_limits)

}
