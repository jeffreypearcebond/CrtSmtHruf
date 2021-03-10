#' @export Interval
library( tibble )

Interval <- setClass(
  Class = "Interval" ,
  slots = c(
    mean = "tbl_df" ,
    sd = "tbl_df"
  )
)

