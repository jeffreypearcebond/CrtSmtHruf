#' @export Estimate
library( tibble )

Estimate <- setClass(
  Class = "Estimate" ,
  slots = c(
    mean = "tbl_df" ,
    sd = "tbl_df"
  )
)

