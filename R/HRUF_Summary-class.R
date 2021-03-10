#' @export HRUF_Summary

HRUF_Summary <- setClass(
  Class = "HRUF_Summary" ,
  slots = c(
    staff_obs = "Staff_Obs" ,
    bed_obs = "Bed_Obs" ,
    covid_estimate = "Interval" ,
    growth_estimate = "Interval"
  )
)
