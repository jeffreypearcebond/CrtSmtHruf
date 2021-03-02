#' @export COVID_Forecast

COVID_Forecast <- setClass(
  Class = "COVID_Forecast" ,
  slots = c(
    bed_obs = "Bed_Obs" ,
    covid_estimate = "Estimate" ,
    growth_estimate = "Estimate"
  )
)
