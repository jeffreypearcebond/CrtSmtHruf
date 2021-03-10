#' @export HRUF_Weekly_Report
HRUF_Weekly_Report <- function( scalar_arg = NULL , relation_arg = NULL ) {
  library( gridExtra )

  print( "Retrieve")
  historical <- Historical( report_date = scalar_arg["Report_Date"] )
  emresource <- EMResource( file_name = paste( scalar_arg["File_In_Folder"] , relation_arg["Hospital_Resource"] , sep = "/" ) )

  print( "Adapt")
  case_obs <- CrtSmtHruf::adapt( historical , report_date = scalar_arg["Report_Date"] )
  staff_obs <- CrtSmtHruf::adapt( emresource , report_date = scalar_arg["Report_Date"] , return_type = "staff_obs" )
  bed_obs <- CrtSmtHruf::adapt( emresource , report_date = scalar_arg["Report_Date"] , return_type = "bed_obs" )

  print( "Estimate")
  growth_estimate <- estimate( case_obs )
  covid_estimate <- estimate( COVID_Obs( bed_obs$covid ) )
  hruf_summary <- HRUF_Summary( staff_obs = staff_obs , bed_obs = bed_obs , covid_estimate = covid_estimate , growth_estimate = growth_estimate )

  print( "Display and Create")
  
  CrtSmtHruf::display( .Object = hruf_summary , scalar_arg = scalar_arg  )
  
  print( "Successful Completion")
  
}

