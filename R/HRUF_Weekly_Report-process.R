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
  covid_forecast <- COVID_Forecast( bed_obs = bed_obs , covid_estimate = covid_estimate , growth_estimate = growth_estimate )

  pl <- CrtSmtHruf::display( covid_forecast )
  try( dev.off() )
  tryCatch(
    ml <- marrangeGrob(grobs = pl, nrow= 4 , ncol= 2 ,layout_matrix = matrix( 1:8 , nrow = 4 , ncol = 2 , byrow = TRUE ) ) ,
    error = function(e) { print(e) }
  )
  print( "Display and Create")
  file_out_root <- paste( scalar_arg["File_Out_Folder"] , scalar_arg["Ticket"] , sep = "/" )
  ggsave( paste( file_out_root , "pdf" , sep = "." ) , ml , width = 8 , height = 10  )

  print( "Display Staff")
  display( .Object = staff_obs , file_name = paste( file_out_root , "Staff.xlsx" , sep = "_" ) )
  print( "Display Bed")
  display( .Object = bed_obs , growth = growth_estimate , report_date = scalar_arg["Report_Date"] , file_name = paste( file_out_root , "Bed.xlsx" , sep = "_" ) )

  print( "Successful Completion")

}

