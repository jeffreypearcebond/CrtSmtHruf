#' @export display
# overwrite test comment
setGeneric( "display" , function( .Object , ... ) { standardGeneric( "display" ) } )

setMethod(
  f = "display" ,
  signature = signature( "Staff_Obs" ) ,
  definition = function( .Object , file_name ) {
    sheets <- names( .Object )
   as( .Object , "list" , strict = FALSE )
  }
)

setMethod(
  f = "display" ,
  signature = signature( "Bed_Obs" ) ,
  definition = function( .Object , file_name , report_date , growth ) {
    list( 
      Bed_Obs = dplyr::left_join( .Object$total_beds , .Object$iba , by = "Region" ) %>%
        rename( Total = Count.x , IBA = Count.y ) %>%
        dplyr::left_join( dplyr::filter( .Object$covid , Report_Date == report_date ) ) %>%
        rename( COVID = Count ) %>%
        mutate( Census = Total - IBA ) %>%
        dplyr::left_join( growth@mean , by = "Region" ) %>%
        dplyr::mutate( Additional = round( COVID * ( exp( Estimate * 14 ) - 1 ) , 0 ) ) %>%
        dplyr::select( Census , Additional , IBA )
    )
  }
)

setMethod(
  f = "display" ,
  signature = signature( "HRUF_Summary" ) , 
  definition = function( .Object , scalar_arg ) {
    ticket = paste( "CRT_SMT_HRUF" , gsub( "-" , "" , scalar_arg["Task_Date"] ) )
     regions <- sort( .Object@covid_estimate@mean$regions )
    lwd <- 1.0
    day <- max( .Object@bed_obs$covid$Report_Date )
    iba <- structure( .Data = .Object@covid_estimate@mean$mu[,format(day)] , names = .Object@covid_estimate@mean$regions)
    census <- structure( .Data = .Object@bed_obs[[2]]$Count , names = .Object@bed_obs[[2]]$Region )
    slope <- structure( .Data = .Object@growth_estimate@mean$Estimate , names = .Object@growth_estimate@mean$Region )
    uci <- structure(
      .Data = .Object@growth_estimate@mean$Estimate + qnorm( 0.975) * .Object@growth_estimate@sd$SD ,
      names = .Object@growth_estimate@mean$Region
    )
    lci <- structure(
      .Data = .Object@growth_estimate@mean$Estimate - qnorm( 0.975) * .Object@growth_estimate@sd$SD ,
      names = .Object@growth_estimate@mean$Region
    )
    dt <- day + 1:14
    pl <- lapply(
     regions ,
      function( region ) {
        covidp <- .Object@bed_obs$covid %>% dplyr::filter( Region == region )
        estimate <- as.numeric( unlist( as_tibble( .Object@covid_estimate@mean ) %>% dplyr::filter( regions == region ) )[29] )

        ggplot(
          data.frame(
            x = covidp$Report_Date ,
            y = covidp$Count ,
            z = as.numeric( unlist( as_tibble( .Object@covid_estimate@mean ) %>% dplyr::filter( regions == region ) )[2:29] )
          ) ,
          aes(x=x , y=y ) ) +
          ggtitle( region ) +
          xlab( "Date") +
          ylab( "Total COVID-19 Patients" ) +
          geom_point() +
          geom_line(size = lwd , aes(x=x, y=z ) ) +
          geom_line(data=data.frame( t = dt , y = census[region] + iba[region] ), size = lwd , aes(x=t, y=y ) , color = "red" ) +
          geom_line(data=data.frame( t = dt , y = estimate * exp( slope[region] * 1:14 ) ) , size = lwd , aes(x=t, y=y ) , color = "blue" ) +
          geom_line(data=data.frame( t = dt , y = estimate * exp( uci[region] * 1:14 ) ) , size = lwd , aes(x=t, y=y ) , color = "blue" , lty = "dashed") +
          geom_line(data=data.frame( t = dt , y = estimate * exp( lci[region] * 1:14 ) ) , size = lwd , aes(x=t, y=y ) , color = "blue" , lty = "dashed" )

      }
    )
    
    try( dev.off() )
    tryCatch(
      ml <- marrangeGrob(grobs = pl, nrow= 4 , ncol= 2 ,layout_matrix = matrix( 1:8 , nrow = 4 , ncol = 2 , byrow = TRUE ) ) ,
      error = function(e) { print(e) }
    )
    
    file_out_root <- paste( scalar_arg["File_Out_Folder"] , ticket , sep = "/" )
    ggsave( paste( file_out_root , "pdf" , sep = "." ) , ml , width = 8 , height = 10  )
    
    format_interval <- function( interval ) {
      paste( 
        format( interval , format = "%m/%d" ) ,
        collapse = "-"
      )
    }
    occ <- function( beds ) { round( 100 * ( beds[1] - beds[2] ) / beds[1] , 0 ) }
    day <- as.Date( scalar_arg["Report_Date"] )
    var <- tibble(
      Variable = c( "Report_Date" , "TaskDate" , "Current Two Weeks" , "Forecast Two Weeks" , "Occupancy" ) ,
      Value = c(
        scalar_arg["Report_Date"] ,
        scalar_arg["Task_Date"] ,
        format_interval( c( day - 13 , day ) ) ,
        format_interval( c( day + 1 , day + 14) ) ,
        occ(
          unlist(
            left_join( .Object@bed_obs[[1]] , .Object@bed_obs[[2]] , by = "Region") %>%
              dplyr::filter( Region == "Wisconsin" ) %>%
              ungroup() %>% 
              dplyr::select( Count.x , Count.y )
          )
        )
      ) 
      
    )
    workbook <- c( 
      list( Variables_For_Text = var ) ,
      display( .Object = .Object@staff_obs , file_name = paste( file_out_root , "Staff.xlsx" , sep = "_" ) ) ,
      display( .Object = .Object@bed_obs , growth = growth_estimate , report_date = scalar_arg["Report_Date"] , file_name = paste( file_out_root , "Bed.xlsx" , sep = "_" ) )
    )
    
    openxlsx::write.xlsx( workbook , paste( paste( scalar_arg["File_Out_Folder"] , ticket , sep = "/" ) , "xlsx" , sep = "." ) )
  }
)

