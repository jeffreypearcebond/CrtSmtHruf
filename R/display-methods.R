#' @export display

setGeneric( "display" , function( .Object , ... ) { standardGeneric( "display" ) } )

setMethod(
  f = "display" ,
  signature = signature( "Staff_Obs" ) ,
  definition = function( .Object , file_name ) {
    sheets <- names( .Object )
    workbook <- as( .Object , "list" )
    names( workbook ) <- sheets
    openxlsx::write.xlsx( x = workbook , file = file_name )
  }
)

setMethod(
  f = "display" ,
  signature = signature( "Bed_Obs" ) ,
  definition = function( .Object , file_name , report_date , growth ) {
    workbook <- dplyr::left_join( .Object$total_beds , .Object$iba , by = "Region" ) %>%
      rename( Total = Count.x , IBA = Count.y ) %>%
      dplyr::left_join( dplyr::filter( .Object$covid , Report_Date == report_date ) ) %>%
      rename( COVID = Count ) %>%
      mutate( Census = Total - IBA ) %>%
      dplyr::left_join( growth@mean , by = "Region" ) %>%
      dplyr::mutate( Additional = round( COVID * ( exp( Estimate * 14 ) - 1 ) , 0 ) ) %>%
      dplyr::select( Census , Additional , IBA )
    openxlsx::write.xlsx( x = workbook , file = file_name )

  }
)

setMethod(
  f = "display" ,
  signature = signature( "COVID_Forecast" ) ,
  definition = function( .Object ) {
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
    lapply(
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
  }
)

