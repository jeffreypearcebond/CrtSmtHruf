#' @export estimate

setGeneric( "estimate" , function( .Object , ... ) { standardGeneric( "estimate" ) } )

setMethod(
  f = "estimate" ,
  signature = signature( "Case_Obs" ) ,
  definition = function( .Object , report_date , return_type = "ConfirmedCase" ) {
    N <- dim(.Object )[1]
    retVal <- t(
      sapply(
        1:N ,
        function( i ) {
          X <- as.integer( .Object[i,c("case_weekly_2","case_weekly_1")])
          fit <- glm( X ~ factor( -1:0 ) , family = "poisson" )
          c( fit$coefficients[2] / 7 , 5 * sqrt( vcov( fit )[2,2] ) / 7 )
        }
      )
    )
    Interval(
      mean = tibble( Region = .Object$Region , Estimate = retVal[,1] ) ,
      sd = tibble( Region = .Object$Region , SD = retVal[,2] )
    )
  }
)

setMethod(
  f = "estimate" ,
  signature = signature( "COVID_Obs" ) ,
  definition = function( .Object , report_date , return_type = "ConfirmedCase" ) {
    .Object <- as_tibble( .Object ) %>% dplyr::arrange( "Region" , "Report_Date" )
    regions <- sort( unique( .Object$Region ))
    mu <- matrix( 0 , nrow = 0 , ncol = 28 )
    sapply(
      regions ,
      function( region ) {
        y <- dplyr::filter( .Object , Region == region )$Count
        mu <<- rbind( mu , Arima( y = y , order = c(1,0,0),include.drift = TRUE )$fitted )
       }
    )

    dimnames( mu )[[2]] <- format(sort( unique( .Object$Report_Date ) ))
    Interval(
      mean = tibble( regions, mu ) , sd = tibble()
    )
  }
)

