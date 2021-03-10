#' @export adapt

setGeneric( "adapt" , function( .Object , ... ) { standardGeneric( "adapt" ) } )

setMethod(
  f = "adapt" ,
  signature = signature( "Historical" ) ,
  definition = function( .Object , report_date , return_type = "ConfirmedCase" ) {
    Case_Obs(
      .Object %>%
        dplyr::group_by(.data$fips, .data$geo_name, .data$pop_2018) %>%
        dplyr::mutate(
          weeknum = rolling_week(date_vector = .data$post_date, end_date = report_date)
        ) %>%
        dplyr::group_by(.data$fips, .data$geo_name, .data$pop_2018, .data$weeknum) %>%
        dplyr::summarize(
          case_weekly = as.integer(sum(.data$case_daily)),
          week_end = max(.data$post_date),
          .groups = "drop_last"
        ) %>%
        dplyr::filter(.data$weeknum <= 2) %>%
        tidyr::pivot_wider(id_cols = c("fips", "geo_name", "pop_2018"),
                           values_from = c("case_weekly", "week_end"),
                           names_from = "weeknum") %>%
        dplyr::filter( !grepl( "^55..." , fips ) ) %>%
        ungroup() %>%
        dplyr::select( geo_name , case_weekly_1 , case_weekly_2 ) %>%
        dplyr::rename( Region = geo_name )
    )
  }
)

setMethod(
  f = "adapt" ,
  signature = signature( "EMResource" ) ,
  definition = function( .Object , report_date , return_type ) {
    switch(
      return_type ,
      staff_obs = {
        critical_staff_today <- as_tibble( .Object ) %>%
          dplyr::filter(  Report_Date == report_date )  %>%
          group_by( Region ) %>%
          dplyr::summarise(
            Today_Yes = sum( Critical_Staff_Today == "Yes" ),
            Today_No = sum( Critical_Staff_Today == "No" ) ,
            Today_NA = sum( Critical_Staff_Today == "--" ) ,
            .groups = "keep" ) %>%
          arrange( Region ) %>%
          bind_rows( data.frame( Region = "Wisconsin" , lapply( .[-1] , sum ) ) ) %>%
          bind_cols( Fraction = round( .$Today_Yes / ( .$Today_Yes + .$Today_No ) , 2 ) )

        critical_staff_week <- as_tibble( .Object ) %>%
          dplyr::filter(  Report_Date == report_date ) %>%
          group_by( Region ) %>%
          dplyr::summarise(
            Week_Yes = sum( Critical_Staff_Week == "Yes" ),
            Week_No = sum( Critical_Staff_Week == "No" ) ,
            Week_NA = sum( Critical_Staff_Week == "--" ) ,
            .groups = "keep" ) %>%
          arrange( Region ) %>%
          bind_rows( data.frame( Region = "Wisconsin" , lapply( .[-1] , sum ) ) ) %>%
          bind_cols( Fraction = round( .$Week_Yes / ( .$Week_Yes + .$Week_No ) , 2 ) )

        Staff_Obs(
          list( Critical_Staff_Today = critical_staff_today , Critical_Staff_Week = critical_staff_week )
        )
      } ,
       bed_obs = {
        total_beds <- as_tibble( .Object ) %>%
          dplyr::select( Report_Date , Region , Total_Intermediate_Care_Beds , Total_ICU_Beds , Total_Neg_Flow_Isolation_Beds , Total_Medical_Surgical_Beds ) %>%
          dplyr::filter(  Report_Date == report_date ) %>%
          group_by( Region ) %>%
          dplyr::summarise( Count = sum(  Total_Intermediate_Care_Beds + Total_ICU_Beds + Total_Neg_Flow_Isolation_Beds + Total_Medical_Surgical_Beds, na.rm = T ) , .groups = "keep" ) %>%
          arrange( Region ) %>%
          bind_rows( data.frame( Region = "Wisconsin" , lapply( .[-1] , sum ) ) ) 
        
        iba <- as_tibble( .Object ) %>%
          dplyr::select( Report_Date , Region , starts_with( "IBA") ) %>%
          dplyr::filter(  Report_Date == report_date ) %>%
          group_by( Region ) %>%
          dplyr::summarize( Count = sum( IBA__ICU + IBA__Intermediate_Care + IBA__Neg_Flow_Isolation + IBA__Medical_Surgical , na.rm = TRUE ) , .groups = "keep" ) %>%
          arrange( Region ) %>%
          bind_rows( data.frame( Region = "Wisconsin" , lapply( .[-1] , sum ) ) ) 

        covid <- as_tibble( .Object ) %>%
          dplyr::select( Report_Date , Region , Total___COVID_patients ) %>%
          dplyr::filter( ( Report_Date <= report_date ) & ( Report_Date >= format( as.Date( report_date ) - 27) ) ) %>%
          group_by( Region , Report_Date ) %>%
          dplyr::summarize( Count = sum( Total___COVID_patients , na.rm = TRUE ) , .groups = "keep" ) %>%
          arrange( Region )
        covid <- bind_rows(
          covid , 
          bind_cols( 
            Region = "Wisconsin" , 
            covid %>% group_by( Report_Date ) %>% dplyr::summarize( Count = sum( Count , na.rm = TRUE ) )
          )
        )

        Bed_Obs(
          list( total_beds = total_beds , iba = iba , covid = covid )
        )
      }
    )
  }
)

