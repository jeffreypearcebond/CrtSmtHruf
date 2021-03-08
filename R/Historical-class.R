#' @export Historical
#' @export Case_Obs
library( tibble )

Historical <- setClass(
  Class = "Historical" ,
  contains = c( "tbl_df" )
)

Case_Obs <- setClass(
  Class = "Case_Obs" ,
  contains = c( "tbl_df" )
)

setMethod(
  f = "initialize" ,
  signature = signature( "Historical" ) ,
  definition = function(
    .Object ,
    report_date
  ) {
    query <- "SELECT * FROM [DDI].[WedssCovid19_BCDGIS_Historical_v] where GEO='County'"
    case_df <- pull_wedss( query = query, conn = odbcConnect( relation_arg[["Confirmed_Case"]] ) , end_date = format( as.Date( report_date ) ) )

    callNextMethod( .Object , case_df )
  }
)

process_confirmed_cases_region <- function(case_df, crit_cat = TRUE) {
  clean_case_df <- shape_case_data(case_df)

  out_sum <- dplyr::ungroup(clean_case_df$summary) %>%
    dplyr::mutate(
      Count = .data$case_weekly_1 + .data$case_weekly_2,
      Burden = score_burden(curr = .data$case_weekly_1,
                            prev = .data$case_weekly_2,
                            pop = .data$pop_2018),
      Burden_Class = class_burden(.data$Burden),
      Trajectory = D(curr = .data$case_weekly_1,
                     prev = .data$case_weekly_2),
      Trajectory_SD = SD(curr = .data$case_weekly_1,
                         prev = .data$case_weekly_2),
      Trajectory_P = pval_trajectory(curr = .data$case_weekly_1,
                                     prev = .data$case_weekly_2),
      Trajectory_Class = class_trajectory(traj = .data$Trajectory,
                                          pval = .data$Trajectory_P),
      Trajectory_FDR = fdr_trajectory(pval = .data$Trajectory_P),
      Composite_Class = confirmed_case_composite(traj_class = .data$Trajectory_Class,
                                                 burd_class = .data$Burden_Class)

    ) %>%
    dplyr::select(
      Region_ID = .data$fips,
      Region = .data$geo_name,
      Conf_Case_Trajectory = .data$Trajectory ,
      Trajectory_SD = .data$Trajectory_SD
    )

  out_sum %>% dplyr::filter( !grepl( "^55...$" , Region_ID ))
}


shape_case_data <- function(case_df) {
  #Alter date to reflect date cases were confirmed rather than posted.
  case_df$post_date = case_df$post_date

  max_date <- max(case_df$post_date)

  cases_daily <- case_df %>%
    dplyr::filter(post_date >= (max_date - lubridate::days(13))) %>%
    dplyr::select(fips, geo_name, post_date, case_daily)

  cases_summary <- case_df %>%
    dplyr::group_by(.data$fips, .data$geo_name, .data$pop_2018) %>%
    dplyr::mutate(
      weeknum = rolling_week(date_vector = .data$post_date, end_date = max_date)
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
                       names_from = "weeknum")

  #Re-type pop to integer
  cases_summary$pop_2018 <- as.integer(cases_summary$pop_2018)

  list(summary = cases_summary,
       daily = cases_daily)
}

clean_histTable <- function(hdt, end_date) {
  utils::data("county_data")

  #Basic Selection/wrangling
  hdt <- hdt %>%
    dplyr::arrange(.data$GEOID, .data$DATE) %>%
    dplyr::mutate( GEOID=as.character(.data$GEOID) ) %>%
    dplyr::rename(fips = GEOID ) %>%
    dplyr::group_by(.data$fips) %>%
    dplyr::transmute(
      geo_type = .data$GEO,
      geo_name = .data$NAME,
      post_date = .data$DATE,
      case_daily = dplyr::if_else(is.na(.data$POS_NEW), .data$POSITIVE, .data$POS_NEW),
      test_daily = dplyr::if_else(is.na(.data$TEST_NEW), .data$POSITIVE + dplyr::if_else(is.na(.data$NEGATIVE), 0L, as.integer(.data$NEGATIVE)), as.integer(.data$TEST_NEW)),
      death_daily = dplyr::if_else(is.na(.data$DTH_NEW), .data$DEATHS, .data$DTH_NEW)
    ) %>%
    dplyr::ungroup(.) %>%
    tidyr::complete(tidyr::nesting(fips, geo_type, geo_name), post_date,
                    fill = list(case_daily = 0L, test_daily = 0L, death_daily = 0L)) %>%
    dplyr::left_join(dplyr::select(county_data, .data$fips, .data$herc_region, .data$pop_2018), by = "fips")

  if (inherits(hdt$post_date, "POSIXt")
  ) {
    hdt$post_date <- as.Date(hdt$post_date, tz = "America/Chicago")
  } else {
    hdt$post_date <- as.Date(as.POSIXct(as.numeric(hdt$post_date)/1000, origin = "1970-01-01 00:00.000 UTC"), tz = "America/Chicago")
  }

  if (!is.null(end_date)) {
    hdt <- dplyr::filter(hdt, .data$post_date <= as.Date(end_date))
  }

  #Clean reversals at county level
  if (any(hdt$case_daily < 0)) {
    message("Cleaning reversals in daily confirmed cases")
    hdt$case_daily_raw <- hdt$case_daily
    hdt <- hdt %>%
      dplyr::mutate(
        case_daily = clean_reversals(.data$case_daily, verbose = FALSE)
      )

    num_negs <- sum(hdt$case_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$case_daily <- clean_reversals(hdt$case_daily, verbose = FALSE)
      num_negs <- sum(hdt$case_daily < 0)
      i <- i + 1
    }
    message("  I had to call clean_reversals() on case_daily ", i, " times.")
  }

  if (any(hdt$test_daily < 0)) {
    message("Cleaning reversals in daily tests")
    hdt$test_daily_raw <- hdt$test_daily
    hdt <- hdt %>%
      dplyr::mutate(
        test_daily = clean_reversals(.data$test_daily, verbose = FALSE)
      )

    num_negs <- sum(hdt$test_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$test_daily <- clean_reversals(hdt$test_daily, verbose = FALSE)
      num_negs <- sum(hdt$test_daily < 0)
      i <- i + 1
    }
    message("  I had to call clean_reversals() on test_daily ", i, " times.")
  }

  if (any(hdt$death_daily < 0)) {
    message("Cleaning reversals in daily deaths")
    hdt$death_daily_raw <- hdt$death_daily
    hdt <- hdt %>%
      dplyr::mutate(
        death_daily = clean_reversals(.data$death_daily, verbose = FALSE)
      )

    num_negs <- sum(hdt$death_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$death_daily <- clean_reversals(hdt$death_daily, verbose = FALSE)
      num_negs <- sum(hdt$death_daily < 0)
      i <- i + 1
    }
    message("  I had to call clean_reversals() on death_daily ", i, " times.")
  }

  #Add in HERC and STATE rows
  herc <- hdt %>%
    dplyr::group_by(.data$post_date, .data$herc_region) %>%
    dplyr::summarize_at(dplyr::vars("case_daily", "test_daily", "death_daily", "pop_2018"), sum) %>%
    dplyr::mutate(
      fips = .data$herc_region,
      geo_name = .data$herc_region,
      geo_type = "HERC Region"
    )

  state <- hdt %>%
    dplyr::group_by(.data$post_date) %>%
    dplyr::summarize_at(dplyr::vars("case_daily", "test_daily", "death_daily", "pop_2018"), sum) %>%
    dplyr::mutate(
      fips = "55",
      geo_name = "Wisconsin",
      geo_type = "State"
    )

  dplyr::bind_rows(hdt, herc, state) %>%
    dplyr::mutate(
      case_cum = cumsum(.data$case_daily),
      test_cum = cumsum(.data$test_daily),
      death_cum = cumsum(.data$death_daily)
    ) %>%
    select(-.data$herc_region)

}

pull_wedss <- function(query, conn, end_date = NULL) {
  if (inherits(conn, "RODBC")) {
    hdt <- RODBC::sqlQuery(conn, query)
  } else if (inherits(conn, "DBIConnection")) {
    hdt <- odbc::dbGetQuery(conn, query)
  }

  #Might need to do some basic data cleaning in here

  clean_histTable(hdt, end_date)
}

