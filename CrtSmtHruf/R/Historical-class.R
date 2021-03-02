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

