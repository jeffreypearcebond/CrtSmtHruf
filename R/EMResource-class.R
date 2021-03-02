#' @export EMResource
#' @export COVID_Obs
#' @export Staff_Obs
#' @export Bed_Obs

library( tibble )
EMResource <- setClass(
  Class = "EMResource" ,
  contains = "tbl_df"
)

COVID_Obs <- setClass(
  Class = "COVID_Obs" ,
  contains = "tbl_df"
)

Staff_Obs <- setClass(
  Class = "Staff_Obs" ,
  contains = "list"
)

Bed_Obs <- setClass(
  Class = "Bed_Obs" ,
  contains = "list"
)

setMethod(
  f = "initialize" ,
  signature = signature( "EMResource" ) ,
  definition = function(
    .Object ,
    file_name
  ) {
    toInteger <- function( i ) {i <- as.integer(i) ; i[is.na(i)] <- 0L ; i}

    hosp_cols <- readr::cols(
      Report_Date = readr::col_date(format = "%m/%d/%Y"),
      Region = readr::col_double(),
      Most_Recent_Report_Date = readr::col_date(format = "%m/%d/%Y"),
      Hospital = readr::col_character(),
      IBA__ICU = readr::col_double(),
      IBA__Intermediate_Care = readr::col_double(),
      IBA__Medical_Surgical = readr::col_double(),
      IBA__Neg_Flow_Isolation = readr::col_double(),
      Total_Intermediate_Care_Beds = readr::col_double(),
      Total_Medical_Surgical_Beds = readr::col_double(),
      Total_Neg_Flow_Isolation_Beds = readr::col_double(),
      Number_of_Ventilated_Patients = readr::col_double() ,
      Total___COVID_patients = readr::col_double() ,
      Critical_Staff_Today = col_factor( levels = c("Yes","No" , "--" ) ) ,
      Critical_Staff_Week = col_factor( levels = c("Yes","No", "--") )
    )

    emresource_relation <- readr::read_csv(file_name, col_types = hosp_cols) %>%
      mutate(
        Region = dplyr::case_when(
          Region == 1 ~ "Northwest",
          Region == 2 ~ "North Central",
          Region == 3 ~	"Northeast",
          Region == 4 ~ "Western" ,
          Region == 5 ~ "South Central",
          Region == 6 ~ "Fox Valley Area",
          Region == 7 ~ "Southeast"
        )
      ) %>% mutate_at( vars( matches(c("Beds","IBA"))) , toInteger )

    callNextMethod( .Object , emresource_relation )
  }
)

