

#' Get interconnections data.
#'
#' @param pointDirection PointDirection from entsog.
#' @param from From as.Date.
#' @param to To. as.Date.
#' @param periodType PeriodType. Defaults to 'hour'.
#' @param datetime_utc_convert Convert datetimes to UTC. Defaults to TRUE.
#' @param remove_forward_dt Remove datetime observations ahead in time. This is a problem for some
#'   variables where there shouldn't be observations as they are only historic.
#' @param replace_known_operator_zeros Some operators give zeros instead of NAs, for ex NET4GAS.
#'   The zeros are replaced with NAs.
#'
#' @export
#'
#' @examples
#'
#' library(entsog)
#' library(tidyverse)
#'
#' nordstream <- c("cz-tso-0001itp-00010entry", "de-tso-0016itp-00251entry", "de-tso-0017itp-00247entry")
#' opal <- c("de-tso-0016itp-00251entry", "de-tso-0017itp-00247entry", "cz-tso-0001itp-00010entry")
#'
#' head(lapply(opal, function(x){eg_operationaldata_physical_flow(opal, as.Date("2019-10-14"), as.Date("2019-10-15"))}))
#'
eg_operationaldata_physical_flow <- function(pointDirection, from, to, periodType = "hour", datetime_utc_convert = TRUE, remove_forward_dt = TRUE, replace_known_operator_zeros = TRUE){

  # The api is very slow if there are more than one pointDirection, so stop execution if this is the case.
  if(length(pointDirection) > 1){
    stop("Only query one pointDirection.")
  }

  eg_url <- paste0("https://transparency.entsog.eu/api/v1/operationalData.json?pointDirection=", pointDirection, "&from=", from, "&to=", to, "&indicator=Physical%20Flow&periodType=", periodType, "&timezone=CET&limit=-1")

  eg_res <- httr::GET(eg_url)

  eg_cont <- httr::content(x = eg_res, as = "parsed", encoding = "UTF-16")

  eg_cont <- eg_cont[[2]]

  eg_cont <- lapply(eg_cont, function(x){

    x <- x[sapply(x, function(x){!is.null(x)})]
    x <- dplyr::bind_cols(x)
  })

  eg_cont <- dplyr::bind_rows(eg_cont)

  if(datetime_utc_convert){
    eg_cont$periodFrom <- lubridate::with_tz(lubridate::ymd_hms(eg_cont$periodFrom, tz = "CET"), "UTC")
    eg_cont$periodTo <- lubridate::with_tz(lubridate::ymd_hms(eg_cont$periodTo, tz = "CET"), "UTC")
    eg_cont$lastUpdateDateTime <- lubridate::with_tz(lubridate::ymd_hms(eg_cont$lastUpdateDateTime, tz = "CET"), "UTC")
  }

  if(remove_forward_dt){
    eg_cont <- dplyr::filter(eg_cont, lastUpdateDateTime > periodFrom)
  }

  if(replace_known_operator_zeros){
    bad_op_test <- eg_cont$operatorLabel == "NET4GAS" & eg_cont$value == 0
    eg_cont$value[bad_op_test] <- as.integer(NA)
  }

  eg_cont
}
