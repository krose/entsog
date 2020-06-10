

#' Get operational data.
#'
#' @param indicator Indicator
#' @param pointDirection Point Direction.
#' @param periodFrom Period from.
#' @param periodTo Period to.
#' @param periodType Period type.
#' @param operatorKey Operator key.
#' @param tsoEicCode TSO EIC code.
#' @param operatorLabel Operator Label.
#' @param pointKey Point Key.
#' @param pointLabel Point Label.
#' @param tsoItemIdentifier TSO Item Identifier.
#' @param directionKey Direction Key.
#' @param timezone Time zone.
#' @param limit Limit. -1 for unlimited.
#'
#' @export
#'
eg_op <- function(indicator = NULL, pointDirection = NULL,
                  from = NULL, to = NULL, periodType = NULL,
                  operatorKey = NULL, tsoEicCode = NULL,
                  operatorLabel = NULL, pointKey = NULL,
                  pointLabel = NULL, tsoItemIdentifier = NULL,
                  directionKey = NULL,
                  timeZone = "CET",
                  limit = -1){
  if(!is.null(from)) from <- as.character(from)
  if(!is.null(to)) to <- as.character(to)

  argg <- c(as.list(environment()))
  argg <- argg[!sapply(argg, is.null)]
  url <- paste(names(argg), unlist(argg), sep = "=")
  url <- paste(url, collapse = "&")
  rm(list = names(argg))

  url <- paste0("https://transparency.entsog.eu/api/v1/operationaldatas.csv?", url)
  url <- stringr::str_replace_all(string = url, pattern = " ", replacement = "%20")

  en_get <- httr::GET(url)

  en_df <- httr::content(en_get, as = "text", encoding = "UTF-8")
  en_df <- readr::read_csv(en_df, locale = readr::locale(tz = "CET"))

  en_df
}


#' Get operational data.
#'
#' @param indicator Indicator
#' @param periodType Period type.
#' @param periodFrom Period from.
#' @param periodTo Period to.
#' @param operatorKey Operator key.
#' @param tsoEicCode TSO EIC code.
#' @param operatorLabel Operator Label.
#' @param pointKey Point Key.
#' @param pointLabel Point Labe.
#' @param tsoItemIdentifier TSO Item Identifier.
#' @param directionKey Direction Key.
#' @param timezone Time zone.
#' @param limit Limit. -1 for unlimited.
#'
#' @export
#'
eg_agg <- function(indicator = NULL, periodType = NULL,
                  from = NULL, to = NULL,
                  operatorKey = NULL, tsoEicCode = NULL,
                  operatorLabel = NULL, pointKey = NULL,
                  pointLabel = NULL, tsoItemIdentifier = NULL,
                  directionKey = NULL,
                  timeZone = "CET",
                  limit = -1){
  argg <- c(as.list(environment()))
  argg <- argg[!sapply(argg, is.null)]
  url <- paste(names(argg), unlist(argg), sep = "=")
  url <- paste(url, collapse = "&")
  rm(list = names(argg))

  url <- paste0("https://transparency.entsog.eu/api/v1/aggregatedData.csv?", url)
  url <- stringr::str_replace_all(string = url, pattern = " ", replacement = "%20")

  en_get <- httr::GET(url)

  en_df <- httr::content(en_get, as = "text", encoding = "UTF-8")
  en_df <- readr::read_csv(en_df, locale = readr::locale(tz = "CET"))

  en_df
}



#' Get operational data.
#'
#' @param indicator Indicator
#' @param periodType Period type.
#' @param periodFrom Period from.
#' @param periodTo Period to.
#' @param operatorKey Operator key.
#' @param tsoEicCode TSO EIC code.
#' @param operatorLabel Operator Label.
#' @param pointKey Point Key.
#' @param pointLabel Point Labe.
#' @param tsoItemIdentifier TSO Item Identifier.
#' @param directionKey Direction Key.
#' @param timezone Time zone.
#' @param limit Limit. -1 for unlimited.
#'
#' @export
#'
eg_connectionpoints <- function(indicator = NULL, periodType = NULL,
                   from = NULL, to = NULL,
                   operatorKey = NULL, tsoEicCode = NULL,
                   operatorLabel = NULL, pointKey = NULL,
                   pointLabel = NULL, tsoItemIdentifier = NULL,
                   directionKey = NULL,
                   timeZone = "CET",
                   limit = -1){
  argg <- c(as.list(environment()))
  argg <- argg[!sapply(argg, is.null)]
  url <- paste(names(argg), unlist(argg), sep = "=")
  url <- paste(url, collapse = "&")
  rm(list = names(argg))

  url <- paste0("https://transparency.entsog.eu/api/v1/connectionpoints.csv?", url)
  url <- stringr::str_replace_all(string = url, pattern = " ", replacement = "%20")

  en_get <- httr::GET(url)

  en_df <- httr::content(en_get, as = "text", encoding = "UTF-8")
  en_df <- readr::read_csv(en_df, locale = readr::locale(tz = "CET"))

  en_df
}


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
#' head(lapply(opal, function(x){eg_operationaldata_physical_flow(x, as.Date("2019-10-14"), as.Date("2019-10-15"))}))
#'
#' gg_opal <- lapply(opal, function(x){eg_operationaldata_physical_flow(x, Sys.Date() - 5, Sys.Date() + 1)}) %>%
#'   bind_rows() %>%
#'   mutate(value = (value / 1000000) * 24) %>%
#'   ggplot(., aes(periodFrom, value, col = pointLabel)) +
#'   geom_line() +
#'   hrbrthemes::theme_ipsum() +
#'   labs(title = "Opal", y = "GWh / day", x = NULL)
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
