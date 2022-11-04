
#' Get operator point directions data.
#'
#' This list contains all known operator point directions, including the ones involving non-Transmission
#' System operators; i.e. this list can contain also storage system operators, or LNG system operators, all of
#' which do not in any way send data to ENTSOG’s Transparency Platform.
#'
#' @param has_data To filter the list only those operator point directions
#'     which belong to a TSO, which itself is publishing
#'     REG715 information, the parameter hasData can be used.
#'
#' @export
eg_operatorpointdirection <- function(has_data = FALSE){

  eg_url <- "https://transparency.entsog.eu/api/v1/operatorpointdirections.json?limit=-1"

  if(has_data) eg_url <- paste0(eg_url, "&hasData=1")

  eg_res <- httr::GET(eg_url)

  eg_cont <- httr::content(x = eg_res, encoding = "UTF-16")

  eg_cont <- eg_cont[[2]]

  eg_cont <- lapply(eg_cont, function(x){

    x <- x[sapply(x, function(x){!is.null(x)})]
    x <- dplyr::bind_cols(x)
  })

  eg_cont <- dplyr::bind_rows(eg_cont)

  eg_cont
}
