#' Interconnection Points.
#'
#' @export
#'
eg_points <- function ()
{
    eg_url <- "https://transparency.entsog.eu/api/v1/connectionpoints.json?limit=-1"
    eg_res <- httr::GET(eg_url)
    eg_cont <- httr::content(x = eg_res, encoding = "UTF-16")
    eg_cont <- eg_cont[[2]]
    eg_cont <- lapply(eg_cont, function(x) {
        x <- x[sapply(x, function(x) {
            !is.null(x)
        })]
        x <- dplyr::bind_cols(x)
    })
    eg_cont <- dplyr::bind_rows(eg_cont)
    eg_cont
}


