




#' Function to determine the range of an integer list
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
utilRange <- function(x) {
        max(x, na.rm = T) - min(x, na.rm = T)
}


#' Function to evaluate a string name of function on some specified data
#'
#' @param x
#' @param data
#'
#' @return
#' @export
#'
#' @examples
utilEvalOnData <- function(x, data) {
        eval(as.name(x))(data)
}


