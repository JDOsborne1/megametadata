# Functions for determining the metadata of a column automatically



#' Normalise a Vector
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
normalise <- function(vect){
        return(vect/max(vect, na.rm=T))
}


#' Get the uniqueness of a vector
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
uniqueness <- function(vect){
        return((length(unique(vect))-1)/length(vect))
}


#' Does the variable have a constant length
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
constCharLength <- function(vect){
        number_of_lengths <- length(unique(nchar(as.character(vect))))
        return(number_of_lengths == 1)
}

#' Provisional date format checking function
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
 dateForm <- function(vect){
         return(TRUE)
 }

#' Provisional postcode format checker
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
 postForm <- function(vect){
         return(TRUE)
 }

#' Guess the data type based on the attributes
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
guessDataType <- function(vect){
        Type <- dplyr::case_when(
                class(vect) == "Date" ~ "Date"
                , class(vect) == "POSIXct" ~ "Date-Time"
                , (uniqueness(vect) < 0.1) & constCharLength(vect) ~ "Tag"
                , (uniqueness(vect) > 0.8) & constCharLength(vect) ~ "ID"
                , (uniqueness(vect) >= 0.1) & (uniqueness(vect) <= 0.8) & constCharLength(vect) & dateForm(vect) ~ "Date"
                , (uniqueness(vect) >= 0.1) & (uniqueness(vect) <= 0.8) & constCharLength(vect) & postForm(vect) ~ "Post Code"
                , (uniqueness(vect) < 0.1) & !constCharLength(vect) ~ "Category"
                , T ~ "PII/Value"
        )
        return(Type)
}
