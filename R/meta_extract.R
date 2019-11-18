# Functions for determining the metadata of a column automatically

#' Function to compute the distinctiveness of a function, using a variant log-loss
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
util_Distinctiveness <- function(vect){
        stage1 <- meta_Normalise(vect)
        # would need to sort the vector
        stage2 <- sort(stage1)
        # want to jitter the values to prevent the log-cost from exploding
        stage3 <- jitter(stage2)
        # want the differences between the values
        stage4 <- diff(stage3)
        # Want to apply a log-cost to the values in the vector This is not currently scaled for duplicates
        stage5 <- log1p(stage4)
        # Then combining the vector of costed differences to produce a single metric
        stage6 <- sum(stage5)
        return(stage6)
}

#' Normalise a Vector
#'
#' @param vect the vector in question
#'
#' @return
#' @export
#'
#' @examples
util_Normalise <- function(vect){
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
util_Uniqueness <- function(vect){
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
util_ConstCharLength <- function(vect){
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
util_DateForm <- function(vect){
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
util_PostForm <- function(vect){
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
meta_GuessDataType <- function(vect){
        Type <- dplyr::case_when(
                class(vect) == "Date" ~ "Date"
                , class(vect) == "POSIXct" ~ "Date-Time"
                , (util_Uniqueness(vect) < 0.2) & util_ConstCharLength(vect) ~ "Tag"
                , (util_Uniqueness(vect) > 0.8) & util_ConstCharLength(vect) ~ "ID"
                , (util_Uniqueness(vect) >= 0.2) & (util_Uniqueness(vect) <= 0.8) & util_ConstCharLength(vect) & util_DateForm(vect) ~ "Date"
                , (util_Uniqueness(vect) >= 0.2) & (util_Uniqueness(vect) <= 0.8) & util_ConstCharLength(vect) & util_PostForm(vect) ~ "Post Code"
                , (util_Uniqueness(vect) < 0.2) & !util_ConstCharLength(vect) ~ "Category"
                , T ~ "PII/Value"
        )
        return(Type)
}
