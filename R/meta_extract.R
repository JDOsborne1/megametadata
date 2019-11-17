# Functions for determining the metadata of a column automatically

#' Function to compute the distinctiveness of a function, using a variant log-loss
#'
#' @param vect
#'
#' @return
#' @export
#'
#' @examples
distinctiveness <- function(vect){
        stage1 <- normalise(vect)
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
#' @param vect
#'
#' @return
#' @export
#'
#' @examples
normalise <- function(vect){
        return(vect/max(vect, na.rm=T))
}

# getDensity <- function(vect){
#         require(ggplot2)
#         plot <- ggplot() +
#                 aes(x = vect) +
#                 geom_density()
#         return(plot)
# }

#' Get the uniqueness of a vector
#'
#' @param vect
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
#' @param vect
#'
#' @return
#' @export
#'
#' @examples
constCharLength <- function(vect){
        number_of_lengths <- length(unique(nchar(as.character(vect))))
        return(number_of_lengths == 1)
}

# dateForm <- function(vect){
#         return(TRUE)
# }
# postForm <- function(vect){
#         return(TRUE)
# }

#' Guess the data type based on the attributes
#'
#' @param vect
#'
#' @return
#' @export
#'
#' @examples
guessDataType <- function(vect){
        Type <- dplyr::case_when(
                class(vect) == "Date" ~ "Date"
                , class(vect) == "POSIXct" ~ "Date-Time"
                , (uniqueness(vect) < 0.2) & constCharLength(vect) ~ "Tag"
                , (uniqueness(vect) > 0.8) & constCharLength(vect) ~ "ID"
                , (uniqueness(vect) >= 0.2) & (uniqueness(vect) <= 0.8) & constCharLength(vect) & dateForm(vect) ~ "Date"
                , (uniqueness(vect) >= 0.2) & (uniqueness(vect) <= 0.8) & constCharLength(vect) & postForm(vect) ~ "Post Code"
                , (uniqueness(vect) < 0.2) & !constCharLength(vect) ~ "Category"
                , T ~ "PII/Value"
        )
        return(Type)
}
