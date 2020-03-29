

#' Category Extractor
#'
#' @param slice the index of the data field
#' @param metadata the metadata dictionary
#' @param extract_level the level to perform the extract on
#'
#' @return
#' @export
#'
#' @examples
metaCategoryExtract <- function(slice, metadata, extract_level){
        if(class(metadata[[extract_level]][[slice]]) != "list") {stop("Not a field element")}
        metadata[[extract_level]][[slice]][["data_category"]]
}


#' Name Extractor
#'
#' @param slice the index of the data field
#' @param metadata the metadata dictionary
#' @param extract_level the level to perform the extract on
#'
#' @return
#' @export
#'
#' @examples
metaNameExtract <- function(slice, metadata, extract_level){
        if(class(metadata[[extract_level]][[slice]]) != "list") {stop("Not a field element")}
        metadata[[extract_level]][[slice]][["name"]]
}
#' Class Extractor
#'
#' @param slice the index of the data field
#' @param metadata the metadata dictionary
#' @param extract_level the level to perform the extract on
#'
#' @return
#' @export
#'
#' @examples
metaClassExtract <- function(slice, metadata, extract_level){
        metadata[[extract_level]][[slice]] %>% class()
}


#' Variable Type Splitter
#'
#' @param metadata the data fictionary
#' @param extract_level the level to perform the extract on
#'
#' @return
#' @export
#' @importFrom rlang .data
#'
#' @examples
metaSplitNominal <- function(metadata, extract_level = NULL){
        if(any(class(metadata) == "list")){

        if(is.null(extract_level)) {stop("Extract Level Required for list dicts")}

        dict_range <- 1:length(metadata[[extract_level]])

        classes <- dict_range %>% purrr::map(metaClassExtract, metadata, extract_level) %>% unlist()

        data_range <- which(classes == "list")

        types <-  data_range %>% purrr::map(metaCategoryExtract, metadata, extract_level) %>% unlist()

        nominal_slices <- types %in% c("Category", "Tag")
        cont_slices <- !types %in% c("Category", "Tag")

        names <- data_range %>% purrr::map(metaNameExtract, metadata, extract_level) %>% unlist()

        column_split <- list()

        column_split$nominal <- names[nominal_slices]
        column_split$continuous <- names[cont_slices]

        column_split
        } else if(any("data.frame" %in% class(metadata))){
        nominal_names <- metadata %>% dplyr::filter(.data$Type %in% c("Category", "Tag")) %>% dplyr::pull(.data$Column)
        cont_names <- metadata %>% dplyr::filter(!.data$Type %in% c("Category", "Tag")) %>% dplyr::pull(.data$Column)

        column_split <- list()

        column_split$nominal <- nominal_names
        column_split$continuous <- cont_names

        column_split
        } else {
                stop("No method to support that structure of metadata")
        }



}


