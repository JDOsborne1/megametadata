library(yaml)


here::here("dict.yml") %>%
        read_yaml() -> tester

here::here("dict_spec.yml") %>%
        read_yaml() -> spec



write_yaml(tester, here::here("dict.yml") )


## Testing for integer

tester <- list()
appendIntegerMeta <- function(a_dataframe, a_list, a_column){
        if(is.null(a_list[[a_column]])) {
                a_list[[a_column]] <- list()
                }

        a_list[[a_column]][["min"]] <- min(a_dataframe[[a_column]])
        a_list[[a_column]][["max"]] <- max(a_dataframe[[a_column]])
        a_list[[a_column]][["name"]] <- a_column
        a_list
}
test_data <- iris %>% as.data.table()


test_list <- test_data %>%
        appendIntegerMeta(tester, "Sepal.Length")

utilRange <- function(x) {
        max(x, na.rm = T) - min(x, na.rm = T)
        }
utilEvalOnData <- function(x, data) {
        eval(as.name(x))(data)
}


tester$AccountLevel$Sepal.Length = spec$AccountLevel$integer %>% purrr::map(utilEvalOnData, data = iris$Sepal.Length)
