
## Building out the default spec
default_spec <- yaml::read_yaml(here::here("data-raw/default_spec.yml"))

usethis::use_data(default_spec)


## building out the testing dataset
tester <- iris %>%
        as_tibble() %>%
        metaDictionary(list(), levelgen = "AccountLevel")



usethis::use_data(tester, overwrite = TRUE)


tester_aug <- metaUpdateDictWithSpec(tester, megametadata::spec, as_tibble(iris), "AccountLevel")
