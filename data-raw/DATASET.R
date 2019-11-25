

default_spec <- yaml::read_yaml(here::here("data-raw/default_spec.yml"))

usethis::use_data(default_spec)
