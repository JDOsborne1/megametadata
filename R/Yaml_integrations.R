library(yaml)


here::here("dict.yml") %>%
        read_yaml() -> tester


tester$HouseholdLevel$name <- "Household Level"

tester$HouseholdLevel$Description <- "Household Level aggregation of variables"


write_yaml(tester, here::here("dict.yml") )
