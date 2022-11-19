###################################################
#                  Chaire ETIlab                  #
# author : Alexandre Gavaudan                     #
# modified :                                      #
# email : alexandre.gavaudan@etu.minesparis.psl.eu#
# phone : 06 34 27 50 46                          #
###################################################


# Libraries

library(tidyverse)

# Importations

seeds_total <- list(
        readRDS("Input/eti_base_app.rds"),
        readRDS("Input/ge_base_app.rds")
    ) %>%
    bind_rows() %>%
    filter(str_detect(denominationunitelegale, regex("total", ignore_case = TRUE))) %>% #nolint
    select(siren) %>%
    distinct(siren)


saveRDS(seeds_total, "Intermediate/seeds_total.rds")
