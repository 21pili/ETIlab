###################################################
#                  Chaire ETIlab                  #
# author : Pierre Pili                            #
# modified :                                      #
# email : alexandre.gavaudan@etu.minesparis.psl.eu#
# phone : 06 34 27 50 46                          #
###################################################


# Libraries

library(igraph)
library(tidyverse)
library(ggraph)

# Importations

sousgraphes <- readRDS("Intermediate/sousgraphes_connexes.rds")

base_app <- list(
    readRDS("Input/eti_base_app.rds") %>% mutate(categorieentreprise = "ETI"),
    readRDS("Input/ge_base_app.rds") %>% mutate(categorieentreprise = "GE")
    ) %>%
    bind_rows() %>%
    filter(annee_base == 2022) %>%
    select(siren, categorieentreprise) %>%
    distinct(siren, categorieentreprise)

# Traitement

sousgraphes$isousgraphes <- sapply(sousgraphes$structure, graph_from_edgelist)

structures <- tibble(
    structure = sousgraphes$structure
)

structures$n_edges <- sapply(sousgraphes$isousgraphes, gsize)
structures$diameter <- sapply(sousgraphes$isousgraphes, diameter)
structures$degrees <- sapply(sousgraphes$isousgraphes, degree)

saveRDS(structures, "Intermediate/structures.rds")
