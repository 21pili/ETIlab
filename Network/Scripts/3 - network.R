###################################################
#                  Chaire ETIlab                  #
# author : Pierre Pili                            #
# modified :                                      #
# email : alexandre.gavaudan@etu.minesparis.psl.eu#
# phone : 06 34 27 50 46                          #
###################################################


# Libraries

library(tidyverse)
library(doParallel)
library(stringr)

# Importations

base_app <- base_app <- list(
    readRDS("Input/eti_base_app.rds"),
    readRDS("Input/ge_base_app.rds")
    ) %>%
    bind_rows() %>%
    filter(annee_base == 2022)

seeds <- tibble(siren = base_app$siren)

dir_def <- readRDS("Intermediate/links.rds") %>%
    distinct(siren, .keep_all = "T")

from <<- c()
to <<- c()
flagged <<- c("")

# définition d'une fonction

explore <- function(sir) {
    print(sir)
    flagged <<- flagged %>% append(sir)
    if ((sir %in% base_app$siren) & (sir %in% dir_def$siren)) {
        sub <- subset(dir_def, siren == sir)
        parent <- sub$parent
        from <<- from %>% append(parent)
        to <<- to %>% append(sir)
    }
    else {
        parent <- ""
    }
    if (!(parent %in% flagged)) {
        explore(parent)
    }
}

dfs_recursif <- function(seeds) {
    for (siren in seeds$siren){
        if (!(siren %in% flagged)) {
            explore(siren)
        }
    }
}

dfs_recursif(seeds)

network <- tibble(
    from,
    to
)

saveRDS(network, "Intermediate/network.rds")
