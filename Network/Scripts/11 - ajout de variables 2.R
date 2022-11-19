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
library(stringr)
library(ggraph)

# Importations

structures <- readRDS("Intermediate/structures.rds")

base_app <- list(
    readRDS("Input/eti_base_app.rds") %>% mutate(categorieentreprise = "ETI"),
    readRDS("Input/ge_base_app.rds") %>% mutate(categorieentreprise = "GE")
    ) %>%
    bind_rows() %>%
    filter(annee_base == 2022) %>%
    select(siren, categorieentreprise) %>%
    distinct(siren, categorieentreprise)

# Functions

return_categorie <- function(sir) {
        if (sir %in% base_app$siren) {
                sub <- subset(base_app, siren == sir)
                return(sub$categorieentreprise)
        }
        return("Dirigeant")
}

structure_to_categorie <- function(structure) {
    edgelist <- tibble(from = structure[, 1], to = structure[, 2])
    nodes <- tibble(siren = unique(c(edgelist$from, edgelist$to)))
    nodes$categorie <- sapply(nodes$siren, return_categorie)
    return(nodes$categorie)
}

structure_to_nodes <- function(structure) {
    edgelist <- tibble(from = structure[, 1], to = structure[, 2])
    nodes <- tibble(siren = unique(c(edgelist$from, edgelist$to)))
    return(nodes$siren)
}


n_eti <- function(string) {
    return(str_count(paste0(string, collapse = ""), "ETI"))
}

n_ge <- function(string) {
    return(str_count(paste0(string, collapse = ""), "GE"))
}

structure_to_categorie <- function(n_ge) {
    if (n_ge > 0) {
        return("GE")
    }
    return("ETI")
}

structure_to_degree <- function(degrees) {
    return(degrees[1])
}

# Traitement

structures$nodes <- sapply(structures$structure, structure_to_nodes)
structures$catgeories <- sapply(structures$structure, structure_to_categorie)

saveRDS(structures, "Intermediate/structures2.rds")
structures <- readRDS("Intermediate/structures2.rds")


structures$n_eti  <- sapply(structures$catgeories, n_eti)
structures$n_ge <- sapply(structures$catgeories, n_ge)

structures$categoriestructure <- sapply(structures$n_ge, structure_to_categorie)


saveRDS(structures, "Intermediate/structures2.rds")

structures <- readRDS("Intermediate/structures2.rds")

structures$degree <- sapply(structures$degrees, structure_to_degree)

saveRDS(structures, "Intermediate/structures2.rds")
