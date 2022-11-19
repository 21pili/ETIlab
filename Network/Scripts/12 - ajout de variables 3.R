###################################################
#                  Chaire ETIlab                  #
# author : Pierre Pili                            #
# modified :                                      #
# email : alexandre.gavaudan@etu.minesparis.psl.eu#
# phone : 06 34 27 50 46                          #
###################################################


# Libraries

library(tidyverse)
library(stringr)
library(dplyr)

# Importations

structures <- readRDS("Intermediate/structures2.rds")

base_app <- list(
    readRDS("Input/eti_base_app.rds") %>% mutate(categorieentreprise = "ETI"),
    readRDS("Input/ge_base_app.rds") %>% mutate(categorieentreprise = "GE")
    ) %>%
    bind_rows() %>%
    filter(annee_base == 2021) %>%
    distinct(siren, categorieentreprise, .keep_all = "T") %>%
    mutate(
        effectif = recode(libtrancheeffectifsunitelegale,
                        "Unités non employeuses" = 0,
                        "0 salarié" = 0,
                        "1 ou 2 salariés" = 1,
                        "3 à 5 salariés" = 3,
                        "6 à 9 salariés" = 6,
                        "10 à 19 salariés" = 10,
                        "20 à 49 salariés" = 20,
                        "50 à 99 salariés" = 50,
                        "100 à 199 salariés" = 100,
                        "200 à 249 salariés" = 200,
                        "250 à 499 salariés" = 250,
                        "500 à 999 salariés" = 500,
                        "1 000 à 1 999 salariés" = 1000,
                        "2 000 à 4 999 salariés" = 2000,
                        "5 000 à 9 999 salariés" = 5000),
        ) %>%
    mutate(
        ca = as.integer(ca),
        resultat = as.integer(resultat)
    )

# Functions

siren_to_effectif <- function(sir) {
    if (sir %in% base_app$siren) {
        return(subset(base_app, siren == sir)$effectif)
    }
    return(1)
}

structure_to_effectif <- function(nodes) {
    return(sapply(nodes, siren_to_effectif))
}

siren_to_ca <- function(sir) {
    if (sir %in% base_app$siren) {
        return(subset(base_app, siren == sir)$ca)
    }
    return(0)
}

structure_to_ca <- function(nodes) {
    return(sapply(nodes, siren_to_ca))
}

siren_to_resultat <- function(sir) {
    if (sir %in% base_app$siren) {
        return(subset(base_app, siren == sir)$resultat)
    }
    return(0)
}

structure_to_resultat <- function(nodes) {
    return(sapply(nodes, siren_to_resultat))
}

sum_na <- function(effectifs) {
    return(sum(effectifs, na.rm = "T"))
}

# Traitement !

structures$effectifs <- sapply(structures$nodes, structure_to_effectif)
structures$effectif <- sapply(structures$effectifs, sum_na)

structures$cas <- sapply(structures$nodes, structure_to_ca)
structures$ca <- sapply(structures$cas, sum_na)

structures$resultats <- sapply(structures$nodes, structure_to_resultat)
structures$resultat <- sapply(structures$resultats, sum_na)

saveRDS(structures, "Intermediate/structures_plus.rds")
