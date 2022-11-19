###################################################
#                  Chaire ETIlab                  #
# author : Alexandre Gavaudan                     #
# modified :                                      #
# email : alexandre.gavaudan@etu.minesparis.psl.eu#
# phone : 06 34 27 50 46                          #
###################################################


# Libraries

library(tidyverse)
library(doParallel)
library(stringr)

# Importations

base_app <- list(
    readRDS("Input/eti_base_app.rds"),
    readRDS("Input/ge_base_app.rds")
    ) %>%
    bind_rows() %>%
    filter(annee_base == 2022)


# Fonctions

to_siren <- function(nom) {
    if (nom %in% base_app$denominationunitelegale) {
        sir <- base_app[denominationunitelegale == nom]$siren
        return(sir)
    }
    return(nom)
}

traitement <- function(df) {
  foreach (i = 0:nrow(df)) %do% {
    nom <- df$Nom[i]
    print(i)
    len <- nchar(nom)
    nom <- substring(nom, first = 1, last = len - 1)
    df$parent[i] <- to_siren(nom[1])
  }
  return(df)
}

dir_def <- list(
        readRDS("Input/ge_dir_def.rds"),
        readRDS("Input/eti_dir_def.rds")
    ) %>%
    bind_rows() %>%
    distinct(siren, .keep_all = "T") %>%
    filter(
        Titre == "Président" |
        Titre == "Gérant" |
        Titre == "Associé" |
        Titre == "Administrateur" |
        Titre == "Directeur général" |
        Titre == "Associé indéfiniment responsable" |
        Titre == "gérant" |
        Titre == "président" |
        Titre == "Gérant, Associé" |
        Titre == "Associé en nom" |
        Titre == "administrateur" |
        Titre == "directeur général" |
        Titre == "Gérant, Associé indéfiniment responsable" |
        Titre == "Associé indéfiniment et solidairement responsable" |
        Titre == "Directeur général délégué" |
        Titre == "Gérant non associé" |
        Titre == "Président du conseil d'administration") %>%
    mutate(
        parent = NA,
        Nom = toupper(Nom)
    )

dir_def <- traitement(dir_def)

saveRDS(dir_def, "Intermediate/links.rds")