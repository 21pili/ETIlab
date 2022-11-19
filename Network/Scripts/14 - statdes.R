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

structures <- readRDS("Intermediate/structures_plus.rds")

# Plotting

ggplot(data = structures) +
    geom_bar(aes(x = categoriestructure))

summary <- structures %>%

    summarize(
        n = n(),
        mean_diameter = mean(diameter),
        std_diameter = sd(diameter),
        mean_n_edges = mean(n_edges),
        std_n_edges = sd(n_edges),
        mean_degree = mean(degree),
        std_n_degree = sd(degree),
        mean_effectif = mean(effectif),
        sd_effectif = sd(effectif),
        mean_ca = mean(ca),
        sd_ca = sd(ca),
        mean_resultat = mean(resultat),
        sd_resultat = sd(resultat)
    )

ggplot(data = structures) +
    geom_point(aes(x = effectif, y = ca, color = categoriestructure))

ggsave("graph/structures/ca_effectif.png")

base_app <- list(
    readRDS("Input/eti_base_app.rds") %>% mutate(categorieentreprise = "ETI"),
    readRDS("Input/ge_base_app.rds") %>% mutate(categorieentreprise = "GE")
    ) %>%
    bind_rows() %>%
    mutate(
        libeffectif = recode(libtrancheeffectifsunitelegale,
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
        resultat = as.integer(resultat),
        effectif = as.integer(effectif),
        freq_catjur = is.na(categoriejuridiqueunitelegale),
        freq_act = is.na(activiteprincipaleunitelegale),
        freq_catjur = is.na(categoriejuridiqueunitelegale),
        freq_libeffectif = is.na(libtrancheeffectifsunitelegale),
        freq_ca = is.na(ca),
        freq_resultat = is.na(resultat),
        freq_longitude = is.na(longitude),
        freq_latitude = is.na(latitude),
        freq_effectif = is.na(effectif),
        freq_fn = is.na(nombre_etablissements)
    )

summary <- base_app %>%
    summarize(
        n = n(),
        mean_libeffectif = sum(libeffectif, na.rm = "T") / n,
        std_libeffectif = sd(libeffectif, na.rm = "T"),
        mean_effectif = sum(effectif, na.rm = "T") / n,
        std_effectif = sd(effectif, na.rm = "T"),
        mean_netab = sum(nombre_etablissements, na.rm = "T") / n,
        std_netab = sd(nombre_etablissements, na.rm = "T"),
        mean_ca = sum(ca, na.rm = "T") / n,
        std_ca = sd(ca, na.rm = "T"),
        mean_resultat = sum(resultat, na.rm = "T") / n,
        std_resultat = sd(resultat, na.rm = "T"),
        freq_catjur = 1 - (sum(freq_catjur) / n),
        freq_act = 1 - (sum(freq_act) / n),
        freq_catjur = 1 - (sum(freq_catjur) / n),
        freq_libeffectif = 1 - (sum(freq_libeffectif) / n),
        freq_ca = 1 - (sum(freq_ca) / n),
        freq_resultat = 1 - (sum(freq_resultat) / n),
        freq_longitude = 1 - (sum(freq_longitude) / n),
        freq_latitude = 1 - (sum(freq_latitude) / n),
        freq_effectif = 1 - (sum(freq_effectif / n)),
        freq_fn = 1 - (sum(freq_fn / n))
    )

summary2 <- base_app %>%
    group_by(categorieentreprise) %>%
    summarize(
        freq = n(),
        mean_effectif = sum(libeffectif, na.rm = "T")/freq,
        sd_effectif = sd(libeffectif, na.rm = "T"),
        mean_ca = sum(ca, na.rm = "T")/freq,
        sd_ca = sd(ca, na.rm = "T"),
        mean_resultat = mean(resultat, na.rm = "T"),
        sd_resultat = sd(resultat, na.rm = "T"),
        mean_netab = mean(nombre_etablissements, na.rm = "T"),
        std_netab = sd(nombre_etablissements, na.rm = "T")
    )
