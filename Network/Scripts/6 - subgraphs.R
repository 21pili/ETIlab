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

# Fonctions

sousgraphe_connexe <- function(siren) {
    as_edgelist(subgraph(graph, subcomponent(graph, siren, mode = "all"))) #nolint
}

# Importations

network <- readRDS("Intermediate/network.rds")

nodes <- tibble(siren = unique(c(network$from, network$to)))

# Traitement du graph

graph <- graph_from_data_frame(network, vertices = nodes)

nodes$composante_connexe <- sapply(nodes$siren, sousgraphe_connexe)

sousgraphes <- tibble(
        structure = unique(c(nodes$composante_connexe))
    )

saveRDS(sousgraphes, "Intermediate/sousgraphes_connexes.rds")
