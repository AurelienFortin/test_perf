library(dplyr)
library(sf)
library(leaflet)
library(stringr)
library(future)
library(furrr)
library(progressr)
library(parallel)
library(osrm)

centroides_population <- readRDS("centroides.rds")

# Fonction pour calculer un isochrone pour un IRIS donné
calculer_isochrone <- function(i, centroides_population, duree) {
  tryCatch({
    osrmIsochrone(loc = centroides_population$geometry[i], breaks = c(duree))
  }, error = function(e) {
    message(paste("Erreur pour l'IRIS", i, ":", e$message))
    return(NULL)
  })
}
# Fonction pour lancer les calculs pour tous les IRIS
calculer_tous_les_isochrones <- function(centroides_population, duree, departement) {
  plan(multisession, workers = parallel::detectCores() - 1)  # Parallélisation
  handlers(global = TRUE)  # Barre de progression
  
  iris_a_traiter <- centroides_population[centroides_population$DPT == departement,]
  
  with_progress({
    p <- progressor(steps = nrow(iris_a_traiter))  # Initialiser la barre
    
    # Calcul parallèle des isochrones
    isochrones <- future_map(1:nrow(iris_a_traiter), function(i) {
      p()  # Incrémenter la barre
      calculer_isochrone(i, iris_a_traiter, duree)
    })
  })
  
  return(isochrones)
}

isochrones_01 <- calculer_tous_les_isochrones(centroides_population, duree = 30, departement = "01")
saveRDS(isochrones_01, "isochrones/isochrones_01.rds")
