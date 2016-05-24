library("data.table")
library("maptools")
library("geosphere")
library("RColorBrewer")
library("plotGoogleMaps")
library("rgdal")
library("foreign")
library("foreach")

oursin <- function(objectname, filename, echant=0, min_tr=0, proj="WGS84", ...) {
  
  #
  # DOC
  # Auteur : François Malaussena
  # Janvier 2016
  # 
  # La fonction :
  # oursin(min_tr, objectname, filename, echant=0, proj="WGS84"/"LIIE", ...) 
  #
  # Les paramètres :
  # oursin(matrice_OD_en_entree, nom_du_fichier_en_sortie, si_on_veut_juste_un_echantillon, nb_minimum_de_transactions, projection=WGS84_ou_LIIE, liste_des_magasins_voulus)
  #
  # Nomenclature des labels d'objectname (la matrice OD) :
  # "or_id" (character) : id de l'acheteur (code commune par exemple)
  # "de_id" (character) : id du magasin
  # "or_lon" (numeric),"or_lat" (numeric)
  # "de_lon" (numeric),"de_lat" (numeric)
  # facultatif :
  # "X_FREQ" (numeric) : nombre de tickets
  # "nb_tr"(numeric) : nombre de lignes ticket
  #
  # Paramètres
  # objectname (character) = le ficher csv en entrée, matrice avec origines (code commune des acheteurs), destinations (identifiant des magasins) et leurs coordonnées
  # filename (character) = le nom du fichier .KML de sortie
  # min_tr (numeric) = ne selectionner que les flux ayant un nb_tr > min_tr. Mettre 0 pour traiter toutes les lignes
  # echant (numeric) = pour ne prendre qu'un échantillon au hasard des lignes. Mettre 0 pour traiter toutes les lignes
  # proj (character) = "WGS84" ou "LIIE"
  # ... (character) = les identifiants des magasins dont on souhaite voir les flux. Ne rien mettre pour traiter tous les magasins.
  #
  
  # charger projections
  WGS84 <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +no_defs"
  LIIE <- "+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=0 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs"
  
  # charger csv   
  od_matrix_one <- objectname
  # supprimer toutes les lignes where nb_tr < min_tr
  if (min_tr > 0 & "nb_tr" %in% names(od_matrix_one)) {
    od_matrix_one <- od_matrix_one[od_matrix_one$nb_tr > min_tr,]
    rownames(od_matrix_one) <- seq(length=nrow(od_matrix_one))
  }
  
  # se limiter aux magasins ciblés  
  magasins <- list(...)
  if(length(magasins) > 0){
    od_matrix_two <- od_matrix_one[od_matrix_one$de_id == magasins,]
    rownames(od_matrix_two) <- seq(length=nrow(od_matrix_two))
  }
  else{
    od_matrix_two <- od_matrix_one
  }
  rm(od_matrix_one)
  
  # se limiter à l'échantillon demandé
  if(echant>1){
    od_matrix_two <- od_matrix_two[sample(nrow(od_matrix_two), min(nrow(od_matrix_two),echant)),]
    rownames(od_matrix_two) <- seq(length=nrow(od_matrix_two))
  }
  
  # choix de la projection
  if(proj=="LIIE"){
    o_matrix_sp<-SpatialPoints(coords = od_matrix_two[,c("or_lon","or_lat")],proj4string = CRS(LIIE))
    d_matrix_sp<-SpatialPoints(coords = od_matrix_two[,c("de_lon","de_lat")],proj4string = CRS(LIIE))
  } else{
    o_matrix_sp<-SpatialPoints(coords = od_matrix_two[,c("or_lon","or_lat")],proj4string = CRS(WGS84))
    d_matrix_sp<-SpatialPoints(coords = od_matrix_two[,c("de_lon","de_lat")],proj4string = CRS(WGS84))
  }
  
  # écriture fichier shp
  flows <- gcIntermediate(p1 = o_matrix_sp, p2=d_matrix_sp, sp = TRUE) #donne les coordonnées des points qui constituent la ligne entre les deux points 
  flows <- SpatialLinesDataFrame(sl=flows, data = od_matrix_two)
  # plot(flows,col=c(brewer.pal(8,"Set2"),brewer.pal(9,"Set3"))[as.numeric(as.character(od_matrix$de_id))]) #ligne inutile ?
  writeOGR(obj=flows, dsn =filename, layer="flows", driver="ESRI Shapefile", overwrite_layer = TRUE )
  
  # valeurs de retour
  nb_flux <- nrow(od_matrix_two)
  flux <- list(
    "nb_flux" = nb_flux # flux$nb_flux
    #,"result_od_matrix" = od_matrix_two   # décommenter pour que la fonction retourne aussi la matrice résultante (tous les flux avec nb_tr > min_tr)
  )
  return(flux)
}

# Exemples :
# 1000 flux au hasard dans toute la base
oursin(od_matrix, "flows1", 1000, 0)

# Tous les flux avec + de 300 transactions
oursin(od_matrix, "flows2", min_tr = 300)

# Tous les flux des magasins de grenoble grand'place et villiers en biere
oursin(od_matrix, "flows3", 0, 0, "WGS84", "945_009", "907_001")
