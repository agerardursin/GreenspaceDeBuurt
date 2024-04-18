## ---- Data Loading
# check if needed packages are installed
packages <- c("terra", "sf", "plyr", "mapaccuracy", "sampling", "sp", "mapview",
              "foreach", "osmdata", "tidyverse", "spData", "dplyr","fs", 
              "raster", "exactextractr","readxl", "rasterize", "stars", "rgdal",
              "tidyterra", "RColorBrewer", "parallel", "lwgeom", "units")#,"OpenStreetMap")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# load packages
lapply(packages, library, character.only = TRUE)

# cores<- detectCores()
# cl <- makeCluster(cores-4)
# clusterEvalQ(cl, {
#   library(terra)
#   library(sf) 
#   library(plyr) 
#   library(mapaccuracy) 
#   library(sampling) 
#   library(sp) 
#   library(mapview)
#   library(foreach) 
#   library(osmdata) 
#   library(tidyverse) 
#   library(spData) 
#   library(dplyr) 
#   library(fs) 
#   library(raster) 
#   library(exactextractr)
#   library(readxl) 
#   library(rasterize) 
#   library(stars) 
#   library(rgdal)
#   library(tidyterra) 
#   library(RColorBrewer)
# })

my_dir = "C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data\\OSM data" 
setwd(my_dir)

osmBuilding <- st_read("gis_osm_buildings_a_fre_Clip.shp") # use for masking but nothing else
osmVegetation <- st_read("gis_osm_natural_free_1_Clip.shp") # use for tree location
osmRecAreas <- st_read("gis_osm_pois_a_free_1_Clip.shp") # use for design location -> design function
osmMiscServicesAndPlaces <- st_read("gis_osm_pois_free_1_Clip.shp") # use for masking
osmRoads <- st_read("gis_osm_roads_free_1_Clip.shp") #use for road info -> use for masking
osmParking <- st_read("gis_osm_traffic_a_free__Clip.shp") #use for roads
osmStreetMarkers <- st_read("gis_osm_traffic_free_1_Clip.shp") #use for masking
osmBusStops <- st_read("gis_osm_transport_free__Clip.shp") #use for masking

osmGeo <- c(osmBuilding$geometry, osmVegetation$geometry, osmRecAreas$geometry, 
            osmMiscServicesAndPlaces$geometry, osmRoads$geometry, 
            osmParking$geometry,osmStreetMarkers$geometry, osmBusStops$geometry)
# osmGeoRd <- c(osmRoads$geometry,osmParking$geometry)
# osmClassRd <- rep("Road", length(osmRoads$geometry)+length(osmParking$geometry))
# osmTypeRd <- c(osmRoads$fclass,osmParking$fclass)

osmClass <- c(rep("Building",length(osmBuilding$geometry)), 
                 rep("Tree",length(osmVegetation$geometry)), 
                 rep("Rec Area",length(osmRecAreas$geometry)), 
                 rep("Misc Services",length(osmMiscServicesAndPlaces$geometry)), 
                 rep("Road",length(osmRoads$geometry)), 
                 rep("Parking", length(osmParking$geometry)), 
                 rep("Street marker",length(osmStreetMarkers$geometry)), 
                 rep("Bus stop",length(osmBusStops$geometry))) 

osmTypeData <- c(osmBuilding$type, osmVegetation$fclass, osmRecAreas$fclass,
                    osmMiscServicesAndPlaces$fclass, osmRoads$fclass, 
                    osmParking$fclass, osmStreetMarkers$fclass, 
                    osmBusStops$fclass)

osmData <- st_sf(class=unlist(osmClass),details=unlist(osmTypeData),geometry=osmGeo)
osmRoads_sf <- st_sf(class=rep("Road", length(osmRoads$geometry)), 
                     details=osmRoads$fclass, geometry=osmRoads$geometry)
osmBuilding_sf <- st_sf(class=osmBuilding$fclass, details=osmBuilding$type, 
                        geometry=osmBuilding$geometry)
# osmRoads_sf <- st_sf(class=osmClassRd, details=unlist(osmTypeRd), geometry=osmGeoRd)


my_dir = "C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data" 
setwd(my_dir)

# Area of Study
area_of_study  <- st_read("PA_Neighbourhoods_Select.shp")
area_of_study_sf <- st_sf(class="Study Area", 
                          areaName=area_of_study$wijknaam,
                          scores=0, illegal_gs=FALSE, 
                          interventionType="Unknown",
                          priority=0,
                          geometry=area_of_study$geometry)
area_of_study_V <- vect(area_of_study_sf)
convCRS <- crs(vect(area_of_study))
crs(area_of_study_V) <- convCRS

osmData <- st_transform(osmData, st_crs(area_of_study))
osmRoads_sf <- st_transform(osmRoads_sf, st_crs(area_of_study))
osmBuilding_sf <- st_transform(osmBuilding_sf, st_crs(area_of_study))

# natural_map <- rast('NaturalMapV1.tif')
precip_100Yr <- rast('PA_KEA_WaterDepthIntensePrecipitation_1_100years.tif')
precip_100Yr <- crop(precip_100Yr, area_of_study, mask= T)
precip_1000Yr <- rast('PA_KEA_WaterDepthIntensePrecipitation_1_1000Years.tif')
precip_1000Yr <- crop(precip_1000Yr, area_of_study, mask= T)
full_map <- rast("C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\studyArea_unclipped.tif")
crs(precip_100Yr) <- convCRS
crs(precip_1000Yr) <- convCRS

tree_list <- read.csv("C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data\\Tree_data\\tableTrees2.csv")

tree_details_path = "C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data\\Tree_data\\Data_All_Trees_v1.xlsx"

tree_names <- excel_sheets(tree_details_path)
i = 0
tree_details <- list()
for (tree in tree_names)
{
  i = i + 1
  tree_details[[i]] <- read_excel(tree_details_path, sheet = i)
}

names(tree_details) <- tree_names

trees_shp <- st_read("Bomen_Clip.shp")
trees_sf <- st_sf(
  class=rep("Tree", length(trees_shp$geometry)),
  # id=trees_shp$link,
  Height=trees_shp$BOOMHOOGTE,
  Name=trees_shp$SOORT_WET,
  Rayon=trees_shp$RAYON,
  scores=rep(100, length(trees_shp$geometry)),
  illegal_gs=rep(FALSE, length(trees_shp$geometry)),
  interventionType=rep("Edge", length(trees_shp$geometry)),
  priority=rep(10, length(trees_shp$geometry)),
  design=rep("Trees, plants, and/or shrubs", length(trees_shp$geometry)),
  gsWidth=rep(1, length(trees_shp$geometry)),
  geometry=trees_shp$geometry
)
trees_V <- vect(trees_sf)
crs(trees_V) <- convCRS

sewers_system  <- st_read("sewers_clipped.shp")
sewers_system_sf <- st_sf( class=rep("Sewer", length(sewers_system$geometry)),
                           material=sewers_system$MATERIAAL, 
                           systemType=sewers_system$STELSELTYP,
                           precipMixed=sewers_system$SOORTSTREN, 
                           scores=rep(20, length(sewers_system$geometry)),
                           illegal_gs=rep(FALSE, length(sewers_system$geometry)),
                           interventionType=rep("None", length(sewers_system$geometry)),
                           priority=rep(10,length(sewers_system$geometry)),
                           design=rep("Shallow Root Vegetation", length(sewers_system$geometry)),
                           gsWidth=rep(2, length(sewers_system$geometry)),
                           geometry=sewers_system$geometry)
sewers_system_V <- vect(sewers_system_sf)
crs(sewers_system_V) <- convCRS
sewers_systemBufferV <- sewers_system_V


for (i in 1:length(sewers_system_sf$geometry))
{
  sewers_systemBufferV[i] <- buffer(sewers_systemBufferV[i], sewers_system_sf$gsWidth[i])
  # mv_linesBufferV[i] <- buffer(waterlines_V[i], waterlines_sf$gsWidth[i])
  
}
sewers_systemBuffer_sf <- st_as_sf(sewers_systemBufferV)
sewers_systemBuffer_sf <- st_set_crs(sewers_systemBuffer_sf, st_crs(area_of_study))

sewers_systemMask_sf <- st_sf(design=sewers_systemBuffer_sf$design, 
                       priority=sewers_systemBuffer_sf$priority, 
                       scores=sewers_systemBuffer_sf$scores, 
                       illegal_gs=sewers_systemBuffer_sf$illegal_gs,
                       geometry=sewers_systemBuffer_sf$geometry)

sewers_systemMaskV <- vect(sewers_systemMask_sf)
crs(sewers_systemMaskV) <- convCRS

# TODO dsefine rain gardens based on rain
greenspace_percent_private <- st_read("Private_greenShade.shp")
green_percent_private_sf <- st_sf(
  class=rep("Private Greenspace Percentage", length(greenspace_percent_private$geometry)),
  # id=greenspace_percent_private$perceelnum,
  Percentage=greenspace_percent_private$Percentage,
  scores=greenspace_percent_private$Percentage * 0.5,
  illegal_gs=rep(FALSE, length(greenspace_percent_private$geometry)),
  interventionType=rep("Full", length(greenspace_percent_private$geometry)),
  priority=rep(2, length(greenspace_percent_private$geometry)),
  design=rep("Remove tiled garden", length(greenspace_percent_private$geometry)),
  gsWidth=rep(1, length(greenspace_percent_private$geometry)),
  geometry=greenspace_percent_private$geometry
  )
green_percent_private_V <- vect(green_percent_private_sf)
crs(green_percent_private_V) <- convCRS

for (i in 1:length(green_percent_private_sf$geometry))
{
  greenPriv_PctRainIntensity100 <- extract(precip_100Yr, green_percent_private_V[i])
  greenPriv_PctRainIntensity1000 <- extract(precip_1000Yr, green_percent_private_V[i])
  greenPriv_PctRain <-c(greenPriv_PctRainIntensity100$PA_KEA_WaterDepthIntensePrecipitation_1_100years,
                        greenPriv_PctRainIntensity1000$PA_KEA_WaterDepthIntensePrecipitation_1_1000years)
  rainCheck <- match(is.na(greenPriv_PctRain), FALSE)
  rainCheck <- rainCheck[!is.na(rainCheck)]
  
  if (sum(rainCheck) == 0)
  {
    next
  }
  
  greenPriv_PctRain <- greenPriv_PctRain[!is.na(greenPriv_PctRain)]
  greenPriv_PctRain <- max(greenPriv_PctRain)

  if ((greenPriv_PctRain >= 3)& 
      (drop_units(st_area(green_percent_private_sf$geometry[i])) < 100))
  {
    green_percent_private_V$design[i] <- "Rain Garden"
  } else if (greenPriv_PctRain > 1)
  {
    green_percent_private_V$design[i] <- "Garden"
  } else
  {
    green_percent_private_V$design[i] <- "Remove tiled Garden"
    
  }

}

green_percent_private_sf <- st_as_sf(green_percent_private_V)
green_percent_private_sf <- st_set_crs(green_percent_private_sf, 
                                       st_crs(area_of_study))
# greenspace_pct_private_sf <- st_collection_extract(green_percent_private_sf, "POLYGON")

greenspace_percent_public  <- st_read("Municipal_Green_shade.shp")
green_percent_public_sf <- st_sf(
  class=rep("Public Greenspace Percentage", length(greenspace_percent_public$geometry)),
  # id=greenspace_percent_public$perceelnum,
  Percentage=greenspace_percent_public$Percentage,
  scores=greenspace_percent_public$Percentage * 0.5,
  illegal_gs=rep(FALSE, length(greenspace_percent_public$geometry)),
  interventionType=rep("Full", length(greenspace_percent_public$geometry)),
  priority=rep(2, length(greenspace_percent_public$geometry)),
  design=rep("Trees, plants, and/or shrubs", length(greenspace_percent_public$geometry)),
  gsWidth=rep(1, length(greenspace_percent_public$geometry)),
  geometry=greenspace_percent_public$geometry
)
green_percent_public_V <- vect(green_percent_public_sf)
crs(green_percent_public_V) <- convCRS

for (i in 1:length(green_percent_public_sf$geometry))
{
  greenPub_PctRainIntensity100 <- extract(precip_100Yr, green_percent_public_V[i])
  greenPub_PctRainIntensity1000 <- extract(precip_1000Yr, green_percent_public_V[i])
  greenPub_PctRain <-c(greenPub_PctRainIntensity100$PA_KEA_WaterDepthIntensePrecipitation_1_100years,
                       greenPub_PctRainIntensity1000$PA_KEA_WaterDepthIntensePrecipitation_1_1000years)
  rainCheck <- match(is.na(greenPub_PctRain), FALSE)
  rainCheck <- rainCheck[!is.na(rainCheck)]
  
  if (sum(rainCheck) == 0)
  {
    next
  }
  
  greenPub_PctRain <- greenPub_PctRain[!is.na(greenPub_PctRain)]
  greenPub_PctRain <- max(greenPub_PctRain)
  
  if (grepl("Wadi", green_percent_public_V$design[i]))
  {
    if ((greenPub_PctRain >= 4) & 
        (drop_units(st_area(green_percent_public_sf$geometry[i])) < 300))
    {
      green_percent_public_V$design[i] <- "Rain Garden"
    } else if ((greenPub_PctRain > 1) & 
               (drop_units(st_area(green_percent_public_sf$geometry[i])) < 500))
    {
      green_percent_public_V$design[i] <- "Wadi"
    } else
    {
      green_percent_public_V$design[i] <- "Shrubs"
      
    }
  } else if ((greenPub_PctRain >= 4) & 
             (drop_units(st_area(green_percent_public_sf$geometry[i])) < 300))
  {
    green_percent_public_V$design[i] <- "Rain Garden"
  } else if ((greenPub_PctRain > 1) & 
             (drop_units(st_area(green_percent_public_sf$geometry[i])) < 500))
  {
    green_percent_public_V$design[i] <- "Wadi"
  }
}
green_percent_public_sf <- st_as_sf(green_percent_public_V)
green_percent_public_sf <- st_set_crs(green_percent_public_sf, 
                                       st_crs(area_of_study))


green_areas <- st_read("main_CLIPPED_PlantCoverAreas.shp")

# TODO define raingarden based on rain
defGreenAreas <- function(plntTypes)
{
  if (grepl("bodembedekkers", plntTypes))
  {
    typeP <- "Ground cover"
    design <- "Wadi and shrubs"
    width <- 0.5
  }else if (grepl("bosplantsoen", plntTypes))
  {
    typeP <- "Forest plantation"
    design <- "Forest"
    width <- 1.5
  }else if (grepl("gras- en kruidachtigen", plntTypes))
  {
    typeP <- "Grasses and herbs"
    design <- "Wadi and shrubs"
    width <- 1
  }else if (grepl("heesters", plntTypes))
  {
    typeP <- "Shrubs"
    design <- "Trees, plants, and/or shrubs"
    width <- 1
  }else if (grepl("planten", plntTypes))
  {
    typeP <- "Plants"
    design <- "Trees, plants, and/or shrubs"
    width <- 1
  }else
  {
    typeP <- "Unknown"
    design <- "Trees, plants, and/or shrubs"
    width <- 1
  }
  
  return(list(typeP,design,width))
}

plntTypeL <- list()
dsgGSL <- list()
GSWidthL <- list()

for (i in 1:length(green_areas$geometry))
{
  plnTp <- defGreenAreas(green_areas$plus_fysie[i])
  plntTypeL[[length(plntTypeL)+1]] <- plnTp[[1]]
  dsgGSL[[length(dsgGSL)+1]] <- plnTp[[2]]
  GSWidthL[[length(GSWidthL)+1]] <- plnTp[[3]]
}

green_areas_sf <- st_sf(
  class=rep("Greenspace", length(green_areas$geometry)),
  plant_types=unlist(plntTypeL),
  scores=rep(100, length(green_areas$geometry)),
  illegal_gs=rep(FALSE, length(green_areas$geometry)),
  interventionType=rep("Full", length(green_areas$geometry)),
  priority=rep(6,length(green_areas$geometry)),
  design=unlist(dsgGSL),#rep("Trees, plants, and/or shrubs", length(green_areas$geometry)),
  gsWidth=unlist(GSWidthL),
  geometry=green_areas$geometry
)
green_areas_V <- vect(green_areas_sf)
crs(green_areas_V) <- convCRS

for (i in 1:length(green_areas_sf$geometry))
{
  green_areasRainIntensity100 <- extract(precip_100Yr, green_areas_V[i])
  green_areasRainIntensity1000 <- extract(precip_1000Yr, green_areas_V[i])
  green_areasRain <-c(green_areasRainIntensity100$PA_KEA_WaterDepthIntensePrecipitation_1_100years,
                      green_areasRainIntensity1000$PA_KEA_WaterDepthIntensePrecipitation_1_1000years)
  rainCheck <- match(is.na(green_areasRain), FALSE)
  rainCheck <- rainCheck[!is.na(rainCheck)]
  
  if (sum(rainCheck) == 0)
  {
    next
  }
  
  green_areasRain <- green_areasRain[!is.na(green_areasRain)]
  green_areasRain <- max(green_areasRain)
  
  if (grepl("Wadi", green_areas_V$design[i]))
  {
    if ((green_areasRain >= 4) & 
        (drop_units(st_area(green_areas_sf$geometry[i])) < 300))
    {
      green_areas_V$design[i] <- "Rain Garden"
    } else if ((green_areasRain > 1) & 
               (drop_units(st_area(green_areas_sf$geometry[i])) < 500))
    {
      green_areas_V$design[i] <- "Wadi"
    } else
    {
      green_areas_V$design[i] <- "Shrubs"
      
    }
  } else if ((green_areasRain >= 4) & 
             (drop_units(st_area(green_areas_sf$geometry[i])) < 200))
  {
    green_areas_V$design[i] <- "Rain Garden"
  } else if ((green_areasRain > 1) & 
             (drop_units(st_area(green_areas_sf$geometry[i])) < 400))
  {
    green_areas_V$design[i] <- "Wadi"
  }
}

green_areas_sf <- st_as_sf(green_areas_V)
green_areas_sf <- st_set_crs(green_areas_sf, st_crs(area_of_study))


mv_lines <- st_read("main_CLIPPED_Electricity_Mediumvoltage_Lines.shp")
mv_lines_sf <- st_as_sf(mv_lines)
mv_lines_sf$id <- NULL
mv_lines_sf$class <- rep("Medium Voltage Line", length(mv_lines$geometry))
mv_lines_sf$scores<-rep(20, length(mv_lines$geometry))
mv_lines_sf$illegal_gs<-rep(FALSE, length(mv_lines$geometry))
mv_lines_sf$interventionType<-rep("None", length(mv_lines$geometry))
mv_lines_sf$priority<-rep(10, length(mv_lines$geometry))
mv_lines_sf$design<-rep("Shallow Root Vegetation", length(mv_lines$geometry))
mv_lines_sf$gsWidth<-rep(1.5,length(mv_lines$geometry))
mv_lines_V <- vect(mv_lines_sf)
crs(mv_lines_V) <- convCRS
mv_linesBufferV <- mv_lines_V

for (i in 1:length(mv_lines_sf$geometry))
{
  mv_linesBufferV[i] <- buffer(mv_linesBufferV[i], mv_lines_sf$gsWidth[i])
}
mv_linesBuffer_sf <- st_as_sf(mv_linesBufferV)
mv_linesBuffer_sf <- st_set_crs(mv_linesBuffer_sf, st_crs(green_areas))

mv_linesMask_sf <- st_sf(design=mv_linesBuffer_sf$design, 
                           priority=mv_linesBuffer_sf$priority, 
                           scores=mv_linesBuffer_sf$scores, 
                           illegal_gs=mv_linesBuffer_sf$illegal_gs,
                           geometry=mv_linesBuffer_sf$geometry)

mv_linesMaskV <- vect(mv_linesMask_sf)
crs(mv_linesMaskV) <- convCRS


historic_areas <- st_read("Gem Wageningen_historic.shp")
historic_areas <- st_transform(historic_areas, st_crs(green_areas))
historic_areas_sf <- st_sf(
  class=rep("Historic Area", length(historic_areas$geometry)),
  # id = historic_areas$osm_id,
  buildings=historic_areas$building,
  scores=rep(20, length(historic_areas$geometry)),
  illegal_gs=rep(TRUE, length(historic_areas$geometry)),
  interventionType=rep("Unknown", length(historic_areas$geometry)),
  priority=rep(2,length(historic_areas$geometry)),
  verticalGreen=rep("None",length(historic_areas$geometry)),
  typeRoof=rep("None",length(historic_areas$geometry)),
  design=rep("Unknown", length(historic_areas$geometry)),
  gsWidth=rep(1,length(historic_areas$geometry)),
  geometry=historic_areas$geometry
)
historic_areas_V <- vect(historic_areas_sf)
crs(historic_areas_V) <- convCRS

openWater <- st_read("top10_water_Clip.shp")
openWater <- st_set_crs(openWater, st_crs(green_areas))
openWater_sf <- st_sf(
  class=rep("Open water", length(openWater$geometry)),
  # id = openWater$lokaalID,
  waterType=openWater$typeWater,
  scores=rep(50,length(openWater$geometry)),
  illegal_gs=rep(FALSE, length(openWater$geometry)),
  interventionType=rep("Edge", length(openWater$geometry)),
  priority=rep(8,length(openWater$geometry)),
  design=rep("Rain Garden", length(openWater$geometry)),
  gsWidth=rep(1.5,length(openWater$geometry)),
  geometry=openWater$geometry
)
openWater_V <- vect(openWater_sf)
crs(openWater_V) <- convCRS

waterlines <- st_read("top10_water_lines_Clip.shp")
waterlines <- st_set_crs(waterlines, st_crs(green_areas))
waterlines_sf <- st_sf(
  class=rep("Waterline", length(waterlines$geometry)),
  # id=waterlines$lokaalID,
  scores=rep(20,length(waterlines$geometry)),
  illegal_gs=rep(FALSE, length(waterlines$geometry)),
  interventionType=rep("None", length(waterlines$geometry)),
  priority=rep(10,length(waterlines$geometry)),
  design=rep("Shallow Root Vegetation", length(waterlines$geometry)),
  gsWidth=rep(1.5,length(waterlines$geometry)),
  geometry=waterlines$geometry
)
waterlines_V <- vect(waterlines_sf)
crs(waterlines_V) <- convCRS
waterlinesBufferV <- waterlines_V

for (i in 1:length(waterlines_sf$geometry))
{
  waterlinesBufferV[i] <- buffer(waterlinesBufferV[i], waterlines_sf$gsWidth[i])
  
}
waterlinesBuffer_sf <- st_as_sf(waterlinesBufferV)
waterlinesBuffer_sf <- st_set_crs(waterlinesBuffer_sf, st_crs(green_areas))

waterlinesMask_sf <- st_sf(design=waterlinesBuffer_sf$design, 
                            priority=waterlinesBuffer_sf$priority, 
                            scores=waterlinesBuffer_sf$scores, 
                            illegal_gs=waterlinesBuffer_sf$illegal_gs,
                            geometry=waterlinesBuffer_sf$geometry)

waterlinesMaskV <- vect(waterlinesMask_sf)
crs(waterlinesMaskV) <- convCRS

# land class scoring
landClassScores <- function(landClass)
{
  if (grepl("grasland", landClass, ignore.case = TRUE) | 
      grepl("bos: loofbos", landClass, ignore.case = TRUE))
  {
    score <- 100
    intType <- "Full"
    priority <- 4
    if (grepl("bos: loofbos", landClass, ignore.case = TRUE))
    {
      design = "Forest"
      
    }
    else
    {
      design <- "Trees and shrubs"
    }
  }else
  {
    score <- 10
    intType <- "Unknown"
    priority <- 0
    design <- "Unknown"
  }
  return(list(score, intType, priority, design))
}

# landClassLegality <- function(landClass)
# {
#   if (grepl(landClass, "grasland", ignore.case = TRUE) | 
#       grepl(landClass, "bos: loofbos", ignore.case = TRUE))
#   {
#     illegality = FALSE
#   }
#   else
#   {
#     illegality = TRUE
#   }
#   return(illegality)
# }

landClassScoreList <- list()
landIntTypesL <- list()
landClassPriorityList <- list()
landClassDesignL <- list()
# landClassIllegalityList <- list()
land_class <- st_read("top10_terrain_Clip.shp")
land_class <- st_set_crs(land_class, st_crs(green_areas))
for (class in land_class$typeLandge)
{
  scoreAndPrior <- landClassScores(class)
  score <- scoreAndPrior[[1]]
  inTy <- scoreAndPrior[[2]]
  prior <- scoreAndPrior[[3]]
  desg <- scoreAndPrior[[4]]
  # illegal <- landClassLegality(class)
  landClassScoreList[[length(landClassScoreList)+1]] <- score
  landIntTypesL[[length(landIntTypesL)+1]] <- inTy
  landClassPriorityList[[length(landClassPriorityList)+1]] <- prior
  landClassDesignL[[length(landClassDesignL)+1]] <- desg
  # landClassIllegalityList[[length(landClassIllegalityList)+1]] <- illegal
}


land_class_sf <- st_sf(
  class=rep("Land use class", length(land_class$geometry)),
  # id=land_class$lokaalID,
  land_use=land_class$typeLandge,
  scores=unlist(landClassScoreList),
  illegal_gs=rep(FALSE, length(land_class$geometry)),
  interventionType=unlist(landIntTypesL),
  priority=unlist(landClassPriorityList),
  design=unlist(landClassDesignL),
  gsWidth=rep(1,length(land_class$geometry)),
  geometry=land_class$geometry
)

land_class_V <- vect(land_class_sf)
crs(land_class_V) <- convCRS

# land_class_sf$scores<-landClassScoreList
# land_class_sf$illegal_gs<-landClassIllegalityList
roads <- st_read("top10_roads_Clip.shp")
roads <- st_set_crs(roads, st_crs(green_areas))

roadGSWidthCalc <- function(roadWidth)
{
  if (grepl("> 7 meter",roadWidth))
  {
    width <- 1
  }else if (grepl("4 - 7 meter",roadWidth))
  {
    width <- 0.5
  }else if (grepl("2 - 4 meter",roadWidth))
  {
    width <- 0.25
  }else
  {
    width <- 1
  }
  return(width)
}

## -- Road scoring
roadScores <- function(roadUse)
{
  if (grepl("fietsers", roadUse, ignore.case = TRUE) & 
      !grepl("gemengd", roadUse, ignore.case = TRUE))
  {
    score = 10
    priority = 10
  }else if (grepl("bus", roadUse, ignore.case = TRUE) & 
           !grepl("gemengd", roadUse, ignore.case = TRUE))
  {
    score = 20
    priority = 8
  # }
  # else if (grepl(roadUse, "fietsers", ignore.case = TRUE) |
  #          grepl(roadUse, "bus", ignore.case = TRUE))
  # {
  #   score = -10
  # }
  }else
  {
    score = 60
    priority = 4
  }
  
  return(list(score,priority))
}

roadIntervention <- function(roadUse, roadType)
{
  if ((grepl("fietsers", roadUse, ignore.case = TRUE) |
      grepl("bus", roadUse, ignore.case = TRUE)) & 
      !grepl("gemengd", roadUse, ignore.case = TRUE))
  {
    illegality = TRUE
    interventionType = "Edge"
    if (!grepl("bus", roadUse, ignore.case = TRUE))
    {
      design="Wadi and shrubs"
      priority=10
    }else
    {
      design="Trees, plants, and/or shrubs"
      priority=8
    }
  }else if (grepl("lokale weg", roadType) | grepl("hoofdweg", roadType) | grepl("regionale weg", roadType))
  {
    illegality = FALSE
    interventionType = "Edge"
    priority=7
    design="Wadi and shrubs"  #"Trees, plants, and/or shrubs"
  }else if (grepl("parkeerplaats",roadType))
  {
    illegality = FALSE
    interventionType = "Full"
    priority=0
    design="Permeable pavement"
  }else
  {
    illegality = FALSE
    interventionType = "Edge"
    priority=5
    design="Trees, plants, and/or shrubs"
  }
  return(list(illegality, interventionType, priority, design))
}

rdDetailsList <- list()
rdIdxList <- list()

for (i in 1:length(roads$geometry))
{
  rdD <- NULL
  rdD <- st_intersects(roads$geometry[i],osmRoads_sf$geometry)
  rdDetail <- NULL
  idInfo <- NULL
  if (length(rdD[[1]])!=0)
  {
    idInfo <- i
    
    for (j in 1:length(rdD[[1]]))
    {
      idx <- rdD[[1]][j]
      rdDetail <- c(rdDetail, osmRoads_sf$details[idx])
      idInfo <- c(idInfo, idx)
    }
    rdDetailsList[[length(rdDetailsList)+1]] <- rdDetail
    rdIdxList[[length(rdIdxList)+1]] <- idInfo
  }
}

roadScoresList <- list()
roadIllegalityList <- list()
roadInterventionTypeList <- list()
roadPriorityList <- list()
roadDesignL <- list()
roadwidthGS <- list()

vehicles <- roads$hoofdverke
roadTypes <- roads$typeWeg
roadWidth <- roads$verharding

j=1
for (i in 1:length(vehicles))
{
  scoreandPrior <- roadScores(vehicles[i])
  score <- scoreandPrior[[1]]
  prior <- scoreandPrior[[2]]
  rdIntIll <- roadIntervention(vehicles[i], roadTypes[i])
  rdW <- roadGSWidthCalc(roadWidth[i])
  illegal <- rdIntIll[[1]]
  intTypes <- rdIntIll[[2]]
  prior <- (prior + rdIntIll[[3]])/2
  dsgn <- rdIntIll[[4]]
  roadScoresList[[length(roadScoresList)+1]] <- score
  roadIllegalityList[[length(roadIllegalityList)+1]] <- illegal
  roadInterventionTypeList[[length(roadInterventionTypeList)+1]] <- intTypes
  roadPriorityList[[length(roadPriorityList)+1]] <- prior
  roadDesignL[[length(roadDesignL)+1]] <- dsgn
  roadwidthGS[[length(roadwidthGS)+1]] <- rdW
}


roads_sf <- st_sf(
  class=rep("Road", length(roads$geometry)),
  # id=roads$lokaalID,
  infrastructureType=roads$typeInfras,
  roadType=roads$typeWeg,
  trafficType=roads$hoofdverke,
  scores=unlist(roadScoresList),
  illegal_gs=unlist(roadIllegalityList),
  interventionType=unlist(roadInterventionTypeList),
  priority=unlist(roadPriorityList),
  design=unlist(roadDesignL),
  gsWidth=unlist(roadwidthGS),
  geometry=roads$geometry
)
roads_V <- vect(roads_sf)
crs(roads_V) <- convCRS

defineRdMask <- function(typeRoad)
{
  if (grepl("footway",typeRoad))
  {
    dsgn<-"Shallow Root Vegetation"
    w_mask <- 1
    priority <- 8
    score <- 70
    illegalGS <- FALSE
  } else if (grepl("cycleway", typeRoad))
  {
    dsgn<-"None"
    w_mask <- 1.5
    priority <- 8
    score <- 5
    illegalGS <- TRUE
  } else if (grepl("service", typeRoad))
  {
    dsgn<-"Permeable pavement"
    w_mask <- 1.5 
    priority <- 7
    score <- 50
    illegalGS <- FALSE
  } else if (grepl("tertiary", typeRoad) |
             grepl("primary", typeRoad) |
             grepl("secondary", typeRoad))
  {
    dsgn<-"None"
    w_mask <- 2
    priority <- 7
    score <- 5
    illegalGS <- TRUE
  }else
  {
    dsgn<-"None"
    w_mask <- 1.5
    priority <- 5
    score <- 5
    illegalGS <- TRUE
  }
  return(list(dsgn,w_mask,priority,score,illegalGS))
}

designL <- list()
maskWL <- list()
priorL <- list()
scoreL <- list()
illegalL <- list()
for (i in 1:length(osmRoads_sf$geometry))
{
  rdMsk <- defineRdMask(osmRoads_sf$details[i])
  designL[[length(designL)+1]] <- rdMsk[[1]]
  maskWL[[length(maskWL)+1]] <- rdMsk[[2]]
  priorL[[length(priorL)+1]] <- rdMsk[[3]]
  scoreL[[length(scoreL)+1]] <- rdMsk[[4]]
  illegalL[[length(illegalL)+1]] <- rdMsk[[5]]
}

osmRoadsBuffer_sf <- st_sf(class=osmRoads_sf$class, typeRoad=osmRoads_sf$details, 
                         design=unlist(designL), gsWidth=unlist(maskWL), 
                         priority=unlist(priorL), scores=unlist(scoreL),
                         illegal_gs=unlist(illegalL), 
                         geometry=osmRoads_sf$geometry)
osmRoadsBuffer_sf <- st_set_crs(osmRoadsBuffer_sf, st_crs(green_areas))

osmRoadsV <- vect(osmRoadsBuffer_sf)
crs(osmRoadsV) <- convCRS
osmRoadsBufferV <- osmRoadsV
crs(osmRoadsBufferV) <- convCRS

for (i in 1:length(osmRoadsBuffer_sf$geometry))
{
  osmRoadsBufferV[i] <- buffer(osmRoadsBufferV[i], osmRoadsBuffer_sf$gsWidth[i])
  
}

osmRoadsBuffer_sf <- st_as_sf(osmRoadsBufferV)
osmRoadsBuffer_sf <- st_set_crs(osmRoadsBuffer_sf, st_crs(green_areas))

osmRoadsMask_sf <- st_sf(design=osmRoadsBuffer_sf$design, 
                         priority=osmRoadsBuffer_sf$priority,
                         scores=osmRoadsBuffer_sf$scores, 
                         illegal_gs=osmRoadsBuffer_sf$illegal_gs, 
                         geometry=osmRoadsBuffer_sf$geometry) 
osmRoadsMask_sf <- st_set_crs(osmRoadsMask_sf, st_crs(green_areas))
osmRoadsMaskV <- vect(osmRoadsMask_sf)
crs(osmRoadsMaskV) <- convCRS



checkPriorityRds <- function(inVect)
{
  temp_sf <- st_as_sf(inVect)
  temp_sf <- st_set_crs(temp_sf, st_crs(area_of_study))
  
  for (i in 1:length(inVect))
  {
    intersct <- is.related(osmRoadsMaskV, inVect[i], "intersects")
    
    for (j in 1:length(intersct))
    {
      if (intersct[j])
      {
        if (grepl("None", osmRoadsMaskV$design[j]) & 
            (osmRoadsMaskV$priority[j] > 5))
        {
          temp_sf$design[i] <- osmRoadsMaskV$design[j]
          temp_sf$priority[i] <- osmRoadsMaskV$priority[j]
        } 
      }
    }
  }
  
  temp_sf <- st_set_crs(temp_sf, st_crs(area_of_study))
  
}

sewers_system_sf <- checkPriorityRds(sewers_system_V)
sewers_system_V <- vect(sewers_system_sf)
crs(sewers_system_V) <- convCRS

mv_lines_sf <- checkPriorityRds(mv_lines_V)
mv_lines_V <- vect(mv_lines_sf)
crs(mv_lines_V) <- convCRS

waterlines_sf <- checkPriorityRds(waterlines_V)
waterlines_V <- vect(waterlines_sf)
crs(waterlines_V) <- convCRS







parkingClassification <- function(class,name)
{
  if (grepl("removable", name))
  {
    score <- 100
    illegal <- FALSE
    intType <- "Full"
    prior <- 6
    dsgn <- "Trees, plants, and/or shrubs"
    width <- 0.25
  } else if (!grepl("bicycle", class))
  {
    score <- 70
    illegal <- FALSE
    intType <- "Full"
    prior <- 6
    dsgn <- "Permeable pavement"
    width <- 0.5
  }else
  {
    score <- 10
    illegal <- TRUE
    intType <- "None"
    prior <- 10
    dsgn <- "None"
    width <- 1
  }
  return(list(score,illegal, intType,prior,dsgn,width))
}

scoreParkL <- list()
illegalParkL <- list()
intTypeParkL <- list()
priorParkL <- list()
dsgnParkL <- list()
widthParkL <- list()
for (i in 1:length(osmParking$geometry))
{
  parkingInfo <- parkingClassification(osmParking$fclass[i],osmParking$name[i])
  scoreParkL[[length(scoreParkL)+1]] <- parkingInfo[1]
  illegalParkL[[length(illegalParkL)+1]] <- parkingInfo[2]
  intTypeParkL[[length(intTypeParkL)+1]] <- parkingInfo[3]
  priorParkL[[length(priorParkL)+1]] <- parkingInfo[4]
  dsgnParkL[[length(dsgnParkL)+1]] <- parkingInfo[5]
  widthParkL[[length(widthParkL)+1]] <- parkingInfo[6]
}

osmParking_sf <- st_sf(class=rep("Parking", length(osmParking$geometry)), 
                       details=osmParking$fclass, 
                       scores=unlist(scoreParkL),
                       illegal_gs=unlist(illegalParkL),
                       interventionType=unlist(intTypeParkL),
                       priority=unlist(priorParkL),
                       design=unlist(dsgnParkL),
                       gsWidth=unlist(widthParkL),
                       geometry=osmParking$geometry)
osmParking_sf <- st_transform(osmParking_sf, st_crs(area_of_study))

osmParking_V <- vect(osmParking_sf)
crs(osmParking_V) <- convCRS





# roads_sf$scores=unlist(roadScoresList)
# roads_sf$illegal_gs=unlist(roadIllegalityList)
buildings <- st_read("top10_buildings_Clip.shp")
buildings <- st_set_crs(buildings, st_crs(green_areas))
buildingPrior <- function(buildHeight, buildClass)
{
  if (grepl("hoogbouw", buildHeight))
  {
    priority <- 9
    vert <- "Vertical Garden"
    width <- 2
  }else
  {
    priority <- 7
    vert <- "Living Walls"
    width <- 1.25
  }
  
  if (grepl("kerk", buildClass)| grepl("school", buildClass))
  {
    priority <- (priority + 10)/2
    roof <- "None"
  }else
  {
    priority <- (priority + 8)/2
    roof <- "Green"
  }
  
  return(list(priority, vert, roof, width))
}

buildHeight <- buildings$hoogteklas
buildClass <- buildings$typeGebouw
buildPriorityList <- list()
vertGreenL <- list()
typeRoofL <- list() # depends on if roof is 
buildGSWidth <- list()

for (i in 1:length(buildClass))
{
  prior <- buildingPrior(buildHeight[i], buildClass[i])
  buildPriorityList[[length(buildPriorityList)+1]] <- prior[[1]]
  vertGreenL[[length(vertGreenL)+1]] <- prior[[2]]
  typeRoofL[[length(typeRoofL)+1]] <- prior[[3]]
  buildGSWidth[[length(buildGSWidth)+1]] <- prior[[4]]
}

buildings_sf <- st_sf(
  class=rep("Building", length(buildings$geometry)),
  # id=buildings$lokaalID,
  buildingType=buildings$typeGebouw,
  buildingHeight=buildings$hoogteklas,
  scores=rep(20, length(buildings$geometry)),
  illegal_gs=rep(TRUE, length(buildings$geometry)),
  interventionType=rep("Edge", length(buildings$geometry)),
  priority=unlist(buildPriorityList),
  verticalGreen=unlist(vertGreenL),
  typeRoof=unlist(typeRoofL),
  design=rep("Trees, plants, and/or shrubs", length(buildings$geometry)),
  gsWidth=unlist(buildGSWidth),
  geometry=buildings$geometry
)

buildings_V <- vect(buildings_sf)
crs(buildings_V) <- convCRS

# defineBuildingMask <- function


osmBuilding_V <- vect(osmBuilding_sf)
crs(osmBuilding_V) <- convCRS

osmBuildingMask_sf <- st_sf(class=osmBuilding_sf$class, 
                            typeBuilding=osmBuilding_sf$details, 
                            design=rep("None", length(osmBuilding_sf$geometry)), 
                            gsWidth=rep(1, length(osmBuilding_sf$geometry)), 
                            illegal_gs=rep(TRUE, length(osmBuilding_sf$geometry)), 
                            geometry=osmBuilding_sf$geometry)

# buildInter <- is.related(osmBuilding_V, buildings_V[1], "intersects")


for (i in 1:length(buildings_V))
{
  intersct <- is.related(osmBuilding_V, buildings_V[i], "intersects")
  
  for (j in 1:length(intersct))
  {
    if (intersct[j])
    {
      if (grepl("apartment", osmBuilding_V$details[j]) | 
          grepl("retail", osmBuilding_V$details[j]) | 
          grepl("commercial", osmBuilding_V$details[j]) |
          grepl("service", osmBuilding_V$details[j]))
      {
        if (grepl("None",buildings_sf$typeRoof[i]))
        {
          desgn <- buildings_V$verticalGreen[i]
        } else
        {
          desgn <- paste(buildings_V$verticalGreen[i], "and",
                         buildings_sf$typeRoof[i],"roof", sep=" ")
        }
        buildings_sf$design[i] <- desgn
      } else if (grepl("house", osmBuilding_V$details[j]) | 
                 grepl("school", osmBuilding_V$details[j]) |
                 grepl("kindergarten", osmBuilding_V$details[j]) |
                 grepl("shed", osmBuilding_V$details[j]) |
                 grepl("garage", osmBuilding_V$details[j]))
      {
        if (grepl("None",buildings_sf$typeRoof[i]))
        {
          desgn <- "Green façades"
        } else
        {
          desgn <- paste("Green façades", "and",
                         buildings_sf$typeRoof[i],"roof", sep=" ")
        }
        buildings_sf$design[i] <- desgn
      }
      osmBuildingMask_sf$design[j] <- buildings_sf$typeRoof[i]
    }
  }
}
buildings_V <- vect(buildings_sf)
crs(buildings_V) <- convCRS

osmBuildingMaskV <- vect(osmBuildingMask_sf)
crs(osmBuildingMaskV) <- convCRS


shp_list <- list(area_of_study,trees_shp, sewers_system, 
                 greenspace_percent_private, greenspace_percent_public, 
                 green_areas, mv_lines, historic_areas, openWater, waterlines, 
                 land_class, roads, buildings)
sf_list <- list(area_of_study_sf,trees_sf, sewers_system_sf, 
                green_percent_private_sf, green_percent_public_sf, 
                green_areas_sf, mv_lines_sf, historic_areas_sf, openWater_sf, waterlines_sf, 
                land_class_sf, roads_sf, osmParking_sf, buildings_sf)

sf_list <- bind_rows(sf_list)

sf_gs_genList <- list(trees_sf, sewers_system_sf, #green_percent_private_sf, 
                      green_areas_sf, mv_lines_sf,
                  # green_percent_public_sf, green_areas_sf, mv_lines_sf, 
                      openWater_sf, waterlines_sf, roads_sf, osmParking_sf, 
                      buildings_sf)
sf_gs_gen <- bind_rows(sf_gs_genList)

vectList <- list(area_of_study_V,trees_V, sewers_system_V, 
                 green_percent_private_V, green_percent_public_V, 
                 green_areas_V, mv_lines_V, historic_areas_V, openWater_V, waterlines_V, 
                 land_class_V, roads_V, buildings_V)

vCollection <- svc(vectList)

classList <- list("Study Area", "Tree","Sewer","Private Greenspace Percentage",
                  "Public Greenspace Percentage", "Greenspace",
                  "Medium Voltage Line","Historic Area","Open water",
                  "Waterline","Land use class","Road","Parking","Building")
genClassList <- list("Tree","Sewer","Greenspace",#"Private Greenspace Percentage","Greenspace",
                     # "Public Greenspace Percentage","Greenspace",
                     "Medium Voltage Line", "Open water","Waterline","Road", 
                     "Parking","Building")

# for (shp in shp_list)
# {
#   sf_list[[length(sf_list)+1]] = st_as_sf(shp)
# }

geom_list <- list(area_of_study$geometry,trees_shp$geometry, 
                  sewers_system$geometry, greenspace_percent_private$geometry, 
                  greenspace_percent_public$geometry, green_areas$geometry, 
                  mv_lines$geometry, historic_areas$geometry, 
                  openWater$geometry, waterlines$geometry, land_class$geometry, 
                  roads$geometry, buildings$geometry)

geom_arr <- c(area_of_study$geometry,trees_shp$geometry, 
              sewers_system$geometry, greenspace_percent_private$geometry, 
              greenspace_percent_public$geometry, green_areas$geometry, 
              mv_lines$geometry, historic_areas$geometry, 
              openWater$geometry, waterlines$geometry, land_class$geometry, 
              roads$geometry, buildings$geometry)


shp_keys <- c("study area", "trees", "sewer system", 
              "private greenspace percentage", "public greenspace percentage",
              "green areas", "medium voltage lines", "historic areas", 
              "water bodies", "water lines", "land use class", "roads", 
              "buildings")

# Set classes and have it separate between private and public
greenscape_replacement_classes <- c("permeable", "edge intervention", "shrubs", "trees", "vertical")
greenspace_design <- c("wadi", "rain garden", "permeable pavement", "Vertical garden", "living wall")
extent_intervention <- c("full","partial", "edge", "none", "unknown")

# Name the list of shapes, geometries, and 
names(shp_list) <- shp_keys
names(sf_list) <- shp_keys
names(geom_list) <- shp_keys

# plot(sf_list[["study area"]]$geometry)
# 
# for (key in shp_keys)
# {
#   if (key != "study area")
#   {
#     plot(sf_list[[key]]$geometry, add=TRUE)
#   }

# }

distSearch <- function(polyVect)
{
  x_min <- xmin(polyVect)
  x_max <- xmax(polyVect)
  x_dist <- x_max - x_min
  y_min <- ymin(polyVect)
  y_max <- ymax(polyVect)
  y_dist <- y_max - y_min
  
  distS <- sqrt((x_dist)^2 + (y_dist)^2)*0.5
  
  return(distS)
  
}

# Function to determine to what extent one polygon is contained by another
# should only be called if distance = 0
defineContainment <- function(polyVFocus, polyVCheck, focusID, checkID)
{
  # Get intersection 
  intsct <- intersect(polyVFocus, polyVCheck)
  
  if (grepl("Greenspace", polyVCheck$class))
  {
    categ <- polyVCheck$plant_types
  }
  else if (grepl("Roads", polyVCheck$class))
  {
    if (grepl("fietsers", polyVCheck$hoofdverke, ignore.case = TRUE))
    {
      categ <- paste(polyVCheck$hoofdverke,polyVCheck$typeWeg,":")
    }
    else
    {
      categ <- polyVCheck$typeWeg
    }
  }
  else if (grepl("Tree", polyVCheck$class))
  {
    categ <- polyVCheck$Name
  }
  else
  {
    categ <- polyVCheck$class
  }
  
  if (is.empty(intsct))
  {
    contType <- "None"
    contID <- 0
    insID <- 0
    return(list(contType, contID, insID, categ))
  }
  
  # Get areas of primary vector, vector being checked, and intersection
  areaFocus <- expanse(polyVFocus)
  areaCheck <- expanse(polyVCheck)
  areaIntsct <- expanse(intsct)
  
  if (areaFocus == areaCheck)
  {
    contType <- "Same"
    contID <- focusID
    insID <- checkID
  } else if (areaIntsct == areaCheck)
  {
    contType <- "Container"
    contID <- focusID
    insID <- checkID
  } else if (areaIntsct == areaFocus)
  {
    contType <- "Contained"
    contID <- checkID
    insID <- focusID
  } else
  {
    contType <- "Overlap"
    if (areaFocus > areaCheck)
    {
      contID <- focusID
      insID <- checkID
    } else
    {
      contID <- checkID
      insID <- focusID
    }
  }
  

  
  return(list(contType, contID, insID, categ))
}



#Finds all polygons near each point of the polygon
proximitySearch <- function(poly, polID, dataset, dist, priority, typeConv,
                            desgn, width=1, class="None")
{
  if (class == "None")
  {
    dataVect <- vect(dataset)
  }else
  {
    dataVect <- chooseVect(class)
  }
  

  pol <- poly[[1]][[1]]
  pnts <- vect(pol)
  crs(pnts) <- convCRS

    
  polyV <- vect(poly)
  crs(polyV) <- convCRS
  
  # Need to include internal polygons thus need to find largest distance in polygon
  dst <- max(distSearch(polyV),dist)
  
  nearbyPoly <- nearby(polyV, dataVect, distance=dst)
  
  # Ensure that there needs to actually be a nearby polygon for this function to execute and that the 
  # point in question is unique, since the last point and first point of a polygon match
  # print(nearbyPoly)
  # nrbyPol <- unname(nearbyPoly)
  # print(is.empty(nearbyPoly[,1]))
  # print(nrbyPol[,1])
  polyID <- try(nearbyPoly[1,2])
  if(inherits(polyID, "try-error"))
  {
    #error handling code, maybe just skip this iteration using
    stop("No nearby polygons")
  }
  
  prior <- priority
  tConv <- typeConv
  dsgn <- desgn
  lenNBP <- length(nearbyPoly)/2
  
  idL <- list()
  polL <- list()
  intT <- list()
  priorL <- list()
  dstL <- list()
  legalL <- list()
  cntmntL <- list()
  cntIDL <- list()
  insIDL <- list()
  classL <- list()
  detailL <- list()
  dsgnL <- list()
  nearPX <- list()
  nearPY <- list()
  dirX <- list()
  dirY <- list()
  wL <- list()
  prevID <- 1
  
  for (i in 1:lenNBP)
  {
    id <- try(nearbyPoly[i,2])
    if(inherits(id, "try-error"))
    {
      
      if(id == lenNBP)
      #error handling code, maybe just skip this iteration using
      {
        stop("No nearby polygons found")
      } else
      {
        next
      }
    }
    
    
    dstnc <- distance(polyV, dataVect[id])
    
    if (all(dataset[id,] == dataset[prevID,]) & (id != 1))
    {
      priorL[[length(priorL)]] <- priorL[[length(priorL)]]+1
      next
      # Use this statement to ensure that only polygons (outside of polygon of interest) are within
      # acceptable distance as defined by the dist parameter. 
    } else if (dstnc > dist)
    {
      next
    }
    
    nrst <- nearest(dataVect[id],pnts)
    nearX <- xmin(nrst)
    nearY <- ymin(nrst)
    xdir <- nearX - nrst$from_x # from nearby poly to the poly that is the focus
    ydir <- nearY - nrst$from_y

    idL[[length(idL)+1]] <- id
    
    polL[length(polL)+1] <- dataset$geometry[id]

    dstL[[length(dstL)+1]] <- distance(polyV, dataVect[id])
    
    legalL[[length(legalL)+1]] <- dataset$illegal_gs[id]
    
    container <- try(defineContainment(polyVFocus = polyV, 
                                   polyVCheck = dataVect[id], focusID = polID,
                                   checkID = id))
    
    if(inherits(container, "try-error"))
    {
      next
    }
    
    cntmntL[[length(cntmntL)+1]] <- container[[1]]
    
    nearPX[[length(nearPX)+1]] <- nearX
    nearPY[[length(nearPY)+1]] <- nearY
    dirX[[length(dirX)+1]] <- xdir
    dirY[[length(dirY)+1]] <- ydir
        # cntIDL <- container[[2]] # most likely don't need this but just in case
    # insIDL <- container[[3]] # most likely don't need this but just in case
    
    if ((dataVect[id]$priority - dstL[[length(dstL)]]) > priority)
    {
      prior <- dataVect[id]$priority
      tConv <- dataVect[id]$interventionType
      dsgn <- dataVect[id]$design
      w <- dataVect[id]$gsWidth
      polyID <- id
    }
    else
    {
      prior <- priority
      tConv <- typeConv
      dsgn <- desgn
      w <- width
    }
    
    priorL[[length(priorL)+1]] <- prior
    intT[[length(intT)+1]] <- tConv
    classL[[length(classL)+1]] <- dataVect[id]$class
    detailL[[length(detailL)+1]] <- container[[4]]
    dsgnL[[length(dsgnL)+1]] <- dsgn
    wL[[length(wL)+1]] <- w
    # pnts[[length(pnts)+1]] <- pt
    
    prevID <- id
  }
    
    
    # polyList <- c(polyList,unlist(polL))
    # priorList <- c(priorList,unlist(priorL))
    # intvType <- c(intvType,unlist(intT))
    # distList<- c(distList, unlist(dstL))
    # legalityList <- c(legalityList, unlist(legalL))
  # }
  
  polyIDList <- unlist(idL)
  polyList <- polL#unlist(polL)
  priorList<-unlist(priorL)
  intvType<-unlist(intT)
  distList<-unlist(dstL)
  legalityList<-unlist(legalL)
  containmentList <- unlist(cntmntL)
  classList <- unlist(classL)
  detailList <- unlist(detailL)
  designList <- unlist(dsgnL)
  nearestPointsX <- unlist(nearPX)
  nearestPointsY <- unlist(nearPY)
  directionX <- unlist(dirX)
  directionY <- unlist(dirY)
  gsWidth <- unlist(wL)
  
  polData <- st_sf(polygonID=polyIDList, priority=priorList, 
                   interventionType=intvType,distFromPoly=distList, 
                   illegal_gs=legalityList, typeContainment=containmentList,
                   class=classList, details=detailList, design=designList, 
                   nearestPointsX=nearestPointsX, nearestPointsY=nearestPointsY,
                   directionX=directionX, directionY=directionY, 
                   gsWidth=gsWidth, geometry=polyList)
  
  polData <- st_set_crs(polData, st_crs(green_areas))
  
  return(polData)
}

#example code
rdEx <- roads_sf$geometry[497]
rdV <- vect(rdEx)
crs(rdV) <- convCRS
# dataVect <- vect(green_areas_sf)

proxData <- proximitySearch(poly=rdEx, polID=497, dataset=green_areas_sf, dist=5, priority=2, typeConv="Edge", desgn="Trees, plants, and/or shrubs")

derivePoly <- function(x_np, y_np, crd, scl, w, grad, isMin, isX, len)
{
  xRef <- crd[1]
  yRef <- crd[2]
  den <- sqrt((y_np-yRef)^2 + (x_np-xRef)^2)
  den <- if (den == 0) 1 else den
  x_transl <- w*abs(y_np-yRef)/den
  y_transl <- w*abs(x_np-xRef)/den
  
  if (isX)
  {
    addX <- if (y_transl != 0) 2*(x_np-xRef) else 2*w
    addY <- if (x_transl != 0) 2*(y_np-yRef) else len*4
    
    if (((grad > 0) & isMin) | ((grad < 0) & !isMin))
    {
      x1Calc <- x_np + addX * scl 
      x2Calc <- x_np - addX * scl
      y1 <- y_np + addY*0.5 * scl - y_transl
      y2 <- y_np - addY*0.5 * scl - y_transl
      # y3 <- y1 + 2*y_transl
      ynp <- y_np - y_transl
    } else
    {
      x1Calc <- x_np - addX * scl 
      x2Calc <- x_np + addX * scl
      y1 <- y_np - addY*0.5 * scl + y_transl
      y2 <- y_np + addY*0.5 * scl + y_transl
      # y3 <- y1 - 2*y_transl
      ynp <- y_np + y_transl
    }
    
    y3 <- y1
    y4 <- y2
    
    if (isMin)
    {
      x1 <- x1Calc + x_transl
      x2 <- x2Calc + x_transl
      x3 <- x1 + x_transl
      x4 <- x2 + x_transl
      # x3 <- x1 - 2*x_transl
      xnp <- x_np + x_transl
    } else
    {
      x1 <- x1Calc - x_transl
      x2 <- x2Calc - x_transl
      x3 <- x1 - x_transl
      x4 <- x2 - x_transl
      # x3 <- x1 + 2*x_transl
      xnp <- x_np - x_transl
    }
  } else
  {
    addX <- if (y_transl != 0) (x_np-xRef) else len*2
    addY <- if (x_transl != 0) (y_np-yRef) else w
    
    if (((grad > 0) & isMin) | ((grad < 0) & !isMin))
    {
      x1 <- x_np + addX*0.5 * scl - x_transl
      x2 <- x_np - addX*0.5 * scl - x_transl
      # x3 <- x1 + 2*x_transl
      y1Calc <- y_np + addY * scl# - y_transl
      y2Calc <- y_np - addY * scl# - y_transl
      xnp <- x_np - x_transl
      # ynp <- y_np - y_transl
    } else
    {
      x1 <- x_np - addX*0.5 * scl + x_transl
      x2 <- x_np + addX*0.5 * scl + x_transl
      # x3 <- x1 - 2*x_transl
      y1Calc <- y_np - addY * scl# + y_transl
      y2Calc <- y_np + addY * scl# + y_transl
      xnp <- x_np + x_transl
    }
    
    x3 <- x1
    x4 <- x2
    
    if (isMin)
    {
      y1 <- y1Calc + y_transl
      y2 <- y2Calc + y_transl
      y3 <- y1 + y_transl
      y4 <- y2 + y_transl
      # y3 <- y1 - 2*y_transl
      ynp <- y_np + y_transl
    } else
    {
      y1 <- y1Calc - y_transl
      y2 <- y2Calc - y_transl
      y3 <- y1 - y_transl
      y4 <- y2 - y_transl
      # y3 <- y1 + 2*y_transl
      ynp <- y_np - y_transl
    }
  }
  
  # if (isX)
  # {
  if (((grad > 0) & isMin) | ((grad < 0) & !isMin))
  {
    # poly <- ext(x1,xnp,x2,x2,x2,xnp,y1,y1,y1,ynp,y2,ynp) 
    # xL <- c(x1,x2,x3,x4)#x1,xnp,x2,x2,x2,xnp)#x1,x2,x2)
    # yL <- c(y1,y2,y3,y4)#y1,y1,y1,ynp,y2,ynp)#y1,y1,y2)
    # polDef <- cbind(xL,yL)
    # poly <- vect(polDef, type="polygons", crs=convCRS)
    poly <- ext(min(x1,x4),max(x1,x4),min(y1,y4),max(y1,y4))
    # poly <- ext(x1,x2,x2,y1,y1,y2)
    # triag1 <- ext(x1,x2,x2,y1,y2,y1) 
    # triag2 <- ext(xnp, x2, xnp, y1, ynp, ynp)
  }else if (grad == 0)
  {
    # poly <- ext(x1,x2,y1,y2)
    poly <- ext(min(x1,x2),max(x1,x2),min(y1,y2),max(y1,y2))
    
  }else
  {
    # poly <- ext(x1,x1,x1,xnp,x2,xnp,y1,ynp,y2,y2,y2,ynp)
    # poly <- ext(x1,x1,x2,y1,y2,y2)
    # xL <- c(x1,x2,x3,x4)#x1,x1,x1,xnp,x2,xnp)#x1,x1,x2)
    # yL <- c(y1,y2,y3,y4)#y1,ynp,y2,y2,y2,ynp)#y1,y2,y2)
    # polDef <- cbind(xL,yL)
    # poly <- vect(polDef, type="polygons", crs=convCRS)
    poly <- ext(min(x1,x4),max(x1,x4),min(y1,y4),max(y1,y4))
    
    # triag2 <- ext(x1, xnp, xnp, ynp, y2, ynp)
  }
  # crs(poly) <- convCRS
  # } else
  # {
  #   if (((grad > 0) & isMin) | ((grad < 0) & !isMin))
  #   {
  #     triag <- ext(x1,xnp,x2,x2,x2,xnp,y1,y1,y2,ynp,y1,ynp) 
  #     # triag1 <- ext(y1,y2,y2,x1,x2,x1) 
  #     # triag2 <- ext(ynp, y2, ynp, x1, xnp, xnp)
  #   } else
  #   {
  #     triag1 <- ext(y1,y2,y1,x1,x2,x2) 
  #     triag2 <- ext(y1, ynp, ynp, xnp, x2, xnp)
  #   }
  # }
  return(poly)
}

defineEdge <- function(x_np, y_np, bisectVect, priority, doubleProp, dist, 
                       maxD, width, onlyLongest=FALSE, full=FALSE, sidePref=0, 
                       yside=0, xside=0, onlyInner=FALSE, onlyOuter=FALSE)
{

  coordsV <- crds(bisectVect)
  
  
  x_min <- xmin(bisectVect)
  x_max <- xmax(bisectVect)
  y_min <- ymin(bisectVect)
  y_max <- ymax(bisectVect)
  
  doubleSided <- TRUE
  x_side <- TRUE
  y_side <- TRUE
  bufferN <- FALSE
  
  xmin_dist <- x_np - x_min
  xmax_dist <- x_max - x_np
  x_length <- x_max - x_min
  x_dist <- min(xmax_dist, xmin_dist)
  x_prop <- x_dist/x_length
  
  ymin_dist <- y_np - y_min
  ymax_dist <- y_max - y_np
  y_length <- y_max - y_min
  y_dist <- min(ymax_dist, ymin_dist)
  y_prop <- y_dist/y_length
  # 
  # print(x_prop)
  # print(y_prop)
  

  if (onlyLongest)
  {
    doubleSided <- FALSE
    if (y_length > x_length)
    {
      y_side <- TRUE
      x_side <- FALSE
    } else if (x_length > y_length)
    {
      y_side <- FALSE
      x_side <- TRUE
    } else
    {
      doubleSided <- TRUE
      y_side <- TRUE
      x_side <- TRUE
    }
  }else if (((y_prop > x_prop) & (y_prop > doubleProp) & (yside == 0)) | (yside < 0))
  {
    y_side <- FALSE
    doubleSided <- FALSE
  }else if(((x_prop > y_prop) & (x_prop > doubleProp) & (xside == 0)) | 
           (xside < 0))
  {
    x_side <- FALSE
    doubleSided <- FALSE
  }
  # 
  # print(x_side)
  # print(y_side)
  # print(doubleSided)
  # 
  if (full)
  {
    edgeWProp <- 1
    propScale <- 0
  } else
  {
    edgeWProp <- min(min((priority/(dist+1)),priority/10),1)
    propScale <- 1
  }
  
  # Define x polygon
  if(sidePref > 0) # Set to greater than zero since if the dir is positive from nearby poly that means it is coming from the left or below
  {
    # Get X triangles
    xmin_coord <- coordsV[match(x_min,coordsV),]
    # xmin_coord <- drop_units(xmin_coord)
    edgeLengthX <- edgeWProp*x_length*(1-xmin_dist*propScale/x_length)
    gradX <- (x_np - xmin_coord[1])*(y_np - xmin_coord[2])
    gradX <- gradX/abs(gradX)

    
    # Get Y triangles
    ymin_coord <- coordsV[match(y_min,coordsV)-length(coordsV)/2,]
    # ymin_coord <- drop_units(ymin_coord)
    edgeLengthY <- edgeWProp*y_length*(1-ymin_dist*propScale/y_length)
    gradY <- (x_np - ymin_coord[1])*(y_np - ymin_coord[2])
    gradY <- gradY/abs(gradY)
    
    if (((x_np == xmin_coord[1]) & (y_np == xmin_coord[2])) |
        ((x_np == ymin_coord[1]) & (y_np == ymin_coord[2])))
    {
      bufferN <- TRUE
    }else
    {
      polyX <- try(derivePoly(x_np = x_np, y_np = y_np, crd = xmin_coord, 
                                  scl=0.5, w=width, grad=gradX, isMin=TRUE, 
                                  isX=TRUE, len = edgeLengthX))
      
      polyY <- try(derivePoly(x_np = x_np, y_np = y_np, crd = ymin_coord, 
                                  scl=0.5, w=width, grad=gradY, isMin=TRUE, 
                                  isX=FALSE, len = edgeLengthY))
      if((inherits(polyX, "try-error")) & (inherits(polyY, "try-error")))
      {
        #error handling code, maybe just skip this iteration using
        stop("Cannot derive edge")
      }else if (inherits(polyX, "try-error"))
      {
        x_side <- FALSE
        y_side <- TRUE
        doubleSided <- FALSE
      }else if (inherits(polyY, "try-error"))
      {
        x_side <- TRUE
        y_side <- FALSE      
        doubleSided <- FALSE
      }
    }
  }else if(sidePref < 0)
  {
    # Get X triangles
    xmax_coord <- coordsV[match(x_max,coordsV),]
    # xmax_coord <- drop_units(xmax_coord)
    edgeLengthX <- edgeWProp*x_length*(1-xmax_dist*propScale/x_length)
    gradX <- (x_np - xmax_coord[1])*(y_np - xmax_coord[2])
    gradX <- gradX/abs(gradX)

    
    # Get Y triangles
    ymax_coord <- coordsV[match(y_max,coordsV)-length(coordsV)/2,]
    # ymax_coord <- drop_units(ymax_coord)
    edgeLengthY <- edgeWProp*y_length*(1-ymax_dist*propScale/y_length)
    gradY <- (x_np - ymax_coord[1])*(y_np - ymax_coord[2])
    gradY <- gradY/abs(gradY)
    
    if (((x_np == xmax_coord[1]) & (y_np == xmax_coord[2])) |
        ((x_np == ymax_coord[1]) & (y_np == ymax_coord[2])))
    {
      bufferN <- TRUE
    }else
    {
      polyX <- try(derivePoly(x_np = x_np, y_np = y_np, crd = xmax_coord, 
                                  scl=0.5, w=width, grad=gradX, isMin=FALSE, 
                                  isX=TRUE, len = edgeLengthX))
      
      polyY <- try(derivePoly(x_np = x_np, y_np = y_np, crd = ymax_coord, 
                                  scl=0.5, w=width, grad=gradY, isMin=FALSE, 
                                  isX=FALSE, len = edgeLengthY))
      
      if((inherits(polyX, "try-error")) & (inherits(polyY, "try-error")))
      {
        #error handling code, maybe just skip this iteration using
        stop("Cannot derive edge")
      }else if (inherits(polyX, "try-error"))
      {
        x_side <- FALSE
        y_side <- TRUE
        doubleSided <- FALSE
      }else if (inherits(polyY, "try-error"))
      {
        x_side <- TRUE
        y_side <- FALSE      
        doubleSided <- FALSE
      }
      
    }
    

  }else  
  {
    if (xmax_dist >= xmin_dist)
    {
      # Get X triangles
      xmin_coord <- coordsV[match(x_min,coordsV),]
      # xmin_coord <- drop_units(xmin_coord)      
      edgeLengthX <- edgeWProp*x_length*(1-x_prop*propScale)
      gradX <- (x_np - xmin_coord[1])*(y_np - xmin_coord[2])
      gradX <- gradX/abs(gradX)
      
      if ((x_np == xmin_coord[1]) & (y_np == xmin_coord[2]))
      {
        bufferN <- TRUE
      }else
      {
        polyX <- try(derivePoly(x_np = x_np, y_np = y_np, crd = xmin_coord, 
                                    scl=0.5, w=width, grad=gradX, isMin=TRUE, 
                                    isX=TRUE, len = edgeLengthX))
        if(inherits(polyX, "try-error"))
        {
          #error handling code, maybe just skip this iteration using
          stop("Cannot derive vertical edge")
        }
      }
    } else
    {
      # Get X triangles
      xmax_coord <- coordsV[match(x_max,coordsV),]
      # xmax_coord <- drop_units(xmax_coord)
      edgeLengthX <- edgeWProp*x_length*(1-x_prop*propScale)
      gradX <- (x_np - xmax_coord[1])*(y_np - xmax_coord[2])
      gradX <- gradX/abs(gradX)
      
      if ((x_np == xmax_coord[1]) & (y_np == xmax_coord[2]))
      {
        bufferN <- TRUE
      }else
      {
        polyX <- try(derivePoly(x_np = x_np, y_np = y_np, crd = xmax_coord, 
                                    scl=0.5, w=width, grad=gradX, isMin=FALSE, 
                                    isX=TRUE, len = edgeLengthX))
        if(inherits(polyX, "try-error"))
        {
          #error handling code, maybe just skip this iteration using
          stop("Cannot derive vertical edge")
        }
      }
    }

    if (ymax_dist >= ymin_dist)
    {
      # Get Y triangles
      ymin_coord <- coordsV[match(y_min,coordsV)-length(coordsV)/2,]
      # ymin_coord <- drop_units(ymin_coord)
      edgeLengthY <- edgeWProp*y_length*(1-y_prop*propScale)
      gradY <- (x_np - ymin_coord[1])*(y_np - ymin_coord[2])
      gradY <- gradY/abs(gradY)
      
      if((x_np == ymin_coord[1]) & (y_np == ymin_coord[2]))
      {
        bufferN <- TRUE
      }else
      {
        polyY <- try(derivePoly(x_np = x_np, y_np = y_np, crd = ymin_coord, 
                                    scl=0.5, w=width, grad=gradY, isMin=TRUE, 
                                    isX=FALSE, len = edgeLengthY))
        if(inherits(polyY, "try-error"))
        {
          #error handling code, maybe just skip this iteration using
          stop("Cannot derive vertical edge")
        }
      }
    }else
    {
      # Get Y triangles
      ymax_coord <- coordsV[match(y_max,coordsV)-length(coordsV)/2,]
      # ymax_coord <- drop_units(ymax_coord)
      edgeLengthY <- edgeWProp*y_length*(1-y_prop*propScale)
      gradY <- (x_np - ymax_coord[1])*(y_np - ymax_coord[2])
      gradY <- gradY/abs(gradY)
      
      if((x_np == ymax_coord[1]) & (y_np == ymax_coord[2]))
      {
        bufferN <- TRUE
      }else
      {
        polyY <- try(derivePoly(x_np = x_np, y_np = y_np, crd = ymax_coord, 
                                    scl=0.5, w=width, grad=gradY, isMin=FALSE, 
                                    isX=FALSE, len = edgeLengthY))
        if(inherits(polyY, "try-error"))
        {
          #error handling code, maybe just skip this iteration using
          stop("Cannot derive vertical edge")
        }
        
      }
    }
  }
  
  if (!bufferN)
  {
    biV <- as.lines(bisectVect)
    edgeV <- buffer(biV,width)
    polX <- crop(edgeV,polyX)
    polY <- crop(edgeV,polyY)
    
    
    if (onlyInner)
    {
      polX <- crop(bisectVect,polX)
      polY <- crop(bisectVect,polY)
    } #else if (onlyOuter)
    # {
    #   polMaskX <- crop(bisectVect,polX)
    #   polMaskY <- crop(bisectVect,polY)
    #   polX <- polX - polMaskX#, inverse=TRUE)
    #   polY <- polY - polMaskY#, inverse=TRUE)
    # }
    
    crs(polX) <- convCRS
    
    crs(polY) <- convCRS
  }else
  {
    pnts_ext <- ext(x_np-width/2, x_np+width/2, y_np-width/2, y_np+width/2)
    polV <- vect(pnts_ext)
    crs(polV) <- convCRS
    pol <- buffer(polV,width)
    polout <- crop(bisectVect,pol)
  }

  if(bufferN)
  {
    polyout <- polout
  } else if (doubleSided)
  {
    if (geomtype(polX) == geomtype(polY))
    {
      polyout <- union(polX,polY)
      polyouy <- union(polyout)
    } else
      if (expanse(polX) > expanse(polY))
    {
      polyout <- polX
    } else if (expanse(polX) < expanse(polY))
    {
      polyout <- polY
    } else
    {
      stop("Cannot derive edge due to strange geometries")
      polyout <- polX
    }
    
  } else if (x_side)
  {
    polyout <- polX
  } else if (y_side)
  {
    polyout <- polY
  }
  return(polyout)
}

#example code
x_nearP <- proxData$nearestPointsX[1]
y_nearP <- proxData$nearestPointsY[1]
biVect <- rdV
prior <- proxData$priority[1]
doubleProp <- 0.3
dist <- proxData$distFromPoly[1]
maxD <- 5
width <- 1

edgePoly<-defineEdge(x_np=x_nearP, y_np=y_nearP, bisectVect=biVect, 
                     priority=prior, doubleProp=doubleProp, dist=dist, 
                     maxD=maxD, width=width, onlyLongest=FALSE, full=FALSE, sidePref=0, 
                     yside=0, xside=0, onlyOuter=TRUE)

# plot(rdV, col="grey")
# plot(edgePoly, col="green", add=TRUE)
# plot(edgePoly)


chooseVect <- function(class)
{
  if (grepl("Study Area",class))
  {
    retVect <- area_of_study_V
  } else if (grepl("Tree",class))
  {
    retVect <- trees_V
  } else if (grepl("Sewer",class))
  {
    retVect <- sewers_system_V
  } else if (grepl("Private Greenspace Percentage",class))
  {
    retVect <- green_percent_private_V
  } else if (grepl("Public Greenspace Percentage",class))
  {
    retVect <- green_percent_public_V
  } else if (grepl("Greenspace",class))
  {
    retVect <- green_areas_V
  } else if (grepl("Medium Voltage Line",class))
  {
    retVect <- mv_lines_V
  } else if (grepl("Historic Area",class))
  {
    retVect <- historic_areas_V
  } else if (grepl("Open water",class))
  {
    retVect <- openWater_V
  } else if (grepl("Waterline",class))
  {
    retVect <- waterlines_V
  } else if (grepl("Land use class",class))
  {
    retVect <- land_class_V
  } else if (grepl("Road",class))
  {
    retVect <- roads_V
  } else if (grepl("Parking",class))
  {
    retVect <- osmParking_V
  } else if (grepl("Building",class))
  {
    retVect <- buildings_V
  } else
  {
    retVect <- NA
    print("Error no class found")
  }
  return(retVect)
}
# Need a better way of defining parts inside shape that are illegal to convert into gs -> e.g. parking lot needs area close to road
conversionGS <- function(dataset, searchDist)
{
  data <- dataset
  # print(dataset)
  # dataVect <- vect(dataset)
  treesDone <- FALSE
  sewerDone <- FALSE
  mvLinesDone <- FALSE
  waterLinesDone <- FALSE
  openWaterDone <- FALSE
  
  idList <- list()
  dsgList <- list()
  priorList <- list()
  gsList <- list()
  gsScoreList <- list()
  illegalL<- list()
  prevClass <- dataset$class[1]
  polID <- 0
  startIdx <- 0
   # make an index for returning correct vector?
  # most likely should be parallelized somehow -> make a list that can have it performed over rather than iterating
  for (i in 1:length(dataset$geometry))
  {
    # Removing first row for simplification purposes
    # data <- data[-1,]
    print("Poly:")
    print(i)
    print("Class:")
    print(dataset$class[i])
    pctDone <- 100*i/length(dataset$geometry)
    pctDoneOut <- paste(pctDone, "%", " Done (",i,"/", length(dataset$geometry),
                        ")", sep="")
    print(pctDoneOut)
    
    if (prevClass != dataset$class[i])
    {
      prevClass <- dataset$class[i]
      startIdx <- i-1
    }
    
    polID <- i - startIdx
    
    geoV <- chooseVect(dataset$class[i])
    
    pol <- dataset$geometry[i]
    GS_finished <- FALSE
    geoList <- list()
    plantTypeL <- list()
    dsgnL <- list()
    # print("Vect Outer")
    gsGeo <- vect(dataset$geometry[i])
    # print("Vect Outer Success")
    crs(gsGeo) <- convCRS
    gsDesign <- dataset$design[i]
    # geoV <- chooseVect(dataset$class[i])
    
    if ((grepl("Tree", dataset$class[i]) & !treesDone) |
        (grepl("Sewer",dataset$class[i]) & !sewerDone) |
        (grepl("Medium Voltage Line",dataset$class[i]) & !mvLinesDone) |
        (grepl("Waterline", dataset$class[i]) & !waterLinesDone) |
        (grepl("Open water", dataset$class[i]) & !openWaterDone))
    {
      
      if (grepl("Tree",dataset$class[i]))
      {
        treesDone = TRUE
      }else if (grepl("Sewer",dataset$class[i]))
      {
        sewerDone = TRUE
      } else if (grepl("Medium Voltage Line",dataset$class[i]))
      {
        mvLinesDone = TRUE
      } else if (grepl("Waterline",dataset$class[i]))
      {
        waterLinesDone = TRUE
      } else if (grepl("Open water",dataset$class[i]))
      {
        openWaterDone = TRUE
      }
      
      gsV <- buffer(geoV, dataset$gsWidth[i])
      CurrSF <- st_as_sf(gsV)
      
      for (k in 1:length(CurrSF$geometry))
      {
        dsgList[[length(dsgList)+1]] <- CurrSF$design[k]
        priorList[[length(priorList)+1]] <- CurrSF$priority[k]
        gsList[length(gsList)+1] <- CurrSF$geometry[k]
        gsScoreList[[length(gsScoreList)+1]] <- CurrSF$scores[k]
        illegalL[[length(illegalL)+1]] <- CurrSF$illegal_gs[k]
      }

      next
    }else if((grepl("Tree", dataset$class[i]) & treesDone) |
             (grepl("Sewer",dataset$class[i]) & sewerDone) |
             (grepl("Medium Voltage Line",dataset$class[i]) & mvLinesDone) |
             (grepl("Waterline", dataset$class[i]) & waterLinesDone) |
             (grepl("Open water", dataset$class[i]) & openWaterDone))
    {
      next
    }
    
    if (grepl("None", dataset$design[i]))
    {
      next
    }
    
    # print("Finding Nearby Poly")
    nearbyPolyList <- list()
    
    if (grepl("Building",dataset$class[i]))
    {
      searchDist <- searchDist * dataset$priority[i]/2
    } else if (grepl("Road",dataset$class[i]))
    {
      searchDist <- searchDist/2 * dataset$priority[i]/10
    }
    
    for (cls in genClassList)
    {
      # print("Hola")
      if (cls != dataset$class[i])
      {
        nrbyPol <- try(proximitySearch(
          poly=dataset$geometry[i],dataset=data, dist=searchDist, polID = polID,
          priority = dataset$priority[i],typeConv = dataset$interventionType[i], 
          desgn = dataset$design[i], width = dataset$gsWidth[i], class = cls))
        
        if(inherits(nrbyPol, "try-error"))
        {
          #error handling code, maybe just skip this iteration using
          next
        }else
        {
          nearbyPolyList[[length(nearbyPolyList)+1]] <- nrbyPol
        }
        
        
      }
    }
    
    nearbyPolData <- bind_rows(nearbyPolyList)
    
    # print("Found nearby Poly")
    
    if (length(nearbyPolData) < 10)
    {
      next
    }
    
    
    for (j in 1:length(nearbyPolData$geometry))
    {
      if ((grepl("Study Area",nearbyPolData$class[j]) | 
          grepl("None", nearbyPolData$design[j])) & (j != 1))
      {
        next
      }
      
      if (grepl("Road",dataset$class[i]) & 
          # grepl("Wadi and shrubs",dataset$design[i])) &
          (grepl("lokale weg",nearbyPolData$details[j]) |
           grepl("hoofdweg",nearbyPolData$details[j]) |
           grepl("regionale weg",nearbyPolData$details[j]) | 
           grepl("fietsers",nearbyPolData$details[j])))
      {
        onlyLong <- TRUE
        full <- TRUE
        onlyIn <- FALSE
        # width = max(0.5,width*0.5)
        
      }else
      {
        onlyLong <- FALSE
        full <- FALSE
        onlyIn <- FALSE
        # width=max(1,width)
      }
      
      if (dataset$illegal_gs[i])
      {
        onlyOuter = TRUE
      }else
      {
        onlyOuter = FALSE
      }
      
      score_scale <- (nearbyPolData$priority[j]/dataset$priority[i])
      # print("Generating greenspace")
      
      if (grepl("Full",dataset$interventionType[i]))
      {
        # group points together based on object proximity - if points are near the same object that expands the object
        if (j == 1)
        {
          geoNV <- buffer(gsGeo, dataset$gsWidth[i])
          geoN <- st_as_sf(geoNV)
          
          if (length(geoN$geometry) == 0)
          {
            next
          }
          

          
          # for (l in 1:length(geoN$geometry))
          # {
            # geoN <- geoN$geometry
            dsgList[[length(dsgList)+1]] <- dataset$design[i]
            priorList[[length(priorList)+1]] <- dataset$priority[i]
            gsList[length(gsList)+1] <- geoN$geometry
            gsScoreList[[length(gsScoreList)+1]] <- dataset$scores[i]
            illegalL[[length(illegalL)+1]] <- dataset$illegal_gs[i]
          # }
        }
        print("Generating greenspace full")
        
        # if (grepl("Greenspace", nearbyPolData$class[j]) | grepl("Tree", nearbyPolData$class[j]))
        # {
        score_scale <- (dataset$priority[i]/nearbyPolData$priority[j])
        # if(nearbyPolData$scores[j]*nearbyPolData$priority[j] > dataset$scores[i]*dataset$priority[i])
        # {
        #   score <- nearbyPolData$scores[j]
        # }
        ar <- drop_units(st_area(nearbyPolData$geometry[j]))
        
        if( grepl("Contained",nearbyPolData$typeContainment[j]) | #grepl(nearbyPolData$typeContainment[j], "Container") |
           grepl("Overlap", nearbyPolData$typeContainment[j]) ) 
        {
          # dst <- ar/drop_units(st_perimeter(nearbyPolData$geometry[j]))
          # inDesign <- nearbyPolData$design[j]
          print("Vect Contained")
          geoV <- vect(nearbyPolData$geometry[j])
          crs(geoV) <- convCRS
          geoNV <- buffer(geoV, nearbyPolData$gsWidth[j])
          geoN <- st_as_sf(geoNV)
          # geoN <- geoN$geometry
          if (length(geoN$geometry) == 0)
          {
            next
          }
          
          # for (l in 1:length(geoN$geometry))
          # {
            dsgList[[length(dsgList)+1]] <- nearbyPolData$design[j]
            priorList[[length(priorList)+1]] <- nearbyPolData$priority[j]
            gsList[length(gsList)+1] <- geoN$geometry
            gsScoreList[[length(gsScoreList)+1]] <- min(nearbyPolData$scores[j] * 
                                                          score_scale,100)
            illegalL[[length(illegalL)+1]] <- dataset$illegal_gs[i]
          # }
          
        }else 
        {
          # bbox <- st_bbox(nearbyPolData$geometry[j])
          # num <-min(bbox[3]-bbox[1],bbox[4]-bbox[2])
          # den <-max(bbox[3]-bbox[1],bbox[4]-bbox[2])
          # dst <- num/den
          
          
          if ((grepl("Greenspace", dataset$class[i]) | 
               grepl("Forest", dataset$design[i]) | 
               grepl("Trees", dataset$design[i]) | 
               grepl("shrubs", dataset$design[i], ignore.case=TRUE)) &
              !grepl("wadi", dataset$design[i], ignore.case=TRUE) &
              grepl("Permeable", nearbyPolData$design[j]) & (ar > 25) )#& 
              # num > 2)
          {
            inDesign <- "Rain Garden" 
            geoNV <-try(defineEdge(x_np=nearbyPolData$nearestPointsX[j],
                             y_np=nearbyPolData$nearestPointsY[j], 
                             bisectVect=gsGeo, 
                             priority=nearbyPolData$priority[j], 
                             doubleProp=0.4, 
                             dist=nearbyPolData$distFromPoly[j], 
                             maxD=searchDist, width=dataset$gsWidth[i]*3, 
                             onlyLongest=onlyLong, full=full, sidePref=0,
                             yside=0, xside=0, onlyInner=TRUE))
            if(inherits(geoNV, "try-error"))
            {
              #error handling code, maybe just skip this iteration using
              next
            }else
            {
              geoN <- st_as_sf(geoNV)
            }
            # geoN <- geoN$geometry
            if (length(geoN$geometry) == 0)
            {
              next
            }
            # for (l in 1:length(geoN$geometry))
            # {
              dsgList[[length(dsgList)+1]] <- inDesign
              priorList[[length(priorList)+1]] <- nearbyPolData$priority[j]
              gsList[length(gsList)+1] <- geoN$geometry#[l]
              gsScoreList[[length(gsScoreList)+1]] <- min(nearbyPolData$scores[j] * 
                                                            score_scale,100)
              illegalL[[length(illegalL)+1]] <- dataset$illegal_gs[i]
            # }

            
            
          } else if (nearbyPolData$priority[j] > dataset$priority[i])
          {
            if (!grepl(dataset$design[i], "Trees, plants, and/or shrubs") | 
                !grepl(dataset$design[i], "Wadi and shrubs"))
            {
              w <- dataset$gsWidth[i] 
            } else
            {
              w <- nearbyPolData$gsWidth[i]*
                (nearbyPolData$priority[j] - dataset$priority[i])*
                max(1,searchDist - nearbyPolData$distFromPoly[j])
              onlyIn <- TRUE
            }
            geoNV <-try(defineEdge(x_np=nearbyPolData$nearestPointsX[j],
                              y_np=nearbyPolData$nearestPointsY[j], 
                              bisectVect=gsGeo, 
                              priority=nearbyPolData$priority[j], 
                              doubleProp=0.4, 
                              dist=nearbyPolData$distFromPoly[j], 
                              maxD=searchDist, width=w, 
                              onlyLongest=onlyLong, full=full, sidePref=0,
                              yside=0, xside=0, onlyInner=onlyIn, 
                              onlyOuter = onlyOuter))
            if(inherits(geoNV, "try-error"))
            {
              #error handling code, maybe just skip this iteration using
              next
            }else
            {
              geoN <- st_as_sf(geoNV)
            }
            # geoN <- geoN$geometry
            if (length(geoN$geometry) == 0)
            {
              next
            }
            
            # for (l in 1:length(geoN$geometry))
            # {
              dsgList[[length(dsgList)+1]] <- nearbyPolData$design[j]
              priorList[[length(priorList)+1]] <- nearbyPolData$priority[j]
              gsList[length(gsList)+1] <- geoN$geometry#[l]
              gsScoreList[[length(gsScoreList)+1]] <- min(nearbyPolData$scores[j] * 
                                                            score_scale,100)
              illegalL[[length(illegalL)+1]] <- dataset$illegal_gs[i]
            # }
            
            
          }else
          {
            inDesign <- nearbyPolData$design[j]
          }
        # } 
        
        # geoList[[length(geoList)]] <- st_buffer(nearbyPolData$geometry[j], dist = nearbyPolData$gsWidth[j])
        # plantTypeL[[length(plantTypeL)+1]] <- nearbyPolData$details[j]
        # idList[[length(idList)]] <- nearbyPolData$polygonID[j]
        # dsgnL[[length(dsgnL)]] <- inDesign
        }
      } else if (grepl("Edge", dataset$interventionType[i])) # define params for different "Edge" cases
      {
        print("Generating greenspace edge")
        
        if (grepl("Building",dataset$class[i]) & (j == 1))
        {
          geoNV <- buffer(gsGeo, dataset$gsWidth[i])
          geoN <- st_as_sf(geoNV)
          if (length(geoN$geometry) == 0)
          {
            next
          }
          
          # for (l in 1:length(geoN$geometry))
          # {
            dsgList[[length(dsgList)+1]] <- dataset$design[i]
            priorList[[length(priorList)+1]] <- dataset$priority[i]
            gsList[length(gsList)+1] <- geoN$geometry#[l]
            gsScoreList[[length(gsScoreList)+1]] <- dataset$scores[i]
            illegalL[[length(illegalL)+1]] <- dataset$illegal_gs[i]
          # }
          
          # geoN <- geoN$geometry

        }
        if ((grepl("lokale weg",dataset$roadType[i]) |
            grepl("hoofdweg",dataset$roadType[i]) |
            grepl("regionale weg",dataset$roadType[i]) ) &
            (!grepl("lokale weg",nearbyPolData$details[j]) |
             !grepl("hoofdweg",nearbyPolData$details[j]) |
             !grepl("regionale weg",nearbyPolData$details[j]) )&
             grepl("fietsers",nearbyPolData$details[j]))
        {
          nearbyPolData$design[j] <- "Shrubs"
        } else if ((grepl("lokale weg",nearbyPolData$details[j]) |
                    grepl("hoofdweg",nearbyPolData$details[j]) |
                    grepl("regionale weg",nearbyPolData$details[j]) ) &
                   (!grepl("lokale weg",dataset$roadType[i]) |
                    !grepl("hoofdweg",dataset$roadType[i]) |
                    !grepl("regionale weg",dataset$roadType[i]) )&
                   grepl("fietsers",dataset$trafficType[i]))
        {
          nearbyPolData$design[j] <- "Wadi"
          
        }
        
        geoNV<-try(defineEdge(x_np=nearbyPolData$nearestPointsX[j], 
                             y_np=nearbyPolData$nearestPointsY[j], bisectVect=gsGeo, 
                             priority=nearbyPolData$priority[j], doubleProp=0.4, dist=nearbyPolData$distFromPoly[j], 
                             maxD=5, width=dataset$gsWidth[i], onlyLongest=onlyLong, full=full, sidePref=0, 
                             yside=0, xside=0, onlyOuter = onlyOuter))
        
        if(inherits(geoNV, "try-error"))
        {
          #error handling code, maybe just skip this iteration using
          next
        }else
        {
          geoN <- st_as_sf(geoNV)
        }

                # geoN <- geoN$geometry
        if (length(geoN$geometry) == 0)
        {
          next
        }
        
        # for (l in 1:length(geoN$geometry))
        # {
          dsgList[[length(dsgList)+1]] <- nearbyPolData$design[j]
          priorList[[length(priorList)+1]] <- nearbyPolData$priority[j]
          gsList[length(gsList)+1] <- geoN$geometry#[l]
          gsScoreList[[length(gsScoreList)+1]] <- min(nearbyPolData$scores[j] * 
                                                        score_scale,100)
          illegalL[[length(illegalL)+1]] <- dataset$illegal_gs[i]
        # }
        
      }
      # use distance as a way to generate radius or sidelength + use it to define how deep into poly we can go
      # in case there is a need to only work on the edges
      # generate polygon based on this determination
      
      #overlay greenspace polygon while making sure only exist within the geometry of the polygon of interest
      
      #compute total coverage, if exceeds then remove the one with lowest priority -> continue until percent coverage
      #is reached
    }
  }
  greenspace_sf <- st_sf(design=unlist(dsgList),priority=unlist(priorList),
                         scores=unlist(gsScoreList), illegal_gs=unlist(illegalL),geometry=gsList)
  
  return(greenspace_sf)
}

rastVect <- function(vectIn, res=0.125)
{
  nams <- names(vectIn)
  # print("rast")
  tempRast<-rast(vectIn, res=res)
  
  
  print("Rasterizing")
  allrast <- lapply(nams, function(x) {
    print(c("Starting ",x))
    rasterize(vectIn, tempRast,
              field = x,
              touches = TRUE
    )
    # print(c("Finished ",x))
  })
  allrast <- do.call("c", allrast)
  names(allrast) <- nams
  return(allrast)
}

# genClassList <- list("Tree","Sewer","Private Greenspace Percentage",
#                      "Public Greenspace Percentage","Greenspace",
#                      "Medium Voltage Line", "Open water","Waterline","Road", 
#                      "Parking","Building")
# genWidthList <- list(5, 5, 5, 10, 5, 5, 5, 5, 2.5, 10, 10)
# names(genWidthList) <- genClassList
# 
# greenSpaceGeneratorOut <- list()

# for (i in 1:length(sf_gs_genList))
# {
#   greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(sf_gs_genList[[i]], genWidthList[[i]])
# }

# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(trees_sf, genWidthList[[1]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(sewers_system_sf, genWidthList[[2]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(green_percent_private_sf, genWidthList[[3]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(green_percent_public_sf, genWidthList[[4]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(green_areas_sf, genWidthList[[5]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(mv_lines_sf, genWidthList[[6]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(openWater_sf, genWidthList[[7]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(waterlines_sf, genWidthList[[8]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(roads_sf, genWidthList[[9]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(osmParking_sf, genWidthList[[10]])
# greenSpaceGeneratorOut[[length(greenSpaceGeneratorOut)+1]] <- conversionGS(buildings_sf, genWidthList[[11]])


tstGS <- conversionGS(sf_gs_gen, 5)
tstGS <- st_set_crs(tstGS, st_crs(green_areas))


green_percent_private_V_masked <- mask(green_percent_private_V, osmBuildingMaskV)#osmRoadsBufferV)
# green_percent_private_V_masked <- mask(green_percent_private_V_masked, osmBuildingMaskV)
green_pct_private_masked_sf <- st_as_sf(green_percent_private_V_masked)
green_pct_private_masked_sf <- st_set_crs(green_pct_private_masked_sf, st_crs(green_areas))

green_percent_public_V_masked <- mask(green_percent_public_V, osmBuildingMaskV)#osmRoadsBufferV)
# green_percent_private_V_masked <- mask(green_percent_private_V_masked, osmBuildingMaskV)
green_pct_public_masked_sf <- st_as_sf(green_percent_public_V_masked)
green_pct_public_masked_sf <- st_set_crs(green_pct_public_masked_sf, st_crs(green_areas))


plot(green_percent_private_V_masked, col="green")


roadAdditionsV <- osmRoadsMaskV[osmRoadsMaskV$design != "None"]
roadAdditions_sf <- st_as_sf(roadAdditionsV)
roadAdditions_sf <- st_set_crs(roadAdditions_sf, st_crs(green_areas))

roadMaskV <- osmRoadsMaskV[osmRoadsMaskV$design == "None"]
roadMask_sf <- st_as_sf(roadMaskV)
roadMask_sf <- st_set_crs(roadMask_sf, st_crs(green_areas))


my_dir = "C:\\Users\\alek-\\Documents\\Wageningen Period 5\\Planning and Design of Urban Spaces\\Project Data\\V5" 
setwd(my_dir)

tstGS_mod <- tstGS
tstGS_mod <- tstGS_mod %>% add_row(design=green_pct_private_masked_sf$design,
                      priority=green_pct_private_masked_sf$priority,
                      scores=green_pct_private_masked_sf$scores,
                      illegal_gs=green_pct_private_masked_sf$illegal_gs,
                      geometry=green_pct_private_masked_sf$geometry)

tstGS_mod <- tstGS_mod %>% add_row(design=green_pct_public_masked_sf$design,
                      priority=green_pct_public_masked_sf$priority,
                      scores=green_pct_public_masked_sf$scores,
                      illegal_gs=green_pct_public_masked_sf$illegal_gs,
                      geometry=green_pct_public_masked_sf$geometry)

# tstGS_mod <- tstGS_mod %>% add_row(design=roadAdditions_sf$design,
#                       priority=roadAdditions_sf$priority,
#                       scores=roadAdditions_sf$scores,
#                       illegal_gs=roadAdditions_sf$illegal_gs,
#                       geometry=roadAdditions_sf$geometry)


tstGS_modV <- vect(tstGS_mod)
crs(tstGS_modV) <- convCRS


# 
# tstGS_modV_waterBodiesMask <- mask(tstGS_modV, openWater_V, inverse=TRUE)
# crs(tstGS_modV_waterBodiesMask) <- convCRS

gsGenAll <- tstGS_modV

gsGenAllSorted <- gsGenAll %>% arrange(desc(priority))#sort(gsGenAll, priority, decreasing=TRUE)
gsGenAllRast <- rastVect(gsGenAllSorted)


roadMaskRast <- rasterize(roadMaskV, gsGenAllRast, getCover=TRUE)
roadMaskRast[roadMaskRast == 0] <- NA
gsGenAllRast <- mask(gsGenAllRast, roadMaskRast, inverse=TRUE)
# gsGenAllRast2 <- rastVect(tstGS_modV2)

gsGenAllRastNoneMasked <- gsGenAllRast
gsGenAllRastNoneMasked[gsGenAllRastNoneMasked$design == "None"] <- NA
gsGenAllRastMask <- mask(gsGenAllRast, gsGenAllRastNoneMasked)#, inverse=TRUE)


writeVector(gsGenAllSorted,"Greenspace_generation_all_v5.shp", overwrite=TRUE)
writeVector(gsGenAllSorted,"Greenspace_generation_all_v5.gpkg", overwrite=TRUE)
writeRaster(gsGenAllRastMask, "Greenspace_generation_all_v5.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')


# gsGenAllWaterMasked <- tstGS_modV_waterBodiesMask
# gsGenAllWaterMaskedRast <- rastVect(tstGS_modV_waterBodiesMask)
# writeVector(gsGenAllWaterMasked,"Greenspace_generation_all_waterBodiesMaske_v3.shp", overwrite=TRUE)
# writeVector(gsGenAllWaterMasked,"Greenspace_generation_all_waterBodiesMaske_v3.gpkg", overwrite=TRUE)
# writeRaster(gsGenAllWaterMaskedRast, "Greenspace_generation_all_waterBodiesMaske_v3.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')


gsGenPrivate <- mask(gsGenAllSorted,green_percent_private_V)
gsGenPrivateRast <- rastVect(gsGenPrivate)

gsGenPrivateRastNoneMasked <- gsGenPrivateRast
gsGenPrivateRastNoneMasked[gsGenPrivateRastNoneMasked$design == "None"] <- NA
gsGenPrivateRastMask <- mask(gsGenPrivateRast, gsGenPrivateRastNoneMasked)#, inverse=TRUE)

writeVector(gsGenPrivate,"Greenspace_generation_private_v5.shp", overwrite=TRUE)
writeVector(gsGenPrivate,"Greenspace_generation_private_v5.gpkg", overwrite=TRUE)
writeRaster(gsGenPrivateRastMask, "Greenspace_generation_private_v5.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')

gsGenPublic <- mask(gsGenAllSorted,green_percent_public_V)
gsGenPublicRast <- rastVect(gsGenPublic)

gsGenPublicRastNoneMasked <- gsGenPublicRast
gsGenPublicRastNoneMasked[gsGenPublicRastNoneMasked$design == "None"] <- NA
gsGenPublicRastMask <- mask(gsGenPublicRast, gsGenPublicRastNoneMasked)#, inverse=TRUE)

writeVector(gsGenPublic,"Greenspace_generation_public_v5.shp", overwrite=TRUE)
writeVector(gsGenPublic,"Greenspace_generation_public_v5.gpkg", overwrite=TRUE)
writeRaster(gsGenPublicRastMask, "Greenspace_generation_public_v5.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')

# gsGenAllMerge <- merge(gsGenPrivate, gsGenPublic)
# gsGenAllMergeRast <- rastVect(gsGenAllMerge)

### After this bit doesn't run properly anymore
# tstGS_w_old_GS <- gsGenAllSorted
# tstGS_w_old_GS<- tstGS_w_old_GS %>% add_row(design=green_areas_sf$design,
#                            priority=green_areas_sf$priority,
#                            scores=green_areas_sf$scores,
#                            illegal_gs=green_areas_sf$illegal_gs,
#                            geometry=green_areas_sf$geometry)
# 
# tstGS_mod_w_oldV <- vect(tstGS_w_old_GS)
# crs(tstGS_mod_w_oldV) <- convCRS
# 
# # tstGS_mod_w_old_waterBodiesMask <- mask(tstGS_modV, openWater_V)
# # crs(tstGS_mod_w_old_waterBodiesMask) <- convCRS
# 
# gsGenAndOldAll <- tstGS_mod_w_oldV
# gsGenAndOlfAllSorted <- gsGenAndOldAll %>% arrange(desc(priority))#sort(gsGenAll, priority, decreasing=TRUE)
# gsGenAndOldAllRast <- rastVect(gsGenAndOlfAllSorted)
# gsGenAndOldAllRastNoneMasked <- gsGenAndOldAllRast
# gsGenAndOldAllRastNoneMasked[gsGenAndOldAllRastNoneMasked$design == "None"] <- NA
# gsGenAndOldAllRastMask <- mask(gsGenAndOldAllRast, gsGenAndOldAllRastNoneMasked)#, inverse=TRUE)
# writeVector(gsGenAndOlfAllSorted,"Greenspace_generation_and_old_all_v5.shp", overwrite=TRUE)
# writeVector(gsGenAndOlfAllSorted,"Greenspace_generation_and_old_all_v5.gpkg", overwrite=TRUE)
# writeRaster(gsGenAndOldAllRastMask, "Greenspace_generation_and_old_all_v5.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')
# 
# 
# 
# # gsGenAndOldAllWaterMasked <- tstGS_mod_w_old_waterBodiesMask
# # gsGenAndOldAllWaterMaskedRast <- rastVect(tstGS_mod_w_old_waterBodiesMask)
# # writeVector(gsGenAndOldAllWaterMasked,"Greenspace_generation_and_old_all_waterBodiesMaske_v3.shp", overwrite=TRUE)
# # writeVector(gsGenAndOldAllWaterMasked,"Greenspace_generation_and_old_all_waterBodiesMaske_v3.gpkg", overwrite=TRUE)
# # writeRaster(gsGenAndOldAllWaterMaskedRast, "Greenspace_generation_and_old_all_waterBodiesMaske_v3.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')
# # 
# 
# gsGenAndOldPrivate <- mask(gsGenAndOlfAllSorted,green_percent_private_V)
# gsGenAndOldPrivateRast <- rastVect(gsGenAndOldPrivate)
# gsGenAndOldPrivateRastNoneMasked <- gsGenAndOldPrivateRast
# gsGenAndOldPrivateRastNoneMasked[gsGenAndOldPrivateRastNoneMasked$design == "None"] <- NA
# ggsGenAndOldPrivateRastMask <- mask(gsGenAndOldPrivateRast, gsGenAndOldPrivateRastNoneMasked)#, inverse=TRUE)writeVector(gsGenAndOldPrivate,"Greenspace_generation_and_old_private_v4.shp", overwrite=TRUE)
# 
# writeVector(gsGenAndOldPrivate,"Greenspace_generation_and_old_private_v5.shp", overwrite=TRUE)
# writeVector(gsGenAndOldPrivate,"Greenspace_generation_and_old_private_v5.gpkg", overwrite=TRUE)
# writeRaster(ggsGenAndOldPrivateRastMask, "Greenspace_generation_and_old_private_v5.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')
# 
# gsGenAndOldPublic <- mask(gsGenAndOlfAllSorted,green_percent_public_V)
# gsGenAndOldPublicRast <- rastVect(gsGenAndOldPublic)
# gsGenAndOldPublicRastNoneMasked <- gsGenAndOldPublicRast
# gsGenAndOldPublicRastNoneMasked[gsGenAndOldPublicRastNoneMasked$design == "None"] <- NA
# gsGenAndOldPublicRastMask <- mask(gsGenAndOldPublicRast, gsGenAndOldPublicRastNoneMasked)
# 
# writeVector(gsGenAndOldPublic,"Greenspace_generation_and_old_public_v5.shp", overwrite=TRUE)
# writeVector(gsGenAndOldPublic,"Greenspace_generation_and_old_public_v5.gpkg", overwrite=TRUE)
# writeRaster(gsGenAndOldPublicRastMask, "Greenspace_generation_and_old_public_v5.tif", overwrite=TRUE, gdal=c("COMPRESS=NONE", "TFW=YES"), datatype='INT1U')
# 

# tsGSV <- vect(tstGS)
# crs(tsGSV) <- convCRS
# 
# gsGeneratedRast <- rastVect(tsGSV)
# crs(gsGeneratedRast)<- convCRS
# gsPrivAreaRast <- rastVect(green_percent_private_V)
# crs(gsPrivAreaRast)<- convCRS
# gsPrivAreaRast <- resample(gsPrivAreaRast,gsGeneratedRast)
# gsPubAreaRast <- rastVect(green_percent_public_V)
# crs(gsPubAreaRast)<- convCRS
# gsPubAreaRast <- resample(gsPubAreaRast,gsGeneratedRast)
# osmBuildingMaskRast <- rastVect(osmBuildingMaskV)
# crs(osmBuildingMaskRast)<- convCRS
# osmBuildingMaskRast <- resample(osmBuildingMaskRast,gsGeneratedRast)
# osmRoadsMaskRast <- rastVect(osmRoadsMaskV)
# crs(osmRoadsMaskRast)<- convCRS
# osmRoadsMaskRast <- resample(osmRoadsMaskRast,gsGeneratedRast)
# # sewersMaskRast <- rastVect(sewers_systemMaskV)
# # crs(sewersMaskRast)<- convCRS
# # sewersMaskRast <- resample(sewersMaskRast,gsGeneratedRast)
# # MVLinesMaskRast <- rastVect(mv_linesMaskV)
# # crs(MVLinesMaskRast)<- convCRS
# # MVLinesMaskRast <- resample(MVLinesMaskRast,gsGeneratedRast)
# # waterlinesMaskRast <- rastVect(waterlinesMaskV)
# # crs(waterlinesMaskRast)<- convCRS
# # waterlinesMaskRast <- resample(waterlinesMaskRast,gsGeneratedRast)
# 
# # gsGeneratedRastMasked <- cover(gsGeneratedRast, sewersMaskRast, identity=TRUE)#, maskvalues="Shallow Root Vegetation")
# # gsGeneratedRastMasked2 <- mask(gsGeneratedRastMasked, MVLinesMaskRast, maskvalues=TRUE)
# # gsGeneratedRastMasked3 <- mask(gsGeneratedRastMasked2, waterlinesMaskRast, maskvalues=TRUE)
# # outRast$scores <- mask(outRast$scores, outR$illegal_gs,
# #                        maskvalues=TRUE, updatevalue=-100)
# gsGeneratedRastRoads <- gsGeneratedRast
# gsGeneratedRastRoads <- mask(gsGeneratedRast, osmRoadsBufferV, inverse=TRUE)
# gsGeneratedRastRoads2 <- mask(tsGSV, osmRoadsMaskV, inverse=TRUE)
# plot(gsGeneratedRastRoads2, col="green")
# gsGeneratedVRoads <- tsGSV
# 
# m <- osmRoadsMaskRast[osmRoadsMaskRast$design == "Permeable pavement"]
# 
# gsGeneratedVRoads <- mask(tsGSV, m)#, maskvalue=TRUE)#, inverse=TRUE)
# gsGeneratedVRoads2 <- mask(tsGSV, osmRoadsBufferV)
# 
# gsGeneratedRastRoadsInverse <- mask(osmRoadsBufferV, gsGeneratedRast)
# 
# # gsGeneratedRastRoadsTest <- merge(tsGSV, osmRoadsMask_sf, first=FALSE)
# # gsGeneratedRastRoads$design<- mask(gsGeneratedRast$design, 
# #                                    osmRoadsMaskRast$scores, maskvalues=50,
# #                                    updatevalue="Permeable pavement")
# # gsGeneratedRastRoads$design<- mask(gsGeneratedRast$design, 
# #                                    osmRoadsMaskRast$scores, maskvalues=5,
# #                                    updatevalue="None")
# 
# #, maskvalues=TRUE, updatevalue=NA)
# gsGeneratedRastMaskedPrivate <- mask(gsGeneratedRastMaskedRoads, gsPrivAreaRast, maskvalues=TRUE)
# gsGeneratedRastMaskedPublic <- mask(gsGeneratedRastMaskedRoads, gsPubAreaRast, maskvalues=TRUE)
# 
# # sewers_systemMaskV <- sewers_systemMaskV[!is.na(sewers_systemMaskV)] 
# sewerAndGenerateRastCollection <- sprc(gsGeneratedRast,sewersMaskRast)
# gsGeneratedRastMasked1 <- merge(sewerAndGenerateRastCollection)
# gsGeneratedRastMasked12 <- mask(gsGeneratedRastMasked1, MVLinesMaskRast)
# gsGeneratedRastMasked13 <- mask(gsGeneratedRastMasked12, waterlinesMaskRast)
# gsGeneratedRastMaskedRoads1 <- mask(gsGeneratedRastMasked13, osmRoadsMaskRast)
# gsGeneratedRastMaskedPrivate2 <- mask(gsGeneratedRastMaskedRoads1, gsPrivAreaRast)
# gsGeneratedRastMaskedPublic2 <- mask(gsGeneratedRastMaskedRoads1, gsPubAreaRast)
# 
# gsGeneratedRastMasked2 <- mask(gsGeneratedRast, sewersMaskRast, maskvalues=TRUE, updatevalue=NA)
# gsGeneratedRastMasked22 <- mask(gsGeneratedRastMasked2, MVLinesMaskRast, maskvalues=TRUE, updatevalue=NA)
# gsGeneratedRastMasked23 <- mask(gsGeneratedRastMasked22, waterlinesMaskRast, maskvalues=TRUE, updatevalue=NA)
# gsGeneratedRastMaskedRoads2 <- mask(gsGeneratedRastMasked23, osmRoadsMaskRast, maskvalues=TRUE, updatevalue=NA)
# gsGeneratedRastMaskedPrivate3 <- mask(gsGeneratedRastMaskedRoads2, gsPrivAreaRast,maskvalues=TRUE, updatevalue=NA)
# gsGeneratedRastMaskedPublic3 <- mask(gsGeneratedRastMaskedRoads2, gsPubAreaRast,maskvalues=TRUE, updatevalue=NA)
# 
# 
# #TODO Figure out what to do with Trees, plants, and/or shrubs
# gsPrivMaskedV <- mask(green_percent_private_V, osmBuildingMaskV)
# gsPubMaskedV <- mask(green_percent_public_V, osmBuildingMaskV)
# gsPrivMaskedV <- mask(gsPrivMaskedV, osmRoadsMaskV)
# gsPubMaskedV <- mask(gsPubMaskedV, osmRoadsMaskV)
# gsPrivMaskedV <- mask(gsPrivMaskedV, sewers_systemMaskV)
# gsPubMaskedV <- mask(gsPubMaskedV, sewers_systemMaskV)
# gsPrivMaskedV <- mask(gsPrivMaskedV, mv_linesMaskV)
# gsPubMaskedV <- mask(gsPubMaskedV, mv_linesMaskV)
# gsPrivMaskedV <- mask(gsPrivMaskedV, waterlinesMaskV)
# gsPubMaskedV <- mask(gsPubMaskedV, waterlinesMaskV)
# crs(gsPrivMaskedV) <- convCRS
# crs(gsPubMaskedV) <- convCRS
# 
# 
# # 
# sewers_systemPrivMaskedV <- mask(sewers_systemMaskV, green_percent_private_V)
# sewers_systemPubMaskedV <- mask(sewers_systemMaskV, green_percent_public_V)
# mv_linesPrivMaskedV <- mask(mv_linesMaskV, green_percent_private_V)
# mv_linesPubMaskedV <- mask(mv_linesMaskV, green_percent_public_V)
# waterlinesPrivMaskedV <- mask(waterlinesMaskV, green_percent_private_V)
# waterlinesPubMaskedV <- mask(waterlinesMaskV, green_percent_public_V)
# 
# 
# 
# tsGSVPriv <-  mask(tsGSV, gsPrivMaskedV)
# crs(tsGSVPriv) <- convCRS
# tstGS_Priv_sf <- st_as_sf(tsGSVPriv)
# tstGS_Priv_sf <- st_set_crs(tstGS_Priv_sf, st_crs(green_areas))
# gsPrivMaskedV <- mask(gsPrivMaskedV, tsGSVPriv)
# crs(gsPrivMaskedV) <- convCRS
# 
# tsGSVPub <-  mask(tsGSV, gsPubMaskedV)
# crs(tsGSVPub) <- convCRS
# tsGS_Pub_sf <- st_as_sf(tsGSVPub)
# tsGS_Pub_sf <- st_set_crs(tsGS_Pub_sf, st_crs(green_areas))
# gsPubMaskedV <- mask(gsPubMaskedV, tsGSVPub)
# crs(gsPubMaskedV) <- convCRS
# # maxGreen <- mask(tsGSV,osmBuilding_V, inverse=TRUE)
# # plot(maxGreen, col="green")
# gsPrivMasked_sf <- st_as_sf(gsPrivMaskedV)
# gsPrivMasked_sf <- st_set_crs(gsPrivMasked_sf, st_crs(green_areas))
# gsPubMasked_sf <- st_as_sf(gsPubMaskedV)
# gsPubMasked_sf <- st_set_crs(gsPubMasked_sf, st_crs(green_areas))
# 
# # gsDesignPct <- list(green_percent_private_sf$design, green_percent_public_sf$design)
# # gsPriorityPct <- list(green_percent_private_sf$priority, green_percent_public_sf$priority)
# # gsScoresPct <- list(green_percent_private_sf$scores, green_percent_public_sf$scores)
# # gsGeoPct <- c(green_percent_private_sf$geometry, green_percent_public_sf$geometry)
# # totalGSPct <- st_sf(design=unlist(gsDesignPct), priority=unlist(gsPriorityPct), 
# #                  scores=unlist(gsScoresPct), geometry=gsGeoPct)
# totalGSPriv <- st_sf(design=gsPrivMasked_sf$design,
#                      priority=gsPrivMasked_sf$priority, 
#                      scores=gsPrivMasked_sf$scores,
#                      illegal_gs=gsPrivMasked_sf$illegal_gs,
#                      geometry=gsPrivMasked_sf$geometry)
# totalGSPub <- st_sf(design=gsPubMasked_sf$design,
#                      priority=gsPubMasked_sf$priority, 
#                      scores=gsPubMasked_sf$scores,
#                      illegal_gs=gsPubMasked_sf$illegal_gs,
#                      geometry=gsPubMasked_sf$geometry)
# 
# # totalGSPct <- st_set_crs(totalGSPct, st_crs(green_areas))
# # totalGSPriv <- st_set_crs(totalGSPriv, st_crs(green_areas))
# # totalGSPub <- st_set_crs(totalGSPub, st_crs(green_areas))
# 
# totalGSPrivV <- vect(totalGSPriv)
# crs(totalGSPrivV) <- convCRS
# totalGSPubV <- vect(totalGSPub)
# crs(totalGSPubV) <- convCRS
# 
# totalGSPrivMaskedV <- mask(tsGS_Priv_sf, sewers_systemMaskV)
# totalGSPubMaskedV <- mask(tsGS_Pub_sf, sewers_systemMaskV)
# totalGSPrivMaskedV <- mask(totalGSPrivMaskedV, waterlinesMaskV)
# totalGSPubMaskedV <- mask(totalGSPubMaskedV, waterlinesMaskV)
# totalGSPrivMaskedV <- mask(totalGSPrivMaskedV, mv_linesMaskV)
# totalGSPubMaskedV <- mask(totalGSPubMaskedV, mv_linesMaskV)
# crs(totalGSPrivMaskedV) <- convCRS
# crs(totalGSPubMaskedV) <- convCRS
# 
# tstGS_PrivMasked_sf <- st_as_sf(totalGSPrivMaskedV)
# tstGS_PrivMasked_sf <- st_set_crs(tstGS_PrivMasked_sf, st_crs(green_areas))
# tstGS_PubMasked_sf <- st_as_sf(totalGSPubMaskedV)
# tstGS_PubMasked_sf <- st_set_crs(tstGS_PubMasked_sf, st_crs(green_areas))
# # totalGSPct <- st_difference(totalGSPct, osmBuilding_sf)
# # totalGSPct <- st_difference(totalGSPct, roads_sf) # too comp intensive <- find vector equivalent
# 
# totalPrivateGS <- bind_rows(list(tstGS_PrivMasked_sf,totalGSPriv,
#                                  sewers_systemPrivMaskedV,mv_linesPrivMaskedV,
#                                  waterlinesPrivMaskedV))
# totalPublicGS <- bind_rows(list(tstGS_PubMasked_sf,totalGSPub,
#                                 sewers_systemPubMaskedV,mv_linesPubMaskedV,
#                                 waterlinesPubMaskedV))
# 
# totalPrivateV <- vect(totalPrivateGS)
# totalPublicV <- vect(totalPublicGS)
# crs(totalPrivateV) <- convCRS
# crs(totalPublicV) <- convCRS
# 
# 
# gsPrivV <- mask(totalPrivateV, osmRoadsMaskV)
# gsPubV <- mask(totalPublicV, osmRoadsMaskV)
# crs(gsPrivV) <- convCRS
# crs(gsPubV) <- convCRS
# 
# 
findOverlapDesign <- function(gsVect)
{
  # desList <- list()
  # priorList <- list()
  # scoresList <- list()
  # illegalList <- list()
  # geomList <- list()
  overLOut_sf<-NULL
  for (i in 1:length(gsVect))
  {
    print("Polygon:")
    print(i)
    print("Polygons left:")
    print(length(gsVect) - i)
    if (i == length(gsVect))
    {
      next
    } 
    overlap_sf <- compareOverlaps(gsVect[i],gsVect[i+1:length(gsVect)])
    if (is.null(overLOut_sf))
    {
      overLOut_sf <- overlap_sf
    }else
    {
      # print("Bind rows Top Level")
      # print(overlap_sf)
      # outList <- list(outList, overlap_sf)
      overLOut_sf %>% add_row(ovLpOut_sf)
    }

  }
  #   overlap_sf <- st_as_sf(overlapV)
  #   overlap_sf <- st_set_crs(overlap_sf, st_crs(green_areas))
  #   
  #   if (length(overlapV) != 0)
  #   {
  #     # print(length(overlap_sf))
  #     for (j in 1:length(overlapV))
  #     {
  #       print("Poly:")
  #       print(i)
  #       print("Num Poly Left:")
  #       print(length(gsVect) - i)
  #       print("Compare:")
  #       print(j)
  #       print("Num compare left:")
  #       print(length(overlapV) - j)
  #       score1 <- overlap_sf$priority[j]*overlap_sf$scores[j]
  #       score2 <- overlap_sf$priority.1[j]*overlap_sf$scores.1[j]
  #       # print(score1)
  #       # print(score2)
  #       # print(overlap_sf$illegal_gs[j])
  #       # print(overlap_sf$illegal_gs.1[j])
  #       legalPresentFlag1 <- if (is.na(overlap_sf$illegal_gs[j])) FALSE else overlap_sf$illegal_gs[j]
  #       legalPresentFlag2 <- if (is.na(overlap_sf$illegal_gs.1[j])) FALSE else overlap_sf$illegal_gs.1[j]
  #       
  #       legal1Flag <- legalPresentFlag1 & !legalPresentFlag2
  #       legal2Flag <- !legalPresentFlag1 & legalPresentFlag2
  #       
  #       if (is.na(legal1Flag))
  #       {
  #         legal1Flag <- FALSE
  #       }
  #       
  #       if (is.na(legal2Flag))
  #       {
  #         legal2Flag <- FALSE
  #       }
  #       
  #       if ((score1 >= score2) | legal1Flag)
  #       {
  #         desList[[length(desList)+1]] <- overlap_sf$design[j]
  #         priorList[[length(priorList)+1]] <- overlap_sf$priority[j]
  #         scoresList[[length(scoresList)+1]] <- overlap_sf$scores[j]
  #         illegalList[[length(illegalList)+1]] <- overlap_sf$illegal_gs[j]
  #       }else if ((score1 < score2) | legal2Flag)
  #       {
  #         desList[[length(desList)+1]] <- overlap_sf$design.1[j]
  #         priorList[[length(priorList)+1]] <- overlap_sf$priority.1[j]
  #         scoresList[[length(scoresList)+1]] <- overlap_sf$scores.1[j]
  #         illegalList[[length(illegalList)+1]] <- overlap_sf$illegal_gs.1[j]
  #       }
  #       geomList[length(geomList)+1] <- overlap_sf$geometry[j] 
  #     }
  #   } else
  #   {
  #     noOverlap_sf <- st_as_sf(gsVect)
  #     noOverlap_sf <- st_set_crs(noOverlap_sf, st_crs(green_areas))
  #     desList[[length(desList)+1]] <- noOverlap_sf$design[i]
  #     priorList[[length(priorList)+1]] <- noOverlap_sf$priority[i]
  #     scoresList[[length(scoresList)+1]] <- noOverlap_sf$scores[i]
  #     illegalList[[length(illegalList)+1]] <- noOverlap_sf$illegal_gs[i]
  #     geomList[length(geomList)+1] <- noOverlap_sf$geometry[i] 
  #     next
  #   }
  # }
  # 
  # overLapOut_sf <- st_sf(design=unlist(desList),
  #                        priority=unlist(priorList), 
  #                        scores=unlist(scoresList),
  #                        illegal_gs=unlist(illegalList),
  #                        geometry=geomList)
  return(overLapOut_sf)
}

compareOverlaps <- function (gsVectIn, gsVectCompare)
{
    overLapOut_sf <- NULL
    # noIntersect <- FALSE
    overlapV <- intersect(gsVectIn,gsVectCompare)
    
    overlap_sf <- st_as_sf(overlapV)
    overlap_sf <- st_set_crs(overlap_sf, st_crs(green_areas))
    
    if (length(overlapV) != 0)
    {
      for (j in 1:length(overlapV))
      {
        if (j == length(overlapV))
        {
          next
        }
        
        score1 <- overlap_sf$priority[j]*overlap_sf$scores[j]
        score2 <- overlap_sf$priority.1[j]*overlap_sf$scores.1[j]

        legalPresentFlag1 <- if (is.na(overlap_sf$illegal_gs[j])) FALSE else overlap_sf$illegal_gs[j]
        legalPresentFlag2 <- if (is.na(overlap_sf$illegal_gs.1[j])) FALSE else overlap_sf$illegal_gs.1[j]
        
        legal1Flag <- legalPresentFlag1 & !legalPresentFlag2
        legal2Flag <- !legalPresentFlag1 & legalPresentFlag2
        
        if (is.na(legal1Flag))
        {
          legal1Flag <- FALSE
        }
        
        if (is.na(legal2Flag))
        {
          legal2Flag <- FALSE
        }
        
        if ((score1 >= score2) | legal1Flag)
        {
          des <- overlap_sf$design[j]
          prior <- overlap_sf$priority[j]
          scores <- overlap_sf$scores[j]
          illegal <- overlap_sf$illegal_gs[j]
        }else if ((score1 < score2) | legal2Flag)
        {
          des <- overlap_sf$design.1[j]
          prior <- overlap_sf$priority.1[j]
          scores <- overlap_sf$scores.1[j]
          illegal <- overlap_sf$illegal_gs.1[j]
        }
        geoms <- overlap_sf$geometry[j] 
        
        overLapOut_sf <- st_sf(design=des,
                               priority=prior, 
                               scores=scores,
                               illegal_gs=illegal,
                               geometry=geoms)
        
        overLapOutV <- vect(overLapOut_sf)
        crs(overLapOutV) <- convCRS
        
        ovLpOut_sf <- compareOverlaps(overLapOutV, 
                                      overlapV[j+1:length(overlapV)])
        # print("Overlap out:")
        # print(ovLpOut_sf)
        if (is.null(overLapOut_sf))
        {
          overLapOut_sf <- ovLpOut_sf
        }else
        {
          print("Bind rows Internal")
          overLapOut_sf %>% add_row(ovLpOut_sf)
        }
      }
    } else
    {
      print("End condition")
      noOverlap_sf <- st_as_sf(gsVectIn)
      noOverlap_sf <- st_set_crs(noOverlap_sf, st_crs(green_areas))
      overLapOut_sf <- st_sf(design=noOverlap_sf$design,
                             priority=noOverlap_sf$priority, 
                             scores=noOverlap_sf$scores,
                             illegal_gs=noOverlap_sf$illegal_gs,
                             geometry=noOverlap_sf$geometry)
      overLapOut_sf <- st_set_crs(overLapOut_sf, st_crs(green_areas))
      
      # overLapOutV <- vect(overLapOut_sf)
      # crs(overLapOutV) <- convCRS
      
      return(overLapOut_sf)
    }
 
    # overLapOut_sf <- outList
    return(overLapOut_sf)
  

}



overlapPrivate<- st_intersection(totalPrivateGS) 
nonOverlapPrivate <- overlapPrivate %>% filter(n.overlaps == 1)

overlapPublic<- st_intersection(totalPublicGS) 
nonOverlapPublic <- overlapPublic %>% filter(n.overlaps == 1)

# gsRainIntensity100 <- intersect(totalGS, precip_100Yr)
# gsRainIntensity1000 <- intersect(totalGS, precip_1000Yr)



plot(totalGS$geometry, col="green")

# for (i in length(green_percent_private_sf))
# {
#   tstGS$design
#   tstGS$priority
#   tstGS$scores
#   tstGS$geometry[[length(tstGS$geometry)]]
# }
# for (i in length(green_percent_public_sf))
# {
#   ts
# }


# Finding objects with acceptable differences
findDST <- function(dst, maxD, minD)
{
  returnList <- list()
  dst_T<- list(dst)
  i=0
  dist <- list()
  idx_list <- list()
  for (d in dst_T[[1]])
  {
    i=i+1
    if((d < maxD) & (d > minD))
    {
      dist[[length(dist)+1]]<-d
      idx_list[[length(idx_list)+1]]<-i
    }
  }
  returnList[1] <- unlist(dist)
  returnList[2] <- unlist(idx_list)
  return(returnList)
}


# privateGS <- mask(raster_list$`study area`, privateBLRast)
publicGS1 <- mask(BLRast, publicBLRast)
privateGS1 <- mask(BLRast, publicBLRast, inverse=T)

maskT <- resample(BLRast, GSArea)
maskPub <- resamble(publicGS1, GSArea)
maskPrv <- resample(privateGS1, GSArea)
totalGS <- mask(GSArea, maskT, inverse=T)
publicGS <- mask(GSArea, maskPub, inverse=T)
privateGS <- mask(GSArea, maskPrv)

findCorridor <- function(oldGreenSpace_sf, newGreenSpace_sf, maxD=5, minD=0, pct)
{
  # finding index of obj in sf_obj_def nearest each item in the sf_obj
  # nr_ft <- st_nearest_feature(sf_obj, sf_obj_def)
  # nr_ft_alt <- st_nearest_feature(sf_obj_def, sf_obj)
  
  nearestFeatures <- st_nearest_feature(oldGreenSpace_sf, newGreenSpace_sf)
  nearestPoints <- st_nearest_points(oldGreenSpace_sf, newGreenSpace_sf)
  

  
  
  # Creating another column for idx of other obj that is nearest to said obj
  sfT_obj <- sf_obj
  sfT_obj$nearest <- nr_ft
  sf_defT_obj <- sf_obj_def
  sf_defT_obj$nearest <- nr_ft_alt
  
  # creating sf object based on these components i.e. closest obj per obj idx
  sf_nr <- sf_defT_obj %>% slice(nr_ft) #poly <- sf_obj_def %>% slice(nr_ft)
  sf_nr_def <- sfT_obj %>% slice(nr_ft_alt) #poly_def <- sf_obj %>% slice(nr_ft_alt)
  
  dst <- st_distance(sfT_obj, sf_nr, by_element = TRUE)
  dst_alt <- st_distance(sf_defT_obj, sf_nr_def, by_element = TRUE)
  
  dst_objL <- findDST(dst, maxD, minD)
  dstL <- dst_objL[1]
  idxL <- dst_objL[2]
  
  dst_def_objL <- findDST(dst_alt, maxD, minD)
  dst_defL <- dst_def_objL[1]
  idx_defL <- dst_def_objL[2]
  
  comp_Main <- sf_nr %>% slice(idx)
  comp_def <- sf_nr_def %>% slice(idx_defL)
  
}


greenSpaceAreaScore <- function(rast, design="General", tree=FALSE)
{
  tempRast <- rast
  tempRast$scores <- subst(outR$scores, NA, -999)

}



rastObj <- function(sf_obj, centerWeight, res=0.1, moveWindow=TRUE, rmNA=TRUE, resample=TRUE) {
  
  # print(names(sf_obj))
  print("Initializing raster function:")
  krnl <- c(1,1,1,1,centerWeight,1,1,1,1)
  kernel <- matrix(krnl, nrow=3)/sum(krnl)
  print("vect")
  tempVect<-vect(sf_obj)
  print("names")
  nams <- names(tempVect)
  print("rast")
  tempRast<-rast(tempVect, res=res)
  

  print("Rasterizing")
  allrast <- lapply(nams, function(x) {
    # print(c("Starting ",x))
    rasterize(tempVect, tempRast,
              field = x,
              touches = TRUE
    )
    # print(c("Finished ",x))
  })
  allrast <- do.call("c", allrast)
  names(allrast) <- nams
  
  
  # }

  # print(allrast)
  # cat(outR)
  
  if (moveWindow)
  {
    # Merge (bind) all objects
    print("Applying raster")
    # allrast$scores %>% rename(scores=last)
    # allrast$illegal_gs %>% rename(illegal_gs=last)
    if (resample)
    {
      print("Resampling")
      outR <- resample(allrast, precip_100Yr, method="bilinear")
    }
    else
    {
      outR <- allrast
    }

    # print(inters$scores)
    if (rmNA)
    {
      print("Substitution scores")
      outR$scores <- subst(outR$scores, NA, 0)
      print("substitution illegal")
      outR$illegal_gs <- subst(outR$illegal_gs, NA, FALSE)
      print("Applying moving window")
      outRast <- focal(outR, w=kernel, na.rm=TRUE)
      outRast$scores <- mask(outRast$scores, outR$illegal_gs,
                             maskvalues=TRUE, updatevalue=-100)
    }
    else
    {
      print("Applying moving window")
      outRast <- focal(outR, w=kernel, na.rm=TRUE)
      outRast$scores <- mask(outRast$scores, outR$illegal_gs,
                             maskvalues=TRUE, updatevalue=NA)
    }
  }
  else
  {
    if (resample)
    {
      print("Resampling")
      outR <- resample(allrast, precip_100Yr, method="bilinear")
      outRast <- outR
    }
    else
    {
      outRast <- allrast
    }
  }
  

  return(outRast)
  
}


index_list <- list()

i = 0
for (key in shp_keys)
{
  idx <- c()
  for (j in 1:length(geom_list[[key]]))
  {
    i = i + 1
    idx <- c(idx, i)
  }
  index_list[[length(index_list)+1]] <- idx
}

names(index_list) <- shp_keys

## -- Final Data declarations
category_names = rep(NA, length(geom_arr))
scores<- rep(100, length(geom_arr))
illegal_gs<- rep(FALSE, length(geom_arr))

for (key in shp_keys)
{
  print(key)
  category_names[index_list[[key]]] <- rep(key, length(index_list[[key]]))
  if (key != "study area")
  {
    scores[index_list[[key]]] <- sf_list[[key]]$scores
    illegal_gs[index_list[[key]]] <- sf_list[[key]]$illegal_gs 
  }
  else
  {
    scores[index_list[[key]]] <- 100
    illegal_gs[index_list[[key]]] <- FALSE
  }
}


tHeight<- rep(NA, length(geom_arr)) 
tHeight[index_list[["trees"]]] = trees_shp$BOOMHOOGTE
tName<- rep(NA, length(geom_arr))
tName[index_list[["trees"]]] = trees_shp$SOORT_WET
tRayon<- rep(NA, length(geom_arr))
tRayon[index_list[["trees"]]] = trees_shp$RAYON

gPrivatePct<- rep(NA, length(geom_arr))
gPrivatePct[index_list[["private greenspace percentage"]]] = greenspace_percent_private$Percentage

gPublicPct<- rep(NA, length(geom_arr))
gPublicPct[index_list[["public greenspace percentage"]]] = greenspace_percent_public$Percentage

ptypes<- rep(NA, length(geom_arr))
ptypes[index_list[["green areas"]]] = green_areas$plus_fysie

histB<- rep(NA, length(geom_arr))
histB[index_list[["historic areas"]]] = historic_areas$building

wType<- rep(NA, length(geom_arr))
wType[index_list[["water bodies"]]] = openWater$typeWater

l_use<- rep(NA, length(geom_arr))
l_use[index_list[["land use class"]]] = land_class$typeLandge

rdInfrType<- rep(NA, length(geom_arr))
rdInfrType[index_list[["roads"]]] = roads$typeInfras
rdType<- rep(NA, length(geom_arr))
rdType[index_list[["roads"]]] = roads$typeWeg
tfcType<- rep(NA, length(geom_arr))
tfcType[index_list[["roads"]]] = roads$hoofdverke

bType<- rep(NA, length(geom_arr))
bType[index_list[["buildings"]]] = buildings$typeGebouw

# scores<- rep(100, length(geom_arr))


full_sf <- st_sf(
  classType=category_names,
  treeHeight=tHeight, 
  treeName=tName, 
  treeRayon=tRayon,
  greenPrivatePercentage=gPrivatePct, 
  greenPublicPercentage=gPublicPct,
  plant_types=ptypes, 
  historicBuildings=histB, 
  waterType=wType, 
  land_use=l_use,
  roadInfrastructureType=rdInfrType, 
  roadType=rdType, 
  trafficType=tfcType,
  buildingType=bType,
  scores=scores,
  illegal_gs=illegal_gs,
  geometry=geom_arr
)

# testGrid <- st_make_grid(full_sf, cellsize=1)

## ----- computations
# FIRST MAP ->
# look into st_join per class with respect to area of study, basically create a polygon with values = N/a
# unless defined by each dataframe. Null all nonused data categories. 
#Add all these joined sfs with correct scaling factors and only considering specified elements of the dataframe.
# Might need to create switch statements wrt the dataframes where the data is string based. Add them all to each other
# and scale during this addition. -> produce a dataframe that can be converted to raster file then displayed 
inters_list <- list()
raster_list <- list()

# Create df for each intersect with the main values being the score associated with each class
i = 0
kWeight <- c(2,4,5,1,1,10,5,2,3,2,2,5,5)
for (key in shp_keys)
{
  i <- i + 1
  print(key)
  print("Intersect")
  inters <- st_intersection(st_zm(sf_list[[key]]), st_as_sfc(st_zm(sf_list[["study area"]])))
  print("Rasterize")
  
  if (key == "study area")
  {
    rObj <- rastObj(inters, kWeight[i], res = 1, moveWindow=FALSE, rmNA = FALSE, resample=FALSE)
  }
  else
  {
    rObj <- rastObj(inters, kWeight[i], res = 1, rmNA = FALSE, resample=FALSE)
  }

  print("Append intersect list")
  inters_list[[length(inters_list)+1]] <- inters
  # print("Plot intersect")
  # windows()
  # plot(inters, key.width=lcm(6.3))
  # title(key)
  
  # if (key == "study area")
  # {
  outRast <- rObj
  # }
  # else
  # {
  #   outR <- rObj
  #   # print(inters$scores)
  #   print("Substitution scores")
  #   outR$scores <- subst(rObj$scores, NA, 0)
  #   print("substitution illegal")
  #   outR$illegal_gs <- subst(rObj$illegal_gs, NA, FALSE)
  #   outRast <- focal(outR, w=kernel, na.rm=TRUE)
  #   outRast$scores <- mask(outRast$scores, outR$illegal_gs, 
  #                          maskvalues=TRUE, updatevalue=0)
  # }
  

  print("Append raster list")
  raster_list[[length(raster_list)+1]] <- outRast
  # print("Plot raster")
  # windows()
  # plot(rObj, key.width=lcm(6.3))
  # title(key)
  print(key)
}

names(inters_list) <- shp_keys
names(raster_list) <- shp_keys

scoresOut <- raster_list$`study area`$scores

for (lyr in raster_list)
{
  scoresOut <- scoresOut + lyr$scores
}

normScores <- minmax(scoresOut)    
normalizedScores <- (scoresOut - normScores[1,])*100 / (normScores[2,] - normScores[1,])

# col <- brewer.pal(8, "Spectral")
# pal <- colorRampPalette(col)
col2 <- brewer.pal(3, "Greens")
pal2 <- colorRampPalette(col2)
# suit = mapView(normalizedScores, col.regions = pal, layer.name="Suitability Score")
# roadsMap = mapView(raster_list$roads$roadType, layer.name="Road types")
# greenSpace = mapView(raster_list$`green areas`$scores, col.regions = pal2(10), 
#                      layer.name="Greenspace")#alpha.regions = 0.5, alpha = 1, layer.name="Greenspace")
# greenspace_percent_private = mapView(raster_list$`private greenspace percentage`$Percentage, col.regions = pal2(10), 
#                                      layer.name="Private Greenspace percentage")#alpha.regions = 0.5, alpha = 1, layer.name="Private Greenspace percentage")
# greenspace_percent_public = mapView(raster_list$`public greenspace percentage`$Percentage, col.regions = pal2(10), 
#                                     layer.name="Public Greenspace percentage")#alpha.regions = 0.5, alpha = 1, layer.name="Public Greenspace percentage")
# treesMap = mapView(trees_shp)
# buildingsMap = mapView(raster_list$buildings$buildingType, na.rm=TRUE)
# 
# suit + greenSpace + greenspace_percent_private + treesMap + greenspace_percent_public + roadsMap + buildingsMap


##--- Greenspace locations
# Need to define all existing greenspace locations - greenspace map + OSM data on greenspace
# Define parking and non road locations -> create polygons of areas without roads or buildings (include OSM data later)
# If parking define areas near trees/greenspace as permeable pavement -> look at intersect between greenspace and parking area
#
# 
# builtLand <- st_union(st_zm(inters_list[["roads"]]), st_zm(inters_list[["buildings"]]))
# nonIntersect <- st_difference(st_zm(sf_list[["study area"]]), st_zm(builtLand))
builtLand <- bind_rows(inters_list[["buildings"]], inters_list[["roads"]])
nonIntersect <- st_difference(st_zm(sf_list[["study area"]]), st_zm(builtLand))#inters_list[["buildings"]]$geometry))
# builtLand <- st_difference(st_zm(nonIntersect$geometry), st_zm(inters_list[["roads"]]$geometry))

# Private intersect does not work for rastering
# privateBuiltLand <- st_intersection(st_zm(builtLand), st_zm(inters_list[["private greenspace percentage"]]$geometry))
publicBuiltLand <- st_intersection(st_zm(builtLand), st_zm(inters_list[["public greenspace percentage"]]$geometry))

# privateBuiltLand <- st_intersection(st_zm(builtLand), st_as_sfc(st_zm(inters_list[["private greenspace percentage"]])))
# publicBuiltLand <- st_intersection(st_zm(builtLand), st_as_sfc(st_zm(inters_list[["public greenspace percentage"]])))
# 
# 


BLRast <- rastObj(st_zm(builtLand), centerWeight=1, res=1, moveWindow=FALSE, resample = FALSE)#mask(raster_list$`study area`, vect(privateBuiltLand$geometry))
publicBLRast <- rastObj(publicBuiltLand, centerWeight=1, res=1, moveWindow=FALSE, resample = FALSE)
GSArea <- rastObj(nonIntersect, centerWeight=1, res=1, moveWindow=FALSE, resample = FALSE)
# prvBL <- st_rasterize(privateBuiltLand) # only thing that worked for private <- think of alternative later

# privateGS <- mask(raster_list$`study area`, privateBLRast)
publicGS1 <- mask(BLRast, publicBLRast)
privateGS1 <- mask(BLRast, publicBLRast, inverse=T)

maskT <- resample(BLRast, GSArea)
maskPub <- resamble(publicGS1, GSArea)
maskPrv <- resample(privateGS1, GSArea)
totalGS <- mask(GSArea, maskT, inverse=T)
publicGS <- mask(GSArea, maskPub, inverse=T)
privateGS <- mask(GSArea, maskPrv)

prGS = mapView(privateGS, col.regions = pal2(10), 
                 layer.name="Private Greenspace Options", trim=TRUE, na.color="transparent")#alpha.regions = 0.5, alpha = 1, layer.name="Private Greenspace percentage")
                 
pbGS = mapView(publicGS, col.regions = pal2(10), 
                      layer.name="Public Greenspace Options", trim=TRUE, na.color="transparent")

prGS + pbGS

# shrtAltPath
# 
# 
# #Wednesday
# #Greenspace locations
# #if local, main road, or regional road -> no greenspace recommendations
# #requires creating a layer of all features -> currently not possible
# 
# if (road == local | road == regional | road == main)
# {
#   for (edge in polygon)
#   {
#     
#   }
#   if (proximity == bikelane) # need to define method to allow for selecting edge where intervention occurs
#   {
#     intervention = edge_intervention
#   }
#   else if (road_width == "7 m")
#   {
#     intervention = center_intervention
#     if (trafficType != bus)
#     {
#       percentage = 10
#     }
#     else
#     {
#       percentage = 5
#     }
#   }
# }
# else if (road == parking)
# {
#   intervention = partial
#   if (proximity == greenspace)
#   {
#     
#   }
# }
# 


# else if road has alternatives path to all buildings then suggest removal -> new greenspace road removal -> this will need to be computed with own function
# fi
#else if parking then suggest permeable pavement greenspace closest to greenspace so that 50% of area is covered
#else if near building and unused, suggest shrubs/bushes if public, garden if private
#else if near tree then add greenspace if possible

#Thursday
#Interventions
# if near road, suggest polygon split to 50% wadi, 50%shrub/bushes (closer to road)
# else if near permeable pavement and in greenspace, suggest rain garden
# 

#Thursday/Friday
#Tree specifc interventions
# if near tree and no other trees around, find soil conditions needed and recommend tree according to nearest trees and rain+soil conditions
# also include predicted max radius of each tree and height of nearby trees and try to match that

#Thursday/Friday
#Data needs
# For all combinations of OSM data and current data, need to ensure that fields match including naming conventions
#combine OSM data for greenspace with current data for greenspace
#combine osm data for buildings with current data for buildings 
#combine OSM data for roads with current data for roads
#get soil data raster

#Wednesday/Thursday
# Trees, plants, and/or shrubs corridor
# Find nearest path between greenspaces based on areas with best greenspace scores -> in otherwords map out all areas with scores above x as potential greenspace paths
# for each area of score, if score is above 50%, greenSpacePlacement = TRUE

#GOAL:
# Produce rasters for Trees, plants, and/or shrubs corridor, greenspace placement, interventions, new tree locations, and feasibility
# Produce output shapefiles from raster layers for maps
# Produce interactive map with all the layers
# Add design proposals to mapview

