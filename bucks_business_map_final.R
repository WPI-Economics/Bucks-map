################################################################################

#                              Bucks Green economy map                         #

################################################################################


# Initializing
#=================
.libPaths("C:/Users/MatthewTibbles/Documents/R/Library")
rm(list = ls())
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))
packages <- c("geojsonio", "readxl", "devtools","PostcodesioR","tidyverse","purrr", "sf",
              "rmapshaper","htmltools","leaflet","leaflet.extras","leaflegend",
              "htmltools")
for (pkg in packages) {
  if (!require(pkg,character.only = T)){
    if (pkg == "PostcodesioR"){devtools::install_github(paste0("ropensci/",pkg))}
    else{install.packages(pkg,character.only = T)}
  }
  library(pkg,character.only = T)
}

set.seed(373645)



# Getting green business data
#===============================

# Load buckinghamshire green economy busineses
grbus.df <- read_excel(
  "C:/Users/MatthewTibbles/WPI Economics Dropbox/Matthew Tibbles/WPI team folder/Buckinghamshire LEP/Map and dashboard data/NEW MAP Data City list.xlsx",
  sheet = "Companies as columns") %>%
  select("Companyname","BestEstimateEmployees", "BestEstimateTurnover",
         "URLs", "Registeredpostcode","CICs")
names(grbus.df) <- c("name", "employee_est", "turn_est",
                     "url", "postcode","cic")
grbus.df <- grbus.df[which(!is.na(grbus.df$postcode)),]

# Add peripheral businesses
grbus_periph.df <- read_csv(
  "C:/Users/MatthewTibbles/WPI Economics Dropbox/Matthew Tibbles/WPI team folder/Buckinghamshire LEP/Map and dashboard data/manual_map_additions_edit.csv"
)
grbus.df <- rbind(grbus.df, grbus_periph.df)



# Get coordinates for green businesses
postcode_area_lookup <- purrr::map_df(grbus.df$postcode, PostcodesioR::postcode_lookup)
problem <- postcode_area_lookup[which(is.na(postcode_area_lookup$latitude) &
                                        is.na(postcode_area_lookup$longitude)),]
postcode_area_lookup <- postcode_area_lookup[which(!is.na(postcode_area_lookup$latitude) &
                                                     !is.na(postcode_area_lookup$longitude)),]




# Get Non-Bucks businesses
#out <- postcode_area_lookup[which(postcode_area_lookup$admin_district != "Buckinghamshire"),] 
#grbus_out <- left_join(out, grbus.df,by="postcode")
#grbus_out <- grbus_out[c(36:ncol(grbus_out), 1:35)]
#grbus_out <- grbus_out[!duplicated(grbus_out[1:2]),]
#grbus_out$include <- 0
#grbus_out <- grbus_out[c(ncol(grbus_out),2:ncol(grbus_out)-1)]
#write_csv(grbus_out, "outside_Bucks_list.csv")


#bucks_periph.vec <- c("Buckinghamshire", "Oxford", "West Oxfordshire", "South Oxfordshire")
#postcode_area_lookup_bucks <- postcode_area_lookup[which(postcode_area_lookup$admin_district %in% bucks_periph.vec),] 


#lookup <- read_csv("The Health Foundation/Source data/Lookups/MSOA11_to_LAD_lookup.csv")
#names(lookup)[1:2] <- c("la_code", "msoa_code")
#postcode_area_lookup <- merge(postcode_area_lookup, lookup[c("la_code","msoa_code")],by="msoa_code")
#postcode_area_lookup <- postcode_area_lookup[which(postcode_area_lookup$la_code == "E06000060"),] 

# Add business coordinates to main dataframe
grbus_bounds_bucks.df <- merge(grbus.df, postcode_area_lookup[c("postcode","longitude","latitude", "admin_district", "admin_ward")], by = "postcode")                                                         






# Drop non-Bucks businesses 
bucks_periph_wards <- c("Fringford & Heyfords")

#grbus_bounds_bucks.df <- grbus_bounds_bucks.df[which(grbus_bounds_bucks.df$admin_district == "Buckinghamshire"|grbus_bounds_bucks.df$name %in% grbus_periph.df$name),]
grbus_bounds_bucks.df <- grbus_bounds_bucks.df[which(grbus_bounds_bucks.df$admin_district == "Buckinghamshire"|
                                                       grbus_bounds_bucks.df$admin_ward %in% bucks_periph_wards),]

# Drop duplicates
grbus_bounds_bucks.df <- grbus_bounds_bucks.df %>% distinct(name, .keep_all =T)

# Filter out businesses with multiple firms in same location (e.g. BIFFA)
#... keeping only records where employee count is not NA)
grbus_bounds_bucks.df$name_first <- stringr::word(grbus_bounds_bucks.df $name, 1)
#dropped <- grbus.df[which(duplicated(grbus.df$name_first,grbus.df$postcode) & grbus.df$employee_est == 0),]
grbus_bounds_bucks.df  <- grbus_bounds_bucks.df [-which(duplicated(grbus_bounds_bucks.df$name_first,grbus_bounds_bucks.df$postcode) & grbus_bounds_bucks.df$employee_est == 0),]

###################### Experimenting #########################

# Temporarily rename RRRR industry (to allow separation of multple CICs by comma character)
grbus_bounds_bucks.df$cic <- gsub("Reduce, Reuse, Recycle, Repair","RRRR", grbus_bounds_bucks.df$cic)

# Split business with multiple CICs into separate records (1 for each CIC aka un-nested) 
grbus_bounds_bucks.df$cic_lab <- grbus_bounds_bucks.df$cic
grbus_full.df <- grbus_bounds_bucks.df  %>% 
  mutate(cic = strsplit(as.character(cic), ",")) %>% 
  unnest(cic)
grbus_full.df$cic <- str_trim(grbus_full.df$cic)


# CIC frequency ranking (based on un-nessted data)
cic_count <- grbus_full.df %>% 
  group_by(cic) %>%
  count(cic) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(n))) %>%
  arrange(rank)


# Op2. Randomly select single CIC from nested data (but not one that goes in "Other")
grbus_bounds_bucks.df$cic_lab <- grbus_bounds_bucks.df$cic
grbus_bounds_bucks.df  <- grbus_bounds_bucks.df 
for (i in 1:nrow(grbus_bounds_bucks.df )) { # loop over rows
  cic_num <- count.fields(textConnection(grbus_bounds_bucks.df$cic[i]), sep = ",")
  cic_num <- length(strsplit(grbus_bounds_bucks.df $cic[i],",")[[1]]) # count CICs
  cic_range <- 1:cic_num
  if (cic_num > 1) { # if multiple Cic
    rdm_num <- sample(cic_range,1) # random no 
    rdm_cic <- str_trim(strsplit(grbus_bounds_bucks.df $cic[i],",")[[1]][rdm_num])  # random cic
    grbus_bounds_bucks.df$cic[i] <- rdm_cic 
  }
}

################################################################################  

# Re-rename RRRR industry 
grbus_bounds_bucks.df$cic <- gsub("RRRR", "Reduce, Reuse, Recycle, Repair",grbus_bounds_bucks.df$cic)

# Drop climate Adaptation (no businesses)
#grbus_bounds_bucks.df  <- grbus_bounds_bucks.df[-grep("Climate Adaptation", grbus_bounds_bucks.df$cic),]

# SIC as factor variable
grbus_bounds_bucks.df$cic <- factor(grbus_bounds_bucks.df$cic, 
                                    levels = c("Business and Industry", "Climate Adaptation", "Diversion of Waste From Landfill", 
                                               "Green Professional Services", "Homes and Buildings", "Low Carbon Transport", "Power",
                                               "Reduce, Reuse, Recycle, Repair", "Reducing Localised Pollution"))

# Turnover range 
grbus_bounds_bucks.df$turn_est_range  <- ifelse(grbus_bounds_bucks.df$turn_est >= 0 & grbus_bounds_bucks.df$turn_est <= 100000,3,
                                                ifelse(grbus_bounds_bucks.df$turn_est > 100000 & grbus_bounds_bucks.df$turn_est < 1000000, 7,
                                                       ifelse(grbus_bounds_bucks.df$turn_est > 1000000 & grbus_bounds_bucks.df$turn_est < 10000000, 11, 
                                                              ifelse(grbus_bounds_bucks.df$turn_est > 10000000, 15, 3))))

grbus_bounds_bucks.df $turn_est_range2 <- grbus_bounds_bucks.df$turn_est_range *1.5




# No. employee ntiltes (16 for opitmal legend fit)
grbus_bounds_bucks.df $employee_est_range  <- ifelse(grbus_bounds_bucks.df $employee_est >= 1 & grbus_bounds_bucks.df $employee_est <= 5,3,
                                                     ifelse(grbus_bounds_bucks.df $employee_est > 5 & grbus_bounds_bucks.df $employee_est < 20, 7,
                                                            ifelse(grbus_bounds_bucks.df $employee_est > 20 & grbus_bounds_bucks.df $employee_est < 100, 11, 
                                                                   ifelse(grbus_bounds_bucks.df $employee_est > 100, 15, 3))))

grbus_bounds_bucks.df $employee_est_range2 <- grbus_bounds_bucks.df $employee_est_range *1.5

# Recode zero turnover/employee counts as NA
grbus_bounds_bucks.df $employee_est[grbus_bounds_bucks.df $employee_est == 0] <- NA
grbus_bounds_bucks.df $turn_est[grbus_bounds_bucks.df $turn_est == 0] <- NA

# Add random noise to prevent overlapping markers
grbus_bounds_bucks.df $longitude <- jitter(grbus_bounds_bucks.df $longitude,factor = 300)
grbus_bounds_bucks.df $latitude <- jitter(grbus_bounds_bucks.df $latitude,factor = 300)


# Re-adjust position select business
silverstone <- c("SAIETTA", "LUNAZ")
greatmoor <- c("SUNNICA LIMITED", "MARINE ENVIRONMENTAL MONITORING SOLUTIONS LTD","PADERO SOLAER LTD")
bourne_end <- c("ATOMIXAIR TECHNOLOGIES LTD","BIRCHWOOD SOLICITORS LIMITED")
grbus_bounds_bucks.df <- grbus_bounds_bucks.df %>%
  mutate(longitude = case_when(name %in% silverstone ~ as.numeric(longitude +0.009), T~as.numeric(longitude))) %>%
  mutate(latitude = case_when(name == "CORPORATE CITIZENSHIP LIMITED" | name == "FLEETDRIVE MANAGEMENT LIMITED"  ~ as.numeric(latitude +0.005), T~ as.numeric(latitude))) %>%
  mutate(latitude = case_when(name %in% greatmoor ~ as.numeric(latitude +0.011), T~ as.numeric(latitude))) %>%
  mutate(longitude = case_when(name %in% bourne_end ~ as.numeric(longitude +0.005), T~as.numeric(longitude))) %>%
  mutate(latitude = case_when(name == "MPL TECHNOLOGY GROUP HOLDINGS LIMITED"~ as.numeric(latitude +0.04), T~ as.numeric(latitude))) %>%
  mutate(latitude = case_when(name == "LAPWING FEN II LTD."~ as.numeric(latitude +0.001), T~ as.numeric(latitude))) 
# Transform to point shapefile
grbus_bounds_bucks.sf  <- grbus_bounds_bucks.df  %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 


# Getting Buckinghamshire polygons
#==================================

# Local authority polygons
bounds.df <- readRDS("C:/Users/MatthewTibbles/WPI Economics Dropbox/Matthew Tibbles/WPI team folder/The Health Foundation/Source data/geo files/LAD2020.RDS")
bounds.sf <- ms_simplify(bounds.df)

# Bucks polygon 
bucks_bounds.sf <- bounds.sf[which(bounds.sf$LAD20NM == "Buckinghamshire"),]
bbox <- st_bbox(bucks_bounds.sf) %>% as.vector() 
bbox <- bbox + c(1 ,.985, -1, -1)


# Getting peripheral Ward-level polygons
#==============================================

# Ward polygons with lookup 
#ward_bounds.df <- readRDS("C:/Users/MatthewTibbles/WPI Economics Dropbox/Matthew Tibbles/WPI team folder/Buckinghamshire LEP/Map and dashboard data/WARD2021.rds")
#colnames(ward_bounds.df)[colnames(ward_bounds.df) == "WD21NM"] <- "ward_name"

# Ward to LA lookup 
#lookup <- read_csv("C:/Users/MatthewTibbles/WPI Economics Dropbox/Matthew Tibbles/WPI team folder/Buckinghamshire LEP/Map and dashboard data/Ward_to_Local_Authority_District_to_County_to_Region_to_Country_(December_2020)_Lookup_in_United_Kingdom_V2.csv")
#colnames(lookup)[colnames(lookup) == "WD20NM"] <- "ward_name"

# Merge
#ward_bounds.df <- merge(ward_bounds.df, lookup)

# Restrict to periphery LAs
#peripheries <- c("Cherwell","South Northamptonshire")
#ward_bounds.df <- ward_bounds.df[which(ward_bounds.df$LAD20NM %in% peripheries),]

# Shape
#ward_bounds.sf <- ms_simplify(ward_bounds.df)



# Add postcodes
#ward_bounds.df <- merge(ward_bounds.df, postcode_area_lookup, by = "admin_ward_code")

# Add businesses by Ward
#ward_bounds_grbus.df <- merge(ward_bounds.df,grbus.df, by = "postcode")

# Transform to shapefile
#ward_bounds_grbus.sf <- ms_simplify(ward_bounds_grbus.df)

### Silverstone
#silver_bounds_grbus.sf <-ward_bounds_grbus.sf[which(ward_bounds_grbus.sf$admin_ward_code == "E05013265"),]
#silver_bounds.sf <- ward_bounds.sf[which(ward_bounds.sf$ward_name == "Silverstone"),]


### Bicester
#fring_bounds.sf <- ward_bounds.sf[which(ward_bounds.sf$ward_name == "Fringford and Heyfords"),]


# Bucks + periphery polygons
#bucks_bounds.sf <- bucks_bounds.sf[c("LONG", "LAT", "geometry")] %>% st_set_crs(4326)
#fring_bounds.sf <- fring_bounds.sf[c("LONG", "LAT", "geometry")] %>% st_set_crs(4326)
#bucks_periph_bounds.sf <- bind_rows(bucks_bounds.sf, fring_bounds.sf) %>% st_as_sf
#bucks_periph_bounds.sf <- 
#  st_union(bucks_bounds.sf, fring_bounds.sf) %>% 
#  st_make_valid() %>%
#  select(LONG, LAT, geometry) %>%
#  st_cast("MULTIPOLYGON") %>%
#  st_as_sf()
#bbox <- st_bbox(bucks_periph_bounds.sf) %>% as.vector()

# 
outer_bounds.sf <- st_difference(bounds.sf,bucks_bounds.sf)

# Non-Bucks-non-periphery polygon
bounds.sf <- bounds.sf[c("LONG", "LAT", "geometry")] %>%
  st_cast("MULTIPOLYGON") %>%
  st_as_sf()

#outer_bounds.sf <- setdiff(bounds.sf,bucks_periph_bounds.sf) %>% st_as_sf()
outer_bounds.sf <- st_difference(bounds.sf,bucks_bounds.sf) %>%
  st_set_crs(4326) %>%
  st_make_valid() %>%
  select(LONG, LAT, geometry)






# Formatting for plot
#=========================

grbus_bounds_bucks.sf  <- grbus_bounds_bucks.sf  %>% drop_na(cic)

grbus_bounds_bucks.sf $ID <- (1:nrow(grbus_bounds_bucks.sf ))+100



# Marker labels
label.map <- sprintf("<b>Company name:</b> %s  <br><b>URL:</b> %s <br><b>Green Sub-Sector:</b> %s <br><b>Turnover (Est.):</b> %s <br><b>No. Employees (Est.):</b> %s",
                     grbus_bounds_bucks.sf $name,
                     paste0("<a href='https://www.",grbus_bounds_bucks.sf $url,"/'>",grbus_bounds_bucks.sf $url,"</a>"),
                     grbus_bounds_bucks.sf $cic_lab,
                     ifelse(!is.na(grbus_bounds_bucks.sf $turn_est),
                            paste0("£",formatC(as.numeric(grbus_bounds_bucks.sf $turn_est), 
                                               format="f", digits=0, big.mark=",")),NA),
                     round(grbus_bounds_bucks.sf $employee_est,0)) %>%
  lapply(htmltools::HTML)


# cic markers
cic_colours <- c("black", "gold", "green" , "blue", "yellow", "violet", "red", 
                 "orange", "grey")
cic_markers.list <- sapply(
  c(1:9),
  function(i) {
    colour <- paste0("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-",cic_colours[i],".png")
    return(colour)
  }
)

cic_markers <- ifelse(grbus_bounds_bucks.sf$cic == levels(grbus_bounds_bucks.sf $cic)[1], cic_markers.list[[1]],
                      ifelse(grbus_bounds_bucks.sf $cic == levels(grbus_bounds_bucks.sf $cic)[2], cic_markers.list[[2]],
                             ifelse(grbus_bounds_bucks.sf $cic == levels(grbus_bounds_bucks.sf $cic)[3], cic_markers.list[[3]],
                                    ifelse(grbus_bounds_bucks.sf $cic == levels(grbus_bounds_bucks.sf $cic)[4], cic_markers.list[[4]],
                                           ifelse(grbus_bounds_bucks.sf $cic == levels(grbus_bounds_bucks.sf $cic)[5], cic_markers.list[[5]],
                                                  ifelse(grbus_bounds_bucks.sf $cic == levels(grbus_bounds_bucks.sf $cic)[6], cic_markers.list[[6]],
                                                         ifelse(grbus_bounds_bucks.sf $cic == levels(grbus_bounds_bucks.sf $cic)[7], cic_markers.list[[7]],
                                                                ifelse(grbus_bounds_bucks.sf $cic == levels(grbus_bounds_bucks.sf $cic)[8], cic_markers.list[[8]],
                                                                       ifelse(grbus_bounds_bucks.sf $cic == levels(grbus_bounds_bucks.sf $cic)[9], cic_markers.list[[9]],NA)))))))))

location_ops <- makeIcon(
  iconUrl = cic_markers,
  iconWidth = 12, iconHeight = 18)

# Turnover markers
turnover_ops <- makeIcon(
  iconUrl = cic_markers,
  iconWidth = grbus_bounds_bucks.sf $turn_est_range, iconHeight = grbus_bounds_bucks.sf $turn_est_range2
)

# Employee markers
employee_ops <- makeIcon(
  iconUrl = cic_markers,
  iconWidth = grbus_bounds_bucks.sf $employee_est_range, iconHeight = grbus_bounds_bucks.sf $employee_est_range2
)



# Plot
#===============



map <- leaflet( 
  options = leafletOptions(padding = 100, zoomSnap = 1,
                           zoomDelta = 1, minZoom =9.1, maxZoom = 18,  
                           maxNativeZoom = 18, detectRetina = T)) %>% 
  #setView(zoom = 8) %>%
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
  setMaxBounds(bbox[1], bbox[2], bbox[3], bbox[4])%>%
  setMapWidgetStyle(list(background = "white")) %>% 
  addMapPane(name = "background", zIndex = 400) %>%
  addMapPane(name = "labels", zIndex = 500) %>%
  addMapPane(name = "markers", zIndex = 480) %>%
  addMapPane(name = "bucks", zIndex = 450) %>%
  addMapPane(name = "outer", zIndex = 415) %>%
  addProviderTiles(providers$CartoDB.PositronOnlyLabels ,
                   providerTileOptions(opacity = 0.6),
                   tileOptions(tileSize = 256),
                   options = leafletOptions(pane = "labels")) %>%
  addProviderTiles(providers$CartoDB.Positron,
                   providerTileOptions(opacity = 0.6),
                   tileOptions(tileSize = 256),
                   options = leafletOptions(pane = "background")) %>%
  addPolygons(data = bucks_bounds.sf, color = "black", fillColor = "white", fillOpacity = 0.05,
              weight = 2, options = pathOptions(pane = "bucks")) %>%
  addPolygons(data = outer_bounds.sf$geometry, fillColor = "black", color = "white",fillOpacity = 0.75,
              weight = 2, options = pathOptions(pane = "outer"))  %>%
  
  # Location
  addMarkers(data=grbus_bounds_bucks.sf ,
             icon = location_ops ,
             label = label.map,
             labelOptions = labelOptions(noHide = F,
                                         style = list("box-shadow" = "3px 3px rgba(0,0,0,0.1)")),
             options = pathOptions(pane = "markers"),
             group = ~cic,
             popup = label.map,
             layerId = ~paste(ID,"Location", sep="")
  ) %>%
  # Turnover
  addMarkers(data=grbus_bounds_bucks.sf ,
             icon = turnover_ops ,
             label = label.map,
             labelOptions = labelOptions(noHide = F,
                                         style = list("box-shadow" = "3px 3px rgba(0,0,0,0.1)")),
             options = pathOptions(pane = "markers"),
             group = ~cic,
             popup = label.map,
             layerId = ~paste(ID,"Turnover (National Est.)", sep="")
  ) %>%
  #Employees
  addMarkers(data=grbus_bounds_bucks.sf ,
             icon = employee_ops ,
             label = label.map,
             labelOptions = labelOptions(noHide = F,
                                         style = list("box-shadow" = "3px 3px rgba(0,0,0,0.1)")),
             options = pathOptions(pane = "markers"),
             group = ~cic,
             popup = label.map,
             layerId = ~paste(grbus_bounds_bucks.sf $ID,"Number of Employees (National Est.)", sep=""
             ))%>%
  addLayersControl(
    baseGroups = c("Location","Turnover (National Est.)", "Number of Employees (National Est.)"),
    overlayGroups = c("Business and Industry", "Climate Adaptation", "Diversion of Waste From Landfill", 
                      "Green Professional Services", "Homes and Buildings", "Low Carbon Transport", "Power",
                      "Power","Reduce, Reuse, Recycle, Repair", "Reducing Localised Pollution" ),
    options = layersControlOptions(collapsed = F, positon = "topleft")
  ) %>%
  hideGroup(c( "Turnover (National Est.)", "No. Employees (National Est.)")) %>% 
  addResetMapButton() %>%
  addSearchFeatures(targetGroups = c("Business and Industry", "Climate Adaptation", "Diversion of Waste From Landfill", 
                                     "Green Professional Services", "Homes and Buildings", "Low Carbon Transport", "Power",
                                     "Power","Reduce, Reuse, Recycle, Repair", "Reducing Localised Pollution" ), 
                    options = searchFeaturesOptions(
                      zoom=14,openPopup = T, collapsed = T, firstTipSubmit = T,
                      hideMarkerOnCollapse = F, autoCollapse = T, textPlaceholder = "Find a green business in Buckinghamshire...")
  ) %>%
  addLegendImage(
    images = cic_markers.list,
    labels = levels(grbus_bounds_bucks.df$cic),
    title = "Green Economy Sub-Sector",
    width = rep(12,9), height = rep(18,9),
    labelStyle = 'font-size: 10.5px; font-weight: bold;',
    orientation = 'vertical',
    position = 'topleft'
  ) %>%
  addLegendImage(
    images = rep('https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png',4),
    labels = c("Low (£0-£100,000)\n", "Medium (£100,000-£1,000,000)\n", "High (£1,000,000-£10,000,000)\n", "Very high (£10,000,000+)\n"),
    title = "Turnover (National Est.)",
    width = c(3,7,11,15), height = c(4.5,10.5,16.5,22.5),
    group = "Turnover (National Est.)",
    labelStyle = 'font-size: 10.5px; font-weight: bold;',
    orientation = 'vertical',
    position = 'topleft'
  ) %>%
  addLegendImage(
    images = rep('https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-black.png',4),
    labels = c("Low (1-5)\n", "Medium (5-20)\n", "High (20-100)\n", "Very high (100+)\n"),
    title = "Number of Employees (National Est.)",
    width = c(3,7,11,15), height = c(4.5,10.5,16.5,22.5),
    group = "Number of Employees (National Est.)",
    labelStyle = 'font-size: 11.5px; font-weight: bold;',
    orientation = 'vertical',
    position = 'topleft'
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      var baseLayer = 'Location';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if ('Location' !== id.substring(3,)){
            layer.getElement().style.display = 'none';
          }
        }
      })
      console.log(myMap.baselayer);
      myMap.on('baselayerchange',
        function (e) {
          baseLayer=e.name;
          myMap.eachLayer(function (layer) {
              var id = layer.options.layerId;
              if (id){
                if (e.name !== id.substring(3,)){
                  layer.getElement().style.display = 'none';
                  layer.closePopup();
                }
                if (e.name === id.substring(3,)){
                  layer.getElement().style.display = 'block';
                }
              }

          });
        })
        myMap.on('overlayadd', function(e){
          myMap.eachLayer(function(layer){
            var id = layer.options.layerId;
            if (id){
                if (baseLayer !== id.substring(3,)){
                  layer.getElement().style.display = 'none';
                }
            }    
          })
        })
    }") 
map

map$sizingPolicy$padding <- 0
map$sizingPolicy$margin <- 0
#htmlwidgets::saveWidget(map, "C:/Users/MatthewTibbles/Downloads/index2.html")
htmlwidgets::saveWidget(map, "docs/index2.html")


    
#########################################################################################