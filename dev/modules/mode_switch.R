library(sf)  

##Split Polyline##
stdh_cast_substring <- function(x, to = "MULTILINESTRING") {
  ggg <- st_geometry(x)
  
  if (!unique(st_geometry_type(ggg)) %in% c("POLYGON", "LINESTRING")) {
    stop("Input should be  LINESTRING or POLYGON")
  }
  for (k in 1:length(st_geometry(ggg))) {
    sub <- ggg[k]
    geom <- lapply(
      1:(length(st_coordinates(sub)[, 1]) - 1),
      function(i)
        rbind(
          as.numeric(st_coordinates(sub)[i, 1:2]),
          as.numeric(st_coordinates(sub)[i + 1, 1:2])
        )
    ) %>%
      st_multilinestring() %>%
      st_sfc()
    
    if (k == 1) {
      endgeom <- geom
    }
    else {
      endgeom <- rbind(endgeom, geom)
    }
  }
  endgeom <- endgeom %>% st_sfc(crs = st_crs(x))
  if (class(x)[1] == "sf") {
    endgeom <- st_set_geometry(x, endgeom)
  }
  
  if (to == "LINESTRING") {
    endgeom <- endgeom %>% st_cast("LINESTRING")
  }
  return(endgeom)
} 

load(file="MSSI-Demonstration/Rdata/Potential_Car.Rdata")
scenario <- potential_car[potential_car$Cycling_Dist <= 4340 & 
                            potential_car$Elevation_Gain <= 45 & 
                            potential_car$Time_Ratio <=2.46,] #select cyclable trips

##calculate number of cyclable trips on road segments##
car <- scenario[which(scenario$Car>0),]
car_segments <- stdh_cast_substring(car, "LINESTRING") #Spline lines into segments
car_segments$grp = sapply(st_equals(car_segments), max) #Identify duplicate geometries
car_final <- car_segments %>% group_by(grp) %>% summarise(total_car = sum(Car)) #car_final is used to create a map

##Calculate VMT and GHG reduction per day
sum(car$Car) #number of cyclable trips
sum(car$Car*car$Car_Dist*0.0006) #VMT reduction
sum(car$Car*car$Car_Dist/100000*19) #GHG reduction (kg)


