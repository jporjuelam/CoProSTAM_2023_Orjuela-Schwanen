#Code for assessment of multiple CDI locations
#Written by Juan Pablo Orjuela (jporjuelam@gmail.com)
#Started 25 Nov 2023
#No exact locations to protect personal data
############################



library(sp)
library(sf)
library(ggmap)
library(stplanr)
library(dodgr)
library(sf)
library(osmdata)
library(scales)
library(raster)
library(ggplot2)
library(geodist)
library(dplyr)  

rm(list = ls())

setwd("/Users/jporjuela/OneDrive - Nexus365/Oxford/TSU/PEAK/STAM")
Place <- "Valle de Aburrá"

#downloads osm data as class sc
amva_sc <- opq(bbox = Place) %>% 
  add_osm_feature(key = 'highway') %>% 
  osmdata_sc()
#Load Air pollution data
AirPollution <- raster("GIS/Air pollution/RasterAPInterpolation.tif")

#Location of CDIs
CDI <- data.frame(name=c("Pedregal","CerroLuces","SantaMaria","Naranjos","PequeGenios","Bariloche","HuellasCreativas"),
                  Lon=c(-75.611146,-75.602512,-75.597905,-75.605895,-75.612405,-75.640401,-75.640276),
                  Lat=c(6.190444,6.185353,6.184751,6.172103,6.168180,6.167106,6.156906))


#Create profile definition - want to have lunch

#Do it for one CDI first 
i <- 2
#Home 2 is a few meters away to avoid location repetition (same places creates problems in FOS)
FixedActivities <- data.frame(Place_type=c("CDI","home","CDI","home","home2"),
                              Lon=c(CDI$Lon[[i]],-75.609534,CDI$Lon[[i]],-75.609534,-75.608934),
                              Lat=c(CDI$Lat[[i]],6.191594,CDI$Lat[[i]],6.191594,6.1911),
                              Start_trip_time=c("8:10","12:30","16:10","18:00",NA), #time to leave this place, NA for last trip of the day
                              End_trip_time=c(NA,"10:00","16:00","17:00","22:00"),#time to arrive to this place, NA for first trip of the day
                              DayWeek = c(1,1,1,1,1)) 
FixedActivitiesResults <- FixedActivities
FixedActivitiesResults$foot <- NA
FixedActivitiesResults$motorcar <- NA
ShopList <- c("department_store","supermarket","bakery","butcher","cheese","coffee",
              "convenience","deli","dairy","pastry","seafood","food","general","kiosk","wholesale")

FlexibleActivities <- data.frame(key=c("work","work","work","shop","work","work","amenity","visit"), #in order of priority
                                 value=c("client","client","client","convenience","client","client","ice_cream","friend"), #in order of priority
                                 Lon=c(NA,NA,NA,NA,NA,NA,NA,-75.620213),
                                 Lat=c(NA,NA,NA,NA,NA,NA,NA,6.193111),
                                 Open_time=c("8:00","8:00","8:00","7:00","8:00","8:00","14:00","17:00"),
                                 Close_time=c("16:00","16:00","16:00","19:00","16:00","16:00","18:00","22:00"),
                                 MinActivityDuration=c(0.25,0.25,0.25,1,0.25,0.25,1,2),#in hours
                                 Status =c(1,1,1,1,1,1,1,1)) #0 indicates that has already been done 
ClientList <- data.frame(key=c("work","work","work","work","work"),
                         value=c("client","client","client","client","client"),
                         name=c("Itagui","Sabaneta","Envigado","Calatrava","LosGomez"),
                         Lon=c(-75.609123,-75.615303,-75.586889,-75.611721,-75.610557),
                         Lat=c(6.173099,6.152001,6.169286,6.181267,6.190462))

VistList <- data.frame(key=c("vist","vist"),
                         value=c("friend","family"),
                         name=c("Ajizal","LosGomez"),
                         Lon=c(-75.620213,-75.610557),
                         Lat=c(6.193111,6.190462))

#Code for amenities - could start with a list of places like work etc
#Ice Cream is key = amenity, value = ice_cream
#Shop for groceries is key = shop, value = convenience

Results <- vector(mode='list',length=2)
d <- 1
for (d in 1:1){#Open day loop
  FixedActivitiesDay <- FixedActivities[FixedActivities$DayWeek==d,]
  FixedActivitiesDayResults <- FixedActivitiesDay
  FixedActivitiesDayResults$foot <- NA
  FixedActivitiesDayResults$motorcar <- NA
  FlexibleActivitiesR <- FlexibleActivities
  
  l <- 1
  g <- l
  for (l in 1:dim(FixedActivitiesDay)[1]){#Open trip loop - dim(FixedActivitiesDay)[1]
    
    CodeRunStart <- Sys.time()
    FlexibleActivitiesR <- FlexibleActivitiesR[FlexibleActivitiesR$Status==1,]#Exclude flexible activities that have been done 
    
    ######Step 0 - Initialize #######
    #Step 0.1
    #Available modes
    Modes <- data.frame("Mode" = c("foot","motorcar"), 
                        "Cost" = c(0,0.25), #Cost as proportion of income and independent of distance (needs checking!)
                        "PM25"=c(1.9,2.5)) #From de Nazelle, Bode and Orjuela (2017)
    
    start_location <- c(FixedActivitiesDay[l,2],FixedActivitiesDay[l,3])  
    end_location <- c(FixedActivitiesDay[l+1,2],FixedActivitiesDay[l+1,3]) 
    
    if (dim(FlexibleActivitiesR)[1]>0){
      f <- 1 #f will always be the first flexible activity in the new list 
      
      MinAct <- FlexibleActivitiesR$MinActivityDuration[f] #Min activity duration in hours
      
      
      #Opening hours
      ActivityOpen <- as.POSIXct(FlexibleActivitiesR$Open_time[f],"%H:%M",tz="America/Bogota") 
      ActivityClose <- as.POSIXct(FlexibleActivitiesR$Close_time[f],"%H:%M",tz="America/Bogota") 
      
      #Step 0.2
      StartTime <- as.POSIXct(FixedActivitiesDay$Start_trip_time[l],"%H:%M",tz="America/Bogota")
      EndTime <- as.POSIXct(FixedActivitiesDay$End_trip_time[l+1],"%H:%M",tz="America/Bogota")
      ExtMinAct <- 10/60 #in hours, generic for the time being
      Total_F <- as.numeric(difftime(EndTime,StartTime, units="hours"))
      Total_T <- Total_F-ExtMinAct
      
      #Open Mode loop
      ModeAccess <- data.frame("Mode","access")
      TravelTimesModeList <- vector(mode='list',length=2)
      m <- 1
      for (m in 1 :1){
        #Step 0.3
        #These are called _bicycle from previous code, but are generic
        street_network_dodgr_sc_bicycle <- weight_streetnet(amva_sc , wt_profile = Modes$Mode[m]) #to make it a network
        street_network_dodgr_sc_bicycle <- street_network_dodgr_sc_bicycle[street_network_dodgr_sc_bicycle$component == 1, ] #check connectivity
        
        #Step 0.4
        dodgr_fastest_ids <- dodgr_paths(street_network_dodgr_sc_bicycle, start_location, end_location, vertices = FALSE )
        dodgr_fastest_path <- street_network_dodgr_sc_bicycle[dodgr_fastest_ids[[1]][[1]], ] #distances in meters and times in seconds
        Shrt_T <- (sum(dodgr_fastest_path$time_weighted,na.rm = T)/60)/60 #from seconds to minutes to hours
        
        #Step 0.5
        #Check if space-time prism can be made. If not, go to step 0.2 to work with the next set of activities.
        if(Total_T>=Shrt_T){ #Opens space-time prism check loop
          
          ######Step 1 - Delimit initial search areas for FOS and FOSc #######
          
          #Step 1.1
          Serv_Tbig <- Shrt_T+(Total_T-Shrt_T)/2 #in hours
          Serv_Tsm <- Total_T/2 #in hours
          #Step 1.2
          MAX_threshold <- 3*MinAct #This can be changed according to literature 
          Serv_Tbig <- min(Serv_Tbig,MAX_threshold)
          Serv_Tsm <- min(Serv_Tsm, MAX_threshold)
          #Step 1.3 - at the moment it is the same set up as 0.3 but different name (_2)
          street_network_dodgr_sc_bicycle_2 <- weight_streetnet(amva_sc , wt_profile = Modes$Mode[m])
          street_network_dodgr_sc_bicycle_2 <- street_network_dodgr_sc_bicycle_2[street_network_dodgr_sc_bicycle_2$component == 1, ]
          
          #Step 1.4
          Serv1 <- dodgr_isochrones (street_network_dodgr_sc_bicycle_2, from = start_location, Serv_Tbig*60*60) #dodgr_isochrones wants seconds, Serv_T are in hours
          Serv1 <- SpatialPolygons(list(Polygons(list(Polygon(Serv1[, c(4,5)])), ID=1))) #Turns into polygon class
          proj4string(Serv1) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #Add a CRS
          Serv1 <- st_as_sf(Serv1) #Latest update of R migrated back to sf so I added this command 
          Serv2 <- dodgr_isochrones (street_network_dodgr_sc_bicycle_2, from = end_location, Serv_Tbig*60*60) #there is a problem with the direction here! 
          Serv2 <- SpatialPolygons(list(Polygons(list(Polygon(Serv2[, c(4,5)])), ID=1)))
          proj4string(Serv2) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
          Serv2 <- st_as_sf(Serv2)
          
          #Step 1.5
          Serv3 <- dodgr_isochrones (street_network_dodgr_sc_bicycle_2, from = start_location, Serv_Tsm*60*60)
          Serv3 <- SpatialPolygons(list(Polygons(list(Polygon(Serv3[, c(4,5)])), ID=1)))
          proj4string(Serv3) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #Add a CRS
          Serv3 <- st_as_sf(Serv3) #Latest update of R migrated back to sf so I added this command 
          Serv4 <- dodgr_isochrones (street_network_dodgr_sc_bicycle_2, from = end_location, Serv_Tsm*60*60)#there is a problem with the direction here! 
          Serv4 <- SpatialPolygons(list(Polygons(list(Polygon(Serv4[, c(4,5)])), ID=1)))
          proj4string(Serv4) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #Add a CRS
          Serv4 <- st_as_sf(Serv4) #Latest update of R migrated back to sf so I added this command 
          
          #Step 1.6
          ISAbig <- st_intersection(Serv1,Serv2) #It's intersects from Kim and Kwan but I have my doubts
          ISAsm <- st_union(Serv3,Serv4) #It's union from Kim and Kwan but I have my doubts
          ISA <- st_intersection(ISAbig,ISAsm, dimension = "polygon") #It's intersects from Kim and Kwan but I have my doubts
          #In the function above, dimension = "polygon" fixes the error of multiple polygons on intersection shown as:
          #Error in `[[<-.data.frame`(`*tmp*`, attr(x, "sf_column"), value = list( : 
          #replacement has 2 rows, data has 1
          
          #Step 1.7
          if (FlexibleActivitiesR$key[f]!="work"){
            if (FlexibleActivitiesR$key[f]!="visit"){
            call2 <- opq(bbox = Place) 
            #Below is set fixed for store. Needs to be fixed for general case
            call2 <- add_osm_feature(call2, key = "shop", value=c("department_store","supermarket","bakery","butcher","cheese","coffee",
                                                                  "convenience","deli","dairy","pastry","seafood","food","general","kiosk","wholesale"))
            mydata <- osmdata_sf(call2)
            
            FOSc <- st_intersection(mydata$osm_points,ISA)
            FOSc <- FOSc[!is.na(FOSc$name),] #The assumption here is that only stores with names are really available 
            
            ######Step 2 - Identify opportunities  #######
            #For simplicity, I tried with a maximum of the first 6 opportunities but they were all too far
            FOSc_test <- FOSc #head(FOSc)
            #Step 2.1
            } else {
              VisitLocation <- st_as_sf(VisitList,coords=c(4,5),crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
              FOSc <- st_intersection(VisitLocation,ISA)
              FOSc_test <- FOSc
            }
          } else {
            clientLocation <- st_as_sf(ClientList,coords=c(4,5),crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
            FOSc <- st_intersection(clientLocation,ISA)
            FOSc_test <- FOSc
          }
          
          if (dim(FOSc_test)[1]!=0){
          DFstart_location <- data.frame(X=start_location[1], Y=start_location[2])
          DFstart_location <- DFstart_location %>% slice(rep(1:n(), each = dim(st_coordinates(FOSc_test))[1])) 
          DFend_location <- data.frame(X=end_location[1], Y=end_location[2])
          DFend_location <- DFend_location %>% slice(rep(1:n(), each = dim(st_coordinates(FOSc_test))[1])) 
          
          OPCost_id <- dodgr_paths(street_network_dodgr_sc_bicycle, DFstart_location, st_coordinates(FOSc_test), vertices = FALSE,pairwise = T)
          PDCost_id <- dodgr_paths(street_network_dodgr_sc_bicycle, st_coordinates(FOSc_test),DFend_location, vertices = FALSE, pairwise = T )     
          
          i <- 1
          TravelTimes <- data.frame("OP","PD","PM25")
          for (i in 1:length(OPCost_id)){
            OPCost_atributes <- street_network_dodgr_sc_bicycle[OPCost_id[[i]][[1]], ]
            OPtravel_time <- (sum(OPCost_atributes$time_weighted,na.rm = T)/60)/60
            OPCost_atributes$PM25 <- raster::extract(AirPollution,OPCost_atributes[,4:5])
            
            PDCost_atributes <- street_network_dodgr_sc_bicycle[PDCost_id[[i]][[1]], ]
            PDtravel_time <- (sum(PDCost_atributes$time_weighted,na.rm=T)/60)/60
            PDCost_atributes$PM25 <- raster::extract(AirPollution,PDCost_atributes[,4:5])
            
            
            wtPM25 <- sum(OPCost_atributes$PM25*(OPCost_atributes$time_weighted/60/60))+
              sum(PDCost_atributes$PM25*(PDCost_atributes$time_weighted/60/60))
            travel_time <- c(OPtravel_time,PDtravel_time,wtPM25)
            
            TravelTimes[i,] <- travel_time
          }
          
          #Step 2.2
          TravelTimes$X.OP. <- as.numeric(TravelTimes$X.OP.)
          TravelTimes$X.PD. <- as.numeric(TravelTimes$X.PD.)
          TravelTimes$X.PM25. <- as.numeric(TravelTimes$X.PM25.)
          TravelTimes$OPDCost <- TravelTimes$X.OP.+TravelTimes$X.PD.
          
          #Step 2.3
          TravelTimes$ACT <- Total_T-TravelTimes$OPDCost
          
          #Step 2.4
          TravelTimes$FOSc <- 1
          TravelTimes$FOSc[TravelTimes$ACT<MinAct] <- 0
          
          #Step 2.5 - no need. They are all done in DataFrame
          
          ######Step 3 - Identify final FOS and calculate accessibility  #######
          #Step 3.1
          #I need to define attractiveness in a better way than weighted area - this was decided to be on activity duration 
          #I will for now, create a variable based on cost, time and air pollution 
          beta_time <- -0.66 #From Lucero area in Bocarejo & Oviedo 2012
          beta_cost <- -11.7 #From Lucero area in Bocarejo & Oviedo 2012
          meanAP <- 16.7 #see detail explanation on Conceptualizing the STAM_v2.docx
          beta_PM25 <- ((0.54/(75.8+0.54))*beta_time)/meanAP #see detail explanation on Conceptualizing the STAM_v2.docx
          TravelTimes$wtCost_inv <- exp(beta_time*TravelTimes$OPDCost+beta_cost*Modes$Cost[m]+beta_PM25*(TravelTimes$X.PM25.*Modes$PM25[m]))
          
          #Step 3.2
          TravelTimes$FOScOpening <- 1
          TravelTimes$arrive <- StartTime+(TravelTimes$X.OP.*60*60)
          TravelTimes$leave <- StartTime+(TravelTimes$OPDCost+MinAct)*60*60
          
          TravelTimes$FOScOpening[TravelTimes$arrive<ActivityOpen] <- 0
          TravelTimes$FOScOpening[TravelTimes$leave>ActivityClose] <- 0
          
          #Step 3.3
          TravelTimes$DUR <- as.numeric(min(EndTime-(TravelTimes$X.PD.*60*60),ActivityClose)-TravelTimes$arrive)/60 #Result in min and transformed to hr
          
          #Step 3.4 
          TravelTimes$WAreaDur <- TravelTimes$DUR*TravelTimes$wtCost_inv*
            TravelTimes$FOSc*TravelTimes$FOScOpening #Multiply by FOSc to exclude those not valid
          TravelTimes$long <- as.data.frame(st_coordinates(FOSc_test))$X
          TravelTimes$lat<- as.data.frame(st_coordinates(FOSc_test))$Y
          #Step 3.5 & 3.6
          Access <- sum(TravelTimes$WAreaDur,na.rm = T)
          ModeAccessLoc <- c(Modes$Mode[m],Access)
          ModeAccess[m,] <- ModeAccessLoc
          TravelTimesModeList[[m]] <- TravelTimes
          } else { #Closes if for opportunities within ISA
            Access <- 0
            ModeAccessLoc <- c(Modes$Mode[m],NA)
            ModeAccess[m,] <- ModeAccessLoc
            TravelTimesModeList[[m]] <- NA
            test5 <- "no opportunities in ISA"
          } #Closes ELSE for opportunities within ISA
          
        } else { #Closes space-time prism check loop
          Access <- 0
          ModeAccessLoc <- c(Modes$Mode[m],NA)
          ModeAccess[m,] <- ModeAccessLoc
          TravelTimesModeList[[m]] <- NA
          test1 <- "no time for flex"
        } #Closes ELSE of space-time prism check loop
        FixedActivitiesDayResults[l,6+m] <- Access
      } #closes Modes loop (m)
      
      if (is.na(ModeAccess$X.Mode.[ModeAccess$X.access.==max(ModeAccess$X.access.,na.rm=T)][1])) {
        test2 <- "no room for flex"
      } else if (sum(as.numeric(ModeAccess$X.access.))==0) {
        test3 <- "no flex available"
      } else {
        mode_max_access <- ModeAccess$X.Mode.[ModeAccess$X.access.==max(ModeAccess$X.access.,na.rm=T)][1]
        listplace <- which(Modes$Mode==mode_max_access)
        maxTravelTimes <- TravelTimesModeList[[listplace]]
        bestOptionFlex <- data.frame(Place_type=FlexibleActivitiesR$value[f],
                                     Lon=as.numeric(as.character(maxTravelTimes$long[which(maxTravelTimes$wtCost_inv==max(maxTravelTimes$wtCost_inv, na.rm=T))])),
                                     Lat=as.numeric(as.character(maxTravelTimes$lat[which(maxTravelTimes$wtCost_inv==max(maxTravelTimes$wtCost_inv, na.rm=T))])),
                                     Start_trip_time=as.character(strftime(maxTravelTimes$leave[which(maxTravelTimes$wtCost_inv==max(maxTravelTimes$wtCost_inv, 
                                                                                                                                     na.rm=T))],
                                                                           format="%H:%M",tz="America/Bogota")),
                                     End_trip_time=as.character(strftime(maxTravelTimes$arrive[which(maxTravelTimes$wtCost_inv==max(maxTravelTimes$wtCost_inv, 
                                                                                                                                    na.rm=T))],
                                                                         format="%H:%M",tz="America/Bogota")),
                                     DayWeek=d)
        
        FlexibleActivitiesR$Status[f] <- 0 #Delete from flex activities (FlexibleActivitiesR)
        
        #Next steps needed to remove from client list in case a client has been served 
        ClientList$LonCheck <- bestOptionFlex$Lon
        ClientList$LatCheck <- bestOptionFlex$Lat
        ClientList$LonCheck <- ClientList$Lon-ClientList$LonCheck
        ClientList$LatCheck <- ClientList$Lat-ClientList$LatCheck
        
        ClientList$AllCheck <- abs(ClientList$LonCheck)+abs(ClientList$LatCheck)
        
        ClientList <- ClientList[ClientList$AllCheck>0.00005,]
        #we need to check that flexible activities for clients are deleted if all clients have been served
        if (dim(ClientList)[1]==0){
          FlexibleActivitiesR$Status[FlexibleActivitiesR$value=="client"] <- 0
          test4 <- "Out of clients"
        }
        FixedActivitiesDay1 <- rbind(FixedActivitiesDay[1:l,],bestOptionFlex)
        FixedActivitiesDay <- rbind(FixedActivitiesDay1,FixedActivitiesDay[(l+1):(dim(FixedActivitiesDay)[1]),])
        
        bestFlexResults <- bestOptionFlex
        bestFlexResults$foot <- NA
        bestFlexResults$motorcar <- NA
        
        FixedActivitiesDayResults1 <- rbind(FixedActivitiesDayResults[1:l,],bestFlexResults)
        FixedActivitiesDayResults <- rbind(FixedActivitiesDayResults1,FixedActivitiesDayResults[(l+1):(dim(FixedActivitiesDayResults)[1]),])
        
      } #closes if that checks for no activities (both NA and access = 0), operations are in ELSE
    } else {test <- "No flexible activities left"} #Ends if for no flexible activities left 
    
    CodeRunEnd <- Sys.time()
    CodeRunTime <- CodeRunEnd - CodeRunStart #Time for one trip to go through
  }#Step 3.7 - end trip loop (l)
  
  Results[[d]] <- FixedActivitiesDayResults
} #Step 3.8 - end day loop (d)

#Step 3.9 - end profile loop