####################################################################################

## read and clean raw data and add important columns like group id, seaonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "ebd_IN_relMay-2020.txt", 
                            sensitivepath = "ebd_relApr-2020_sensitive.txt")
{
  require(lubridate)
  require(tidyverse)
  
  # select only necessary columns
  preimp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
             "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY","LAST.EDITED.DATE",
             "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
             "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","LOCALITY",
             "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")
  
  # CATEGORY - species, subspecies, hybrid, etc.; COMMON.NAME - common name of species;
  # SCIENTIFIC NAME - scientific name; OBSERVATION.COUNT - count of each species observed in a list;
  # LOCALITY.ID - unique location ID; LOCALITY.TYPE - hotspot, etc.;
  # LATITUDE and LONGITUDE - coordinates; OBSERVATION.DATE - checklist date; 
  # TIME.OBSERVATIONS.STARTED - checklist start time; OBSERVER ID - unique observer ID;
  # PROTOCOL TYPE - stationary, traveling, historical, etc.; DURATION.MINUTES - checklist duration;
  # EFFORT.DISTANCE.KM - distance traveled; NUMBER.OBSERVERS - no. of birders;
  # ALL.SPECIES.REPORTED - indicates whether a checklist is complete or not;
  # GROUP.IDENTIFIER - unique ID for every set of shared checklists (NA when not shared);
  # SAMPLING.EVENT.IDENTIFIER - unique checlist ID
  
  nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                   na.strings = c(""," ",NA))
  nms = names(nms)
  nms[!(nms %in% preimp)] = "NULL"
  nms[nms %in% preimp] = NA
  
  nms1 = read.delim(sensitivepath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                    na.strings = c(""," ",NA))
  nms1 = names(nms1)
  nms1[!(nms1 %in% preimp)] = "NULL"
  nms1[nms1 %in% preimp] = NA
  
  # read data from certain columns only
  data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                    stringsAsFactors = F, na.strings = c(""," ",NA))
  
  sesp = read.delim(sensitivepath, colClasses = nms1, sep = "\t", header = T, quote = "", 
                     stringsAsFactors = F, na.strings = c(""," ",NA))
  
  # read sensitive species data

  
  # merge both data frames
  data = rbind(data,sesp)
  
  
  # create and write a file with common names and scientific names of all Indian species
  # useful for mapping
  temp = data %>%
    filter(REVIEWED == 0 | APPROVED == 1) %>%
    filter(CATEGORY == "species" | CATEGORY == "issf") %>%
    distinct(COMMON.NAME,SCIENTIFIC.NAME)
  
  write.csv(temp,"indiaspecieslist.csv", row.names=FALSE)
  
  ## choosing important columns required for further analyses
  
  # no of days in every month, and cumulative number
  days = c(31,28,31,30,31,30,31,31,30,31,30,31)
  cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)
  
  # create a column "group.id" which can help remove duplicate checklists
  data = data %>%
    mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))
  
  imp = c("GLOBAL.UNIQUE.IDENTIFIER","CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
          "LOCALITY.ID", "REVIEWED","APPROVED","SAMPLING.EVENT.IDENTIFIER","LAST.EDITED.DATE",
          #"LOCALITY.TYPE",
          "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
          "PROTOCOL.TYPE","LOCALITY",
          "DURATION.MINUTES","EFFORT.DISTANCE.KM",
          "ALL.SPECIES.REPORTED","group.id")
  
  imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
          "LOCALITY.ID", "REVIEWED","APPROVED",
          #"LOCALITY.TYPE",
          "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
          "PROTOCOL.TYPE",
          "DURATION.MINUTES","EFFORT.DISTANCE.KM",
          "ALL.SPECIES.REPORTED","group.id")
  
  ## setup eBird data ##
  
  ## filter species, slice by single group ID, remove repetitions
  ## remove repeats by retaining only a single group.id + species combination
  ## set date, add month, year and day columns using package LUBRIDATE
  ## add number of species/list length column (no.sp), for list length analyses (lla)
  
  
  data = data %>%
    group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
    dplyr::select(all_of(imp)) %>%
    mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
           month = month(OBSERVATION.DATE),
           day = day(OBSERVATION.DATE) + cdays[month],
           #daym = day(OBSERVATION.DATE),
           #week = week(OBSERVATION.DATE),
           #fort = ceiling(day/14),
           cyear = year(OBSERVATION.DATE)) %>%
    #mutate(LAST.EDITED.DATE = as.Date(LAST.EDITED.DATE), 
    #       eyear = year(LAST.EDITED.DATE)) %>%
    dplyr::select(-c("OBSERVATION.DATE")) %>%
    #dplyr::select(-c("LAST.EDITED.DATE")) %>%
    mutate(year = ifelse(day <= 151, cyear-1, cyear)) %>%
    group_by(group.id) %>% mutate(no.sp = n_distinct(COMMON.NAME)) %>%
    ungroup
  
  data = data %>% filter(year < 2020)
  
  assign("data",data,.GlobalEnv)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("data")), pos = ".GlobalEnv")
  
  # save workspace
  save.image("rawdata.RData")
  rm(data, pos = ".GlobalEnv")
}

##########################################################################################


## requires shapefiles and several packages - path1 = India; path2 = India States; 
## path3 = India Districts
## provide path to folder and name of file within

## this can be edited for more flexibility with grid sizes; current default is 25,50,100,200

## current default args are c("India","India_2011","India States","IndiaStates_2011","India Districts","IndiaDistricts_2011")

## saves a workspace image called "maps.RData"

createmaps = function(g1=25,g2=50,g3=100,g4=200,path1="India",name1="India_2011",
                      path2="in_states_2019",name2="in_states_2019",path3="in_dists_2019",name3="in_dist_2019",
                      papath = "PA_Boundaries_WII", paname = "pa_bounds")
{
  require(tidyverse)
  require(rgdal)
  require(sp)
  require(sf)
  
  # reading maps
  
  assign("indiamap",readOGR(path1,name1),.GlobalEnv)
  assign("statemap",readOGR(path2,name2),.GlobalEnv)
  names(statemap@data)[2] = "ST_NM"
  assign("statemap",statemap,.GlobalEnv)
  assign("districtmap",readOGR(path3,name3),.GlobalEnv)
  names(districtmap@data)[1:2] = c("DISTRICT","ST_NM")
  assign("districtmap",districtmap,.GlobalEnv)
  
  assign("pamap",readOGR(papath,paname),.GlobalEnv)
  pamap = pamap[,c(3,15)]
  assign("pamap",pamap,.GlobalEnv)
  
  # creating SPDF grids below that can be intersected with various maps and overlaid on to data
  
  bb = bbox(indiamap) # creates a box with extents from map
  cs = c(g1*1000/111111,g1*1000/111111)  # cell size g1 km x g1 km
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd) # create required grids
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd))) # create spatial grid data frame
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame") # SGDF to SPDF
  assign("gridmapg1",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g2*1000/111111,g2*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("gridmapg2",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g3*1000/111111,g3*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("gridmapg3",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g4*1000/111111,g4*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("gridmapg4",sp_grd_poly,.GlobalEnv)
  
  # indiamap = spTransform(indiamap,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  # not required here, CRS is NA
  
  assign("gridlevels",c(g1,g2,g3,g4),.GlobalEnv)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("districtmap", "statemap", "indiamap", "gridmapg1", "gridmapg2", 
                                            "gridmapg3", "gridmapg4", "pamap",
                                            "gridlevels")), 
     pos = ".GlobalEnv")
  
  save.image("maps.RData")
  
  rm(districtmap,statemap,indiamap,gridmapg1,gridmapg2, 
     gridmapg3,gridmapg4,pamap,
     gridlevels, pos = ".GlobalEnv")
}


## prepare data for analyses, add map variables, grids
## place the 'maps' workspace in working directory

addmapvars = function(datapath = "rawdata.RData", mappath = "maps.RData")
{
  require(tidyverse)
  require(data.table)
  require(sp)
  require(rgeos)
  
  load(datapath)
  
  ## add map details to eBird data
  
  load(mappath)
  load("clips.RData")
  
  # add columns with DISTRICT and ST_NM to main data 
  
  temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 
  
  rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
  coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
  proj4string(temp) = "+proj=longlat +datum=WGS84"
  temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
  temp = data.frame(temp) # convert into data frame for left_join
  temp = temp[,1:2]
  temp$group.id = rownames(temp) # add column to join with the main data
  data = left_join(temp,data)
  
  # add columns with protected area name to main data 
  
  temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 
  
  rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
  coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
  proj4string(temp) = "+proj=longlat +datum=WGS84"
  temp = over(temp,pamap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
  temp = data.frame(temp) # convert into data frame for left_join
  temp$group.id = rownames(temp) # add column to join with the main data
  data = left_join(temp,data)
  
  
  # add columns with GRID ATTRIBUTES to main data
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg1)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg1"
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg2)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg2"
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg3)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg3"
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg4)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg4"
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,g2clip)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "g2clip"
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,g3clip)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "g3clip"
  
  ## 
  
  assign("gridlevels",gridlevels,.GlobalEnv)
  
  assign("data",data,.GlobalEnv)
  rm(list=setdiff(ls(envir = .GlobalEnv), c("data","gridlevels")), 
     pos = ".GlobalEnv")
  
  save.image("data.RData")
  rm(data, gridlevels, pos = ".GlobalEnv")
  
}


########################################################################################

## remove all probable errors
## type can be "trends" or "range"
## to use in dataspeciesfilter()

completelistcheck = function(data)
{
  require(tidyverse)
  require(lubridate)
  
  # create 2 columns from the "TIME.OBSERVATIONS.STARTED' column
  temp = data.frame(data$TIME.OBSERVATIONS.STARTED)
  temp = temp %>%
    separate(data.TIME.OBSERVATIONS.STARTED, c("hr","min"))
  data = cbind(data,temp)
  
  # calculate speed and species/unit time (sut)
  data = data %>%
    mutate(speed = EFFORT.DISTANCE.KM*60/DURATION.MINUTES,
           sut = no.sp*60/DURATION.MINUTES) %>%
    mutate(hr = as.numeric(hr), min = as.numeric(min)) %>%
    mutate(end = floor((hr*60+min+DURATION.MINUTES)/60)) # caluclate time checklist ended
  
  temp = data %>%
    filter(ALL.SPECIES.REPORTED == 1, PROTOCOL.TYPE != "Incidental") %>%
    group_by(group.id) %>% slice(1)
  
  # exclude any list that may in fact be incomplete
  # set threshholds for speed and sut
  
  vel = 20
  time = 2
  
  # choose checklists without info on duration with 3 or fewers species
  grp = temp %>%
    filter(no.sp <= 3, is.na(DURATION.MINUTES)) %>%
    distinct(group.id)
  grp = grp$group.id
  
  # exclude records based on verious criteria 
  data = data %>%
    mutate(ALL.SPECIES.REPORTED = 
             case_when(ALL.SPECIES.REPORTED == 1 & (group.id %in% grp | speed > vel |
                                                      (sut < time & no.sp <= 3) | 
                                                      PROTOCOL.TYPE == "Incidental" | 
                                                      (!is.na(hr) & ((hr <= 4 & end <= 4) | 
                                                                       (hr >= 20 & end <= 28)))) ~ 0, 
                       ALL.SPECIES.REPORTED == 0 ~ 0,
                       TRUE ~ 1))
  
  data = data %>%
    select(-speed,-sut,-hr,-min,-end)
}

################################################

## remove vagrants
## to use in dataspeciesfilter()

removevagrants = function(data)
{
  migstatus = read.csv("Migratory Status - Migratory Status.csv")
  migspecies = migstatus %>%
    filter(Summer.Visitor == 1 | Winter.Visitor == 1 | 
             Strictly.Passage == 1) %>%
    select(eBird.English.Name)
  migspecies = as.vector(migspecies$eBird.English.Name)
  
  d = data %>%
    filter(COMMON.NAME %in% migspecies) %>%
    group_by(gridg4,month,COMMON.NAME) %>% summarize (nyear = n_distinct(year)) %>% ungroup %>%
    filter(nyear <= 3) %>% select(gridg4,month,COMMON.NAME)
  
  d = left_join(d,data)
  d = d %>%
    filter(year > 2013)
  
  save(d,file = "vagrantdata.RData")
  
  data = anti_join(data,d)
  return(data)
}



#################################################################

## select species for State of India's Birds, and species for historical and recent trends
## includes all diurnal endemics (endemicity) and essential species (SelectSpecies)

dataspeciesfilter = function(datapath = "data.RData")
{
  require(tidyverse)
  require(DataCombine)
  
  load(datapath)
  
  data$gridg1 = as.character(data$gridg1)
  data$gridg2 = as.character(data$gridg2)
  data$gridg3 = as.character(data$gridg3)
  data$gridg4 = as.character(data$gridg4)

  data = data %>%
    filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) %>%
    filter(REVIEWED == 0 | APPROVED == 1)
  
  data = data %>%
    mutate(timegroups = as.character(year)) %>%
    mutate(timegroups = ifelse(year <= 1999, "before 2000", timegroups)) %>%
    #mutate(timegroups = ifelse(year >= 1990 & year <= 1999, "1990-1999", timegroups)) %>%
    mutate(timegroups = ifelse(year > 1999 & year <= 2006, "2000-2006", timegroups)) %>%
    mutate(timegroups = ifelse(year > 2006 & year <= 2010, "2007-2010", timegroups)) %>%
    mutate(timegroups = ifelse(year > 2010 & year <= 2012, "2011-2012", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2013, "2013", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2014, "2014", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2015, "2015", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2016, "2016", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2017, "2017", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2018, "2018", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2019, "2019", timegroups))
  
  data = removevagrants(data)
  data = completelistcheck(data)
  
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% summarize(lists = n_distinct(group.id), year = round(median(year)))
  
  data1 = data
  data1 = data1 %>% select(-CATEGORY,-LOCALITY.ID,-REVIEWED,-APPROVED,
                           -TIME.OBSERVATIONS.STARTED,-PROTOCOL.TYPE,
                           -DURATION.MINUTES,-EFFORT.DISTANCE.KM,-day,-cyear)
  
  assign("data",data1,.GlobalEnv)
  assign("databins",databins,.GlobalEnv)
  
  assign("gridlevels",gridlevels,.GlobalEnv)

  rm(list=setdiff(ls(envir = .GlobalEnv), c("data","databins",
                                            "gridlevels")), 
     pos = ".GlobalEnv")
  
  save.image("dataforanalyses.RData")

  rm(data, databins, gridlevels, pos = ".GlobalEnv")
}


######################################################################################
################################################################


## ensure that the working directory has list of India's birds with scientific names 
## (just a safety mechanism for the function to work for small subsets, needs to be enabled if required)
## only need to input data, the species of interest and the complete list of India's bird species
## also groupspecs if required (a dataframe with all relevant list level info), it is defaulted to data

expandbyspecies = function(data, species)
{
  require(tidyverse)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$g3clip = as.factor(data$g3clip)
  data$g2clip = as.factor(data$g2clip)
  
  data$timegroups = as.factor(data$timegroups)
  
  ## considers only complete lists
  
  checklistinfo = data %>%
    distinct(gridg1,gridg2,gridg3,gridg4,
             ALL.SPECIES.REPORTED,OBSERVER.ID,
             NAME,LATITUDE,LONGITUDE,g3clip,g2clip,REP_AREA,
             DISTRICT,ST_NM,
             group.id,month,year,no.sp,timegroups)
  
  checklistinfo = checklistinfo %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% slice(1) %>% ungroup
  
  ## expand data frame to include all bird species in every list
  
  expanded = checklistinfo
  expanded$COMMON.NAME = species
  
  ## join the two, deal with NAs next
  
  expanded = left_join(expanded,data)
  expanded = expanded %>%
    dplyr::select(-c("COMMON.NAME","gridg2","gridg4","OBSERVER.ID",
                     "ALL.SPECIES.REPORTED","group.id","year",
                     "LATITUDE","LONGITUDE","g3clip","g2clip","REP_AREA",
                     "DISTRICT","ST_NM"))
  
  ## deal with NAs
  
  expanded = expanded %>% mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, is.na(OBSERVATION.COUNT), "0"))
  
  
  expanded = expanded %>%
    mutate(OBSERVATION.NUMBER=OBSERVATION.COUNT,
           OBSERVATION.COUNT=replace(OBSERVATION.COUNT, OBSERVATION.COUNT != "0", "1"))
  
  
  
  expanded$OBSERVATION.COUNT = as.numeric(expanded$OBSERVATION.COUNT)
  expanded$OBSERVATION.NUMBER = as.numeric(expanded$OBSERVATION.NUMBER)
  
  return(expanded)
}



#######################################################################################

freqtrends = function(data,species,databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018,2019),
                      error=T,type="full",nsim=1000)
{
  require(tidyverse)
  require(lme4)
  require(VGAM)
  require(parallel)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)

  ## considers only complete lists
  
  data = data %>%
    filter(ALL.SPECIES.REPORTED == 1)
  
  data$month = as.factor(data$month)
  
  if (!species %in% unique(data$COMMON.NAME))
    return(paste(species,"is not a valid species name for the region selected"))
  
  
  data$timegroups = as.factor(data$timegroups)
  temp = data %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3,month)
  data = temp %>% left_join(data)
  
  ## calculate a median list length to use to predict
  
  datay = data %>%
    group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
    group_by(gridg3,gridg1) %>% summarize(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% summarize(medianlla = mean(medianlla)) %>%
    summarize(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  ## expand dataframe to include absences as well
  
  ed = expandbyspecies(data,species)
  tm = unique(data$timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  ## the model
  
  if (type == "full")
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg3/gridg1), data = ed, 
               family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  if (type == "part")
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg1), data = ed, 
               family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  if (type == "prot")
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|NAME), data = ed, 
               family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  
  ## prepare a new data file to predict
  
  f = data.frame(unique(tm))
  f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
  names(f) = "timegroups"
  f$month = rep(unique(ed$month), each = length(f$timegroups)/length(unique(ed$month)))
  ltemp = data.frame(timegroups = f$timegroups,
                     no.sp = medianlla, month = f$month)
  
  f1 = data.frame(timegroups = tm)
  
  f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
  f2$se = numeric(length(ltemp$no.sp))
  f2$timegroups = ltemp$timegroups
  
  if (type %in% c("full","part","prot"))
  {
    
    ## bootstrap to get errors
    
    if (error)
    {
      predFun = function(m1) {
        predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
      }
      
      cr = max(1, detectCores() - 4)
      cl = makeCluster(cr)
      clusterEvalQ(cl, library(lme4))
      
      pred = bootMer(m1, nsim = nsim, FUN = predFun, parallel = "snow", seed = 1000,
                     use.u = FALSE, type = "parametric", ncpus = cr, cl = cl)
      
      stopCluster(cl)
      
      for (i in 1:length(ltemp$no.sp))
      {
        f2$freq[i] = median(na.omit(pred$t[,i]))
        f2$se[i] = sd(na.omit(pred$t[,i]))
      }
      
      f2$freqt = clogloglink(f2$freq,inverse = T)
      f2$cl = clogloglink((f2$freq-f2$se),inverse = T)
      f2$set = f2$freqt-f2$cl
      
      fx = f2 %>%
        filter(!is.na(freqt) & !is.na(set)) %>%
        group_by(timegroups) %>% summarize(freq = mean(freqt), se = sqrt(sum(set^2)/n())) 
      
      f1 = left_join(f1,fx)
    }
    
    if(!isTRUE(error))
    {
      f2$freq = predict(m1, newdata = ltemp,
                        type="response", re.form = NA)
      f1 = f2 %>%
        filter(!is.na(freq)) %>%
        group_by(timegroups) %>% summarize(freq = mean(freq))
      f1$se = NA
    }
  }
  
  if (type == "null")
  {
    m1 = glm(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups, data = ed, 
               family=binomial(link = 'cloglog'))
    
    fx = predict(m1, ltemp, se.fit = T, "response")
    f2$freq = fx$fit
    f2$se = fx$se.fit
    
    fx = f2 %>%
      filter(!is.na(freq) & !is.na(se)) %>%
      group_by(timegroups) %>% summarize(freq = mean(freq), se = sqrt(sum(se^2)/n())) 
    
    f1 = left_join(f1,fx)
  }
  
  f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                   "2011-2012","2013","2014","2015","2016","2017","2018","2019"))
  f1 = f1[order(f1$timegroups),]
  names(f1)[1] = "timegroupsf"
  mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
                                  "2011-2012","2013","2014","2015","2016","2017","2018","2019"), 
                  timegroups = as.numeric(databins))
  f1 = left_join(f1,mp)
  f1$species = species
  
  return(f1)
  
}



###########################################################

errordiv = function(x1,x2,se1,se2)
{
  r = x1/x2
  t = data.frame(se1/x1,se2/x2)
  ser = r*sqrt(t[,1]^2 + t[,2]^2)
  a = data.frame(freq = numeric(length(r)))
  a$freq = r
  a$se = ser
  return(a)
}

erroradd = function(vec)
{
  err = sqrt(sum(vec^2))
  return(err)
}




#################################################################################

## standardize trends

stdtrends = function(trends)
{
  require(tidyverse)
  
  modtrends = na.omit(trends)
  
  tg = unique(modtrends$timegroups)
  
  recenttrends = modtrends %>%
    filter(timegroups %in% tg) %>%
    group_by(species) %>% mutate(freq1 = first(freq)) %>% ungroup() %>%
    group_by(species) %>% mutate(se1 = first(se)) %>% ungroup() %>%
    mutate(nmfreqbyspec = as.numeric(errordiv(freq,freq1,se,se1)[,1])) %>%
    mutate(nmsebyspec = as.numeric(errordiv(freq,freq1,se,se1)[,2])) %>%
    mutate(nmfreq = freq/max(freq1))
  recenttrends$nmsebyspec[recenttrends$timegroups == min(recenttrends$timegroups)] = 0
  
  
  recenttrends$nmfreqbyspec = recenttrends$nmfreqbyspec*100
  recenttrends$nmsebyspec = recenttrends$nmsebyspec*100
  
  recenttrends = recenttrends %>%
    dplyr::select(timegroupsf,timegroups,species,nmfreqbyspec,nmsebyspec)
  
  return(recenttrends)
}

