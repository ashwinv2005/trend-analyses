source('~/GitHub/trend-analyses/functions.R')
createmaps()
source('~/GitHub/trend-analyses/functions.R')
readcleanrawdata("ebd_IN_relMay-2020.txt","ebd_relApr-2020_sensitive.txt") 
source('~/GitHub/trend-analyses/functions.R')
addmapvars()
source('~/GitHub/trend-analyses/functions.R')
dataspeciesfilter()

#################################

map = read.csv("Map to Other Lists - map.csv")

map = map %>%
  group_by(eBird.English.Name.2019) %>% slice(1)

mig = read.csv("Migratory Status - Migratory Status.csv")
mig = left_join(map,mig,by = c("eBird.English.Name.2018" = "eBird.English.Name"))
mig = mig %>% select(-eBird.English.Name.2018,-eBird.Scientific.Name.2018,-India.Checklist.Name,
                     -India.Scientific.Name,-IUCN,-Schedule,-CITES.Appendix,-CMS.Appendix,-eBird.Scientific.Name)
names(mig)[1:2] = c("eBird.English.Name","eBird.Scientific.Name")

write.csv(mig,"Migratory Status - Migratory Status.csv",row.names=F)

#################################






