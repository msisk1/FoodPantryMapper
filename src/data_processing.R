rm(list=ls(all=TRUE)) # clear memory

# install.packages("googlesheets4")

library(googlesheets4)
library(sf)
library(tidyverse)


tab.values <- function(df, starts.with){
  #A function to tabulate a series of fields. Needs to be either indexes or a string seperated by |
  if (is.integer(starts.with)){
    # print("int")
    d2 <- df  %>%
      mutate(numlow = rowSums(select(.,starts.with) == "Low", na.rm = T),
             nummed = rowSums(select(.,starts.with) == "Medium", na.rm = T),
             numhi = rowSums(select(.,starts.with) == "High", na.rm = T),
             totl = numlow + nummed +numhi,
             avquant = (numlow + nummed*2 +numhi*3)/totl) 
  }
  if (is.character(starts.with)){
    # print("char")
    d2 <- df  %>%
      mutate(numlow = rowSums(select(.,matches(paste0("^",starts.with))) == "Low", na.rm = T),
             nummed = rowSums(select(.,matches(paste0("^",starts.with))) == "Medium", na.rm = T),
             numhi = rowSums(select(.,matches(paste0("^",starts.with))) == "High", na.rm = T),
             totl = numlow + nummed +numhi,
             avquant = (numlow + nummed*2 +numhi*3)/totl) 
  }
  return(d2$avquant)
}#end TabValues


setwd("C:\\Users\\msisk1\\Documents\\Rprojs\\FoodPantryMapper") #only necessary for command line running

load(file = "St_Joe_PantryMap/allData.RData")

old.length <- nrow(form.responces)
remove(locations)
remove(form.responces)


latlong <- 4326

sheet.gid.sj <- "1XE6yGgP_t7D5iAPC3vjJhTPZgjw5umAmeqy2k3sCYsI"
sheet.gid.elk <- "1rRGbtQdfga8iXiJiM4YlMWxAF9BCBTI2bphrKk5k-Bo"

#st.joe section
form.responces <- read_sheet(sheet.gid.sj, sheet = "Form Responses 1")
1
locations <- read_sheet(sheet.gid.sj, sheet = "Locations/Hours")
#converting locations into spatial data
locations <- locations%>%
  sf::st_as_sf(coords = c("X","Y")) %>% 
  sf::st_set_crs(latlong) 

#removing duplicates
form.responces <- form.responces %>%
  arrange(Timestamp) %>%
  group_by(`Pantry Name:`,`Week ending:`) %>% 
  summarise_all(last)%>%
  ungroup()



if (old.length == nrow(form.responces)){
  print("No new responces, not updating")
}else{
  print(paste((nrow(form.responces) - old.length), "new responces, updating the app"))
  
  form.responces$`Week ending:` <- as.Date(form.responces$`Week ending:`, format = "%m/%d/%Y")
  
  #Making this work with regular expressions instead of just column IDs.
  form.responces$avFood <- tab.values(df = form.responces, starts.with = "Dairy|Grains|Bread|Meat|Vegetables")
  form.responces$avDairy <- tab.values(df = form.responces, starts.with = "Dairy")
  form.responces$avStock <- tab.values(df = form.responces, starts.with = "Dairy|Grains|Bread|Meat|Vegetables|Personal")
  form.responces$avFridge <- tab.values(df = form.responces, starts.with = "Estimated levels of storage space available \\[Refr")
  form.responces$avFreeze <- tab.values(df = form.responces, starts.with = "Estimated levels of storage space available \\[Fre")
  
  
  
  form.responces<- form.responces %>%
    select("Timestamp",                                                  
           "Pantry Name:",                                               
           "Week ending:" ,                                              
           "Number of families served this week:"     ,                  
           "Number of individuals served this week:"   ,  
           "Number of NEW families  this week:"         ,                
           "Number of NEW individuals served this week:" ,               
           "Number of requests that you could not fill (estimate):"  , 
           "Estimated levels of storage space available [Freezer]"  ,      
           "Estimated levels of storage space available [Refridgerator]"  ,
           "Dairy [Cheese]"                               ,              
           "Dairy [Eggs]"                                             ,  
           "Dairy [Milk]"                                              , 
           "Grains [Cereal]"                                            ,
           "Grains [Convenience (eg Hamburger Helper)]"                 ,
           "Grains [Pasta]"                                             ,
           "Grains [Rice]"                                              ,
           "Bread [Sandwich]"                                           ,
           "Bread [Other]"                                              ,
           "Meat (Frozen) [Beef]"                                       ,
           "Meat (Frozen) [Chicken]"                                    ,
           "Meat (Frozen) [Pork]"  ,
           "Meat (Frozen) [Fish]"   ,                                     
           "Meat/Other Protein (Canned) [Beef/Beef Stew]"               ,
           "Meat/Other Protein (Canned) [Chicken]"                      ,
           "Meat/Other Protein (Canned) [Pork]"                         ,
           "Meat/Other Protein (Canned) [Tuna/other fish]"              ,
           "Meat/Other Protein (Canned) [Other - Peanut Butter]"        ,
           "Vegetables/Fruits (Canned) [Vegetables]"                    ,
           "Vegetables/Fruits (Canned) [Fruit]"  ,
           "Vegetables/Fruits (Canned) [Juice]"   ,                     
           "Personal and Household Care Items [Deodorant]" ,             
           "Personal and Household Care Items [Feminine products]"      ,
           "Personal and Household Care Items [Soap/hand soap]"         ,
           "Personal and Household Care Items [Toilet paper]"           ,
           "Personal and Household Care Items [Toothpaste/toothbrushes]",
           "Personal and Household Care Items [Shampoo]"                ,
           "Personal and Household Care Items [Dish soap]"              ,
           "Personal and Household Care Items [Laundry soap]"           ,
           "Personal and Household Care Items [Paper towels]"           ,
           "Personal and Household Care Items [Razors]"                  ,                        
           "Notes (if needed)" ,
           "avFood"             ,                                        
           "avDairy"             ,                                       
           "avStock",
           "avFreeze",
           "avFridge")
  
  
  form.responces$`Number of families served this week:` <- as.integer(form.responces$`Number of families served this week:` )
  
  save(form.responces,locations,file = "PantryMap_Trial/allData.RData")
  
  library(shiny)
  library(rsconnect)
  deployApp("PantryMap_Trial", forceUpdate = TRUE)
  
  
  
    
}#end else no new responces



#elkhart section:

form.responces.elk <- read_sheet(sheet.gid.elk, sheet = "Form Responses 1")
1
locations.elk <- read_sheet(sheet.gid.elk, sheet = "Locations/Hours")
#converting locations into spatial data
locations.elk <- locations.elk%>%
  sf::st_as_sf(coords = c("X","Y")) %>% 
  sf::st_set_crs(latlong) 

#removing duplicates
form.responces.elk <- form.responces.elk %>%
  arrange(Timestamp) %>%
  group_by(`Pantry Name:`,`Week ending:`) %>% 
  summarise_all(last)%>%
  ungroup()



if (old.length == nrow(form.responces.elk)){
  print("No new responces, not updating")
}else{
  print(paste((nrow(form.responces.elk) - old.length), "new responces, updating the app"))
  
  form.responces.elk$`Week ending:` <- as.Date(form.responces.elk$`Week ending:`, format = "%m/%d/%Y")
  
  #Making this work with regular expressions instead of just column IDs.
  form.responces.elk$avFood <- tab.values(df = form.responces.elk, starts.with = "Dairy|Grains|Bread|Meat|Vegetables")
  form.responces.elk$avDairy <- tab.values(df = form.responces.elk, starts.with = "Dairy")
  form.responces.elk$avStock <- tab.values(df = form.responces.elk, starts.with = "Dairy|Grains|Bread|Meat|Vegetables|Personal")
  form.responces.elk$avFridge <- tab.values(df = form.responces.elk, starts.with = "Estimated levels of storage space available \\[Refr")
  form.responces.elk$avFreeze <- tab.values(df = form.responces.elk, starts.with = "Estimated levels of storage space available \\[Fre")
  
  
  
  form.responces.elk<- form.responces.elk %>%
    select("Timestamp",                                                  
           "Pantry Name:",                                               
           "Week ending:" ,                                              
           "Number of families served this week:"     ,                  
           "Number of individuals served this week:"   ,  
           "Number of NEW families  this week:"         ,                
           "Number of NEW individuals served this week:" ,               
           "Number of requests that you could not fill (estimate):"  , 
           "Estimated levels of storage space available [Freezer]"  ,      
           "Estimated levels of storage space available [Refrigerator]"  ,
           "Dairy [Cheese]"                               ,              
           "Dairy [Eggs]"                                             ,  
           "Dairy [Milk]"                                              , 
           "Grains [Cereal]"                                            ,
           "Grains [Convenience (eg Hamburger Helper)]"                 ,
           "Grains [Pasta]"                                             ,
           "Grains [Rice]"                                              ,
           "Bread [Sandwich]"                                           ,
           "Bread [Other]"                                              ,
           "Meat (Frozen) [Beef]"                                       ,
           "Meat (Frozen) [Chicken]"                                    ,
           "Meat (Frozen) [Pork]"  ,
           "Meat (Frozen) [Fish]"   ,                                     
           "Meat/Other Protein (Canned) [Beef/Beef Stew]"               ,
           "Meat/Other Protein (Canned) [Chicken]"                      ,
           "Meat/Other Protein (Canned) [Pork]"                         ,
           "Meat/Other Protein (Canned) [Tuna/other fish]"              ,
           "Meat/Other Protein (Canned) [Other - Peanut Butter, soups]"        ,
           "Vegetables/Fruits  [Vegetables (canned)]"                    ,
           "Vegetables/Fruits  [Fruit (canned)]"  ,
           # "Vegetables/Fruits (Canned) [Juice]"   ,                     
           "Personal and Household Care Items [Deodorant]" ,             
           "Personal and Household Care Items [Feminine products]"      ,
           "Personal and Household Care Items [Soap/hand soap]"         ,
           "Personal and Household Care Items [Toilet paper]"           ,
           "Personal and Household Care Items [Toothpaste/toothbrushes]",
           "Personal and Household Care Items [Shampoo]"                ,
           "Personal and Household Care Items [Dish soap]"              ,
           "Personal and Household Care Items [Laundry soap]"           ,
           "Personal and Household Care Items [Paper towels]"           ,
           "Personal and Household Care Items [Razors]"                  ,                        
           "Notes (if needed)" ,
           "avFood"             ,                                        
           "avDairy"             ,                                       
           "avStock",
           "avFreeze",
           "avFridge")
  
  
  form.responces.elk$`Number of families served this week:` <- as.integer(form.responces.elk$`Number of families served this week:` )
  save(form.responces.elk,locations.elk,file = "Elkhart_PantryMap/allData.RData")
  
  # library(shiny)
  # library(rsconnect)
  # deployApp("Elkahrt_PantryMap", forceUpdate = TRUE)
  
  
  
  }

