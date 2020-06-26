

list.categories <- c("Dairy","Grains",
                     "Bread","Meat (Frozen)",
                     "Meat/Other Protein (Canned)","Vegetables/Fruits (Canned)",
                     "Personal and Household Care Items","Notes (if needed)"  )

each <- form.responces %>% filter(`Pantry Name:` == 'Clay United Methodist Food Pantry')%>%
  # each <- form.responces %>% filter(`Pantry Name:` == input$report.pantry)%>%
  mutate_at(vars(4:8), 
            funs(as.character))%>%
  select(-Timestamp, -avFood,-avDairy, -avStock, -avFreeze, -avFridge)%>%
  arrange(desc(`Week ending:`))%>%
  pivot_longer(cols = -c(1:2), 
               names_to = "key",
               values_to = "val", values_ptypes = list(val = 'character'))%>%
  pivot_wider(names_from = `Week ending:`, values_from = val)%>%
  separate(col = "key", sep = " \\[", into=c("Category","Type"))%>%
  select(-`Pantry Name:`) %>%
  mutate(Type = 
           if_else(condition = is.na(Type), 
                   true = Category, 
                   false = substr(Type,1,nchar(Type)-1)))%>%
  mutate(Category = if_else(condition = substr(Category,1,6)=="Number",true = "Numeric Summary",false = Category))


each <- each %>%
  filter(Category %in% input$report.filters)%>%
  mutate(Category =  factor(Category, levels = input$report.filters)) %>%
  arrange(Category) %>%
  mutate(Category = if_else(duplicated(Category), true = "",false = as.character(Category)))

each2 <- datatable(data = each,
                   rownames = FALSE,
                   plugins = "ellipsis",
                   colnames = c(" " = "Type"),
                   options = list(  pageLength = 50,
                                    scrollX = TRUE, 
                                    autowidth = T,
                                    fixedColumns = list(leftColumns = 2),
                                    columnDefs = list(list(
                                      # targets = c(3:ncol(each)-1),
                                      render = JS("$.fn.dataTable.render.ellipsis( 17, false )"),
                                      width = '300px', targets = "_all"
                                    ))#end column defs
                   )#end options
) %>%
  formatStyle(columns = c(3:ncol(each)),
              backgroundColor = styleEqual(c("None","Low", "Medium","High"), c('red', 'orange', 'yellow',"lightblue")),
  )%>%
  formatStyle(columns = c("Category"), fontWeight = 'bold', `text-align` = 'left')#end formatStyle

# path <- "G:\\My Drive\\GoogleSync\\FoodPantries\\PantryMap_Trial\\src" # folder containing dataTables.rowsGroup.js
# dep <- htmltools::htmlDependency(
#     "RowsGroup", "2.0.0", 
#     path, script = "dataTables.rowsGroup.js")
# each2$dependencies <- c(each2$dependencies, list(dep))


each <- each %>%
  filter(Category %in% input$report.filters)%>%
  mutate(Category =  factor(Category, levels = input$report.filters)) %>%
  arrange(Category) %>%
  mutate(Category = if_else(duplicated(Category), true = "",false = as.character(Category)))

each2 <- datatable(data = each,
                   rownames = FALSE,
                   plugins = "ellipsis",
                   colnames = c(" " = "Type"),
                   options = list(  pageLength = 50,
                                    scrollX = TRUE, 
                                    autowidth = T,
                                    fixedColumns = list(leftColumns = 2),
                                    columnDefs = list(list(
                                      # targets = c(3:ncol(each)-1),
                                      render = JS("$.fn.dataTable.render.ellipsis( 17, false )"),
                                      width = '300px', targets = "_all"
                                    ))#end column defs
                   )#end options
) %>%
  formatStyle(columns = c(3:ncol(each)),
              backgroundColor = styleEqual(c("None","Low", "Medium","High"), c('red', 'orange', 'yellow',"lightblue")),
  )%>%
  formatStyle(columns = c("Category"), fontWeight = 'bold', `text-align` = 'left')#end formatStyle

# path <- "G:\\My Drive\\GoogleSync\\FoodPantries\\PantryMap_Trial\\src" # folder containing dataTables.rowsGroup.js
# dep <- htmltools::htmlDependency(
#     "RowsGroup", "2.0.0", 
#     path, script = "dataTables.rowsGroup.js")
# each2$dependencies <- c(each2$dependencies, list(dep))
  
  
#3rd Tab working

form.responces$realDate <- as.Date(form.responces$`Week ending:`, format = "%m/%d/%Y")

ggplot(data = form.responces, mapping = aes(x = `Week ending:`, y = avStock,
                                            color = `Pantry Name:`))+
  geom_point()+
  geom_line()+
  theme_classic()+
  theme(legend.position="bottom")+
  scale_y_continuous(#limits = 1,3,
                     breaks = c(1,2,3),
                     labels = c("Low", "Medium", "High")
)


#Renames
#categories

df <- form.responces
cols2 <- c(41:43)
cols2 <- c(9:24,36:37,41:43)


d1 <- df  %>%
  mutate(numlow = rowSums(select(.,cols2) == "Low", na.rm = T),
         nummed = rowSums(select(.,cols2) == "Medium", na.rm = T),
         numhi = rowSums(select(.,cols2) == "High", na.rm = T),
         totl = numlow + nummed +numhi,
         avquant = (numlow + nummed*2 +numhi*3)/totl) 
cols2 <- "Dairy"
cols2 <- "Dairy|Grains|Bread|Meat|Vegetables"


d2 <- df  %>%
  mutate(numlow = rowSums(select(.,matches(paste0("^",cols2))) == "Low", na.rm = T),
         nummed = rowSums(select(.,matches(paste0("^",cols2))) == "Medium", na.rm = T),
         numhi = rowSums(select(.,matches(paste0("^",cols2))) == "High", na.rm = T),
         totl = numlow + nummed +numhi,
         avquant = (numlow + nummed*2 +numhi*3)/totl) 
identical(d1$avquant, d2$avquant) 





dat <- data.frame(
  A = c("fnufnufroufrcnoonfrncacfnouafc", "fanunfrpn frnpncfrurnucfrnupfenc"),
  B = c("DZDOPCDNAL DKODKPODPOKKPODZKPO", "AZERTYUIOPQSDFGHJKLMWXCVBN")
)

datatable(
  each, 
  plugins = "ellipsis",
  options = list(
    paging = F,
    columnDefs = list(list(
      targets = c(3:5),
      render = JS("$.fn.dataTable.render.ellipsis( 17, false )")
    ))
  )
)




a <- form.responces %>%
  group_by(`Pantry Name:`)%>%
  mutate(n = n())%>%
  ungroup()%>%
  filter(n>1)



map.size.choices <- c( "Families Served" = "`Number of families served this week:`",                       
                       "Individuals Served" ="`Number of individuals served this week:`",
                       "New Families Served" ="`Number of NEW families  this week:`",                         
                       "New Individuals Served"="`Number of NEW individuals served this week:`",
                       "Estimated unfilled requests" ="`Number of requests that you could not fill (estimate):`" )
cap <- map.size.choices[2]
# cap <- c("Individuals Served" ="Number of individuals served this week:")

fr.sub <- form.responces %>%
  filter(`Week ending:` == max(weeks))
if (nrow(fr.sub) != length(unique(fr.sub$`Pantry Name:`))){
  print("Someone entered a form twice, taking the most recent")
  fr.sub %>% 
    group_by(`Pantry Name:`) %>% 
    arrange(Timestamp) %>%  
    slice(n())
}

as <- left_join(x = locations, y = fr.sub, by = c("FormName" ="Pantry Name:"))

as <- as %>% mutate(act = get(str_remove_all(cap, "[``]")))
as$act

# as <- as%>%
#   mutate(size = 3 + 8 * get(input$map.size.variable) / max(get(input$map.size.variable),na.rm = T))
# as$size <- 3 + 8 * as$`Number of individuals served this week:` / max(as$'Number of individuals served this week:',na.rm = T)
print(as$size)
as[is.na(as$size),]$size <- 2
# print(paste(sort(unique(as$size))[2]))


library(tidycensus)
v18 <- load_variables(year = 2018, dataset = "acs5")
vars <- c("Households" = "B08014_001" ,
          "NoVehicles" = "B08014_002",
          "PovTotal" = "B17001_001",
          "PovBelow" = "B17001_002")
dat <- get_acs(geography = "tract",variables = vars, state = "IN", county = 141, geometry = T, output = "wide")
dat <- dat %>%
  mutate(PovRate = PovBelowE / PovTotalE,
         NoVeRate = NoVehiclesE/HouseholdsE,
         )

ggplot()+
  geom_sf(data = dat, mapping = aes(fill =NoVeRate))
ggplot()+
  geom_sf(data = dat, mapping = aes(fill =PovRate))


leaflet()
