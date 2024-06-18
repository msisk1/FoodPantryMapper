#ST JOE!!!!

library(shiny)
library(leaflet)
library(tidyverse)
library(DT)
library(sf)
library(shinythemes)
library(plotly)

#TODO:
#Time Series Tab
#Map Tab: Size chooser, default date
#Data tab: sort the dates, fix the sizes.

if(FALSE){
    rm(list=ls(all=TRUE)) # clear memory
    setwd("./PantryMap_Trial")
}

load("allData.RData")

locations$popup <- paste("<b>",locations$`Pantry Name`,"</b><br><hl>",
                      locations$Address, "<br>",
                      locations$Phone, "<br><br>",
                      "<b>Times Open: </b>",locations$`Times Open`,"<br>",
                      "<b>Restrictions: </b>",locations$`Who can go`,"<br>",
                      sep="")


plot.choice.list <- c("Average All Stock" = "avStock",
  "Average Dairy Stock" = "avDairy",
  "Average Food Stock" = "avFood",
  "Families Served" = "`Number of families served this week:`",                       
  "Individuals Served" ="`Number of individuals served this week:`",
  "New Families Served" ="`Number of NEW families  this week:`",                         
  "New Individuals Served"="`Number of NEW individuals served this week:`",
  "Estimated unfilled requests" ="`Number of requests that you could not fill (estimate):`" ,    
  "Storage, Freezer" = "avFreeze",
  "Storage, Fridge" ="avFridge"
)
map.color.choices <- c("Average All Stock" = "avStock",
                       "Average Dairy Stock" = "avDairy",
                       "Average Food Stock" = "avFood")
map.size.choices <- c( "Families Served" = "`Number of families served this week:`",                       
                       "Individuals Served" ="`Number of individuals served this week:`",
                       "New Families Served" ="`Number of NEW families  this week:`",                         
                       "New Individuals Served"="`Number of NEW individuals served this week:`",
                       "Estimated unfilled requests" ="`Number of requests that you could not fill (estimate):`" )


list.categories <- c("Dairy" ="Dairy",
                     "Grains"="Grains",
                     "Bread" ="Bread",
                     "Meat (Frozen)"="Meat (Frozen)",
                     "Meat (Canned)"="Meat/Other Protein (Canned)",
                     "Vegetables/Fruits (Canned)"="Vegetables/Fruits (Canned)",
                     "Personal Care"="Personal and Household Care Items",
                     "Numeric Summaries" ="Numeric Summary", 
                     "Storage"="Estimated levels of storage space available", 
                     "Notes" = "Notes (if needed)"  )


pal <- colorNumeric(
    palette = colorRampPalette(c('red', 'blue'))(length(form.responces$avFood)), 
    domain = c(0,3))
weeks <- unique(form.responces$'Week ending:')
week.selected <- max(unique(form.responces %>%group_by(`Week ending:`) %>% summarise(n = n()) %>% filter(n>3))$`Week ending:`)


# Define UI for application that draws a histogram
ui <- navbarPage("Pantry Explorer v2", theme = shinytheme("yeti"), id = "navpanel",
    tabPanel("Map",
            sidebarLayout(
                sidebarPanel(
                    selectInput(inputId = "report.date",
                                label = "Report Date",
                               choices = sort(weeks),
                               selected = week.selected),
                    radioButtons(inputId = "map.color.variable",
                                 choices =map.color.choices,
                                 label = HTML("<b>Map Color: </b>"),
                                 selected = "avFood"),
                    radioButtons(inputId = "map.size.variable",
                                 choices = map.size.choices,
                                 label = HTML("<b>Map Point Size: </b>"),
                                 selected = "`Number of individuals served this week:`")
                ),
        
                # Show a plot of the generated distribution
                mainPanel(
                    tags$style(type = "text/css", "html, body {width:100%;height:100%}",
                               ".leaflet .legend i{
                              border-radius: 50%;
                              width: 10px;
                              height: 10px;
                              margin-top: 4px;
                              }
                            "),
                    leafletOutput("mymap", height = "90vh")
                )#end MainPanel
            )#endSideBar
    ),#end Tab Panel MAP
    tabPanel("Individual Reports", value = "IndReport",
             sidebarLayout(
                 sidebarPanel(
                    selectInput(inputId = "report.pantry",
                                 label = "Pantry",
                                 choices = unique(form.responces$`Pantry Name:`)),
                   checkboxGroupInput(inputId = "report.filters",
                                      label = "Categories",
                                      choices = list.categories,
                                      selected = list.categories)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     # htmlOutput("indreport"),
                     DT::dataTableOutput("tableView")
                 )#end MainPanel
             )#endSideBar
             
     ),#end tabpabel Individual Reports
    tabPanel(title = "Time Series", value = "TimeSeries",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "plot.variable",
                                 label = "Variable",
                                 choices = plot.choice.list,
                                 selected = "`Number of families served this week:`"),
                     checkboxInput(inputId = "plot.singles",
                                 label = "Remove Single",value = TRUE)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotlyOutput(outputId = "timePlot", height = "90vh")
                 )#end MainPanel
             )#endSideBar
             
    )#end tabPanel("Time Series")
)# end NavBarPage

# Define server logic required to draw a histogram
server <- function(input, output,session) {

    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, leg.title){
        colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
        labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
        return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, position = "bottomleft", title = leg.title))
    }
    
    active.summary <- eventReactive(c(input$report.date, input$map.size.variable),{
        fr.sub <- form.responces %>%
            filter(`Week ending:` == input$report.date)
        if (nrow(fr.sub) != length(unique(fr.sub$`Pantry Name:`))){
            print("Someone entered a form twice, taking the most recent")
            fr.sub %>% 
                group_by(`Pantry Name:`) %>% 
                arrange(Timestamp) %>%  
                slice(n())
        }
        as <- left_join(x = locations, y = fr.sub, by = c("FormName" ="Pantry Name:"))
        # print(as%>%select(get(input$map.size.variable)))
        # as <- as %>%
        #   mutate(act = str_remove_all(input$map.size.variable, "[``]"))
        as <- as %>% mutate(act = get(str_remove_all(input$map.size.variable, "[``]")))
        
        as <- as%>%
           mutate(size = 3 + 8 * act / max(act,na.rm = T))
        # as$size <- 3 + 8 * as$`Number of individuals served this week:` / max(as$'Number of individuals served this week:',na.rm = T)
        # print(as$size)
        as[is.na(as$size),]$size <- 2
        # print(paste(sort(unique(as$size))[2]))
        return(as)    
    })
    output$mymap <- renderLeaflet({
        min.size <- min(active.summary()$size, na.rm = T) *3
        max.size <- 22
        min.label <- min(active.summary()$act, na.rm = T)
        max.label <- max(active.summary()$act, na.rm = T)
        # print(paste(min.size,max.size,min.label,max.label))
        color.lab <- names(map.color.choices)[map.color.choices == input$map.color.variable]
        size.lab <- names(map.size.choices)[map.size.choices == input$map.size.variable]
        
        leaflet()%>%
            addProviderTiles(provider = providers$Esri.WorldGrayCanvas)%>%
            addCircleMarkers(data = active.summary(),
                             fillColor = ~pal(get(input$map.color.variable)),
                             stroke = 0,
                             radius =  ~size,
                             popup = ~popup,
                             fillOpacity = 1) %>%
            addLegend("bottomright", pal = pal, values = c(3:0),
                      title = color.lab,
                      opacity = 1,
                      labFormat = function(type, cuts, p) {  # Here's the trick
                          paste0(c("  Low", " Medium","  High"))
                      }
                    )%>%
            addLegendCustom(colors = c("grey", "blue", "blue"), 
                            labels = c("NA", min.label,max.label), 
                            sizes = c(2, min.size, max.size),
                            leg.title = size.lab)
    })# end output$mymAP
    output$tableView <- DT::renderDataTable({
      each <- form.responces %>% filter(`Pantry Name:` == input$report.pantry)%>%
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
        
        return(each2)
        
    },
    options = list(
        autoWidth = TRUE,
        columnDefs = list(list(width = '200px', targets = "_all"))
    ))#end output datatable
    output$timePlot <- renderPlotly({
        if (input$plot.singles){
            fr2 <- form.responces %>%
                group_by(`Pantry Name:`)%>%
                mutate(n = n())%>%
                ungroup()%>%
                filter(n>1)
        }else{
            fr2 <- form.responces
        }
        y.lab <- names(plot.choice.list)[plot.choice.list == input$plot.variable]              
        # print(input$plot.variable)
        plot<- ggplot(data = fr2, mapping = aes_string(x = "`Week ending:`", y = input$plot.variable,
                                                    color = "`Pantry Name:`"))+
            geom_point(size = 3)+
            xlab("Week")+
            ylab(y.lab)+
            # theme_classic()+
            theme(legend.position="bottom")
        if (input$plot.variable %in% c("avFood","avStock","acDairy","avFreeze","avFridge")){
          # print("plot inside")
          plot <- plot +  scale_y_continuous(limits = c(1,3),
                    breaks = c(1,2,3),
                    labels = c("Low", "Medium", "High"))
        }
        
        if(!input$plot.variable %in% c("avFreeze","avFridge")){
          plot <- plot+ geom_line()
        }
        
        ggplotly(plot) %>%
          layout(legend = list(orientation = "h", x = 0.4, y = -0.4))
    })#end output$timePlot
    observe({
      query <- parseQueryString(session$clientData$url_search)
      print(query)
      if (!is.null(query[['display']])) {
        # updateSliderInput(session, "bins", value = query[['bins']])
        #      selectInput("toDisplay","Display",c("Enslavers","LocationAccuracy"), selected = "Enslavers"),
        if(query$display=="TimeSeries"){
          print(query$display)
          updateTabsetPanel(session, "navpanel",selected = query$display )
          # updateSelectInput(session, inputId = "toDisplay",selected = "LocationAccuracy")
        }# end if time series
        
      }#end testing null
      
    })# end observe 
    
}#end server

# Run the application 
shinyApp(ui = ui, server = server)
