library(shiny)
library(htmltools)
library(leaflet)
library(dplyr)

karate = read.csv2("data/karate.csv", stringsAsFactors = FALSE) %>%
  mutate(place = factor(place), name = paste(first_name, name))
cities = read.csv("data/worldcities.csv", stringsAsFactors = FALSE) %>%
  filter(capital == "primary" | city == "Hong Kong") %>%
  select(city, country, lat, lng)

karate = karate %>%
  left_join(cities, by = "country") %>%
  mutate(hover = paste0("<b>", country, "</b><br>", name))

pal = colorFactor(c("#FFD700","#C0C0C0", "#CD7F32"), 1:3)

ui <- fluidPage(
   
   titlePanel("Explore the 2018 WKF World Karate Championship Results"),
   
   sidebarLayout(
     sidebarPanel(
       width = 3,
       selectInput("category", "Select Category",
                   c("Kata Individual", "Kumite Individual", "Kata Team", "Kumite Team"),
                   "Kata Individual"),
       selectInput("sex", "Select Sex", c("Female", "Male"), "Female"),
       conditionalPanel(
         "input.category == 'Kumite Individual'",
         selectInput("weight_class", "Select Weight Class", c())
       )
     ),
      
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Map", leafletOutput("map")),
          tabPanel("Medal Table", tableOutput("medal_table")),
          tabPanel("Documentation", htmlOutput("documentation"))
        )
      )
   )
)

server <- function(input, output, session) {
  weight_classes = reactive({
    if (input$sex == "Male") {
      return(c("-60kg", "-67kg", "-75kg", "-84kg", "+84kg"))
    } else {
      return(c("-50kg", "-55kg", "-61kg", "-68kg", "+68kg"))
    }
  })
  
  mapData = reactive({
    if (input$sex == "Male") {
      req("+84kg" %in% weight_classes())
    } else {
      req("+68kg" %in% weight_classes())
    }
    if (input$category == "Kumite Individual") {
      # req(input$weight_class)
      data = filter(karate, category == input$category & sex == input$sex &
                    weight_class == input$weight_class)
    } else {
      data = filter(karate, category == input$category & sex == input$sex)
    }
    data %>%
      select(place, name, country, lat, place, lng, hover)
  })
  
  medalTable = reactive({
    if (input$category == "Kumite Individual") {
      data = filter(karate, category == input$category & sex == input$sex &
                    weight_class == input$weight_class)
    } else {
      data = filter(karate, category == input$category & sex == input$sex)
    }
    
    if (grepl("Team", input$category)) {
      data = select(data, place, country) %>%
        rename(Place = place, Country = country)
    } else {
      data = select(data, place, name, country) %>%
        rename(Place = place, Name = name, Country = country)
    }
    data
  })
  
  observeEvent(input$sex, {
    s = if (input$sex == "Male") "+84kg" else "+68kg"
    updateSelectInput(session, "weight_class", choices = weight_classes(),
                      selected = s)
  })
  
  output$map = renderLeaflet({
    leaflet(mapData()) %>%
      addTiles() %>%
      addCircleMarkers(label = ~lapply(hover, HTML), color = ~pal(place),
                       opacity = 0.8, fillOpacity = 0.4, weight = 6) %>%
      addLegend(pal = pal, values = ~place)
  })
  
  output$medal_table = renderTable(
    medalTable(), striped = TRUE, hover = TRUE
  )
  
  output$documentation = renderUI({
    HTML(
    "<h3>Introduction</h3>",
    "Welcome! This Shiny app let's you explore the medallist of the ",
    "2018 WKF Karate World Championships.",
    "<h3>How To</h3>",
    "<ul>",
      "<li>Select the descipline you wish to display using the top list box on the left. This can be either of Kata Individual, Kata Team, Kumite Individual, Kumite Team</li>",
      "<li>Choose whether to display male or female events in the second list box</li>",
      "<li>If you choose the category <i>Kumite Individual</i> a third list box will appear allowing you to choose a weight class</li>",
    "</ul>",
    "<h3>Display Types</h3>",
    "You can choose to display the medallist either on a world map (tab Map) ",
    "or as a table (tab Medal Table).",
    "<h3>References</h3>",
    "The 2018 WKF World Karate Championship results have been obtained from ",
    "<a href = https://www.sportdata.org/wkf/set-online/popup_main.php?popup_action=results&vernr=188&active_menu=calendar>Sportdata</a>."
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
