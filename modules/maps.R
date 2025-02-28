
map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("map"))
  )
}

map_server <- function(id, df, v, color, map_height = '800px', w.ln=-99, w.lt=40, w.zm=3) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    filtered_map_lyr <- reactive({
      df |> 
        select("state", rate = all_of(str_to_lower(v())), comparison = all_of(paste0(str_to_lower(v()), "_comparison")))
    })
    
    # Charts & Maps
    output$tract_map <- renderLeaflet({create_emphasis_area_map(lyr = filtered_map_lyr(), emphasis = v(), ln=w.ln, lt=w.lt, zm=w.zm)})
    
    # Tab layout
    output$map <- renderUI({
      tagList(
        
        card(
          full_screen = TRUE,
          tags$div(
            role = "img",
            `aria-label` = "Map showing comparison to national averages for emphasis areas",
            leafletOutput(ns("tract_map"), height = map_height),
          )
          ),

        br(),
        
        tags$div(class = "chart_source", "Source: US Census Bureau American Community Survey Table"),
        
      )
    }) 
  })  # end moduleServer
}
