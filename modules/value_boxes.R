
value_box_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("summary_boxes"))
  )
}

value_box_server <- function(id, df, v, gt) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    national_rate <- reactive({
      if(v() == "Birth") {
        national_birth
      } else if (v() == "Marriage") {
        national_marriage
      } else {
        national_youth
      }
      })
    
    filtered_df <- reactive({
      df |> 
        st_drop_geometry() |>
        select("state", rate = all_of(str_to_lower(v()))) |>
        filter(state == "Washington")
      })
    
    # Output Values
    output$national_title <- renderUI(paste0("National ", v(), " rate in ", census_yr))
    output$national_value <- renderText({paste0(round((national_rate()*100), 1), "%")})
    
    output$wa_title <- renderUI(paste0("Washington State ", v(), " rate in ", census_yr))
    output$wa_value <- renderText({paste0(round((filtered_df() |> select("rate") |> pull()*100), 1), "%")})
    
    # Tab layout
    output$summary_boxes <- renderUI({
      tagList(
        
        layout_column_wrap(
          width = 1/2,
          value_box(
            title = htmlOutput(ns("national_title")), 
            value = textOutput(ns("national_value")),
            theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"), 
            showcase = NULL, showcase_layout = "left center",
            full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
            class = "value-box-outcomes"
          ),
          value_box(
           title = htmlOutput(ns("wa_title")), 
           value = textOutput(ns("wa_value")),
           theme = value_box_theme(bg = "#EDF9FF", fg = "#0B4B6E"),
           showcase = NULL, showcase_layout = "left center",
           full_screen = FALSE, fill = TRUE, height = NULL, align = "center",
           class = "value-box-outcomes"
          )
        )
      ) 
      
      })  # end renderui
  }) # end module server
    
}



