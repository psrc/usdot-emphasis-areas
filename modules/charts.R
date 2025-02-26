
bar_chart_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("barchart"))
  )
}

bar_chart_server <- function(id, df, v, color, chart_height = '800px', chart_labels = scales::label_comma(), chart_legend=TRUE) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Filter the dataframe based on the layer passed and strip out the spatial part for the bar chart
    filtered_df <- reactive({
      df |> 
        st_drop_geometry() |>
        select("state", rate = all_of(str_to_lower(v())), comparison = all_of(paste0(str_to_lower(v()), "_comparison"))) |>
        mutate(comparison = case_when(
          state == "Washington" ~ "Washington State",
          state != "Washington" ~ comparison)) |>
        arrange(rate)
                   })
    
    filtered_map_lyr <- reactive({
      df |> 
        select("state", rate = all_of(str_to_lower(v())), comparison = all_of(paste0(str_to_lower(v()), "_comparison")))
    })
    
    # Charts & Maps
    output$bar_chart <- renderPlotly({
      
      p <- psrc_make_interactive(psrc_bar_chart(df = filtered_df(), x = "state", y = "rate", fill = "comparison", colors = color, dec = 1, labels=chart_labels), legend=chart_legend)
    
      # Use onRender to apply JavaScript for responsiveness
      p %>% onRender("
      function(el, x) {
        
        el.setAttribute('aria-label', 'Bar chart of transit metrics for all transit modes for all transit oeprators in the Central Puget Sound Region');
      
        var resizeLabels = function() {
          var layout = el.layout;
          var width = el.clientWidth;
          var fontSize = width < 600 ? 12 : width < 800 ? 14 : 16;
          var legendSize = width < 600 ? 12 : width < 800 ? 14 : 16;
          
          layout.xaxis.tickfont = { size: fontSize};
          layout.yaxis.tickfont = { size: fontSize };
          layout.legend.font = {size: legendSize};
          
          Plotly.relayout(el, layout);
        };
        
        // Run the function initially and on window resize
        resizeLabels();
        window.addEventListener('resize', resizeLabels);
      }
    ")
      
    })
    
    output$map <- renderLeaflet({create_emphasis_area_map(lyr = filtered_map_lyr(), emphasis_area = v())})
    
    # Tab layout
    output$barchart <- renderUI({
      tagList(
        
        card(
          full_screen = TRUE,
          card_body(
            layout_columns(
              col_widths = c(7,5),
              plotlyOutput(ns("bar_chart"), height = chart_height),
              tags$div(
                role = "img",
                `aria-label` = "Map showing comparison to national averages for emphasis areas",
                leafletOutput(ns("map"), height = chart_height),
              )
            )
          )
        ),
        
        # card(
        #   full_screen = FALSE,
        #   plotlyOutput(ns("bar_chart"), height = chart_height)
        # ),
        # 
        # card(
        #   full_screen = FALSE,
        #   leafletOutput(ns("map"))
        # ),

        br(),
        
        tags$div(class = "chart_source", "Source: US Census Bureau American Community Survey Table"),
        
      )
    }) 
  })  # end moduleServer
}
