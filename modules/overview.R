
overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("overview"))
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$overview_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Overview", page_section = "Overview", page_info = "description"))})
    output$site_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Overview", page_section = "Overview-Site", page_info = "description"))})
    output$agency_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Overview", page_section = "Overview-Agency", page_info = "description"))})
  
    # Overview UI
    output$overview <- renderUI({
      tagList(
        
        htmlOutput(ns("overview_text")) |> withSpinner(color=load_clr),
        br(),
        htmlOutput(ns("site_text")),
        br(),
        

      )
    })
  })  # end moduleServer
}
