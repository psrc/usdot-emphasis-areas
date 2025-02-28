shinyServer(function(input, output) {
  
  footer_server('psrcfooter')

# Overview Page -----------------------------------------------------------
  overview_server('OVERVIEW')
  output$howto_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Overview", page_section = "Overview-HowTo", page_info = "description"))})

# State Summary Page -----------------------------------------------------
 output$state_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="State", page_section = "Summary", page_info = "description"))})
 #value_box_server('STATEvaluebox', df=state_data, v=reactive(input$StateMetric), gt="Statewide")
 bar_chart_server('STATEbarchart', 
                  df=state_data, 
                  v=reactive(input$StateMetric), 
                  color = c("#91268F", "#BCBEC0", "#00A7A0"), 
                  chart_height = '800px', 
                  chart_legend=FALSE)

 # Congressional District Summary Page -----------------------------------------------------
 output$congress_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Congressional", page_section = "Summary", page_info = "description"))})
 bar_chart_server('CONGRESSbarchart', 
                  df=congressional_data, 
                  v=reactive(input$CongressionalMetric), 
                  color = c("#91268F", "#BCBEC0", "#00A7A0"), 
                  chart_height = '400px', 
                  chart_legend=FALSE,
                  w.ln = -120, w.lt=47, w.zm=6)
 
 # County Summary Page -----------------------------------------------------
 output$county_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="County", page_section = "Summary", page_info = "description"))})
 bar_chart_server('COUNTYbarchart', 
                  df=county_data, 
                  v=reactive(input$CountyMetric), 
                  color = c("#91268F", "#BCBEC0", "#00A7A0"), 
                  chart_height = '750px', 
                  chart_legend=FALSE,
                  w.ln = -120, w.lt=47, w.zm=6)
 
# City Summary Page -----------------------------------------------------
 output$city_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="City", page_section = "Summary", page_info = "description"))})
 bar_chart_server('CITYbarchart', 
                  df=city_data, 
                  v=reactive(input$CityMetric), 
                  color = c("#91268F", "#BCBEC0", "#00A7A0"), 
                  chart_height = '1200px', 
                  chart_legend=FALSE,
                  w.ln = -122.257, w.lt=47.615, w.zm=8.5)
 
# Tract Summary Page -----------------------------------------------------
 output$tract_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="City", page_section = "Summary", page_info = "description"))})
 map_server('TRACTmap', 
                  df=tract_data, 
                  v=reactive(input$TractMetric), 
                  color = c("#91268F", "#BCBEC0", "#00A7A0"), 
                  map_height = '600px', 
                  w.ln = -122.257, w.lt=47.615, w.zm=8.5)
 
# # Source Tab --------------------------------------------------------------
# 
#   output$source_table <- renderDataTable(create_source_table(d=source_info))  
#   output$source_overview_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Source", page_section = "Overview", page_info = "description"))})

}) # end of shinyServer function 

