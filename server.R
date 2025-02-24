shinyServer(function(input, output) {
  
  footer_server('psrcfooter')

# Overview Page -----------------------------------------------------------

  overview_server('OVERVIEW')
  output$howto_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Overview", page_section = "Overview-HowTo", page_info = "description"))})

# State Summary Page -----------------------------------------------------
# 
#   output$region_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Region", page_info = "description"))})
 value_box_server('STATEvaluebox', df=census_data, v=reactive(input$StateMetric), gt=reactive("Statewide"))
 bar_chart_server('STATEbarchart', df=census_data, v=reactive(input$StateMetric), gt=reactive("Statewide"), color = c("#91268F", "#BCBEC0", "#00A7A0"), chart_height = '600px', chart_legend=FALSE)

# Region Summary Page -----------------------------------------------------
# 
#   output$region_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Region", page_info = "description"))})
 value_box_server('MSAvaluebox', df=census_data, v=reactive(input$MSAMetric), gt=reactive("Metro Region"))
 bar_chart_server('MSAbarchart', df=census_data, v=reactive(input$MSAMetric), gt=reactive("Metro Region"), map_lyr = msa_mapping_data, color = c("#91268F", "#BCBEC0", "#00A7A0"), chart_height = '750px', chart_legend=FALSE)
 
 
# # Mode Summary Page -------------------------------------------------------
# 
#   # Boardings
#   output$mode_boardings_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Boardings", page_info = "description"))})
#   value_box_ntd_server('MODEBoardingsvaluebox', df=ntd_data, m=reactive("Boardings"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
#   bar_chart_server('MODEBoardingsbarchart', df=ntd_data, m=reactive("Boardings"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), color = c("#F05A28"))
#  
#   # Revenue Hours
#   output$mode_hours_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Hours", page_info = "description"))})
#   value_box_ntd_server('MODEHoursvaluebox', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
#   bar_chart_server('MODEHoursbarchart', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), color = c("#91268F"))
#   
#   # Revenue Miles
#   output$mode_miles_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-Miles", page_info = "description"))})
#   value_box_ntd_server('MODEMilesvaluebox', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
#   bar_chart_server('MODEMilesbarchart', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), color = c("#8CC63E"))
#   
#   # Boardings per Hour
#   output$mode_bph_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Mode-BPH", page_info = "description"))})
#   value_box_ntd_server('MODEBPHvaluebox', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
#   bar_chart_server('MODEBPHbarchart', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive(input$NTDModes), g=reactive("Region"), gt=reactive("Region"), color = c("#00A7A0"))
# 
# # Operator Summary Page ---------------------------------------------------
#   
#   # Boardings
#   output$operator_boardings_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Boardings", page_info = "description"))})
#   value_box_ntd_server('OPERATORBoardingsvaluebox', df=ntd_data, m=reactive("Boardings"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
#   bar_chart_server('OPERATORBoardingsbarchart', df=ntd_data, m=reactive("Boardings"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), color = c("#F05A28"))
#   
#   # Revenue Hours
#   output$operator_hours_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Hours", page_info = "description"))})
#   value_box_ntd_server('OPERATORHoursvaluebox', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
#   bar_chart_server('OPERATORHoursbarchart', df=ntd_data, m=reactive("Revenue-Hours"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), color = c("#91268F"))
#   
#   # Revenue Miles
#   output$operator_miles_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-Miles", page_info = "description"))})
#   value_box_ntd_server('OPERATORMilesvaluebox', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
#   bar_chart_server('OPERATORMilesbarchart', df=ntd_data, m=reactive("Revenue-Miles"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), color = c("#8CC63E"))
#   
#   # Boardings per Hour
#   output$operator_bph_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Operator-BPH", page_info = "description"))})
#   value_box_ntd_server('OPERATORBPHvaluebox', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), gr=paste0("Year to Date: Jan-",latest_ntd_month))
#   bar_chart_server('OPERATORBPHbarchart', df=ntd_data, m=reactive("Boardings-per-Hour"), v=reactive("All Transit Modes"), g=reactive(input$NTDoperators), gt=reactive("Transit Operator"), color = c("#00A7A0"))
# 
# # Transit Type Summary Page -----------------------------------------------
# 
#   output$type_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Type", page_info = "description"))})  
#   value_box_access_server('TYPEvaluebox', df=transit_buffer_data, bm=reactive(input$TYPEbuffer), bd=reactive(input$TYPEdist), em=reactive(input$TYPErace))
#   output$transit_type_map <- renderLeaflet({create_stop_buffer_map(lyr=transit_buffers, buffer_name=input$TYPEbuffer, buffer_distance=input$TYPEdist)})
#   
#   type_buffer_metric <- reactive({input$TYPEbuffer})
#   type_efa_metric <- reactive({input$TYPErace})
#   type_buffer_dist <- reactive({input$TYPEdist})
#   
#   filtered_type_chart_df <- reactive({
#     transit_buffer_data |> 
#       filter(transit_buffer == type_buffer_metric() & buffer == type_buffer_dist()) |> 
#       select("year", "transit_buffer", contains("share")) |>
#       pivot_longer(cols = !c(year, transit_buffer)) |>
#       mutate(name = str_remove_all(name, "_share"),
#              name = str_replace_all(name, "population", "People"),
#              name = str_replace_all(name, "poc", "People of Color"),
#              name = str_replace_all(name, "pov", "People with Lower Incomes"),
#              name = str_replace_all(name, "lep", "People with Limited English"),
#              name = str_replace_all(name, "yth", "People under 18"),
#              name = str_replace_all(name, "old", "People over 65"),
#              name = str_replace_all(name, "dis", "People with a Disability")) |>
#       filter(name %in% c("People", type_efa_metric()))
#   })
#   
#   output$transit_type_chart <- renderPlotly({
#     
#     p <- psrc_make_interactive(psrc_line_chart(df = filtered_type_chart_df(), x = "year", y = "value", fill = "name", ymax = max(filtered_type_chart_df()$value)*1.2,labels=scales::label_percent(), colors = c("#4C4C4C", "#91268F"), dec=1), legend=TRUE)
#     
#     # Use onRender to apply JavaScript for responsiveness
#     p %>% onRender("
#       function(el, x) {
#         var resizeLabels = function() {
#           var layout = el.layout;
#           var width = el.clientWidth;
#           var fontSize = width < 600 ? 12 : width < 800 ? 14 : 16;
#           var numTicks = width < 600 ? 3 : width < 800 ? 2 : 1;
#           var legendSize = width < 600 ? 12 : width < 800 ? 14 : 16;
#           
#           layout.xaxis = { dtick: numTicks };
#           layout.xaxis.tickfont = { size: fontSize};
#           layout.yaxis.tickfont = { size: fontSize };
#           layout.legend.font = {size: legendSize};
#           
#           Plotly.relayout(el, layout);
#         };
#         
#         // Run the function initially and on window resize
#         resizeLabels();
#         window.addEventListener('resize', resizeLabels);
#       }
#     ")
#     
#   })  
# 
# # Transit Trips Summary Page ----------------------------------------------
# 
#   output$trip_insights_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Transit", page_section = "Trips", page_info = "description"))})  
#   value_box_access_server('TRIPvaluebox', df=transit_trip_data, bm=reactive(input$TRIPbuffer), bd=reactive(input$TRIPdist), em=reactive(input$TRIPrace))
#   output$transit_trip_map <- renderLeaflet({create_stop_buffer_map(lyr=transit_trip_buffers, buffer_name=input$TRIPbuffer, buffer_distance=input$TRIPdist)})
#   
#   trip_buffer_metric <- reactive({input$TRIPbuffer})
#   trip_efa_metric <- reactive({input$TRIPrace})
#   trip_buffer_dist <- reactive({input$TRIPdist})
#   
#   filtered_trip_chart_df <- reactive({
#     transit_trip_data |> 
#       filter(transit_buffer == trip_buffer_metric() & buffer == trip_buffer_dist()) |> 
#       select("year", "transit_buffer", contains("share")) |>
#       pivot_longer(cols = !c(year, transit_buffer)) |>
#       mutate(name = str_remove_all(name, "_share"),
#              name = str_replace_all(name, "population", "People"),
#              name = str_replace_all(name, "poc", "People of Color"),
#              name = str_replace_all(name, "pov", "People with Lower Incomes"),
#              name = str_replace_all(name, "lep", "People with Limited English"),
#              name = str_replace_all(name, "yth", "People under 18"),
#              name = str_replace_all(name, "old", "People over 65"),
#              name = str_replace_all(name, "dis", "People with a Disability")) |>
#       filter(name %in% c("People", trip_efa_metric()))
#   })
#   
#   output$transit_trip_chart <- renderPlotly({
#     
#     p <- psrc_make_interactive(psrc_line_chart(df = filtered_trip_chart_df(), x = "year", y = "value", fill = "name", ymax = max(filtered_trip_chart_df()$value)*1.2,labels=scales::label_percent(), colors = c("#4C4C4C", "#91268F"), dec=1), legend=TRUE)
#     
#     # Use onRender to apply JavaScript for responsiveness
#     p %>% onRender("
#       function(el, x) {
#         var resizeLabels = function() {
#           var layout = el.layout;
#           var width = el.clientWidth;
#           var fontSize = width < 600 ? 12 : width < 800 ? 14 : 16;
#           var numTicks = width < 600 ? 3 : width < 800 ? 2 : 1;
#           var legendSize = width < 600 ? 12 : width < 800 ? 14 : 16;
#           
#           layout.xaxis = { dtick: numTicks };
#           layout.xaxis.tickfont = { size: fontSize};
#           layout.yaxis.tickfont = { size: fontSize };
#           layout.legend.font = {size: legendSize};
#           
#           Plotly.relayout(el, layout);
#         };
#         
#         // Run the function initially and on window resize
#         resizeLabels();
#         window.addEventListener('resize', resizeLabels);
#       }
#     ")
#     
#   })  
#   
# 
# # Route Map Page ----------------------------------------------------------
# 
#   output$route_map_source <- renderText(paste0("Source: ",str_to_title(service_change)," ", gtfs_year, " General Transit Feed Specification (GTFS) data by Transit Agency"))
#   output$transit_route_map <- renderLeaflet({create_route_map()})
# 
# # Source Tab --------------------------------------------------------------
# 
#   output$source_table <- renderDataTable(create_source_table(d=source_info))  
#   output$source_overview_text <- renderUI({HTML(page_information(tbl=page_text, page_name="Source", page_section = "Overview", page_info = "description"))})

}) # end of shinyServer function 

