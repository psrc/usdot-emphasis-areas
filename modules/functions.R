# General Information -------------------------------------------------------------
page_information <- function(tbl, page_name, page_section=NULL, page_info) {
  
  if(is.null(page_section)) {
    
    t <- tbl |>
      filter(page == page_name) |>
      select(all_of(page_info)) |>
      pull()
    
  } else {
    
    t <- tbl |>
      filter(page == page_name & section == page_section) |>
      select(all_of(page_info)) |>
      pull()
    
  }
  
  
  if(is.na(t)) {f <- ""} else {f <- t}
  
  return(f)
  
}

# Charts ------------------------------------------------------------------

psrc_style <- function() {
  font <- "Poppins"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and color of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       face="bold",
                                       size=13, 
                                       color='#4C4C4C'),
    plot.title.position = "plot",
    
    #This sets the font, size, type and color of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=12,
                                          margin=ggplot2::margin(9,0,9,0)),
    
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    plot.caption =  ggplot2::element_text(family=font,
                                          size=10,
                                          face="italic",
                                          color="#4C4C4C",
                                          hjust=0),
    plot.caption.position = "plot",
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and background for it and sets the requirements for any text within the legend.
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#4C4C4C"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_text(family=font, size=12, color="#2f3030"),
    axis.text = ggplot2::element_text(family=font, size=11, color="#2f3030"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background color from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background sets the panel background for facet-wrapped plots to PSRC Gray and sets the title size of the facet-wrap title
    strip.background = ggplot2::element_rect(fill="#BCBEC0"),
    strip.text = ggplot2::element_text(size  = 12,  hjust = 0)
  )
}

psrc_infogram_style <- function() {
  font <- "Poppins"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and color of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       face="bold",
                                       size=16, 
                                       color='black'),
    plot.title.position = "plot",
    
    #This sets the font, size, type and color of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          color='black',
                                          size=14,
                                          margin=ggplot2::margin(9,0,9,0)),
    
    #This sets the caption text element
    plot.caption =  ggplot2::element_text(family=font,
                                          size=12,
                                          face="plain",
                                          color='black',
                                          hjust=0),
    plot.caption.position = "plot",
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and background for it and sets the requirements for any text within the legend.
    legend.position = "bottom",
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=14,
                                        color="black"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font, size=12, color="black"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines.
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color="#BCBEC0"),
    panel.grid.major.x = ggplot2::element_line(linewidth = 0.2, color="#BCBEC0"),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background color from the plot
    panel.background = ggplot2::element_blank(),
    
    
  )
}

psrc_column_chart <- function(df, x, y, fill, colors, labels=scales::label_comma(), dec=0, chart_style=psrc_infogram_style(), title=NULL, source=NULL, pos="dodge", legend = TRUE) {
  
  c <- ggplot(data=df,
              aes(x=.data[[x]],
                  y=.data[[y]],
                  fill=.data[[fill]],
                  text = paste0(.data[[x]], " ", .data[[fill]], ": ", format(round(.data[[y]], dec), nsmall=0, big.mark=","))))  + 
    geom_bar(position=pos, stat="identity", na.rm=TRUE) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = labels, expand=expansion(mult = c(0, .2)))  +   # expand is to accommodate value labels
    labs(title=title, caption=source) +
    chart_style
  
  if (legend == FALSE) {
    
    c <- c + theme(legend.position = "none")
  }
  
  return (c)
}

psrc_bar_chart <- function(df, x, y, fill, colors, labels=scales::label_comma(), dec=0, chart_style=psrc_infogram_style(), title=NULL, source=NULL, pos="dodge", legend = TRUE) {
  
  # Make sure the data is sorted in the order for the final chart
  df <- df |> arrange(.data[[y]])
  ord <- df |> select(all_of(x)) |> pull()
  df <- df |> mutate(!!x := factor(.data[[x]], levels = ord))
  
  c <- ggplot(data=df,
              aes(x=.data[[x]],
                  y=.data[[y]],
                  fill=.data[[fill]],
                  text = paste0(.data[[x]], ": ", format(round(.data[[y]]*100, dec), nsmall=0, big.mark=","),"%")))  + 
    geom_bar(position=pos, stat="identity", na.rm=TRUE) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    labs(title=title, caption=source) +
    chart_style
  
  if (legend == FALSE) {
    
    c <- c + theme(legend.position = "none")
  }
  
  c <- c + 
    coord_flip() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linewidth = 0.2, color="#BCBEC0"))
    
  
  return (c)
}

psrc_line_chart <- function(df, x, y, fill, lwidth=1, colors, ymin =0, ymax = 1, labels=scales::label_comma(), dec=0, breaks=NULL, title=NULL, source=NULL, legend = TRUE, chart_style=psrc_infogram_style()) {
  
  c <- ggplot(data=df,
              aes(x=.data[[x]],
                  y=.data[[y]],
                  group=.data[[fill]],
                  color=.data[[fill]],
                  text = paste0(.data[[fill]], ": ", format(round(.data[[y]]*100, dec), nsmall=0, big.mark=","), "%")))  + 
    geom_line(linewidth=lwidth, linejoin = "round", na.rm=TRUE) +
    geom_point(fill = "white", shape = 21, stroke = 0.5) +
    scale_color_manual(values = colors)  +
    scale_y_continuous(labels = labels, limits = c(ymin, ymax))  +   
    labs(title=title, caption=source) +
    scale_x_continuous(n.breaks=breaks) +
    chart_style
  
  return(c)
  
}

psrc_make_interactive <- function(plot_obj, legend=FALSE, hover=y) {
  
  c <- plotly::ggplotly(plot_obj, tooltip = "text")
  
  c <- plotly::layout(c,
                      showlegend = legend,
                      legend=list(orientation="h", xanchor="center", xref="container", x=0, y=-0.10,         
                                  title="", font=list(family="Poppins", size=20, color="black"),
                                  pad=list(b=50, t=50)),
                      hoverlabel = list(bgcolor = "#EDF9FF", font = list(size=16, color = "#2F3030", face="bold"))
                      )
  
  return(c)
  
}

# Tables ------------------------------------------------------------------

create_source_table <- function(d=source_info) {

  # Table
  t <- rbind(names(d), d)
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 30,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(0,3), className = 'dt-left'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # Add Section Breaks
  
  summary_tbl <- summary_tbl |>
    formatStyle(0:ncol(t), valueColumns = "Data Point",
                `border-bottom` = styleEqual(c("Boardings per Hour", "Total Population in Stop Buffers", "Transit Service"), "solid 2px"))
  
  summary_tbl <- summary_tbl |>
    formatStyle(0:ncol(t), valueColumns = "Data Point",
                `border-top` = styleEqual(c("Boardings"), "solid 2px"))
    
  return(summary_tbl)
  
}

# Maps --------------------------------------------------------------------
create_emphasis_area_map <- function(lyr, emphasis_area, dec=1, colors=c("#91268F", "#BCBEC0")) {
  
  labels <- paste0("<b>State: </b>", lyr$state, "<br>", "<b>", paste0(emphasis_area, " Rate: "),"</b>", paste0(round(lyr$rate*100, dec),"%")) |> lapply(htmltools::HTML)
  pal <- colorFactor(palette = colors, domain = lyr$comparison)
  
  working_map <- leaflet(data = lyr) |>
    
    addTiles(group = "Open Street Map") |>
    
    addProviderTiles(providers$CartoDB.Positron, group = "Positron (minimal)") |>
    
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
    
    addLayersControl(baseGroups = c("Positron (minimal)", "Open Street Map", "Satellite"),
                     overlayGroups = c(paste0(emphasis_area, "Rate")),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([40,-99],3); }"))) |>
    
    addPolygons(data = lyr, 
                fillColor = ~pal(comparison),
                fillOpacity = 0.75,
                color = "#000000",
                weight = 1,
                opacity = 0.5,
                label = labels,
                group = paste0(emphasis_area, "Rate")) |>
    
    setView(lng = -99, lat = 40, zoom = 3) |>
    
    addLegend(pal = pal, 
              values = lyr$comparison, 
              opacity = 1, 
              position = "bottomleft",
              title = "Rate Compared to National Average") 
  
  return(working_map)
  
}