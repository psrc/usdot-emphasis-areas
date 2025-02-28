shinyUI(
  
  tags$html(
    
    lang = "en",
    
    tags$head(tags$title("USDOT Emphasis Areas")),
    
    page_navbar(
    
      # JavaScript to modify tabindex values to 0 so you can tab to them as well as using a mouse
      tags$script(HTML("
      $(document).ready(function() {
        function setTabindex() {
          $('.nav-link').attr('tabindex', '0');
        }
        // Set tabindex initially
        setTabindex();
  
        // Listen for tab changes and reset tabindex
        $('.nav-link').on('shown.bs.tab', function() {
          setTabindex();
        });
      });
    ")),
    
      position = c("static-top"),
    
      title = tags$a(div(tags$img(src='psrc-logo.png', style="margin-top: 10px; padding-left: 20px; padding-right: 30px;", height = "65", alt = "Link to PSRC Homepage")), href="https://www.psrc.org", target="_blank"),
      fillable = FALSE,
      theme = psrc_theme,

      nav_panel("Overview", 
                h1("USDOT Emphasis Areas"),
                overview_ui('OVERVIEW'),
                h2("Where does the data come from?"),
                htmlOutput("howto_text")),
      
      nav_panel("State", 
               card_body(
                 selectizeInput(
                   "StateMetric",
                   label = "Select an Emphasis Area:",
                   choices = emphasis_areas,
                   selected = "Marriage",
                   options = list(dropdownParent = 'body')
                 ),
                 class = "selection_panel"
               ),
                 
               hr(style = "border-top: 1px solid #000000;"),
               h1("State Summary"),
               withSpinner(bar_chart_ui('STATEbarchart'), color=load_clr, size = 1.5, caption = "Please wait, updating data"),
               hr(style = "border-top: 1px solid #000000;"),
               card_body(h3("Insights & Analysis"), htmlOutput("state_insights_text"), class = "insights_panel"),
               hr(style = "border-top: 1px solid #000000;")
               ),
      
      nav_panel("Congressional District", 
                card_body(
                  selectizeInput(
                    "CongressionalMetric",
                    label = "Select an Emphasis Area:",
                    choices = emphasis_areas,
                    selected = "Marriage",
                    options = list(dropdownParent = 'body')
                  ),
                  class = "selection_panel"
                ),
                
                hr(style = "border-top: 1px solid #000000;"),
                h1("Washington Congressional District Summary"),
                withSpinner(bar_chart_ui('CONGRESSbarchart'), color=load_clr, size = 1.5, caption = "Please wait, updating data"),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(h3("Insights & Analysis"), htmlOutput("congress_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;")
      ),
      
      nav_panel("County", 
                card_body(
                  selectizeInput(
                    "CountyMetric",
                    label = "Select an Emphasis Area:",
                    choices = emphasis_areas,
                    selected = "Marriage",
                    options = list(dropdownParent = 'body')
                  ),
                  class = "selection_panel"
                ),
                
                hr(style = "border-top: 1px solid #000000;"),
                h1("Washington County Summary"),
                withSpinner(bar_chart_ui('COUNTYbarchart'), color=load_clr, size = 1.5, caption = "Please wait, updating data"),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(h3("Insights & Analysis"), htmlOutput("county_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;")
      ),
      
      nav_panel("City", 
                card_body(
                  selectizeInput(
                    "CityMetric",
                    label = "Select an Emphasis Area:",
                    choices = emphasis_areas,
                    selected = "Marriage",
                    options = list(dropdownParent = 'body')
                  ),
                  class = "selection_panel"
                ),
                
                hr(style = "border-top: 1px solid #000000;"),
                h1("PSRC City Summary"),
                withSpinner(bar_chart_ui('CITYbarchart'), color=load_clr, size = 1.5, caption = "Please wait, updating data"),
                hr(style = "border-top: 1px solid #000000;"),
                card_body(h3("Insights & Analysis"), htmlOutput("city_insights_text"), class = "insights_panel"),
                hr(style = "border-top: 1px solid #000000;")
      ),
      
      # 
      # nav_panel(icon("info-circle"), 
      #           h1("Data Sources"),
      #           htmlOutput("source_overview_text"),
      #           hr(style = "border-top: 1px solid #000000;"),
      #           card(
      #             full_screen = TRUE,
      #             withSpinner(dataTableOutput("source_table"), color=load_clr, size = 1.5, caption = "Please wait, loading table")
      #             ),
      #           hr(style = "border-top: 1px solid #000000;")
      # ),
    
      br(), br(),
    
      footer = (footer_ui('psrcfooter'))
      
      ) # end of page_navbar
  ) # end of HTML tag for UI
) # end of Shiny App

