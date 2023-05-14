library(shinydashboard)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(shinysky)
library(shinyjqui)
library(shinycssloaders)
library(tidyverse)
library(tidyr)
library(ggiraph)
library(RISmed)
library(rentrez)
library(rcrossref)
library(DT)
library(lubridate)
library(knitr)
library(kableExtra)
library(rdrop2)
library(fontawesome)
jsfile <- "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/4.3.1/iframeResizer.contentWindow.js"

coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}

pubcounts <- 
  read_csv("pubcountsTable.csv",
           show_col_types = FALSE)

pubslist <- 
  read_csv("pubslistTable.csv",
           show_col_types = FALSE) %>%
  mutate(Reporting = 
           factor(Reporting, levels = c("Y1-5", 
                                        "Y6Q1", "Y6Q2", "Y6Q3", "Y6Q4",
                                        "Y7Q1", "Y7Q2", "Y7Q3", "Y7Q4",
                                        "Y8Q1", "Y8Q2", "Y8Q3", "Y8Q4",
                                        "Y9Q1", "Y9Q2", "Y9Q3", "Y9Q4",
                                        "Y10Q1", "Y10Q2", "Y10Q3", "Y10Q4",
                                        "Y11Q1", "Y11Q2", "Y11Q3", "Y11Q4")
                  )
         ) %>%
  arrange(Reporting) 

# Construct a tooltip----
tooltip_css <- "background-color: #f7f7f7;
                color: #181818;
                padding:10px;
                font-family: Jost, sans-serif;
                font-weight: 500;
                border-radius:5px;"

ui <- dashboardPage(
  
 
    dashboardHeader(title = "WVCTSI Linked Publications Hub", 
                    titleWidth = 500),
    
    dashboardSidebar(width = 375,
                     tags$head(
                         tags$link(rel = "stylesheet", 
                                   type = "text/css", 
                                   href = "sidebar.css"),
                         
                         tags$style(type = "text/css",
                                    HTML("th { text-align: center; }")
                         ),
                         
                         tags$script(src = jsfile,
                                     integrity = "sha512-ngVIPTfUxNHrVs52hA0CaOVwC3/do2W4jUEJIufgZQicmY27iAJAind8BPtK2LoyIGiAFcOkjO18r5dTUNLFAw==",
                                     crossorigin = "anonymous",
                                     referrerpolicy = "no-referrer")
                     ),
                     
                     sidebarMenu(
                         menuItem("General Information",
                                  icon = icon("whiskey-glass"),
                                  tabName = "home"),
                         
                         menuItem("Publication Counts",
                                  icon = icon("calendar-days"),
                                  tabName="publicationCounts"),
                         
                         menuItem("Linked Publications",
                                  icon = icon("link"),
                                  tabName = "publicationList"),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         menuItem("Build & Pull",
                                  icon = icon("code-branch"),
                                  tabName = "svinfo"),
                         
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$br(),
                         tags$hr(style="border-color: #e9c5ca; width: 75%; margin: auto;"),
                         tags$br(),
                         tags$br(),
                         tags$div(
                           "Created by", 
                           tags$a(href="https://iam.asocialdatascientist.com/", 
                                  "Dr. Abhik Roy", .noWS = "after"),
                           style="text-align: center;"),
                         tags$br(),
                         tags$br(),
                         tags$div(
                           tags$a(
                             href="http://creativecommons.org/licenses/by-nc-sa/4.0/",
                             target="_blank",
                             rel="license",
                             class="norm",
                             tags$img(src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png", 
                                      title="Creative Commons License",
                                      style="border-width:0")
                           ),
                           tags$br(),
                           tags$br(),
                           "This work is licensed under a ",
                           tags$br(),
                           tags$a(
                             href="http://creativecommons.org/licenses/by-nc-sa/4.0/",
                             "Creative Commons Attribution-",
                             tags$br(),
                             "NonCommercial-ShareAlike 4.0",
                             tags$br(),
                             "International License",
                             target="_blank",
                             rel="license",
                             class="norm",
                           ),
                           style="text-align: center;")
                     )
    ),
    
    dashboardBody(
        
        tags$head(
            tags$link(rel = "stylesheet", 
                      type = "text/css", 
                      href = "body.css"),
            
            tags$style(HTML("
                            .btn-primary {
                            color: #212529;
                            background-color: #77cccc;
                            border-color: #5bc2c2;
                            }
                            
                            btn-primary:hover {
                            color: #212529;
                            background-color: #52bebe;
                            border-color: #8ad3d3;
                            }
                            
                            .btn-primary:focus,
                            .btn-primary.focus {
                            #77cccc;
                            }
                            
                            .btn-primary.active,
                            .btn-primary:active, 
                            .open > .dropdown-toggle.btn-primary{
                            color: #212529;
                            background-color: #77cccc;
                            border-color: #5bc2c2;
                            }
                            
                            .btn-primary.active.focus, 
                            .btn-primary.active:focus, 
                            .btn-primary.active:hover, 
                            .btn-primary:active.focus, 
                            .btn-primary:active:focus, 
                            .btn-primary:active:hover, 
                            .open > .dropdown-toggle.btn-primary.focus, 
                            .open > .dropdown-toggle.btn-primary:focus, 
                            .open > .dropdown-toggle.btn-primary:hover{
                            color: #212529;
                            background-color: #77cccc;
                            border-color: #5bc2c2;
                            }
                            
                            .btn-primary.disabled,
                            .btn-primary:disabled {
                            color: #212529;
                            background-color: #77cccc;
                            border-color: #5bc2c2;
                            }
                            
                            .btn-primary:not(:disabled):not(.disabled):active,
                            .btn-primary:not(:disabled):not(.disabled).active,
                            .show>.btn-primary.dropdown-toggle {
                            color: #212529;
                            background-color: #9cdada;
                            border-color: #2e7c7c;
                            }
                            
                            .btn-primary:not(:disabled):not(.disabled):active:focus,
                            .btn-primary:not(:disabled):not(.disabled).active:focus,
                            .show>.btn-primary.dropdown-toggle:focus {
                            #77cccc;
                            }
                            
                            .btn-outline-primary {
                            color: #77cccc;
                            background-color: transparent;
                            background-image: none;
                            border-color: #77cccc;
                            }
                            
                            .btn-outline-primary:hover {
                            color: #222222;
                            background-color: #8ad3d3;
                            border-color: #77cccc;
                            }
                            
                            .btn-outline-primary:focus
                            .btn-outline-primary.focus {
                            #77cccc;
                            }
                            
                            .btn-outline-primary.disabled,
                            .btn-outline-primary:disabled {
                            color: #77cccc;
                            background-color: transparent;
                            }

                            .btn-outline-primary:not(:disabled):not(.disabled):active,
                            .btn-outline-primary:not(:disabled):not(.disabled).active,
                            .show>.btn-outline-primary.dropdown-toggle {
                            color: #212529;
                            background-color: #8ad3d3;
                            border-color: #77cccc;
                            }
                            
                            .btn-outline-primary:not(:disabled):not(.disabled):active:focus,
                            .btn-outline-primary:not(:disabled):not(.disabled).active:focus,
                            .show>.btn-outline-primary.dropdown-toggle:focus {
                            color: #77cccc;
                            }
                            
                            .btn-default,
                            .btn-default.hover, 
                            .btn-default:active, 
                            .btn-default:hover {
                            color: #212529;
                            background-color: #77cccc;
                            border-color: #777777;
                            }
                            
                            .dropdown-menu{
                            background-color: #77cccc;
                            }
                            
                            i {
                            margin-right: 10px;
                            }
                            
                            div.dataTables_wrapper  div.dataTables_filter {
                            width: 100%;
                            float: none;
                            text-align: center;
                            }
                            "
                            )
                       )
            ),
        useShinyjs(),
        tags$br(),
        tags$br(),
        tags$div(
            tabItems( 
                tabItem(
                    tabName = "home", 
                    tags$br(),
                    tags$br(),
                    tags$div(
                        tags$a(
                            href="https://www.wvctsi.org",
                            target="_blank",
                            rel="noopener noreferrer",
                            tags$img(src="wvctsi-logo-only.png", 
                                     title="WVCTSI logo", 
                                     width="275",
                                     target="_blank",
                                     rel="noopener noreferrer")
                        ),
                        style="text-align: center;"
                    ), 
                    tags$br(),
                    tags$br(),
                    tags$div(
                      em("Please select one of the options shown on the left"),
                      style="text-align: center;")
                ),
                
                tabItem(
                    tabName = "publicationCounts",
                    h4(paste0("Current WVCTSI linked publication counts as of 6am on ",
                              format(now(tzone = "EST"), "%A"),
                              ", ",
                              " ",
                              mday(now(tzone = "EST")),
                              ", ",
                              year(now(tzone = "EST")),
                              ".")),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    DT::dataTableOutput("pubcounts") %>% 
                      withSpinner(color="#0dc5c1"),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    girafeOutput("pubcountsInteractive")
                    ),
                
                tabItem(
                    busyIndicator(text = "reticulating splines"),
                    tabName = "publicationList",
                    h4(paste0("Current WVCTSI linked publications as of 6am on ",
                              format(now(tzone = "EST"), "%A"),
                              ", ",
                              month(now(tzone = "EST"), label = TRUE, abbr = FALSE),
                              " ",
                              mday(now(tzone = "EST")),
                              ", ",
                              year(now(tzone = "EST")),
         
                                                   ". Please note that some information may be incorrectly formatted or missing. Additional fields can be turned on/off using Toggle Column Visibility and may be reordered by dragging any column header.")),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$style(HTML("
                                    .dt-button.buttons-columnVisibility {
                                    color: #212529 !important;
                                    background: #ececec !important;
                                    border-color: #292A30 !important;
                                    }
                                    
                                    .dt-button.buttons-columnVisibility.active
                                    {
                                    color: #212529 !important;
                                    background: #77cccc !important;
                                    border-color: #292A30 !important;
                                    }
                                    
                                    .dt-button.buttons-columnVisibility:hover
                                    .dt-button.
                                    {
                                    color: #212529 !important;
                                    background: #ffffff !important;
                                    border-color: #292A30 !important;
                                    }
                                    
                                    button.dt-button:focus:not(.disabled), 
                                    div.dt-button:focus:not(.disabled), 
                                    a.dt-button:focus:not(.disabled), 
                                    input.dt-button:focus:not(.disabled){
                                    color: #212529 !important;
                                    background: #77cccc !important;
                                    border-color: #292A30 !important;
                                    }

                                    .dataTable tbody tr.selected td, 
                                    table.dataTable td.selected {
                                    color: #E6E4E8; 
                                    background-color: ", 
                                    '#382D47'," !important;
                                    }
                                    
                                    .dataTable.hover tbody tr:hover, 
                                    table.dataTable.display tbody tr:hover {
                                     color: #FDFDFE; 
                                     background-color: ",
                                    '#3F354E',"!important;
                                    }
                                    
                                    .dataTable tbody th {
                                     text-align: center;
                                    }
                                    
                                    .dataTables_wrapper .dataTables_length, 
                                    .dataTables_wrapper .dataTables_filter, 
                                    .dataTables_wrapper .dataTables_info, 
                                    .dataTables_wrapper .dataTables_processing
                                    ,.dataTables_wrapper .dataTables_paginate .paginate_button, 
                                    .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                                    color: #ffffff !important;
                                    }
                                    
                                    .dataTables_wrapper .dataTables_length {
                                    float: right;
                                    }
                                    
                                    .dataTables_wrapper .dataTables_filter {
                                    float: right;
                                    text-align: left;
                                    }
                        
                                    thead {
                                    color: #FFFFFF;
                                    }
                                
                                    "
                    )
                ),
                tags$br(),
                tags$br(),
                
                    downloadButton("pubListFull", 
                                   label = "CSV of all publications", 
                                   class = "btn-outline-primary mr-1"),
              # tags$head(tags$style(".pubListFullbutt{background-color:#161928 !important;} .pubListFullbutt{color: #FFFFFF !important;}, .butt1{font-family: Jost }")),
                    
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                # Year and Quarter
                tags$div(pickerInput(
                    inputId = "Reporting",
                    choices = c(as.character(unique(pubslist$Reporting))),     
                    selected = as.character(unique(pubslist$Reporting)),  
                    options = list(
                        style = "btn btn-primary mr-1",
                        `actions-box` = TRUE,
                        `selected-text-format` = paste0("count > ", length(unique(as.character(pubslist$Year)))-1) ,
                        `count-selected-text` = "Filter by Quarter and Year"),
                    choicesOpt = list(
                      style = rep(("color: black;}"), nrow(unique(tibble(pubslist$Year, pubslist$Quarter)))), ".dropdown-menu ul li:nth-child(%s) a {
                        background: blue !important; color: white !important;"),
                    multiple = TRUE
                    ),
                style="display:inline-block"),
                
                 tags$br(),
                    DT::dataTableOutput("publist") %>% 
                    withSpinner(color="#0dc5c1"),
                    tags$br(),
                    tags$br()
                  ),
                
                tabItem(
                  busyIndicator(text = "reticulating splines"),
                    tabName = "svinfo",
                    fluidRow(
                        column(width = 12,
                               tags$div(
                                 "Data pulled daily from the ",
                                 tags$a(
                                   href="https://www.ncbi.nlm.nih.gov/home/develop/api/", 
                                   "National Center for Biotechnology Information", 
                                   .noWS = "after",
                                   target="_blank",
                                   rel="noopener noreferrer"),
                                 style="text-align: center;"),
                               tags$br(),
                               tags$br(),
                               tags$hr(style="width: 35%"),
                               tags$br(),
                               tags$br(),
                               tags$div(
                                   tags$a(
                                       href="https://percwv.com/",
                                       target="_blank",
                                       rel="noopener noreferrer",
                                       tags$img(src="PERC_TEC_logo.png", 
                                                title="R Studio logo", 
                                                width="250")
                                   ),
                                   style="text-align: center;"),
                               tags$br(),
                               tags$br(),
                               tags$div(
                                   tags$a(
                                   href="https://www.rstudio.com/",
                                   target="_blank",
                                   rel="noopener noreferrer",
                                   tags$img(src="rstudio-logo.png", 
                                            title="R Studio logo", 
                                            width="70")
                                   ), 
                                   tags$a(
                                       href="https://shiny.rstudio.com/", 
                                       target="_blank",
                                       rel="noopener noreferrer",
                                       tags$img(src="shiny-logo.png", 
                                                title="Shiny logo", 
                                                width="70")
                                   ), 
                                   style="text-align: center;"),
                               tags$br(),
                               tags$br(),
                               tags$br(),
                               tags$br(),
                               tags$br(),
                               tags$br(),
                               tags$br(),
                               tags$br(),
                               tags$div(p(em("Build version 0.9.2: Lone Starr, I am your father's"),
                                          tags$br(),
                                          em("brother's nephew's cousin's former roommate")),
                                        style="text-align: center;"),
                        ),
                        
                    )
                    
                )
                # End of tabItem
            )
        )
    )
)


server <- function(input, output, session) {
  
  countscont = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 1, 'Status'),
        th(rowspan = 1, 'Timeframe'),
        th(rowspan = 1, 'Q1'),
        th(rowspan = 1, 'Q2'),
        th(rowspan = 1, 'Q3'),
        th(rowspan = 1, 'Q4'),
        th(colspan = 2, 'Total')
      ),
      tr(
        lapply(c("","","","","","", "Publications", "Citations"), th)
      )
    )
  ))
    
output$pubcounts <- 
        DT::renderDataTable(
            {
        DT::datatable(pubcounts,
                      container = countscont,
                      rownames = FALSE,
                      callback = JS('$("button.buttons-csv").css("color","#77cccc");
                                     $("button.buttons-csv").css("background-color","transparent"); 
                                     $("button.buttons-csv").css("background-color","transparent").hover(function(){
                                     $(this).css("color", "#222222");
                                     $(this).css("background-color", "#8ad3d3");
                                     }, function(){
                                     $(this).css("color","#77cccc");
                                     $(this).css("background-color", "#292A30 !important");
                                     });
                                     $("button.buttons-csv").css("border-color","#77cccc"); 
                                     $("button.buttons-copy").css("color","#FFFFFF");
                                     $("button.buttons-copy").css("background","##1aaabf"); 
                                     $("button.buttons-copy").css("border-color","#77cccc"); 
                                     $("button.buttons-excel").css("color","#ffffff");
                                     $("button.buttons-excel").css("background-color","#1d6f42"); 
                                     $("button.buttons-excel").css("border-color","#77cccc"); 
                                     $("button.buttons-pdf").css("color","#FFFFFF");
                                     $("button.buttons-pdf").css("background","#bf2f1a");
                                     $("button.buttons-pdf").css("border-color","#77cccc");
                                     return table;
                                    '),
                      extensions = 'Buttons', 
                      options = list(
                          dom = 'Bfrtip',
                          buttons = c('csv',
                                      'copy',
                                      'excel', 
                                      'pdf'),
                          searching =  FALSE,
                          paging = FALSE,
                          lengthChange = FALSE,
                          autoWidth = FALSE,
                          scrollX = TRUE,
                          columnDefs = list(list(className = 'dt-center', 
                                                 targets = 0:7)
                                            ),
                          initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#292A30', 'color': '#FFFFFF'});",
                              "}")
                      )) %>%
            formatStyle("Publications",  
                        color = '#FFFFFF', 
                        fontWeight = 'bold') %>%
            formatStyle("Citations",  
                         color = '#FFFFFF', 
                         fontWeight = 'bold') %>%
            formatStyle("Status",  
                        color = '#FFFFFF', 
                        fontWeight = 'normal') %>%
            formatStyle(c("Status", "Timeframe", "Q1", "Q2", "Q3", "Q4", "Publications", "Citations"), 
                        backgroundColor = '#292A30', 
                        color = '#FFFFFF') 
        }
    )

Timeframe.fo <- c("Years 1-5", "Year 6", "Year 7", "Year 8", "Year 9", "Year 10", "Year 11")

pubcountsTable <- 
  read_csv("pubcountsTable.csv",
           show_col_types = FALSE) %>%
  mutate(Timeframe = factor(Timeframe, 
                            levels = c("Years 1-5", "Year 6", "Year 7", "Year 8", "Year 9", "Year 10", "Year 11"))) %>%
  select(-starts_with("Total")) %>%
  filter(Status != "Aggregate",
         Timeframe != "Years 1-5") %>%
  droplevels() %>%
  pivot_wider(names_from = "Status", 
              values_from = c(Publications,starts_with("Q"))) %>%
  group_by(Timeframe) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  distinct()  %>%
  pivot_longer(!Timeframe, 
               names_to = c("quarter", ".value"), 
               names_pattern = "(\\w+\\d+)_(\\w+)") %>%
  drop_na(quarter) %>%
  rename(`In press` = In) %>%
  mutate(Quarter = case_when(
    quarter %in% "Q1" ~ "first",
    quarter %in% "Q2" ~ "second",
    quarter %in% "Q3" ~ "third",
    quarter %in% "Q4" ~ "fourth",
    )
  ) %>%
  unite("Cycle", c("Timeframe", "Quarter"), 
        sep = " ", 
        remove = FALSE) %>%
  mutate(Cycle = ordered(Cycle, 
                         levels = c("Year 6 first", "Year 6 second", "Year 6 third", "Year 6 fourth",
                                    "Year 7 first", "Year 7 second", "Year 7 third", "Year 7 fourth",
                                    "Year 8 first", "Year 8 second", "Year 8 third", "Year 8 fourth",
                                    "Year 9 first", "Year 9 second", "Year 9 third", "Year 9 fourth", 
                                    "Year 10 first", "Year 10 second", "Year 10 third", "Year 10 fourth",
                                    "Year 11 first", "Year 11 second", "Year 11 third", "Year 11 fourth")
    )
  ) %>%
  select(-quarter) %>%
  replace(is.na(.), 0)  %>%
  mutate(
    tip = case_when(
      `In press` == 0 ~ paste0(
        "There are ", Published, " linked publications in the ", Quarter, " quarter with none in-press."
      ),
      TRUE ~ paste0(
        "There are ", Published, " linked publications in the ", Quarter, " quarter with ", `In press`, " currently in-press."
      )
    )
  )  %>%
  mutate(tip = str_wrap(tip, 10)) %>%
  arrange(Cycle)

pubcountsPlot <- 
  pubcountsTable %>%
  mutate_all(~replace(., . == 0, NA)) %>%
  ggplot(aes(x = Cycle, 
             y = Published, 
             group = 1)) + 
  geom_line(color = "#85D4E3",
            size = 1, 
            show.legend = FALSE) + 
  ggiraph::geom_point_interactive(
    aes(fill = Published,
        tooltip = tip,
        data_id = Cycle), 
    size = 7, 
    shape = 21,
    stroke = 2.5,
    color = "#292A30",
    show.legend = FALSE) +
  scale_fill_distiller(palette = "Spectral",
                       direction = 1) +
  theme_minimal() +
  theme(plot.title = element_text(size = 26,
                                  family = "Jost* Black", 
                                  hjust = 1,
                                  vjust  = 0.5),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#292A30", 
                                       color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
      # panel.spacing.x = unit(0,"line"),
        strip.text.x = element_text(size = 20, 
                                    family = "Jost* Medium", 
                                    color = "#FFFFFF")) +
  facet_grid(.~ Timeframe, 
             space = 'free_x', 
             scales = 'free_x', 
             switch = 'x')
    
output$pubcountsInteractive <- 
  
  renderGirafe(

        girafe(ggobj = pubcountsPlot,
               fonts = list(sans = "Jost"),
               width = 20, 
               options = list(
                   opts_toolbar(position = "bottomleft"),
                   opts_sizing(rescale = TRUE, 
                               width = 1)
               )
        ) %>%
            girafe_options(ggiraph::opts_tooltip(css = tooltip_css, 
                                                 opacity = 0.95, 
                                                 offx = 20, 
                                                 offy = 20, 
                                                 use_stroke = TRUE, 
                                                 delay_mouseout = 100), 
                           opts_hover(css = "fill:#0b4069;
                                             stroke:#dfe3ee;
                                             stroke-width:0px;"),
                           opts_zoom(max = 20))
        
    )
    
  filteredpubList <- reactive({
        
        pubslist %>%
            filter(Reporting %in% input$Reporting) 
        
    })
    
    output$publist <- 
        DT::renderDataTable(server = FALSE,
                            escape = FALSE,
                            {
        DT::datatable(filteredpubList(),
                      rownames = FALSE,
                      escape = FALSE,
                      callback = JS('$("button.buttons-csv").css("color","#77cccc");
                                     $("button.buttons-csv").css("background-color","transparent"); 
                                     $("button.buttons-csv").css("background-color","transparent").hover(function(){
                                     $(this).css("color", "#222222");
                                     $(this).css("background-color", "#8ad3d3");
                                     }, function(){
                                     $(this).css("color","#77cccc");
                                     $(this).css("background-color", "transparent");
                                     });
                                     $("button.buttons-csv").css("border-color","#77cccc"); 
                                     $("button.buttons-colvis").css("color","#212529");
                                     $("button.buttons-colvis").css("background-color","#77cccc");
                                     $("button.buttons-colvis").css("border-color","#5bc2c2"); 
                                     $("button.buttons-collection").css("background-color","#77cccc"); 
                                     $("button.buttons-collection").css("color","#212529");
                                     $("button.buttons-collection").css("border-color","#5bc2c2"); 
                                     $("#DataTables_Table_0_length select").css("background-color", "#77cccc");
                                     $("#DataTables_Table_0_length select").css("color", "#292A30");
                                     $("#DataTables_Table_0_filter input").css("background-color", "#77cccc");
                                     $("#DataTables_Table_0_filter input").css("color", "#292A30");
                                     return table;'),
                      extensions = c('Buttons',
                                     'Responsive',
                                     'ColReorder'),
                      options = list(
                          dom = 'Blfrtip',
                          lengthMenu = list(c(10, 25, 100, -1), 
                                            c('10', '25', '100', 'All')),
                          pageLength = 10,
                          colReorder = TRUE,
                          searching = TRUE,
                          paging = TRUE,
                          lengthChange = TRUE,
                          autoWidth = TRUE,
                          scrollX = FALSE,
                          columnDefs = list(list(className = 'dt-center', targets = c(0:3,8:12)),
                                            list(targets = c(0:2), visible = TRUE, width = '100'),
                                            list(targets = c(3), visible = TRUE, width = '200'),
                                            list(targets = c(4), visible = TRUE, width = '400'),
                                            list(targets = c(5:14), visible = FALSE)
                                            ),
                          buttons = list(
                              list(extend = 'colvis',
                                   text = 'Toggle Column Visibility',
                                   columns = c(0:14),
                                   className = "btn btn-outline-primary mr-1",
                                   collectionLayout = 'two-column'
                              ),
                              list(extend = 'collection',
                                   buttons = list(list(extend = "copy", exportOptions = list(columns = ":visible")),
                                                  list(extend = "excel", exportOptions = list(columns = ":visible")),
                                                  list(extend = "pdf", exportOptions = list(columns = ":visible")),
                                                  list(extend = "print", exportOptions = list(columns = ":visible"))),
                                   text = 'Special Export',
                                   className = "btn btn-outline-primary mr-1"
                              ),
                              list(extend = "csv",
                                   text = "<span class='glyphicon glyphicon-download-alt'></span> CSV of the current page", 
                                     exportOptions = list(
                                         columns = ":visible",
                                         modifier = list(page = "current")
                                     ),
                                     className = "btn btn-outline-primary mr-1"
                              ),
                              list(extend = "csv", 
                                   text = "<span class='glyphicon glyphicon-download-alt'></span> CSV of the filtered table", 
                                   exportOptions = list(
                                       columns = ":visible",
                                       modifier = list(page = "all")
                                   ),
                                   className = "btn btn-outline-primary mr-1"
                              )
                          ),
                          initComplete = JS(
                              "function(settings, json) {",
                              "$(this.api().table().header()).css({'background-color': '#292A30', 'color': '#FFFFFF'});",
                              "}")
                        )
                      ) %>%
                formatStyle(colnames(pubslist), 
                            backgroundColor = "#292A30",
                            color ="#FFFFFF"
                            )
        }
        
    )

    jqui_sortable("#dtable thead tr")   
    
    output$pubListFull <- downloadHandler(
        file = function() {
            paste0("Full list of WVCTSI linked publications as of ", 
                   format(Sys.time(), "%b %d %Y"), 
                   ".csv", 
                   sep="")
        },
        content = function(file) {
            write.csv(pubslist, file, row.names = FALSE)
          }
        ) 
    }

shinyApp(ui, server)
