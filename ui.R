library("shinydashboard") 
library("shiny")
library("ggplot2")
library("plotly")
library("tidyverse")
library("raster")
library("leaflet")
library("colorRamps")
library("hashmap")


source("helper.R")

# node_taz_table <- read_csv("./data/node_taz_table.csv")
# vertexinfo <- readRDS("./data/vertexinfo.RDS")
taz <- readRDS("./data/TAZs.RDS")

times <- 1:168

ui <- dashboardPage(
  # skin="green",
  dashboardHeader(
    title = "GFEN RideAustin",
    titleWidth = 230
  ),
  dashboardSidebar(
    width=230,
    box(
      width=12, 
      background = "black",
      solidHeader = TRUE,
      # selectInput("model",
      #   label = "Model",
      #   choices = model_choices,
      #   selected = NULL),
      # radioButtons("func", 
      #   label=NULL, 
      #   choices=c( # ---- app panels ----
      #     "MAP SELECTION" = "map",
      #     "RESULTS" = "results",
      #     "TRACES" = "time_traces"
      #   ),
      #   selected = "map"
      # ),
      selectInput("color",
                  label = "Color map by",
                  choices = c("mean productivity"="mean_prod",
                              "ntrips (taz)"="ntrips_total",
                              "ntrips (taz-time)"="ntrips_spacetime"),
                  selected = "mean_prod"
      ),
      selectInput("tazs",
        label = "Select TAZ",
        choices = taz$ID,
        multiple = TRUE,
        selected = 1
      ),
      # p("Examples (12pm)"),
      # span(
      #   actionButton("sel_airport_pm", label = "Airport", width = "65px", class="inline-button"),
      #   actionButton("sel_downtown_pm", label = "Downtown", width = "65px", class="inline-button"),
      #   actionButton("sel_roundrock_pm", label = "RoundRock", width = "65px", class="inline-button"),
      #   actionButton("sel_domain_pm", label = "Domain", width = "65px", class="inline-button")
      # ),
      # p("Examples (12am)"),
      # span(
      #   actionButton("sel_airport_am", label = "Airport", width = "65px", class="inline-button"),
      #   actionButton("sel_downtown_am", label = "Downtown", width = "65px", class="inline-button"),
      #   actionButton("sel_roundrock_am", label = "RoundRock", width = "65px", class="inline-button"),
      #   actionButton("sel_domain_am", label = "Domain", width = "65px", class="inline-button")
      # ),
      selectInput("times",
        label = "Select times",
        choices = setNames(1:168, times_chr(1:168)),
        multiple = TRUE,
        selected = 1
      )
      # p("Or"),
      # selectInput("times",
      #             label = "Select preloaded",
      #             choices = setNames(times, times_chr),
      #             multiple = TRUE,
      #             selected = 1
      # )
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
    # ---- panels controls ----
    # conditionalPanel(
      # condition = "input.func == 'map'",
      fluidRow(
        div(class = "col-sm-12 col-md-10 col-lg-6",
          box(
            title = "Selection Map",
            width="100%",
            leafletOutput("map")# ,
            # p("Color represents number of observations.")
          )
        )
      ),
    # ),
    # conditionalPanel(
      # condition = "input.func == 'map'",
      fluidRow(
        div(class = "col-sm-12 col-md-10 col-lg-6",
          box(
            title = "Estimated density",
            width="100%",
            plotlyOutput("densities"),
            p("Using the Graph Fused Elastic Net")
            )
        )
      )
      # fluidRow(
      #   div(class = "col-sm-12 col-md-10 col-lg-6",
      #     box(
      #       title = "Animated transition",
      #       width = "100%",
      #       h3("Fused Elastic Net"),
      #       img(src="enet.gif", width="400"),
      #       h3("Fused Lasso"),
      #       img(src="fused_lasso.gif", width="400")
      #     )
      #   )
      # )
      # plotlyOutput("traces")
    # )
  )
)

