library("shinydashboard") 
library("shiny")
library("ggplot2")
library("plotly")
library("tidyverse")
library("raster")
library("leaflet")
library("colorRamps")
library("hashmap")


source("helper.R") # include utility functionsload_libraries()

# read taz and graph vertex information
# they have been saved in R binary format for speed
# see the file save to binary.R
# all the files necessary to run it are in the
# repository github.com/mauriciogtec/gfen_suplemental

taz = readRDS("./data/TAZs.RDS")
vertexdata = readRDS("./data/vertex_data.RDS")
splitlevels = readRDS("./data/splitlevels.RDS")
probs = readRDS("./data/probs.RDS")

num_tazs = nrow(vertexdata) / 168
node2vertex = hashmap(vertexdata$node, 1:nrow(vertexdata))
ntrips = vertexdata %>% 
  group_by(taz) %>% 
  summarize(ntrips = sum(node_counts))
taz2ntrips = hashmap(ntrips$taz, ntrips$ntrips)
taz@data$ntrips = taz2ntrips[[taz@data$ID]]


midpoints = 0.5 * (splitlevels[-1] + splitlevels[-length(splitlevels)])
delta = splitlevels[-1] - splitlevels[-length(splitlevels)]
densities = probs / delta
d = length(splitlevels) - 1
global_mean = rep(1.0 / d, d) / delta
dfmean = data.frame(
  x=c(splitlevels[1], midpoints, splitlevels[length(splitlevels)]),
  value=c(0, global_mean, 0))
allmeans = apply(probs *  midpoints, 2, sum)
lb_mean = min(allmeans) - 1e-3
ub_mean = max(allmeans) + 1e-3


airportIcon <- makeIcon(
  iconUrl = "./icons/airport-512.png",
  iconWidth = 20, iconHeight = 20
)

# northIcon <- makeIcon(
#   iconUrl = "./icons/north-arrow-2.png",
#   iconWidth = 15, iconHeight = 20
# )

basemap = leaflet(options=list(attributionControl=FALSE)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(-97.82, 30.3, zoom = 11) %>% 
  addMarkers(-97.6665, 30.2015, icon = airportIcon)  # %>%
  # addMarkers(-97.61, 30.33, icon = northIcon)


shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    if (input$color == "mean_prod") {
      shiny::validate(shiny::need(
        input$times,
        "Select at least one time on sidebar menu to see the map."))
      time = as.integer(input$times[1])
      ts = time_slice(time, num_tazs)
      taz2vertex = hashmap(vertexdata$taz[ts], ts)
      meanprod = apply(probs[ ,ts] * midpoints, 2, sum)
      vertex2prod = hashmap(ts, meanprod)
      taz@data$vertex = taz2vertex[[taz@data$ID]]
      taz@data$prod = vertex2prod[[taz@data$vertex]]
      labs = paste0(
        "id: ", taz@data$ID, "<br>",
        "prod: ", round(taz@data$prod, 2))
      labs = map(labs, HTML)
      lower = min(taz@data$prod, na.rm=TRUE)
      upper = max(taz@data$prod, na.rm=TRUE)
      lower = lb_mean
      upper = ub_mean
      pal = colorNumeric(c("lightblue", "blue", "red"),
                         domain=c(lower, upper),
                         na.color="grey")
      fill_formula = ~pal(prod)
      labelf = labelFormat()
      legtitle = "mean prod."
    }
    
    if (input$color == "ntrips_total") {
      taz@data$ntrips_log10 = sqrt(taz@data$ntrips)
      labs <- paste0(
        "id: ", taz@data$ID, "<br>",
        "ntrips: ", taz@data$ntrips, "<br>")
      labs = map(labs, HTML)
      lower = min(taz@data$ntrips_log10, na.rm=TRUE)
      upper = max(taz@data$ntrips_log10, na.rm=TRUE)
      pal = colorNumeric(c("lightblue", "blue", "red"),
                         domain=c(lower, upper),
                         na.color="grey")
      fill_formula = ~pal(ntrips_log10)
      legtitle = "#trips (TAZ)"
      labelf = labelFormat(transform=function(x) as.integer(x^2))
    }
    
    if (input$color == "ntrips_spacetime") {
      time = as.integer(input$times[1])
      ts = time_slice(time, num_tazs)
      ntrips = vertexdata[ts, ] %>% 
        group_by(taz) %>% 
        summarize(ntrips_st = sum(node_counts))
      taz2ntrips = hashmap(ntrips$taz, ntrips$ntrips_st)
      taz@data$ntrips_st = taz2ntrips[[taz@data$ID]]
      taz@data$ntrips_st_log10 = sqrt(taz@data$ntrips_st)
      
      labs <- paste0(
        "id: ", paste(taz@data$ID, time, step="-"), "<br>",
        "ntrips: ", taz@data$ntrips_st, "<br>")
      labs = map(labs, HTML)
      lower = min(taz@data$ntrips_st_log10, na.rm=TRUE)
      upper = max(taz@data$ntrips_st_log10, na.rm=TRUE)
      pal = colorNumeric(c("lightblue", "blue", "red"),
                         domain=c(lower, upper),
                         na.color="grey")
      fill_formula = ~pal(ntrips_st_log10)
      legtitle = "#trips (TAZ-time)"
      labelf = labelFormat(transform=function(x) as.integer(x^2))
    }
    basemap %>%
      addPolygons(
        data = taz,
        label = labs, 
        weight = 0.2,
        smoothFactor = 0.5,
        fillOpacity = 0.5,
        layerId = ~TAZ,
        fillColor = fill_formula) %>% 
      addLegend(
        position = "bottomleft",
        title = legtitle,
        labFormat = labelf,
        pal = pal,
        opacity=0.5,
        values = c(lower, upper))
  })
  
  data_of_click <- reactiveValues(node=NULL)
  
  observeEvent(input$map_shape_click,{
    data_of_click$taz <- input$map_shape_click$id
    pos <- which(data_of_click$node == input$tazs)
    if (length(pos) > 0) {
      new_selected <- input$nodes[-pos]
    } else {
      new_selected <- c(input$tazs, data_of_click$taz)
    }
    updateSelectInput(session, "tazs", selected = new_selected)
  })
  # 

  output$densities <- renderPlotly({
    if (length(input$tazs) == 0 || length(input$times) == 0) {
      return(NULL)
    }
    
    entries <- expand.grid(i = as.integer(input$tazs), t = as.integer(input$times))
    # ids <- map2_dbl(entries$i, entries$t, idx) # requires dbl instead of int.... why?
    node <- paste(entries$i, entries$t, sep="-")
    ids <- node2vertex[[node]]
    # dens <- map(ids, ~ as.numeric(densities[ ,.])) %>%
    D = rbind(matrix(0, 1, length(ids)),
              densities[ ,ids, drop=FALSE],
              matrix(0, 1, length(ids)))
    D <- data.frame(D, row.names=NULL)
    names(D) <- paste0("TAZ ", entries$i, " - ", times_chr(entries$t))
    D$x = dfmean$x
    plotdata <- D %>%
      gather(variable, value, -x)

    g <- ggplot() +
      geom_area(data=plotdata, aes(fill=variable, x = x, y = value), position="identity", alpha=0.2) +
      geom_line(data=plotdata, aes(color=variable, x = x, y = value), show.legend = FALSE) +
      geom_line(data=dfmean, aes(x=x, y=value), linetype="dashed", show.legend = FALSE) +
      # ylim(0, 0.06) +
      # xlim(0, 72) +
      labs(fill="", colour="")
    g
  })

  # output$traces <- renderPlotly({
  #   if (length(input$nodes) == 0) return(NULL)
  #   traces <- matrix(0, length(times), length(input$nodes))
  #   for (t in times) {
  #     means <- as.integer(input$nodes) %>% 
  #       map_dbl(~ idx(., t)) %>% 
  #       map(~ as.numeric(densities[ ,.])) %>%
  #       map_dbl(~ series_mean(evalpts, .))
  #     print(means)
  #     traces[t, ] <- means
  #   }
  #   traces <- as.data.frame(traces)
  #   names(traces) <- paste("node", input$nodes)
  #   traces$time <- setNames(times, times_chr)
  #   plotdata <- traces %>% 
  #     gather(variable, value, -time) 
  #   g <- ggplot(plotdata) +
  #     aes(colour = variable, x = time, y = value) +
  #     labs(xlab = "time") +
  #     geom_line()
  #   ggplotly(g)
  # })
  
  session$onSessionEnded(function() stopApp())
})
