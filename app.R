# parkrun, access, equity
# version 1.0

# install and/or load required packages
install_n_load <- function(package){
  for(i in 1:length(package)){
    if(eval(parse(text=paste("require(",package[i],")")))==0) {
      install.packages(package)
      }
    }
  return (eval(parse(text=paste("require(",package,")"))))
}
required_packages<-c("ggplot2","cowplot","shiny","leaflet")
install_n_load(required_packages)

# load data
centroid_lsoa = read.csv("./centroids.csv")
data = raster::shapefile("./data")
parkrun_marker = raster::shapefile("./marker")
vars1 <- c("Absolute" = "absolute","Relative (UNSTABLE!)" = "relative")   

# ##   User-interface   #  ### #### ### #  ### #### ### #  
ui <- fluidPage(
  
    sidebarPanel(
      h4("parkrun, access, equity"),
      # color scaling adjuster
      selectInput("color", "Scaling", vars1),
      # plot bivariate relationships
      plotOutput("p1_dist", height = 400),
      width = 3
      ),
    
    mainPanel(
      # leaflet map init
      leafletOutput("mymap", height = "95vh",width = "100%"),
      tags$div(id="ref",
               'Schneider et al.',tags$em('parkrun, access, equity.'), '2018. Data and code available at:',tags$a(href="https://github.com/bitowaqr/parkrun_access_equity", "https://github.com/bitowaqr/parkrun_access_equity",target="_blank")),
      width = 8
      )
    
    )
   


# ##   SERVER   #  ### #### ### #  ### #### ### #  
server <- function(input, output) {
  
  # Define base-map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner Map") %>%
      addTiles(group = "OSM Map") %>%
      addProviderTiles("CartoDB.Positron",group= "Carto Map", options= providerTileOptions(opacity = 0.99)) %>%
      setView(lng = -1.43, lat = 53.36, zoom = 7
      ) 
  })
  
  # Plot bivariate relationships
  output$p1_dist <- renderPlot({
    # plot for area within scope
    areasInBounds <- reactive({
      if (is.null(input$mymap_bounds)){return(data)}
      bounds <- input$mymap_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      subset.data = subset(data,
                           data$lat >= latRng[1] & data$lat <= latRng[2] &
                             data$lon >= lngRng[1] & data$lon <= lngRng[2]
                           )
      return(subset.data)
    })
    # data within scope
    plot_data = areasInBounds()@data
    # plot distance ~population density
    p1 = ggplot(plot_data[sample(1:length(plot_data[,1]),
                                 ifelse(length(plot_data[,1])>1000,1000,length(plot_data[,1]))),]) +
          geom_point(aes(x=log(pp_dnst), y=mn_dstn)) +
          geom_smooth(aes(x=log(pp_dnst),y=mn_dstn),method='lm',formula=y~x) +
          xlab("Population Density (log)") +
          ylab("Distance to nearest event") +
          ggtitle(paste( ifelse(length(plot_data[,1])>1000,1000,length(plot_data[,1])),
                  "sample data points"))
    
    # plot distance ~ deprivation
    p2 = ggplot(plot_data[sample(1:length(plot_data[,1]),
                                 ifelse(length(plot_data[,1])>1000,1000,length(plot_data[,1]))),]) +
            geom_point(aes(x=a, y=mn_dstn)) +
            geom_smooth(aes(x=a,y=mn_dstn),method='lm',formula=y~x) +
            xlab("Deprivation Score") +
            ylab("Distance to nearest event") +
            ggtitle(paste( ifelse(length(plot_data[,1])>1000,1000,length(plot_data[,1])),
                           "sample data points"))
          
    # combine plots
    p3 = cowplot::plot_grid(p1,p2,nrow=2)
    
    return(p3)
  })
  
  
  
  
} # end of server
  
  
# Run the map
shinyApp(ui = ui, server = server)

