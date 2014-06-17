
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output) {
  
  
  variableInputIndicator <- reactive({
    switch(input$indicator,
           "keskipalkka" = "average_nominal_monthly_salary" ,
           "keskitulo" = "average_percapita_income" ,
           "keskimääräinen eläke" = "average_size_of_pensions" ,
           "syntyvyys" = "crude_birth_rate" ,
           "bruttokansantuote aluetasolla" = "gross_regional_product" ,
           "imeväiskuolleisuus" = "infant_mortality_rate" ,
           #"eliniänodote" = "life_expectancy" ,
           "eliniänodote miehillä" = "life_expectancy_men" ,
           "eliniänodote keskimäärin" = "life_expectancy_total" ,
           "eliniänodote naisilla" = "life_expectancy_women" ,
           "kuolleisuus" = "mortality_rate" ,
           "teatterikäyntejä per 1000 ihmistä" = "number_of_theater_goers_per_1000_population" ,
           "museokäyntejä per 1000 ihmistä" = "number_of_visits_to_museums_per_1000_population" ,
           "urheilupaikkoja" = "planar_sports_facilities_(playgrounds_and_fields)" ,
           "maaseutuväestön määrä" = "population_rural" ,
           "väestömäärä" = "population_total" ,
            "kaupunkiväestön määrä" = "population_urban" ,
            "sanomalehtiä per 1000 ihmistä" = "publication_of_newspapers_per_1000_people" ,
            "urheiluhalleja" = "sports_halls" ,
            "urheilustadioneita" = "stadiums_with_stands_for_1500_seats_or_more" ,
            "uima-altaita" = "swimming_pools")
  })
  
  variableInputIndicator_y <- reactive({
    switch(input$indicator_y,
           "keskipalkka" = "average_nominal_monthly_salary" ,
           "keskitulo" = "average_percapita_income" ,
           "keskimääräinen eläke" = "average_size_of_pensions" ,
           "syntyvyys" = "crude_birth_rate" ,
           "bruttokansantuote aluetasolla" = "gross_regional_product" ,
           "imeväiskuolleisuus" = "infant_mortality_rate" ,
           #"eliniänodote" = "life_expectancy" ,
           "eliniänodote miehillä" = "life_expectancy_men" ,
           "eliniänodote keskimäärin" = "life_expectancy_total" ,
           "eliniänodote naisilla" = "life_expectancy_women" ,
           "kuolleisuus" = "mortality_rate" ,
           "teatterikäyntejä per 1000 ihmistä" = "number_of_theater_goers_per_1000_population" ,
           "museokäyntejä per 1000 ihmistä" = "number_of_visits_to_museums_per_1000_population" ,
           "urheilupaikkoja" = "planar_sports_facilities_(playgrounds_and_fields)" ,
           "maaseutuväestön määrä" = "population_rural" ,
           "väestömäärä" = "population_total" ,
           "kaupunkiväestön määrä" = "population_urban" ,
           "sanomalehtiä per 1000 ihmistä" = "publication_of_newspapers_per_1000_people" ,
           "urheiluhalleja" = "sports_halls" ,
           "urheilustadioneita" = "stadiums_with_stands_for_1500_seats_or_more" ,
           "uima-altaita" = "swimming_pools")
  })
  
  
datasetInput_reg <- reactive(function() {

    load("data/datFedDist.RData")
    load("data/tab_geo_reg.RData")
    plotDat <- tab_geo_reg[tab_geo_reg$indicator == variableInputIndicator(),]
    plotDat <- plotDat[plotDat$year == input$year,]
    plotDat <- plotDat[order(plotDat$order),]
#    load("data/tab_geo_fd.RData")
  })

datasetInput_fd <- reactive(function() {
  load("data/tab_geo_fd.RData")
  plotDat <- tab_geo_fd[tab_geo_fd$indicator == variableInputIndicator(),]
  plotDat <- plotDat[plotDat$year == input$year,]
  plotDat <- plotDat[order(plotDat$order),]
})

  
  plotInput_reg <- reactive(function() {
    plotDat <- datasetInput_reg()
    cnames <- stats:::aggregate.formula(cbind(long, lat) ~ region_en, data=plotDat, mean)
        library(ggplot2)
    # subset data for highlighiting Russia
    ggplot(plotDat, aes(long,lat,group=group)) +
      geom_polygon(aes(fill = value)) +
      geom_polygon(data = plotDat, aes(long,lat), fill=NA, color = "white") + # white borders
      geom_text(data=cnames, aes(long, lat, label = region_en, group=region_en), size=3, color="white") +
      coord_map(project="orthographic", xlim=c(25,190),
                ylim=c(45,70))  + # projection
      labs(title=paste(input$indicator," vuonna ",input$year," aluetasolla",sep="")) +
      theme(legend.position="top") +
      guides(fill = guide_legend(keywidth = 3, keyheight = 1))
    
  })

  plotInput_reg_line <- reactive(function() {
    load("data/datReg.RData")
    plotDat <- datReg[datReg$indicator == variableInputIndicator(),]
    library(ggplot2)
    # subset data for highlighiting Russia
    ggplot(plotDat, aes(year,value,group=region_en)) +
      geom_point() + geom_line() +
      geom_text(data=merge(plotDat, aggregate(year ~ region_en, plotDat, max),
                                by=c("year","region_en")),
                  aes(year,value,group=region_en,label=region_en),
                hjust=1) +
      labs(title=paste(input$indicator," vuonna ",input$year," aluetasolla",sep="")) +
      theme(legend.position="top")
    
  })  

plotInput_reg_scatter <- reactive(function() {
  load("data/datReg.RData")
  plotDat_x <- datReg[datReg$indicator == variableInputIndicator(),]
  plotDat_y <- datReg[datReg$indicator == variableInputIndicator_y(),]
  plotDat_x <- plotDat_x[plotDat_x$year == input$year,]
  plotDat_y <- plotDat_y[plotDat_y$year == input$year,]
  plotDat <- merge(plotDat_x,plotDat_y,by=c("region_en","year","level"))
  names(plotDat)[names(plotDat) == 'value.x'] <- "var_x"
  names(plotDat)[names(plotDat) == 'value.y'] <- "var_y"
  
  library(ggplot2)
  # subset data for highlighiting Russia
  ggplot(plotDat, aes(x=var_x,y=var_y,
                      group=1,
                      label=region_en)) +
    geom_point() + 
    geom_smooth() +
    geom_text(vjust=-1) +
    labs(x=as.character(plotDat$indicator.x[1]),
         y=as.character(plotDat$indicator.y[1]))
  
}) 
  
  output$plot_reg <- reactivePlot(function() {
    print(plotInput_reg())
  })
  
  output$plot_reg_line <- reactivePlot(function() {
    print(plotInput_reg_line())
  })

output$plot_reg_scatter <- reactivePlot(function() {
  print(plotInput_reg_scatter())
})
  
  plotInput_fd <- reactive(function() {
    
    plotDat <- datasetInput_fd()
    cnames <- stats:::aggregate.formula(cbind(long, lat) ~ region_en, data=plotDat, mean)
    library(ggplot2)
    ggplot(plotDat, aes(long,lat,group=group)) +
      geom_polygon(aes(fill = value)) +
      geom_polygon(data = plotDat, aes(long,lat), fill=NA, color = "white") + # white borders
      geom_text(data=cnames, aes(long, lat, label = region_en, group=region_en), size=4, color="white") +
      coord_map(project="orthographic", xlim=c(25,190),
                ylim=c(45,70))  + # projection
      labs(title=paste(input$indicator," vuonna ",input$year," federaatiopiiritasolla",sep="")) +
      theme(legend.position="top") +
      guides(fill = guide_legend(keywidth = 3, keyheight = 1))
    
  })
  
  plotInput_fd_line <- reactive(function() {
    load("data/datFedDist.RData")
    plotDat <- datFedDist[datFedDist$indicator == variableInputIndicator(),]
    library(ggplot2)
    # subset data for highlighiting Russia
    ggplot(plotDat, aes(year,value,group=region_en)) +
      geom_point() + geom_line() +
      geom_text(data=merge(plotDat, aggregate(year ~ region_en, plotDat, max),
                           by=c("year","region_en")),
                aes(year,value,group=region_en,label=region_en),
                hjust=1) +
      labs(title=paste(input$indicator," vuonna ",input$year," federaatiopiiritasolla",sep="")) +
      theme(legend.position="top")
    
  })  

plotInput_fd_scatter <- reactive(function() {
  load("data/datFedDist.RData")
  plotDat_x <- datFedDist[datFedDist$indicator == variableInputIndicator(),]
  plotDat_y <- datFedDist[datFedDist$indicator == variableInputIndicator_y(),]
  plotDat_x <- plotDat_x[plotDat_x$year == input$year,]
  plotDat_y <- plotDat_y[plotDat_y$year == input$year,]
  plotDat <- merge(plotDat_x,plotDat_y,by=c("region_en","year","level"))
  names(plotDat)[names(plotDat) == 'value.x'] <- "var_x"
  names(plotDat)[names(plotDat) == 'value.y'] <- "var_y"
  
  library(ggplot2)
  # subset data for highlighiting Russia
    ggplot(plotDat, aes(x=var_x,y=var_y,
                        group=1,
                        label=region_en)) +
      geom_point() + 
    geom_smooth() +
    geom_text(vjust=-1) +
    labs(x=as.character(plotDat$indicator.x[1]),
          y=as.character(plotDat$indicator.y[1]))
  
}) 


  output$plot_fd <- reactivePlot(function() {
    print(plotInput_fd())
  })
  
  output$plot_fd_line <- reactivePlot(function() {
    print(plotInput_fd_line())
  })

output$plot_fd_scatter <- reactivePlot(function() {
  print(plotInput_fd_scatter())
})

output$downloadPlot_reg <- downloadHandler(
  filename = function() { paste("indicator_",input$indicator,"_year_",input$year,Sys.time(),'.png', sep='') },
  content = function(file) {
    png(file, width=3600, height=3600,res=72)
    print(plotInput_reg())
    dev.off()
  })
  
  output$downloadPlot_reg_line <- downloadHandler(
    filename = function() { paste("indicator_",input$indicator,Sys.time(),'.png', sep='') },
    content = function(file) {
      png(file, width=800, height=600,res=72)
      print(plotInput_reg_line())
      dev.off()
    })

output$downloadPlot_reg_scatter <- downloadHandler(
  filename = function() { paste("scatter","indicator_",input$indicator,Sys.time(),'.png', sep='') },
  content = function(file) {
    png(file, width=800, height=600,res=72)
    print(plotInput_reg_scatter())
    dev.off()
  })

output$downloadPlot_fd <- downloadHandler(
  filename = function() { paste("indicator_",input$indicator,"_year_",input$year,Sys.time(),'.png', sep='') },
  content = function(file) {
    png(file, width=1600, height=1600,res=72)
    print(plotInput_fd())
    dev.off()
  })
  
  output$downloadPlot_fd_line <- downloadHandler(
    filename = function() { paste("indicator_",input$indicator,Sys.time(),'.png', sep='') },
    content = function(file) {
      png(file, width=800, height=600,res=72)
      print(plotInput_fd_line())
      dev.off()
    })
  
})


