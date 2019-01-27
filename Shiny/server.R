library(shiny)

# Define server logic required
shinyServer(function(input, output) {
### Gráfico de Linha
  # Renderizando "Selecionando o Município" baseado no Estado
  output$render_municipio <- renderUI({
    filtrada <- unique(base_media[input$estado,
                            .(Município),
                            on = .(`Estado - Sigla`)]
                       [order(Município)])
    
    selectInput('municipio',
                label = 'Selecione o Município:',
                choices = filtrada,
                selected = "SAO PAULO"
    )
  })
  
  # Renderizando "Selecionando a Bandeira" baseado no Estado e Município
  output$render_bandeira <- renderUI({
    filtrada <- unique(base_media[.(input$estado, input$municipio),
                            .(Bandeira),
                            on = .(`Estado - Sigla`,  Município)]
                       [order(Bandeira)])
    
    selectInput('bandeira',
                label = 'Selecione a Bandeira:',
                choices = filtrada,
                selected = "IPIRANGA"
    )
  })
  
  # Renderizando "Selecionando o Produto" baseado no Estado, Município e Produto
  output$render_produto <- renderUI({
    filtrada <- unique(base_media[.(input$estado, input$municipio, input$bandeira),
                            .(Produto),
                            on = .(`Estado - Sigla`,  Município, Bandeira)]
                       [order(Produto)])
    selectInput('produto',
                label = 'Selecione a Produto:',
                choices = filtrada,
                selected = "GASOLINA",
                multiple = F
    )
  })
  
  #
  base_media_filtrada <- reactive({
    filtrada <- base_media[
      `Estado - Sigla` %in% input$estado
      & Município %in% input$municipio
      & Bandeira %in% input$bandeira
      & Produto %in% input$produto
    ]
    
    filtrada[, c("Data da Coleta", "Venda", "Compra", "Lucro")]
  })
  
  #
  output$plot_venda <- renderDygraph({
    dygraph(base_media_filtrada()[, !c("Lucro")], main = paste0("Preço por Venda e Compra de ", input$produto)) %>%
      dyAxis(
        "x", 
        label = "Data de Coleta") %>% 
      dyAxis(
        "y", 
        label = "Preço (R$/Litro)",
        valueFormatter = 'function(x){return "R$ " + x.toFixed(2) +"/litro"}'
      ) %>%
      dyOptions(
        drawPoints = T, 
        connectSeparatedPoints = T,
        fillGraph = T,
        colors = RColorBrewer::brewer.pal(3, "Dark2")
      ) %>% 
      dyCrosshair(direction = "vertical") %>%
      dyLegend(width = 500) %>%
      dyRangeSelector()
    })
  
  output$plot_lucro <- renderDygraph({
    dygraph(base_media_filtrada()[, !c("Venda", "Compra")], main = paste0("Preço por Lucro de ", input$produto)) %>%
      dyAxis(
        "x", 
        label = "Data de Coleta") %>% 
      dyAxis(
        "y", 
        label = "Preço (R$/Litro)",
        valueFormatter = 'function(x){return "R$ " + x.toFixed(2) +"/litro"}'
      ) %>%
      dyOptions(
        drawPoints = T, 
        connectSeparatedPoints = T,
        fillGraph = T,
        colors = RColorBrewer::brewer.pal(3, "Dark2")[3]
      ) %>% 
      dyHighlight(
        highlightSeriesOpts = list(strokeWidth = 3)
      ) %>% 
      dyLegend() %>%
      dyRangeSelector()
  })
  
### Mapa interativo
  # Paleta de Cores
  colorpal <- reactive({
    # Valor de Venda
    if (input$valor == "Valor de Venda") {
      bins <- c(2.00, 2.50, 3.00, 3.50, 4.00, Inf)
      #bins <- c((1 * 4)/7, (2 * 4)/7, (3 * 4)/7, (4 * 4)/7, (5 * 4)/7, (6 * 4)/7, (7 * 4)/7, Inf)
      pal <- colorBin("YlGn", domain = mapa@data$Valor, bins = bins)
    }
    
    # Valor de Compra
    else if (input$valor == "Valor de Compra") {
      bins <- c(2.00, 2.50, 3.00, 3.50, 4.00, Inf)
      #bins <- c((1 * 4)/7, (2 * 4)/7, (3 * 4)/7, (4 * 4)/7, (5 * 4)/7, (6 * 4)/7, (7 * 4)/7, Inf)
      pal <- colorBin("Oranges", domain = mapa@data$Valor, bins = bins)
    }
    
    # Valor de Lucro
    else {
      bins <- c(0.00, 0.20, 0.40, 0.60, 0.80, Inf)
      #bins <- c((1 * 4)/7, (2 * 4)/7, (3 * 4)/7, (4 * 4)/7, (5 * 4)/7, (6 * 4)/7, (7 * 4)/7, Inf)
      pal <- colorBin("BuPu", domain = mapa@data$Valor, bins = bins)
    }
    pal
  })
  
  # Cria o "grosso" do mapa
  output$mapa_interativo <- renderLeaflet({
    # Define terreno a ser usado nos Tiles
    carto = "http://a.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
    
    # Cria Mapa
    leaflet(mapa) %>%
      addTiles(carto, attribution = 'Map by <a href="https://github.com/CartoDB/cartodb/wiki/BaseMaps-available">Carto BaseMaps</a>') %>%
      fitBounds(~mapa@bbox[1], ~mapa@bbox[2], ~mapa@bbox[3], ~mapa@bbox[4])
  })
  
  observeEvent(input$update, {
    #
    média_estados <- base[(Ano %in% input$ano) & (Produto %in% input$produto_mapa), .(Media = mean(get(input$valor), na.rm = T) ), by = `Estado - Sigla`]
    mapa_data <- mapa_data[média_estados, Valor := i.Media, on = c(SIGLA_ESTADO = "Estado - Sigla")]
    
    #
    mapa_filtrado <- mapa
    mapa_filtrado@data <- as.data.frame(mapa_data)
    
    #
    pal <- colorpal()
    
    #
    labels <- sprintf(
      "<strong>%s</strong><br/>R$ %g / Litro",
      mapa_filtrado@data$NM_ESTADO, round(mapa_filtrado@data$Valor, 2)
    ) %>% lapply(htmltools::HTML)
    
    leafletProxy("mapa_interativo", data = mapa_filtrado) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        stroke = T, 
        color = "black", 
        weight = 2,
        fillColor = ~pal(mapa_filtrado@data$Valor), 
        fillOpacity = 0.9,
        highlight = highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~Valor, opacity = 0.7, title = input$valor, position = "bottomright")
  }) # Observe ending
  
}) # Fim do server.r
