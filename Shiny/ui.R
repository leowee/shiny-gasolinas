body <- dashboardBody(
  
  # Não mostra Warnings para o usuário
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tabItems(
    #
    tabItem(
      tabName = "preço",
      fluidRow(
        column(width = 12,
               #
               box(
                 title = "Inputs",
                 width = NULL,
                 status = "warning",
                 solidHeader = TRUE,
                 column(
                   width = 3,
                   selectInput('estado',
                               label = 'Selecione o Estado:',
                               choices = sort(unique(base$`Estado - Sigla`)),
                               selected = "SP"
                   )
                 ),
                 column(
                   width = 3,
                   uiOutput("render_municipio")
                 ),
                 column(
                   width = 3,
                   uiOutput("render_bandeira")
                 ),
                 column(
                   width = 3,
                   uiOutput("render_produto")
                 )
               )
        )
      ),
      
      #
      fluidRow(
        column(width = 12,
               box(
                 title = "Gráfico",
                 status = "primary",
                 width = NULL,
                 solidHeader = TRUE,
                 dygraphOutput("plot_venda"),
                 dygraphOutput("plot_lucro")
               )
        )
      )
    ),
    
    #
    tabItem(tabName = "mapa",
            h2("Mapa tab content"),
            fluidRow(
              column(width = 12,
                     #
                     box(
                         title = "Inputs",
                         width = NULL,
                         status = "warning",
                         solidHeader = TRUE,
                         column(
                           width = 3,
                           selectInput(
                             "ano",
                             label = "Selecione o(s) ano(s):",
                             choices = unique(base$Ano),
                             selected = c("2017", "2018"),
                             multiple = T
                           )  
                         ), # Fim da Coluna
                        column(
                          width = 3,
                          selectInput(
                            "valor",
                            label = "Valor: ",
                            choices = c("Valor de Venda", "Valor de Compra", "Valor de Lucro"),
                            selected = "Valor de Venda"
                          )  
                        ), # Fim da Coluna
                        column(
                          width = 3,
                          selectInput(
                            "produto_mapa",
                            label = "Produto: ",
                            choices = sort(unique(base$Produto)),
                            selected = "GASOLINA"
                          ), # Fim da Coluna
                          column(
                            width = 3,
                            actionButton("update", "Criar/Atualizar Mapa", icon("paper-plane"),
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                          ) # Fim da Coluna
                        )
                       
                     ) # Fim do box
              ) # Fim da column
            ), # Fim do fluidRow
            
            #
            fluidRow(
              column(width = 12,
                     box(
                       title = "Mapa",
                       status = "primary",
                       width = NULL,
                       solidHeader = TRUE,
                       leafletOutput("mapa_interativo", height = 600)
                     )
              ) # Fim da Coluna
            ) # Fim do fluidRow
    ) # Fim do tabItem = "mapa"
    
  ) # Fim do tabItems
) # Fim do dashboardBody

# We'll save it in a variable `ui` so that we can preview it in the console
ui <- dashboardPage(
  dashboardHeader(title = "Gasolinas"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráfico de Preços", icon = icon("line-chart"), tabName = "preço"),
      menuItem("Mapa", icon = icon("globe-americas"), tabName = "mapa",
               badgeLabel = "WIP", badgeColor = "purple")
    )
  ),
  body
)