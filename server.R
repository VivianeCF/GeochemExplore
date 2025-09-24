# Servidor shiny --------------------------------------------------------------
shinyServer(function(input, output, session) {
  # Carrega os pacotes do diretório R
source("R/pacotes.R")

# Carrega as funções do diretório R
source("R/minhas_funcoes.R")
# Cria um caminho de recurso explícito para a pasta www/
# O nome 'icons' é o que será usado na URL web
addResourcePath("icons", "www")
  # Adicione esta função para renderizar a data dinamicamente
output$data_atualizacao_output <- renderUI({
  p(paste("Data:", Sys.Date()))
})
# Lógica para renderizar os inputs de arquivo
output$csv_inputs <- renderUI({
  if (input$use_predefined_files) {
    div(
      p("Usando arquivos CSV predefinidos da pasta 'inputs/'."),
      tags$ul(
        tags$li("mydata.csv"),
        tags$li("myjob.csv"),
        tags$li("mylitho.csv"),
        tags$li("mylegend.csv")
      )
    )
  } else {
    div(
      fileInput("mydata_upload", "1. Escolha mydata.csv",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("myjob_upload", "2. Escolha myjob.csv",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("mylitho_upload", "3. Escolha mylitho.csv",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("mylegend_upload", "4. Escolha mylegend.csv",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    )
  }
})

output$shp_inputs <- renderUI({
  if (input$use_predefined_files) {
    div(
      p("Usando arquivos de forma predefinidos da pasta 'inputs/'."),
      tags$ul(
        tags$li("mygeology.shp"),
        tags$li("mywatershed.shp"),
        tags$li("myoutlet.shp"),
        tags$li("mystream.shp")
      )
    )
  } else {
    div(
      fileInput("mygeology_upload", "5. Escolha os arquivos de mygeology (.shp, .shx, .dbf, .prj)",
                multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")),
      fileInput("mywatershed_upload", "6. Escolha os arquivos de mywatershed (.shp, .shx, .dbf, .prj)",
                multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")),
      fileInput("myoutlet_upload", "7. Escolha os arquivos de myoutlet (.shp, .shx, .dbf, .prj)",
                multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")),
      fileInput("mystream_upload", "8. Escolha os arquivos de mystream (.shp, .shx, .dbf, .prj)",
                multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj"))
    )
  }
})

# Lógica reativa para carregar os dados
mydata_upload <- reactive({
  if (input$use_predefined_files) {
    path <- file.path("inputs", "mydata.csv")
    read_csv2(path, locale = locale(encoding = "latin1"))
  } else {
    req(input$mydata_upload)
    read_csv2(input$mydata_upload$datapath, locale = locale(encoding = "latin1"))
  }
})

# ... Repita a mesma lógica 'if/else' para os outros arquivos ...

mygeology_upload <- reactive({
  if (input$use_predefined_files) {
    path <- file.path("inputs", "mygeology.shp")
    sf::st_read(path)
  } else {
    req(input$mygeology_upload)
    temp_dir <- dirname(input$mygeology_upload$datapath[1])
    temp_files <- input$mygeology_upload
    file.rename(temp_files$datapath, file.path(temp_dir, temp_files$name))
    shp_file <- temp_files$name[grep(".shp$", temp_files$name)]
    if (length(shp_file) == 0) return(NULL)
    sf::st_read(file.path(temp_dir, shp_file))
  }
})

ws_upload <- reactive({
  if (input$use_predefined_files) {
    path <- file.path("inputs", "mywatershed.shp")
    sf::st_read(path)
  } else {
    req(input$mywatershed_upload)
    temp_dir <- dirname(input$mywatershed_upload$datapath[1])
    temp_files <- input$mywatershed_upload
    file.rename(temp_files$datapath, file.path(temp_dir, temp_files$name))
    shp_file <- temp_files$name[grep(".shp$", temp_files$name)]
    if (length(shp_file) == 0) return(NULL)
    sf::st_read(file.path(temp_dir, shp_file))
  }

})
pt_upload <- reactive({
  if (input$use_predefined_files) {
    path <- file.path("inputs", "myoutlet.shp")
    sf::st_read(path)
  } else    {
    req(input$myoutlet_upload)
    temp_dir <- dirname(input$myoutlet_upload$datapath[1])
    temp_files <- input$myoutlet_upload
    file.rename(temp_files$datapath, file.path(temp_dir, temp_files$name))
    shp_file <- temp_files$name[grep(".shp$", temp_files$name)]
    if (length(shp_file) == 0) return(NULL)
    sf::st_read(file.path(temp_dir, shp_file))
  }
})
rios_upload <- reactive({
  if (input$use_predefined_files) {
    path <- file.path("inputs", "mystream.shp")
    sf::st_read(path)
  } else    {
    req(input$mystream_upload)
    temp_dir <- dirname(input$mystream_upload$datapath[1])
    temp_files <- input$mystream_upload
    file.rename(temp_files$datapath, file.path(temp_dir, temp_files$name))
    shp_file <- temp_files$name[grep(".shp$", temp_files$name)]
    if (length(shp_file) == 0) return(NULL)
    sf::st_read(file.path(temp_dir, shp_file))
  }
})
mylitho_upload <- reactive({
  if (input$use_predefined_files) {
    path <- file.path("inputs", "mylitho.csv")
    read_csv2(path, locale = locale(encoding = "latin1"))
  } else {
    req(input$mylitho_upload)
    read_csv2(input$mylitho_upload$datapath, locale = locale(encoding = "latin1"))
  }
})

mylegend_upload <- reactive({
  if (input$use_predefined_files) {
    path <- file.path("inputs", "mylegend.csv")
    read_csv2(path, locale = locale(encoding = "latin1"))
  } else {
    req(input$mylegend_upload)
    read_csv2(input$mylegend_upload$datapath, locale = locale(encoding = "latin1"))
  }
})

myjob_upload <- reactive({
  if (input$use_predefined_files) {
    path <- file.path("inputs", "myjob.csv")
    read_csv2(path, locale = locale(encoding = "latin1"))
  } else {
    req(input$mylegend_upload)
    read_csv2(input$myjob_upload$datapath, locale = locale(encoding = "latin1"))
  }
})

 
  # 2. Controle de upload completo
  upload_completo <- reactive({
    !is.null(mydata_upload()) &&
      !is.null(myjob_upload()) &&
      !is.null(mylitho_upload()) &&
      !is.null(mylegend_upload()) &&
      !is.null(mygeology_upload()) &&
      !is.null(ws_upload()) &&
      !is.null(pt_upload()) &&
      !is.null(rios_upload())
  })
  
  output$upload_status <- renderText({
    if (!upload_completo()) {
      "Aguardando o upload de todos os arquivos..."
    } else {
      "Todos os arquivos foram carregados com sucesso! Você pode ir para a aba 'Mapa'."
    }
  })
  
  # 3. Só processe os dados se upload_completo() for TRUE
  mydata_processed <- reactive({
    req(upload_completo())
    mydata <- dplyr::left_join(mydata_upload(), mylitho_upload(), by = "ID")
    mydata <- dplyr::left_join(mydata, mylegend_upload(), by = "Geo_Reg")
    mydata$Geo_Reg <- factor(mydata$Geo_Reg)
    sm <- summary(mydata$Geo_Reg)
    sm <- data.frame(Geo_Reg = names(sm), cont = sm)
    mydata <- dplyr::left_join(mydata, sm, by = "Geo_Reg")
    mydata_sub <- subset(mydata, cont >= 5)
    mydata_sub <- mydata_sub[order(mydata_sub$Geo_Reg), ]
    mydata_sub$Geo_Reg <- factor(mydata_sub$Geo_Reg,
                                 levels = sort(as.numeric(as.character(unique(mydata_sub$Geo_Reg)))))
    
    return(mydata_sub)
  })
  
  lito_geo_processed <- reactive({
    req(upload_completo())
    mygeology_upload()
  })
  
  ws_bacias <- reactive({
    req(upload_completo())
    ws_geometry <- sf::st_geometry(ws_upload())
    ws_df <- sf::st_drop_geometry(ws_upload())
    bacias <- dplyr::left_join(ws_df, pt_upload(), by = "ID")
    bacias <- dplyr::left_join(bacias, mydata_processed(), by = "ID")
    sf::st_sf(bacias, geometry = ws_geometry)
  })
  
  rios_processed <- reactive({
    req(upload_completo())
    rios_upload()
  })
  
  bb_box <- reactive({
    req(ws_bacias())
    as.numeric(sf::st_bbox(ws_bacias()))
  })
  
  # Atualizar o mapa com o estado salvo ao mudar o elemento
  observeEvent(c(mydata_processed(), myjob_upload()), { # Ou observeEvent(c(mydata, myjob), {
    myjob <- myjob_upload()
    mydata <- mydata_processed()
    variances <- sapply(mydata, function(col) if (is.numeric(col)) var(col, na.rm = TRUE) else NA)
    # unique_counts <- sapply(mydata, function(col) if (is.numeric(col)) length(unique(col[!is.na(col)])) else NA) # Não usado
    m <- sapply(mydata, function(col) if (is.numeric(col)) median(col, na.rm = TRUE) else NA)
    mad <- sapply(mydata, function(col) if (is.numeric(col)) mad(col, na.rm = TRUE) else NA)
    
    valid_columns <- colnames(mydata)[
      # colMeans(!is.na(mydata)) > 0.7 &  # Comentei para focar na funcionalidade
      # variances > 1 &                 # Comentei para focar na funcionalidade
      (m - mad) !=  (m - 2 * mad) & # Esta condição é um pouco estranha, verifique a lógica
        colnames(mydata) %in% myjob$EL
    ]
    
    valid_names <- myjob$Nome[match(valid_columns, myjob$EL)]
    names(valid_columns) <- valid_names
    
    if (length(valid_columns) > 0) {
      updateSelectInput(
        session,
        "variable",
        choices = valid_columns
      )
    } else {
      updateSelectInput(
        session,
        "variable",
        choices = NULL
      )
    }
  }, once = TRUE)
  
  # Process data based on classification type
  # Processa os dados com base no tipo de classificação
  processed_data <- reactive({
    
    # O req() agora verifica se os inputs têm conteúdo
    req(
      mydata_processed(), 
      myjob_upload(), input$variable, input$classification_type)
    
    # Agora que a requisição foi atendida, o código pode ser executado.
    myjob <- myjob_upload()
    mydata <- mydata_processed()
    
    limiares_classifica(
      data = mydata,
      myjob = myjob,
      elemento = input$variable,
      tipo = as.numeric(input$classification_type), nb=4
    )
  })
  
  # Set fixed colors and classes
  cores_fixas_geo <- reactive({
    req(mylegend_upload())
    setNames(mylegend_upload()$RGB, mylegend_upload()$Geo_Reg)
  })
  
  pal_cod_ini <- c("#00007F", "#0000FF", "#007FFF",
                   "#00FFFF", "#7FFF7F", "#FF7F00", 
                   "#FF0000", "#7F0000", "black") 
  pal_cod <- pal_cod_ini[c(3, 4, 5, 6, 7)]
  pal_cod1 <- pal_cod_ini[c(4, 5, 6, 7)]
  
  pal_symb <- rep("black", 5)
  
  # Definir paleta de cores e classes
  classes <- c("1", "2", "3", "4", "5")
  classes1 <- c("1", "2", "3", "4")  # Exemplo de classes
  
  iconFiles <- reactive({
    classes <- c("1", "2", "3", "4", "5")
    setNames(
      paste0(classes, ".png"),
      classes
    )
  })
  
  # Criar correspondência fixa entre classes e cores
  cores_fixas_class <- setNames(c(pal_cod), classes)
  cores_fixas_class1 <- setNames(c(pal_cod1), classes1)
  
  # Variáveis reativas para armazenar o estado do mapa
  map_state <- reactiveValues(center = NULL, zoom = NULL)
  
  # Observar mudanças no mapa e salvar o estado atual
  observe({
    req(input$mymap_bounds, input$mymap_zoom, input$mymap_center)
    isolate({
      map_state$center <- input$mymap_center
      map_state$zoom <- input$mymap_zoom
    })
  })
  
  
  #  debug_data <- reactiveValues(data = NULL, myjob = NULL, elemento = NULL)
  
  # Renderizar o mapa geoquímico
  output$mymap <- renderLeaflet({
    req(processed_data(), ws_bacias(), lito_geo_processed(), mylegend_upload(), iconFiles())
    bacias <- ws_bacias()
    # Corrige geometrias inválidas antes de calcular o centroide
    bacias_valid <- sf::st_make_valid(bacias)
    centroide <- sf::st_centroid(sf::st_union(sf::st_geometry(bacias_valid)))
    ws_clong <- sf::st_coordinates(centroide)[1]
    ws_clat  <- sf::st_coordinates(centroide)[2]
    
    lito_geo <- lito_geo_processed()
    legenda_geo <- mylegend_upload()
    
    dados_classificados <- processed_data()$classificados
    limiares <- processed_data()$limiares  # Obter os limiares das classes
    
    # Obter as classes presentes nos dados classificados e ordená-las
    classes_presentes <- sort(unique(dados_classificados$Classe))
    # limiares <- limiares[classes_presentes]
    
    # Criar paleta de cores fixa para as classes presentes
    if(input$classification_type != 1){    
      pal <- colorFactor(
        palette = cores_fixas_class[classes_presentes],  # Filtrar e ordenar as cores das classes presentes
        domain = classes_presentes  # Define o domínio como as classes presentes
      )
      
      pal_legenda <- colorFactor(
        palette = rev(cores_fixas_class[classes_presentes]),  # Inverte a ordem das cores
        domain = classes_presentes
      )
      
    }else{     
      pal <- colorFactor(
        palette = cores_fixas_class1[classes_presentes],  # Filtrar e ordenar as cores das classes presentes
        domain = classes_presentes  # Define o domínio como as classes presentes
      )
      
      pal_legenda <- colorFactor(
        palette = rev(cores_fixas_class1[classes_presentes]),  # Inverte a ordem das cores
        domain = classes_presentes
      )}
    
    pal_geologia <- colorFactor(
      palette = cores_fixas_geo,  # Inverte a ordem das cores
      domain = legenda_geo$NOME
    )
    # Juntar os dados espaciais de 'bacias' com os dados classificados e filtra nas
    bacias_classificadas <- bacias |>
      dplyr::left_join(dados_classificados, by = "ID") |> 
      dplyr::filter(!is.na(Classe))
    
    # Criar o mapa 
    leaflet(bacias_classificadas, options = leafletOptions(minZoom = 5, maxZoom = 22)) |>
      setView(lng = ws_clong, lat = ws_clat, zoom = 10) |>
      addTiles(group = "Open Street Map") |>
      addProviderTiles(providers$Esri.WorldShadedRelief, 
                       group = "Esri World Shaded Relief") |>
      addPolygons(
        fillColor = ~pal(Classe),  # Aplica as cores fixas às classes presentes
        weight = 2,
        opacity = 0.5,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = ~paste(
          "<b>Estação:</b>", ESTACAO, "<br>",
          "<b>Teor de", input$variable, ":</b>", round(get(input$variable), 2), "<br>"
        ),
        group = "bacias"
      ) |>
      addPolygons(data = lito_geo,
                  stroke = TRUE,                # Mostra a borda
                  color = "black",              # Cor da borda
                  weight = 1,                   # Espessura da borda
                  fillOpacity = 0.5,
                  fillColor = ~RGB,             # Usa a coluna RGB para preencher
                  group = "geologia",
                  popup = ~paste("Nome da Unidade: ", NOME, "<br>")
      ) |>
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE, position = "topleft",
        width = 100,
        height = 100
      ) |>
      addPolylines(data = rios_processed(), group = "rios", weight = 1, color = "blue") |>
      addMarkers(
        lng = ~LONG_DEC, lat = ~LAT_DEC,
        icon = ~icons(
          iconUrl = paste0("icons/",iconFiles()[as.character(Classe)]),
          iconWidth = 20,
          iconHeight = 20,
          iconAnchorX = 10,
          iconAnchorY = 0
        ),
        popup = ~paste(
          "<b>Estação:</b>", ESTACAO, "<br>",
          "<b>Teor de", input$variable, ":</b>", round(get(input$variable), 2), "<br>"
        ),
        group = "estações"
      ) |>
      addLayersControl(
        baseGroups = c("Esri World Shaded Relief", "Open Street Map"),
        overlayGroups = c("estações", "bacias", "geologia", "rios"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      hideGroup(c("geologia", "rios", "estações")) |>  # Ocultar os grupos por padrão
      leafem::addMouseCoordinates() |>
      mapOptions(zoomToLimits = "first")
  })
  
  # Observar eventos de alternância de camadas ---------------------------------
  observeEvent(c(input$mymap_groups, input$variable, input$classification_type,  myjob_upload()),{
    req(processed_data(), input$variable, cores_fixas_class, cores_fixas_class1, ws_bacias(), 
        iconFiles, lito_geo_processed(), cores_fixas_geo, mylegend_upload())
    
    myjob <- myjob_upload()
    bacias <- ws_bacias()
    lito_geo <- lito_geo_processed()
    legenda_geo <- mylegend_upload()
    
    # Obter os dados processados
    dados_classificados <- processed_data()$classificados
    limiares <- processed_data()$limiares  # Obter os limiares das classes
    
    # Obter as classes presentes nos dados classificados e ordená-las
    classes_presentes <- sort(unique(dados_classificados$Classe))
    
    # Filtrar os limiares para as classes presentes
    limiares <- limiares[classes_presentes]
    
    # Arquivos de ícones
    icones <- iconFiles()[classes_presentes]
    
    
    # Criar a paleta de cores para a legenda
    if(input$classification_type == 1){    
      pal_legenda <- colorFactor(
        palette = rev(cores_fixas_class1[classes_presentes]),  # Inverte a ordem das cores
        domain = classes_presentes
      )
    }else{
      pal_legenda <- colorFactor(
        palette = rev(cores_fixas_class[classes_presentes]),  # Inverte a ordem das cores
        domain = classes_presentes
      )
    }
    
    # Atualizar o mapa com a legenda
    proxy <- leafletProxy("mymap")
    # Limpar legendas anteriores
    proxy |> clearControls()
    # Adicionar legenda para polígonos
    if ("bacias" %in% input$mymap_groups) {
      
      proxy |> addLegend(
        position = "topright",
        pal = pal_legenda,
        values = na.omit(dados_classificados$Classe),  # Certifique-se de usar os valores corretos
        title = paste(input$variable, " (", myjob[myjob$EL == input$variable, "UN"], ")"),
        labFormat = function(type, cuts, p) {
          # Formatar os rótulos da legenda com os intervalos das classes
          if (!is.null(limiares)) {
            labels <- c()
            for (i in seq_along(limiares)) {
              if (i == 1) {
                labels <- c(labels, paste0(" <", round(limiares[i], 2)))
              } else if (i == length(limiares)) {
                labels <- c(labels, paste0(" >", round(limiares[i - 1], 2)))
              } else {
                labels <- c(labels, paste0(" ", round(limiares[i - 1], 2), " - ",
                                           round(limiares[i], 2)))
              }
            }
            rev(labels)  # Inverte os rótulos para ordem decrescente
          } else {
            paste0("Classe ", cuts)
          }
        },
        opacity = 0.7
      )
    }
    if ("estações" %in% input$mymap_groups ) {
      # Criar os rótulos da legenda com ícones personalizados
      labels <- c()
      for (i in seq_along(limiares)) {
        if (i == 1) {
          labels <- c(labels, paste0(
            "<img src='", paste0("icons/", icones[i]), "' style='width: 20px; height: 20px; margin-right: 5px;'>",
            " <", round(limiares[i], 2)
          ))
        } else if (i == length(limiares)) {
          labels <- c(labels, paste0(
            "<img src='", paste0("icons/", icones[i]), "' style='width: 20px; height: 20px; margin-right: 5px;'>",
            " >", round(limiares[i - 1], 2)
          ))
        } else {
          
          labels <- (c(labels, paste0(
            "<img src='", paste0("icons/", icones[i]), "' style='width: 20px; height: 20px; margin-right: 5px;'>",
            " ", round(limiares[i - 1], 2), " - ", round(limiares[i], 2))
          ))
        }
      }
      
      # Atualizar o mapa com a legenda
      proxy <- leafletProxy("mymap")
      proxy |> clearControls()  # Limpar legendas anteriores
      
      proxy |> addLegend(
        position = "topright",
        pal = colorFactor(palette = "transparent", domain = dados_classificados$Classe),  # Paleta fictícia
        #        pal = pal_legenda,
        
        values = na.omit(dados_classificados$Classe),
        title = paste(input$variable, " (", myjob[myjob$EL == input$variable, "UN"], ")"),
        labFormat = function(type, cuts, p) {
          rev(labels)  # Retorna os rótulos com ícones
        },
        opacity = 0.7
      )
    }
    if ("estações" %in% input$mymap_groups & "bacias" %in% input$mymap_groups) {
      # Criar os rótulos da legenda com ícones personalizados
      labels <- c()
      for (i in seq_along(limiares)) {
        if (i == 1) {
          labels <- c(labels, paste0(
            "<img src='", paste0("icons/", icones[i]), "' style='width: 20px; height: 20px; margin-right: 5px;'>",
            " <", round(limiares[i], 2)
          ))
        } else if (i == length(limiares)) {
          labels <- c(labels, paste0(
            "<img src='", paste0("icons/", icones[i]), "' style='width: 20px; height: 20px; margin-right: 5px;'>",
            " >", round(limiares[i - 1], 2)
          ))
        } else {
          
          labels <- (c(labels, paste0(
            "<img src='", paste0("icons/", icones[i]), "' style='width: 20px; height: 20px; margin-right: 5px;'>",
            " ", round(limiares[i - 1], 2), " - ", round(limiares[i], 2))
          ))
        }
      }
      
      # Atualizar o mapa com a legenda
      proxy <- leafletProxy("mymap")
      proxy |> clearControls()  # Limpar legendas anteriores
      
      proxy |> addLegend(
        position = "topright",
        pal = pal_legenda,
        
        values = na.omit(dados_classificados$Classe),
        title = paste(input$variable, " (", myjob[myjob$EL == input$variable, "UN"], ")"),
        labFormat = function(type, cuts, p) {
          rev(labels)  # Retorna os rótulos com ícones
        },
        opacity = 0.7
      )
    }
    if ("geologia" %in% input$mymap_groups) {
      proxy <- leafletProxy("mymap")
      cores_legenda <- unique(legenda_geo$RGB)
      nomes_legenda <- unique(legenda_geo$NOME)
      
      # Criar o HTML para os itens da legenda com cores e nomes
      itens_legenda_html <- paste0(
        "<div style='display: flex; align-items: center; margin-bottom: 5px;'>",
        "<span style='background-color:", cores_legenda, "; display: inline-block; width: 10px; height: 10px; margin-right: 5px;'></span>",
        nomes_legenda,
        "</div>",
        collapse = ""
      )
      
      # Construir o HTML completo da legenda com a barra de rolagem
      html_legenda <- paste0(
        "<div style='max-height: 200px; overflow-y: auto; border: 1px solid #ccc; padding: 5px; background-color: white;'>",
        "<b>Geologia</b><br>",
        itens_legenda_html,
        "</div>"
      )
      
      proxy |> addLegend(
        position = "bottomleft",
        title = HTML(html_legenda),
        colors = character(0), # Não precisamos de cores aqui, pois já estão no HTML
        labels = character(0), # Não precisamos de rótulos aqui, pois já estão no HTML
        opacity = 0.7,
        layerId = "geologia_legend" # Adicione um layerId para facilitar a remoção
      )
    }
    
  })
  
  # Atualizar o mapa ao mudar o elemento ---------------------------------------
  observeEvent(c(input$variable, input$classification_type), {
    req(map_state$center, map_state$zoom)
    leafletProxy("mymap") |>
      setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom)
  })
  
  # Renderizar gráficos dinamicamente ------------------------------------------
  output$dynamicPlot <- renderUI({
    if (input$selectedPlot == "histPlot") {
      plotlyOutput("histPlot")
    } 
    if (input$selectedPlot == "histPlot") {
      plotlyOutput("bpxPlot")
    } 
    if (input$selectedPlot == "bpxPlot2"){
      plotOutput("bpxPlot2")
    }
    if (input$selectedPlot == "ca_plot"){
      plotOutput("ca_plot")
    }
  })
  
  ## Histograma de amostras agrupadas pela litologia ---------------------------
  output$histPlot <- renderPlotly({
    req(input$variable)
    hist_litologia(mydata_processed(), input$variable, cores_fixas_geo())
  })
  
  ## Boxplot por litologias ----------------------------------------------------
  output$bpxPlot <- renderPlotly({
    req(input$variable)
    bxp_litologia(mydata_processed(), input$variable, cores_fixas_geo(), myjob_upload())
  })
  
  ## Boxplot de todas as observações -------------------------------------------
  output$bpxPlot2 <- renderPlot({
    req(input$variable)
    bp_layout(
      mydata_processed(),
      myjob_upload(),
      input$variable,  # Passe o vetor, não o nome
      cores_fixas_class
    )
  }, height = 300)
  ## C-A Plot
  output$ca_plot <- renderPlot({
    req(input$variable, input$classification_type, processed_data())  
    
    
    if(input$classification_type == 1){
      dados_classificados <- processed_data()$classificados
      limiares <- processed_data()$limiares
      myjob <- myjob_upload() # <-- Adicione esta linha
      ca_plot(dados_classificados, myjob, limiares,
              input$variable, cores_fixas_class1)
    }
  }, 
  height = 300
  )
  
  # Renderizar a tabela de dados com base no elemento selecionado
  output$dataTable <- DT::renderDataTable({
    req(processed_data(), pt_upload(), input$variable)
    pt <- pt_upload()
    mydata <- mydata_processed()  # Adicione esta linha
    
    dados_classificados  <- left_join(mydata, processed_data()$classificados, 
                                      by = "ID")
    
    dados_classificados  <- left_join(sf::st_drop_geometry(pt)[,-ncol(pt)], 
                                      dados_classificados, by = "ID")
    
    # Selecionar apenas as colunas relevantes
    filtered_data <- dados_classificados[, c("NUM_LAB", "ESTACAO", "LONG_DEC",
                                             "LAT_DEC", input$variable, "Classe")]
    filtered_data <- filtered_data[!is.na(filtered_data$Classe), ]
    # Renderizar a tabela
    DT::datatable(
      filtered_data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE  # Remove os números das linhas
    )
  })
})

