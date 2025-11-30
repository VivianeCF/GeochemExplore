# Carrega os pacotes do diretório R
source("R/pacotes.R")

# Carrega as funções do diretório R
source("R/minhas_funcoes.R")
# Cria um caminho de recurso explícito para a pasta www/
# O nome 'icons' é o que será usado na URL web
addResourcePath("icons", "www")
# Interface do usuário --------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader( 
    title = "GeochemExplore"
  ),
  dashboardSidebar(
    selectInput(
      "variable",
      "Escolha o elemento químico:",
      choices = NULL
    ),
    selectInput(
      "classification_type",
      "Escolha o tipo de classificação:",
      choices = list(
        "CA" = 1,
        "MAD" = 2,
        "TIF" = 3
      ),
      selected = 1
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Entrada de arquivos", tabName = "entrada_arquivos", icon = icon("upload")),
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("Gráficos", icon = icon("chart-bar"),
               menuSubItem("Histograma por Litologia", tabName = "histPlot_tab"),
               menuSubItem("Boxplot por Litologia", tabName = "bpxPlot_tab"),
               menuSubItem("Boxplot Geral", tabName = "bpxPlot2_tab"),
               menuSubItem("Gráfico C-A", tabName = "ca_plot_tab")),
      menuItem("Tabela de Dados", tabName = "tabela", icon = icon("table")),
      menuItem("Informações do aplicativo", tabName = "info", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    # Adiciona CSS personalizado
    tags$style(HTML("
    #mymap {
      height: calc(100vh - 50px) !important; /* Ajusta a altura do mapa */
    }

    .selectize-dropdown {
      z-index: 9999 !important;
    }
    #info {
      margin-bottom: 10px; /* Adiciona margem abaixo de cada parágrafo */
      line-height: 1.5; /* Aumenta o espaçamento entre linhas */
      text-align: justify; /* Justifica o texto */
      font-size: 1.5em;
    }
            .logo-image {
          display: block;
          margin-left: auto;
          margin-right: auto;
        }
  ")

    ),

    # Renderizar os seletores dinamicamente com base na aba ativa
    fluidRow(
      conditionalPanel(
        condition = "input.tabs == 'mapa' || input.tabs == 'tabela'" # Exibe apenas nas abas "Mapa" e "Tabela de Dados"
        
      )
    ),
    
    # Conteúdo das abas
    tabItems(
tabItem(
  tabName = "entrada_arquivos",
  fluidRow(
    box(
      title = "Opções de Arquivos",
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      p("Se você escolher 'Usar Arquivos Predefinidos', os arquivos da pasta 'inputs/' do aplicativo serão usados automaticamente."),
      checkboxInput("use_predefined_files", "Usar Arquivos Predefinidos do Projeto Eldorado Paulista - SG.22-X-B-VI", value = FALSE)
    )
  ),
  fluidRow(
    box(
      title = "Arquivos CSV",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      uiOutput("csv_inputs")
    ),
    box(
      title = "Arquivos de Forma (.shp)",
      width = 6,
      status = "primary",
      solidHeader = TRUE,
      uiOutput("shp_inputs")
    )
  ),
  verbatimTextOutput("upload_status")
),
      
      # Aba Mapa
      tabItem(
        tabName = "mapa",
        leafletOutput("mymap", height = "80vh")
      ),
      
      # Aba Gráficos
      tabItem(
        tabName = "graficos",
        h2("Selecione um gráfico no menu lateral.")
      ),
      tabItem(
        tabName = "histPlot_tab",
        h2("Histograma para cada litologia"),
        plotlyOutput("histPlot", height = "400px")
      ),
      # Conteúdo para o Boxplot por Litologia
      tabItem(
        tabName = "bpxPlot_tab",
        h2("Boxplot para cada litologia"),
        plotlyOutput("bpxPlot", height = "400px")
      ),
      # Conteúdo para o Boxplot Geral
      tabItem(
        tabName = "bpxPlot2_tab",
        h2("Boxplot para todas as amostras"),
        plotOutput("bpxPlot2", height = "400px")
      ),
      tabItem(
        tabName = "ca_plot_tab",
        h2("Gráfico C-A"),
        plotOutput("ca_plot", height = "400px")
      ),
      
      # Aba Tabela de Dados
      tabItem(
        tabName = "tabela",
        DT::dataTableOutput("dataTable")
      ),
      # Aba Informações do aplicativo
      tabItem(
        tabName = "info",
        img(src = "icons/logo.jpg", width = 100, units = "%", alt = "Sua Foto", class = "logo-image"),
        h2("Informações do Aplicativo"),
        p("O aplicativo foi desenvolvido pelo Serviço Geológico do Brasil/CPRM para a comunidade geocientífica. Ele permite que o usuário carregue dados e realize análises, utilizando os seguintes arquivos de entrada:"),
    h3("Arquivos de demonstração"),
        p("Para facilitar o uso do aplicativo, uma pasta chamada 'inputs/' foi criada na raiz do aplicativo. 
        Esta pasta contém arquivos de exemplo que podem ser usados para testar as funcionalidades do aplicativo sem a 
        necessidade de carregar seus próprios dados."),
        p("Fonte dos arquivos de exemplo: "),
        p("FALEIROS, Frederico Meira; PAVAN, Mauricio. Geologia e recursos minerais da folha Eldorado Paulista SG.22-X-B-VI: escala 1:100.000: estados de São Paulo e Paraná. São Paulo: CPRM, 2013. Programa Geologia do Brasil (PGB)."),
        p("Disponível em: <https://rigeo.sgb.gov.br/handle/doc/11525>."),

    h3("Arquivos de Tabela (.csv)"),
    tags$ul(
      tags$li(
        tags$strong("mydata_upload (Dados de Geoquímica)"),
        ": Este arquivo deve conter os dados de geoquímica para análise, incluindo as coordenadas de cada ponto amostral. É a base para as análises e classificações do aplicativo."
      ),
      tags$li(
        tags$strong("myjob_upload (Dados do Trabalho)"),
        ": Este arquivo de metadados serve para vincular a base geoquímica às informações específicas do trabalho, como o código do projeto, data e outras referências."
      ),
      tags$li(
        tags$strong("mylitho_upload (Dados de Litologia)"),
        ": Contém informações litológicas de cada ponto de amostra, permitindo que a análise geoquímica seja contextualizada com a geologia local."
      ),
      tags$li(
        tags$strong("mylegend_upload (Legenda)"),
        ": Este arquivo é usado para definir a simbologia e a legenda para os mapas e gráficos gerados, padronizando a visualização de acordo com as especificações do usuário."
      )
    ),
    
    h3("Arquivos de Geometria (.shp)"),
    tags$ul(
      tags$li(
        tags$strong("mygeology_upload (Geologia)"),
        ": Este é um arquivo de forma (.shp) contendo polígonos que representam as unidades geológicas da área de estudo. A importação deste arquivo é essencial para a criação do mapa geológico."
      ),
      tags$li(
        tags$strong("ws_upload (Bacias Hidrográficas)"),
        ": Este arquivo de forma (.shp) representa as bacias hidrográficas. A bacia é a área de captação de água que drena para um ponto específico. O aplicativo usa este arquivo para realizar análises de bacias e contextualizar os dados geoquímicos."
      ),
      tags$li(
        tags$strong("pt_upload (Pontos de Amostra)"),
        ": Representa os pontos de amostra em campo. Ele pode ser usado para importar os pontos de geoquímica e sua localização para a análise."
      ),
      tags$li(
        tags$strong("rios_upload (Rede de Drenagem)"),
        ": Este arquivo de forma (.shp) representa a rede de drenagem (rios e córregos). A visualização desta camada é crucial para entender o fluxo de água e as relações com os dados geoquímicos."
      ),
    h3("Tipos de Classificação"),
        p("O aplicativo oferece três métodos de classificação para os dados geoquímicos:"),
      tags$ul(
        
        tags$li(
          tags$strong("TIF (Tukey Inner Fence): "),
          "Baseado nos limiares do boxplot segundo o método Tuckey;"),
        
        tags$li(
          tags$strong("MAD (Median absolute deviation): "),
          "Método mais robusto baseado nos valores de mediana e mad;"),
        tags$li(
          tags$strong("C-A (Concentração - Área): "),
          "Método fractal que considera a área acumulada das bacias e teores ordenados de forma ascendente.")
        ),
        p("Para a classificação, os dados são previamente logtransformados."),
    h3("Funcionalidades do Aplicativo"),   
        p("São apresentados os itens Mapa, Gráficos e Tabela de dados. 
        No mapa, o usuário pode visualizar as estações de amostragem, bacias hidrográficas, unidades geológicas e rede de drenagem. 
        Nos gráficos, é possível gerar histogramas e boxplots por litologia, boxplot geral e gráfico C-A. 
        A tabela de dados apresenta os dados processados com as classificações."),
    h3("Desenvolvimento"),
        p("Desenvolvedora: Viviane Carillo Ferrari"),
        uiOutput("data_atualizacao_output"),# Chama a função para mostrar a data atual
        p("Contato: viviane.ferrari@sgb.gov.br")
      )
    )
  )
)
)
# Servidor shiny --------------------------------------------------------------
server <- function(input, output, session) {
  # Adicione esta função para renderizar a data dinamicamente
output$data_atualizacao_output <- renderUI({
  p(paste("Data:", Sys.Date()))
})
# Lógica para renderizar os inputs de arquivo
output$csv_inputs <- renderUI({
  if (input$use_predefined_files) {
    div(
      # p("Usando arquivos CSV predefinidos da pasta 'inputs/'."),
      tags$ul(
        tags$li("Dados Analíticos"),
        tags$li("Dados das condições analíticas"),
        tags$li("Dados litológicos das estações"),
        tags$li("Legenda da simbologia das litologias")
      )
    )
  } else {
    div(
      fileInput("mydata_upload", "1. Escolha dados analíticos",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("myjob_upload", "2. Escolha dados das condições analíticas",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("mylitho_upload", "3. Escolha dados litológicos das estações",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      fileInput("mylegend_upload", "4. Escolha legenda para simbologia das litologias",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    )
  }
})

output$shp_inputs <- renderUI({
  if (input$use_predefined_files) {
    div(
      # p("Usando arquivos de forma predefinidos da pasta 'inputs/'."),
      tags$ul(
        tags$li("Geologia da área de estudo"),
        tags$li("Sub-bacias hidrográficas"),
        tags$li("Estações de coleta"),
        tags$li("Rede de drenagem")
      )
    )
  } else {
    div(
      fileInput("mygeology_upload", "5. Escolha arquivos da geologia (.shp, .shx, .dbf, .prj)",
                multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")),
      fileInput("mywatershed_upload", "6. Escolha arquivos das sub-bacias (.shp, .shx, .dbf, .prj)",
                multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")),
      fileInput("myoutlet_upload", "7. Escolha arquivos das estações de coleta (.shp, .shx, .dbf, .prj)",
                multiple = TRUE, accept = c(".shp", ".dbf", ".shx", ".prj")),
      fileInput("mystream_upload", "8. Escolha arquivos da rede de drenagem (.shp, .shx, .dbf, .prj)",
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
    df <- read_csv2(path, locale = locale(encoding = "latin1"))
  } else {
    req(input$mylegend_upload)
    df <- read_csv2(input$mylegend_upload$datapath, locale = locale(encoding = "latin1"))
  }

  # Normaliza nomes (remoção de acentos/espacos) para detectar RANGE / SIGLA / RGB
  nm_raw <- names(df)
  nm_norm <- toupper(trimws(iconv(nm_raw, from = "latin1", to = "ASCII//TRANSLIT")))

  # se existir coluna RANGE (qualquer variante), renomeia para SIGLA
  idx_range <- which(nm_norm == "RANGE")
  if (length(idx_range)) names(df)[idx_range[1]] <- "SIGLA"

  # Se houver outra variação comum (ex: 'SIGLA '), tenta também
  idx_sigla <- which(nm_norm == "SIGLA")
  if (length(idx_sigla)) names(df)[idx_sigla[1]] <- "SIGLA"

  # Normaliza coluna RGB se existir em variantes ('RGB','Cor','COLOR'...)
  idx_rgb <- which(nm_norm %in% c("RGB", "COR", "COLOR", "COL"))
  if (length(idx_rgb)) names(df)[idx_rgb[1]] <- "RGB"

  # Garantir colunas mínimas
  if (!"SIGLA" %in% names(df)) df$SIGLA <- NA_character_
  if (!"RGB" %in% names(df)) df$RGB <- NA_character_

  # Normaliza valores RGB para #RRGGBB (opcional, mas útil)
  df <- df |>
    dplyr::mutate(
      SIGLA = as.character(SIGLA),
      RGB = as.character(RGB),
      RGB = toupper(trimws(iconv(RGB, from = "latin1", to = "ASCII//TRANSLIT"))),
      RGB = ifelse(grepl("^#", RGB), RGB, paste0("#", RGB)),
      RGB = gsub("[^#A-F0-9]", "", RGB),
      RGB = substr(RGB, 1, 7),
      RGB = ifelse(grepl("^#[A-F0-9]{6}$", RGB), RGB, NA_character_)
    )

  df
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
    mydata <- dplyr::left_join(mydata_upload(), mylitho_upload(), by =  "VALUE")
    mydata <- dplyr::left_join(mydata, mylegend_upload(), by = "Geo_cod")
    mydata$Geo_cod <- factor(mydata$Geo_cod)
    sm <- summary(mydata$Geo_cod)
    sm <- data.frame(Geo_cod = names(sm), cont = sm)
    mydata <- dplyr::left_join(mydata, sm, by = "Geo_cod")
    mydata_sub <- subset(mydata, cont >= 5)
    mydata_sub <- mydata_sub[order(mydata_sub$Geo_cod), ]
    mydata_sub$Geo_cod <- factor(mydata_sub$Geo_cod,
                                 levels = sort(as.numeric(as.character(unique(mydata_sub$Geo_cod)))))
    
    return(mydata_sub)
  })
  
  # Helper local: encontra coluna por nome (tolerante a case/acentos/BOM)
  find_col <- function(df, target) {
    nm <- names(df)
    nm_norm <- toupper(trimws(iconv(nm, from = "latin1", to = "ASCII//TRANSLIT")))
    idx <- which(nm_norm == toupper(target))
    if (length(idx)) nm[idx[1]] else NULL
  }

 
  lito_geo_processed <- reactive({
    req(upload_completo())
    lito_geo <- mygeology_upload()
    lito_geo <- sf::st_make_valid(lito_geo)
    legenda_geo <- mylegend_upload()
  lito_geo <- dplyr::left_join(lito_geo, legenda_geo, by = "SIGLA")
    
    # Harmoniza CRS com watershed se disponível
    try({
      ws_crs <- sf::st_crs(ws_upload())
      if (!is.na(ws_crs)) {
        lito_geo <- sf::st_transform(lito_geo, ws_crs)
      } else {
        lito_geo <- sf::st_transform(lito_geo, 4326)
      }
    }, silent = TRUE)

    message(sprintf("lito_geo_processed: RGB valid for %d of %d features", sum(!is.na(lito_geo$RGB)), nrow(lito_geo)))
    lito_geo
  })
  
  ws_bacias <- reactive({
    req(upload_completo())
    ws_geometry <- sf::st_geometry(ws_upload())
    ws_geometry <- sf::st_make_valid(ws_geometry)
    ws_df <- sf::st_drop_geometry(ws_upload())
    bacias <- dplyr::left_join(ws_df, pt_upload(), by = "VALUE")
    bacias <- dplyr::left_join(bacias, mydata_processed(), by = "VALUE")
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
    lg <- mylegend_upload()
    # prefer Geo_cod, senão SIGLA, senão vazio
    codes <- if ("Geo_cod" %in% names(lg)) as.character(lg$Geo_cod) else if ("SIGLA" %in% names(lg)) as.character(lg$SIGLA) else character(0)
    cols  <- if ("RGB" %in% names(lg)) as.character(lg$RGB) else rep(NA_character_, length(codes))
    if (length(codes) == 0) named <- setNames(character(0), character(0) ) else named <- setNames(cols, codes)
    named
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
    bacias <- sf::st_make_valid(bacias)

    # Compute centroid robustly: temporarily disable s2 (some invalid/degenerate
    # geometries cause s2 to error) and fall back to bbox center if centroid fails.
    s2_orig <- sf::sf_use_s2()
    if (isTRUE(s2_orig)) sf::sf_use_s2(FALSE)
    centroide <- tryCatch({
      sf::st_centroid(sf::st_union(sf::st_geometry(bacias)))
    }, error = function(e) {
      # Fallback: use bbox center as a safe centroid approximation
      bb <- as.numeric(sf::st_bbox(bacias))
      cx <- (bb[1] + bb[3]) / 2
      cy <- (bb[2] + bb[4]) / 2
      sf::st_sfc(sf::st_point(c(cx, cy)), crs = sf::st_crs(bacias))
    })
    if (isTRUE(s2_orig)) sf::sf_use_s2(TRUE)

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
      domain = legenda_geo$SIGLA
    )
    # Juntar os dados espaciais de 'bacias' com os dados classificados e filtra nas
    bacias_classificadas <- bacias |>
      dplyr::left_join(dados_classificados, by = "VALUE") |> 
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
                  stroke = FALSE,                # Mostra a borda
                  color = NA,              # Cor da borda
                  weight = 1,                   # Espessura da borda
                  fillOpacity = 0.5,
                  fillColor = ~RGB,             # Usa a coluna RGB para preencher
                  group = "geologia",
                  popup = ~paste("Unidade: ", SIGLA, "<br>")
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
    legenda_geo <- mylegend_upload()
    lito_geo <- lito_geo_processed()
    
    myjob <- myjob_upload()
    bacias <- ws_bacias()

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
      nomes_legenda <- unique(legenda_geo$SIGLA)
      
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
                                      by = "VALUE")
    
    dados_classificados  <- left_join(sf::st_drop_geometry(pt)[,-ncol(pt)], 
                                      dados_classificados, by = "VALUE")
    
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
}


# Roda a aplicação
shiny::shinyApp(ui = ui, server = server)
