dashboardPage(
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
      menuItem("Sobre o app", tabName = "info", icon = icon("info-circle"))
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
      p("Selecione uma das opções abaixo para carregar os dados. Se você escolher 'Usar Arquivos Predefinidos', os arquivos da pasta 'inputs/' do aplicativo serão usados automaticamente."),
      checkboxInput("use_predefined_files", "Usar Arquivos Predefinidos", value = FALSE)
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
        img(src = "logo.jpg", width = 100, units = "%", alt = "Sua Foto", class = "logo-image"),
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