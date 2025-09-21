GeochemExplore é um aplicativo shiny que permite a visualização interativa de dados geoquímicos e geoespaciais. Ele oferece ferramentas para explorar, analisar e visualizar dados geológicos em um ambiente amigável.

## Instalação
Para instalar o GeochemExplore, você pode usar o seguinte comando no R:

```R
install.packages("devtools") # Caso ainda não tenha o devtools instalado
devtools::install_github("vivianeCF/GeochemExplore")   
```
## Uso
Para iniciar o aplicativo, basta carregar a biblioteca e executar a função `runApp(`:

```R
library(shiny)
library(GeochemExplore)
# Executa o aplicativo Shiny a partir da pasta 'inst/shiny' do seu pacote
shiny::runApp(system.file("shiny", package = "GeochemExplore")
```

## Dependências
O GeochemExplore depende dos seguintes pacotes R:
- shiny
- shinydashboard
- leaflet
- plotly
- sf
- leaflet.extras
- ggplot2
- cowplot
- RColorBrewer
- tidyverse
- dplyr
- sf
- gridExtra
- grid
- lattice
- ggspatial
- gridBase
- MASS
- StatDA
- stats
- data.table
- compositions
- kableExtra
- knitr
- sparkline
- formattable
- gt
- flextable
- renv
- pals
- segmented
- ggpubr
- gridExtra

## Contribuição
Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou pull requests no repositório do GitHub.
## Licença
Este projeto está licenciado sob a licença MIT. Veja o arquivo LICENSE para mais detalhes.
## Autor
Viviane C. Ferrari - [GitHub](#autor) and [LinkedIn](https://www.linkedin.com/in/viviane-carillo-ferrari-8aaa6411b/)
## Agradecimentos
Agradecemos a todos que contribuíram para o desenvolvimento deste aplicativo.
