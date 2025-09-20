# Função para criar o boxplot com limiares
criar_boxplot_com_limiares <- function(data, elemento) {
  
lim_calc_tipo3 <- function(x) {
    z <- log10(x)
    q <- quantile(z, probs = c(0.25, 0.75), na.rm = TRUE)
    hw <- q[2] - q[1]
    zcut <-  c(q[1] - 1.5 * hw, q[1], q[2], q[2] + 1.5 * hw)
    return(10^zcut)
  }
 
  
  # Log-transformar os dados
  data <- data %>%
    dplyr::filter(!is.na(.data[[elemento]])) %>%  # Remover valores NA
    dplyr::mutate(log_value = log10(.data[[elemento]]))  # Adicionar coluna log-transformada
  
  # Calcular os limiares usando a função lim_calc_tipo3
  limiares <- lim_calc_tipo3(data[[elemento]])
  
  # Criar o boxplot
  p <- ggplot(data, aes(x = "", y = log_value)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    geom_hline(yintercept = log10(limiares), color = "red", linetype = "dashed", size = 1) +
    labs(
      title = paste("Boxplot dos dados log-transformados com limiares para", elemento),
      x = "",
      y = "Log10(valor)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  # Adicionar rótulos para os limiares
  for (i in seq_along(limiares)) {
    p <- p + annotate(
      "text",
      x = 1.2,
      y = log10(limiares[i]),
      label = paste0("Limiar ", i, ": ", round(as.numeric(limiares[i]), 2)),
      color = "red",
      hjust = 0
    )
  }
  
  return(p)
}

# Função para calcular limiares e classificar dados
limiares_classifica <- function(data, myjob, elemento, tipo, nb) {
  options(OutDec = ".")
  
  # Força a coluna do elemento a ser numérica, trocando vírgula por ponto se necessário
  data[[elemento]] <- as.numeric(gsub(",", ".", as.character(data[[elemento]])))
  
  # Remove linhas com NA no elemento
  data <- data[!is.na(data[[elemento]]), ]
  
  # Função para tipo 1 - CA
  lim_calc_tipo1 <- function(x, a) {
    df <- data.frame(x, a)
    df <- df[!is.na(df$x), ]
    df <- df[order(df$x, decreasing = TRUE), ]
    df <- unique(df)
    
    df <- df |>
      dplyr::mutate(cum_area = cumsum(a))
    
    # Transformação logarítmica
    y <- log10(df$cum_area)
    x <- log10(df$x)
    
    model <- data.frame(x, y)
    model <- unique(model)
    model <- model[!duplicated(model$x), ]
    model <- model[!duplicated(model$y), ]
    
    x <- model$x
    y <- model$y
    
    o <- lm(y ~ x)
    npsi = 3
    quant = TRUE

    if(length(x)>=20){
       npsi = 3
       quant = TRUE
     }else{
       npsi = 2
       quant = TRUE
     }
   
    o.seg <- segmented(o, seg.Z = ~x, npsi = npsi, stop.if.error = F, 
                       control = seg.control(quant = quant))
    
    breaks <- data.frame(breaks = 10^(o.seg[["indexU"]][["x"]]))
    
    # if(length(x)>=23){
    #   npsi = 4
    #   quant = TRUE
    # }else{
    #   npsi = 3
    #   quant = TRUE
    # }
    zcut <- if (nb == 4) {
      c(lm1 = breaks[1, ], lm2 = breaks[2, ], lm3 = breaks[3, ], lm4 = breaks[4, ])
    } else {
      c(lm1 = breaks[1, ], lm2 = breaks[2, ])
    }
    return(zcut)
  }
  
  # Função para tipo 2 - MAD
  lim_calc_tipo2 <- function(x) {
    # x <- x[!is.na(x)]
    z <- log10(x)
    mad <- mad(z, na.rm = TRUE)
    m <- median(z, na.rm = TRUE)
    
    # Calcula os limites
    zcut <- if (nb == 4) {
      (c(m - 2 * mad, m - mad, m + mad, m + 2 * mad))
    } else {
      c(m - 2 * mad, m + 2 * mad)
    }
    
    
    # Verifica se os limites são suficientes
    if (length(unique(zcut)) < 2) {
      zcut <- c( -mad, mad)
      stop("Erro: O número de limites únicos não são suficientes para a classificação.")
    }
    
    return(10^zcut)
  }
  
  # Função para tipo 3 - TIF
  lim_calc_tipo3 <- function(x) {
    # x <- x[!is.na(x)]
    z <- log10(x)
    
    # Calcular os limites usando boxplot.stats
    zcut <- if (nb == 4) {
      boxplot.stats(z, coef = 1.5)$stats[-3]  # Remove a mediana
    } else {
      boxplot.stats(z, coef = 1.5)$stats[c(2, 4)]  # Quartis inferior e superior
    }
    
    if (length(zcut) < 2) {
      stop("Erro: O número de limites únicos não são suficientes para a classificação.")
    }
    
    return(10^zcut)  # Retornar os limites em escala original
  }
  
  # Processamento principal
  
  # Atribui a unidade de medida (pt)
  unidade <- myjob[myjob$EL == elemento, "UN"]
  
  # Atribui a unidade de medida (en)
  # unidade_en <- myjob[myjob$EL == elemento, "UN_EN"]
  
  # Atribui o número de dígitos significativos
  dig <- myjob[myjob$EL == elemento, "DIG"]
  
  # Rotaciona a tabela 
  dados_classificados <- data |> 
    dplyr::select(ID, Area_km2, all_of(elemento)) |>  # Seleciona as colunas relevantes
    tidyr::pivot_longer(
      cols = all_of(elemento),  # Especifica a coluna do elemento selecionado
      names_to = "elemento", 
      values_to = "value"
    )
  
  # Calcula limiares com base no tipo
  x <- dados_classificados$value
  breaks <- switch(as.character(tipo),
                   "1" = lim_calc_tipo1(x, dados_classificados$Area_km2),
                   "2" = lim_calc_tipo2(x),
                   "3" = lim_calc_tipo3(x),
                   stop("Tipo inválido"))
  
  # Classifica a partir dos breaks
  
  sel_breaks <- breaks[breaks > (min(x, na.rm = TRUE)) & breaks < (max(x, na.rm = TRUE))]
  posicao <- which(breaks %in% sel_breaks)
  dados_classificados$Classe <- cut(dados_classificados$value, c(Inf, sel_breaks, -Inf), 
                                   labels = FALSE, right = TRUE) + min(posicao)-1

  # Armazena lista de limiares para cada elemento
  if (nb == 4 ) {
    # Garante que breaks são numéricos e não NA
    breaks <- as.numeric(breaks)
    breaks[is.na(breaks)] <- 0  # ou outro valor padrão, se preferir
    dig <- as.numeric(dig)
    if (is.na(dig)) dig <- 2

    dados_limiares <- data.frame(
      lm1 = round(breaks[1], dig),
      lm2 = round(breaks[2], dig),
      lm3 = round(breaks[3], dig),
      lm4 = round(breaks[4], dig),
      elemento = elemento
    )
  } else {
    breaks <- as.numeric(breaks)
    breaks[is.na(breaks)] <- 0
    dig <- as.numeric(dig)
    if (is.na(dig)) dig <- 2

    dados_limiares <- data.frame(
      lm1 = round(breaks[1], dig),
      lm2 = round(breaks[2], dig),
      elemento = elemento
    ) 
  }
    
  # Retorna limiares e dados classificados
  return(list(limiares = dados_limiares, classificados = dados_classificados))
}

# Função para criar o boxplot com limiares
bp_layout <- function (mydata, myjob, w, cores_fixas_class) {
  # Definir cores_fixas_class dentro da função
 # cores_fixas_class <- c("1" = "#00007F", "2" = "#0000FF", "3" = "#007FFF", 
  #                       "4" = "#00FFFF", "5" = "#7FFF7F")
  
  # row.names(myjob) <- myjob[, "EL"] # ID
  # # Extrai dados das condições anaíticas
  #   dig_calc <- length(digits((myjob[w, "LDI"])/2, 
  #                             n = NULL, simplify = FALSE))-1# n decimais calc
  #   dig <- length(digits((myjob[w, "LDI"]), n = NULL, 
  #                        simplify = FALSE))-1# n decimais real
  dig <- myjob[myjob$EL == w, "DIG"]
  unidade <- myjob[myjob$EL == w, "UN"]# unidade anal?tica
  ucc <- myjob[myjob$EL == w, "UCC"]
  ld <- myjob[myjob$EL == w, "LDI"]
  # Transforma os dados para logarítmo
  el <- log10(mydata[[w]])
   
  # Definições de design do layout ----------------------------------------------
  # Paleta de cores dos simbolos e mapa
  pal_cod <- c("#00007F", "#0000FF", "#007FFF",
               "#00FFFF", "#7FFF7F", "#FF7F00", 
               "#FF0000", "#7F0000", "black") 
  # Nome dos limites do boxplot
  label <- c("Min", "Q1-4,5*AIQ","Q1-3*AIQ","Q1-1,5*AIQ", "Q1(25%)", "Q3(75%)",
             "Q3+1,5*AIQ", "Q3+3*AIQ", "Q3+4,5*AIQ","Max")
  
  # Criar Matriz das definições dos símbolos da legenda
  ind <- seq(1:9)# indice
  cex1_cod <- c(10, 8, 6, 4, 1, 4, 6, 8, 10)# tamanho do simbolo
  cex2_cod <- c(10, 8, 6, 4, 1, 4, 6, 8, 10)# tamanho do halo
  pch_cod <- c(1, 1, 1, 1, 3, 0, 0, 0, 0)# forma do simbolo
  lwd1_cod <- c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)# espessura simb.
  lwd2_cod <- c(1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6, 1.6)# espessura halo
  halo_cod <- rep("#000000", 9)# cor do halo
  pal_symb <-    pal_symb <- c("#000000", "#303030", "#585858", 
                               "#808080", "#808080", "#808080", 
                               "#585858", "#303030", "#000000")
  
  
  # Cria a matriz de definições dos símbolos da legenda
  df_sb <- data.frame(ind, cex1_cod, cex2_cod, pch_cod, 
                      lwd1_cod, lwd2_cod, halo_cod, pal_symb)
  df_sb[c(3, 4, 5, 6, 7),]
  # Cálculo da estatística boxplot ----------------------------------------------
  # Estatísticas do boxplot dados log coefs 1.5, 3 e 4.5
  p <- boxplot.stats(el, coef = 4.5)
  p_id <- c(2, 5, 0, 6, 9)
  p2 <- boxplot.stats(el, coef = 3)
  p2_id <- c(3, 5, 0, 6, 8)
  p3 <- boxplot.stats(el, coef = 1.5)
  p3_id <- c(4, 5, 0, 6, 7)
  
  # Max e min
  # linhas para adicionar na matriz das estat?sticas do boxplot
  n <- c(min(el), 1)
  l <- c(max(el), 10)
  
  # Valor máximo e mínimo dos dados logtransformados
  lmin <- min(el, na.rm = TRUE)
  lmax <- max(el, na.rm = TRUE)
  
  # data frame das estatísticas de cada coef.
  st <- data.frame(p$stats, p_id)
  st2 <- data.frame(p2$stats, p2_id)
  st3 <- data.frame(p3$stats, p3_id)
  colnames(st) <- c("log_teor", "id")
  colnames(st2) <- c("log_teor", "id")
  colnames(st3) <- c("log_teor", "id")
  
  # Cria a matriz das estatisticas boxplot
  df_lbx <- rbind(st, st2, st3, n, l)
  
  # Elimina a mediana
  df_lbx <- df_lbx |>
    dplyr::filter(id %in% c(1, 4, 5, 6, 7, 10))
  
  # Ordena pelo id
  df_lbx <- dplyr::arrange( df_lbx, id)
  
  ### Remove duplicatas, mantendo o valor máximo e o valor mínimo
  df <- df_lbx |>
    group_by(log_teor) |>
    summarise(
      max_id = max(id, na.rm = TRUE),  # Valor máximo
      min_id = min(id, na.rm = TRUE)   # Valor mínimo
    )
  
  # Garantir que os vetores usados para criar o data.frame tenham o mesmo comprimento
  if (nrow(df) > 0) {
    df_lbx <- data.frame(df)
  } else {
    stop("Erro: Nenhum dado disponível após o agrupamento.")
  }
  
  # Define os rótulos do boxplot
  rotulo_bp <- label[df_lbx$max_id]
  
  # Verificar se os rótulos e os dados têm o mesmo comprimento
  if (length(rotulo_bp) != nrow(df_lbx)) {
    stop("Erro: O número de rótulos não corresponde ao número de linhas do data.frame.")
  }
  
  # Teor do limite
  teor <- 10^df_lbx$log_teor
  teor <- as.numeric(teor) # <-- Garante que é numérico

  dig <- as.numeric(dig)
  if (is.na(dig)) dig <- 2

  rotulo_bp <- c()
  cores_bp <- c()

  for (i in seq_along(teor)) {
    if (i == length(teor)) {
      rotulo_bp <- c(rotulo_bp, paste0(">", round(as.numeric(teor[i - 1]), dig)))
      cores_bp <- c(cores_bp, cores_fixas_class["5"])
    } else {
      rotulo_bp <- c(rotulo_bp, paste0(round(as.numeric(teor[i]), dig)))
      cores_bp <- c(cores_bp, cores_fixas_class[as.character(i)])
    }
  }
  
  # Adicionar cores e rótulos ao data frame do boxplot
  df_lbx <- data.frame(df_lbx,teor, rotulo_bp, cores_bp)
  
  sby <- 0
  
  # Calcula os indices de posição dos limites do boxplot
  sby_id <- df_lbx$max_id[1:(length(df_lbx$max_id) - 1)]
  sby_id[1] <- sby_id[2] -1
  
  # Calcula as posições y das caixas entre os limites
  for (i in 1:(nrow(df_lbx) - 1)) {
    j <- i + 1
    sby[i] <- (df_lbx$log_teor[i] + df_lbx$log_teor[j])/2
  }
  # Define as posições x das caixas e símbolos (1)
  sbx <- c(rep(1, nrow(df_lbx) - 1))
  lbx <- c(rep(1, nrow(df_lbx)))
  
  # Define matrizes de dados para a figura do boxplot
  ##Posição dos símbolos do boxplot
  df_sb <- df_sb[sby_id, ]
  teor <- 10^(sby)
  pos <- rep(0.05, length(sby))
  dados1 <- data.frame(pos,teor)
  
  ## Boxplot do elemento
  teor <- mydata[[w]]
  pos <- rep(0.3, length(teor))
  dados2 <- data.frame(pos,teor)
  
  ## Posição dos rótulos do boxplot
  
  # Rótulo 
  rotulo_lim <- label[df_lbx$max_id]
  teor <- df_lbx$teor
  rot_teor <- round(teor, dig)
  
  # Testa o mínimo para colocar o operador < antes do LD nos rótulos do boxplot
  ldi_val <- as.numeric(myjob[myjob$EL == w, "LDI"])
  if(!is.na(ldi_val) && round(10^lmin, dig) <= round(ldi_val, dig)){
    rot_teor[1] <- paste0("<", ldi_val)
  }
  if(!is.na(ldi_val) && round(10^lmin, dig) < round(ldi_val/2, dig)){
    rot_teor[2] <- ldi_val
  }
  rotulo_lim[1] <- "Min"# Assegura o rótulo do mínimo
  
  pos <- rep(0.5, length(teor))
  dados3 <- data.frame(pos,teor,rot_teor,rotulo_lim)
  ## delei para centralizar a figura
  dl <- 0.1
  # Controle UCC
  ucc_val <- as.numeric(myjob[myjob$EL == w, "UCC"])
  if(!is.na(ucc_val) && ucc_val > 0.001 && ucc_val <= 10^lmax) {
    ln_ucc <- geom_segment(aes(x = 0.2, xend = 0.6, y = ucc_val, 
                               yend = ucc_val), colour = "red", 
                           linetype = "dotdash")
    pos<- c(pos,0.5)
    teor <- c(teor, ucc_val)
    rotulo_lim <- c(rotulo_lim,"UCC*")
    rot_teor <- c(rot_teor, ucc_val)
  } else { 
    ln_ucc <- geom_segment(aes(x = 0, xend = 0.6, y = 10^lmax, yend = 10^lmax), 
                           colour = "transparent", linetype = "dotdash")
  }
  
  # Dados dos rótulos com UCC
  dados4 <- data.frame(pos,teor,rot_teor,rotulo_lim)
  dados4 <- arrange(dados4, teor)
  
  # Tema ggplot do boxplot
  bp_Theme <- theme(axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    legend.position = "none", 
                    axis.ticks = element_blank(),
                    plot.title = element_text(color = "black",
                                              size = 20, hjust = 0.5))
  
  p2 <- ggplot(NULL, aes(x = pos +dl, y = teor)) +
    geom_text( data = dados4, size = 6, 
               aes(label = paste0(rot_teor, ": ", rotulo_lim)), hjust = 0, 
               check_overlap = TRUE) + 
    stat_boxplot( data = dados2, geom = 'errorbar', width = 0.1) +
    geom_boxplot( data = dados2, width = 0.2, position = "dodge") + theme_void() +
    ln_ucc + 
    bp_Theme +
    geom_point( data = dados1, cex = 15, pch = 22, bg = pal_cod[sby_id]) +
    geom_point(data = dados1, cex = df_sb$cex1_cod, pch = df_sb$pch_cod, 
               col = pal_symb[sby_id], stroke = df_sb$lwd1_cod) +
    xlim(-0.02, 1) +
    scale_y_log10() + 
    ggtitle(paste0(w, " - ", unidade)) +
    theme(plot.title = element_text(hjust = 0.4))
  
  p2
  
}

# Boxplot por litologia
bxp_litologia <- function(data, elemento, cores_fixas, myjob){
  # Garante que a coluna do elemento é numérica
  data[[elemento]] <- as.numeric(gsub(",", ".", as.character(data[[elemento]])))
  
  # Remove linhas com NA no elemento
  data <- data[!is.na(data[[elemento]]), ]
  
  # Reordenar fatores para litologias
  lito <- reorder(factor(data$Geo_Reg), data[[elemento]], FUN = median)
  unidade <- reorder(factor(data$NOME), data[[elemento]], FUN = median)
  teor <- data[[elemento]]

  # Criação do gráfico
  p <- ggplot(data, aes(x = unidade, 
                       y = teor,
                       text = paste0("Unidade: ", NOME, "<br>",
                                     "Litologia: ", Geo_Reg, "<br>",
                                     "Teor: ", format(teor, scientific = FALSE, digits = 2)))) +
    geom_boxplot(aes(fill = Geo_Reg), width = .5) +  # Usar Geo_Reg para cores
    scale_fill_manual(
      values = cores_fixas[sort(names(cores_fixas))],  # Ordenar cores pela Geo_Reg
      name = "Litologia"
    ) +
    scale_y_continuous(trans = 'log10') +
    scale_x_discrete(breaks = unidade, labels = lito) +
    geom_point(size = 0.001) + 
    labs(x = NULL, y = paste0("Concentração (", myjob[myjob$EL == elemento, "UN"], ")")) + 
    theme(legend.position = "bottom", axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())

  ggplotly(p, tooltip = "text") |> 
    layout(legend = list(orientation = "h", x = 0, y = 0))
}

# Função para criar o gráfico C-A
ca_plot <- function(dados_classificados, myjob,  limiares,elemento, cores_fixas_class){
  
  
  pal_cod = cores_fixas_class
  # Linha vertical no gráfico C-A
  vsegment <- function(x, X, Y) {
    geom_segment(aes(x = x, xend = x, y = -Inf, yend = approx(X, Y, x)$y),
                 linetype = 2, col = "red") 
  }
  
  # Número de divisões nos eixos 
  number_ticks <- function(n) {function(limits) pretty(limits, n)}
  
  # Definição do formato numérico, vírgula decimal
  options(OutDec = ".", scipen = 999)
  
  data_select <- dados_classificados
  
  # atribui a unidade de medida (pt)
  unidade <- myjob[myjob$EL==elemento, "UN"]
  
  # atribui o número de digitos significativos
  dig <-  myjob[myjob$EL==elemento, "DIG"]
  
  # elimina registros sem área de bacia
  data_select <- dados_classificados[!is.na(dados_classificados$Area_km2),]
  
  # Calcula a área acumulada
  df <- data_select[order(data_select$value, decreasing = TRUE),] %>% 
    mutate(cum_area = cumsum(Area_km2)) 
  
  # Rotaciona a tabela 
  
  # Cria tabela com limiares
  
  df <- as.data.frame(df)
  # Prepara e cria a tabela DF
  X <- df[, "value"]
  Y <- df[, "cum_area"]
  DF <- data.frame(X, Y)
  
  
  
  # Classifica apartir dos breaks
  class <- df$Classe
  
  # Inclui variável class
  DF <- data.frame(DF, class)
  # DF <- DF[!duplicated(DF$X),]
  # DF <- DF[!is.na(DF$X),]
  breaks <- as.numeric(limiares[,1:ncol(limiares)-1])
  
  # Gráfico as.numeric()# Gráfico CA
  # Define o título do mapa
  
  titulo_mapa <- myjob[myjob$EL == elemento, "Nome"]
  
  # Constroi grafico com as quebras (linhas verticais)
  p1 <-   ggplot(DF, aes(x = X, y = Y, color = as.factor(class))) + 
    geom_point(size=0.5) + 
    vsegment(breaks[1], X, log10(Y)) +
    vsegment(breaks[2], X, log10(Y)) +
    vsegment(breaks[3], X, log10(Y)) +
    # vsegment(breaks[4], X, log10(Y)) + 
    geom_smooth(method = "lm", fill = NA, formula = 'y ~ x', lwd =0.4) +
    labs(x=paste0(elemento, " (",  unidade, ")"), 
         y = expression( Area ~ "(" ~ km^2 ~")")) +
    scale_color_manual(name="Class",
                       labels = c("Baixo Background", "Background",  
                                  "Alto Background", "Anomalia"),
                       values = pal_cod,
                       guide = guide_legend(nrow = 4, reverse = FALSE, 
                                            label.position = "right", 
                                            direction =  "vertical", 
                                            byrow = FALSE, col = 1 )) + 
    scale_x_continuous(transform = "log10")  +
    ggtitle(titulo_mapa) + theme(,
                                 legend.title = element_text(size = 14, face = "bold"),
                                 legend.text = element_text(size = 12),
                                 axis.text = element_text(size = 16))
  
  # Extrai a legenda
  leg <- cowplot::get_legend(p1)
  
  # Converte a legenda para o ggplot
  l1 <- ggpubr::as_ggplot(leg)
  
  # Coloca o gráfico de cada elemento numa lista
  p <- p1 + theme_bw() + 
    theme(legend.position = "none", text = element_text(size=6),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          plot.title = element_text(color="black", 
                                    size=10,  
                                    hjust = 0.5),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

  
  gridExtra::grid.arrange(grobs = list(p,l1), 
                          widths = c(4, 1), 
                          layout_matrix = rbind(c(1, 2)))
  
}

# Função auxiliar para cortar os dados em classes
cutter <- function (x, cuts) {
  ncuts <- length(cuts)
  xi <- cut(x, breaks = cuts, labels = FALSE, include.left = TRUE)
  xi[x <= cuts[1]] <- 0
  xi[x >= cuts[ncuts]] <- ncuts
  xi <- xi + 1
  invisible(xi)
}

# Função para extrair os dígitos de um número
digits <- function(x, n=NULL, simplify = FALSE) {
    if(length(x) > 1) {
        if(is.null(n) & simplify) {
            n <- floor(max(log10(x))) + 1
        }
        sapply(x,digits, simplify=simplify, n=n)
    } else {
        if(is.null(n)) {
            n <- floor(log10(x))
        } else {
            n <- n - 1
        }
        x %/% 10^seq(n,0) %% 10
    }
}

# Histograma por litologia
hist_litologia <- function(data, elemento, cores_fixas){
  # Garante que a coluna do elemento é numérica
  data[[elemento]] <- as.numeric(gsub(",", ".", as.character(data[[elemento]])))
  
  # Remove linhas com NA no elemento
  data <- data[!is.na(data[[elemento]]), ]
  
  # Reordenar fatores para litologias
  teor <- data[[elemento]]
  unidade <- reorder(factor(data$NOME), teor, FUN = median)
  
  # Crie o histograma
  p <- ggplot(data, aes(x = teor,
                        text = paste0("Unidade: ", NOME, "<br>",
                                      "Litologia: ", Geo_Reg, "<br>",
                                      "Teor: ", format(teor, scientific = FALSE, digits = 2)))) +
    geom_histogram(aes(fill = Geo_Reg), na.rm = TRUE, color = "black") +
    scale_fill_manual(values = cores_fixas , name = "Unidade Litológica") +
    scale_x_continuous(trans = 'log10') +
    labs(title = "Histograma com Cores da litologia",
         x = "Concentração",
         y = "Frequência") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.title.x=element_blank())
  
  ggplotly(p, tooltip = "text") |>
    layout(legend = list(orientation = "h", x = 0, y = -0.2),
           xaxis = list(
             title = list(text = "Concentração",
                          standoff = 5)
           ))
 
}

# Função para classificação tipo boxplot
log_class_bxp <- function (x) {
  cutter(x, zcut_bxp(x))
}

# Função para criar ícones de símbolos
pchIcons <- function(pch = 1, width = 30, height = 30, bg = "transparent", col = "black", cex = 1, ...) {
  n <- length(pch)
  files = character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f = tempfile(fileext = '.png')
    png(f, width = width, height = height, bg = bg)
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], col = col[i], cex = min(width, height) / 14*cex[i], ...)
    dev.off()
    files[i] = f
  }
  files
}

# Função para calcular os limites do boxplot
zcut_bxp <- function (x) {
  z <- log10(x)
  q <-quantile(z, probs = c(0.25, 0.75), na.rm = TRUE)
  zcut <- numeric(8)
  hw <- q[2] - q[1]
  zcut[1] <- q[1] - 4.5 * hw
  zcut[2] <- q[1] - 3 * hw
  zcut[3] <- q[1] - 1.5 * hw
  zcut[4] <- q[1]
  zcut[5] <- q[2]
  zcut[6] <- q[2] + 1.5 * hw
  zcut[7] <- q[2] + 3 * hw
  zcut[8] <- q[2] + 4.5 * hw
  10^zcut
}
