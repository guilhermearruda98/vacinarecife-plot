# Instituto Federal de Pernambuco - Campus Recife
# Displina: Matemática Aplicada
# Docente: Samuel Macedo
# Discentes: Andryene Pessoa e Guilherme Max
#
# Esse programa tem o intuito de capturar dados sobre
# a vacinação do COVID19 na cidade de Recife/PE

#'@export
vacinarecife <- function(){

  # Carregando os pacotes necessários
  library("tidyverse")
  library("rvest")
  library("ggplot2")
  library("dplyr")
  library("cowplot")

  # Obtendo a página WEB com os dados
  site <- "https://conectarecife.recife.pe.gov.br/vacinometro/"

  # Raspando e processando os dados
  conectarec <- read_html(site)
  tabelaVacina <- html_table(
    html_nodes(conectarec, "table")[1])[[1]]

  tabelaVacina <- rename(tabelaVacina,
                         'Dose1' = 'Dose 1',
                         'Dose2' = 'Dose 2',
                         'DoseUnica' = 'Dose Única')

  tabelaVacina <- tabelaVacina[(-4),]

  # Gerando os gráficos
  d1 <- ggplot(tabelaVacina, aes(x = "", y = Dose1, fill = Sexo)) +
    geom_bar(width = 1, stat = "identity", color="green") +
    coord_polar("y", start = 0, direction = -1) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      legend.title = element_blank()) +
    geom_text(data = tabelaVacina,
              aes(x = "", y=Dose1, label = Dose1),
              position = position_stack(vjust = 0.5)) +
    labs(title = "Grafico 1: Primeira Dose",
         subtitle = "Fonte: Conecta Recife")

  d2 <- ggplot(tabelaVacina, aes(x = "", y = Dose2, fill = Sexo)) +
    geom_bar(width = 1, stat = "identity", color="green") +
    coord_polar("y", start = 0, direction = -1) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      legend.title = element_blank()) +
    geom_text(data = tabelaVacina,
              aes(x = "", y=Dose2, label = Dose2),
              position = position_stack(vjust = 0.5)) +
    labs(title = "Grafico 2: Segunda Dose",
         subtitle = "Fonte: Conecta Recife")

  dU <- ggplot(tabelaVacina, aes(x = "", y = DoseUnica, fill = Sexo)) +
    geom_bar(width = 1, stat = "identity", color="green") +
    coord_polar("y", start = 0, direction = -1) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      legend.title = element_blank()) +
    geom_text(data = tabelaVacina,
              aes(x = "", y = DoseUnica, label = DoseUnica),
              position = position_stack(vjust = 0.5)) +
    labs(title = "Grafico 3: Dose Única",
         subtitle = "Fonte: Conecta Recife")

  # Combinando os gráficos gerados
  title_plot =  ' Vacinação COVID19 RECIFE - Número de doses
                              aplicadas por gênero.
   Alunos: Andryene Pessoa e Guilherme Arruda'

  plot_grid(ncol = 3, labels = title_plot,
            label_size = 16, label_y = 1,
            rel_heights = c(.65,-0.05),
            d1, d2, dU, nrow = 2)
}
