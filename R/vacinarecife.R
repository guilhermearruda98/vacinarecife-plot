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

  # Gerando os gráficos
  d1 <- ggplot(tabelaVacina, aes(x = Sexo, y = Dose1)) +
    geom_col(fill = 'red') +
    xlab("Sexo") +
    ylab("Dose 1")

  d2 <- ggplot(tabelaVacina, aes(x = Sexo, y = Dose2)) +
    geom_col(fill = 'blue') +
    xlab("Sexo") +
    ylab("Dose 2")

  dU <- ggplot(tabelaVacina, aes(x = Sexo, y = DoseUnica)) +
    geom_col(fill = 'green') +
    xlab("Sexo") +
    ylab("Dose Única")

  # Combinando os gráficos gerados
  title <- ggdraw() +
    draw_label("Vacinação COVID 19 RECIFE - Número de doses aplicadas por gênero
               Alunos: Andryene Pessoa e Guilherme Arruda",
               fontface='bold')

  plot_grid(title, d1, d2, dU, ncol=1)
}
