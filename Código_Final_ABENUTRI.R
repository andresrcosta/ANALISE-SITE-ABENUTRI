# Definindo o diretório

setwd("C:\\Users\\aluno\\Desktop\\PORTIFOLIO\\ABENUTRI")

library(ggplot2)
library(dplyr)
library(tidyr)

# Definindo o esquema de cores
cores_graficos <- c("limegreen", "blue4", "yellow1")

# Dados agrupados em um único dataframe
dados <- list(
  rotulagem = c(Aprovados = 5, Reprovados = 79),
  laboratorial = c(Aprovados = 51, Reprovados = 33),
  analise_geral = c(Aprovados = 4, Reprovados = 80)
) %>% bind_rows(.id = "Categoria") %>% pivot_longer(-Categoria, names_to = "Status", values_to = "Frequencia")

# Transformando Categoria e Status em fatores para ordenação nos gráficos
dados$Categoria <- factor(dados$Categoria, levels = c("rotulagem", "laboratorial", "analise_geral"))
dados$Status <- factor(dados$Status, levels = c("Aprovados", "Reprovados"))

# Função para calcular proporção e posicão para texto
dados <- dados %>%
  group_by(Categoria) %>%
  mutate(Total = sum(Frequencia)) %>%
  ungroup() %>%
  mutate(Prop = round(100 * Frequencia / Total, 2),
         Posicao = cumsum(Prop) - 0.5 * Prop)

# Função para criar gráficos
criar_grafico_setor <- function(dataframe, categoria, cores_estat, nome_arquivo) {
  filtro <- dataframe %>% filter(Categoria == categoria)
  grafico <- ggplot(filtro, aes(x = "", y = Prop, fill = Status)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    geom_text(aes(y = Posicao, label = paste0(Prop, "%")), color = "black") +
    labs(x = NULL, y = NULL, fill = "Classificação", title = categoria) +
    theme_void() +
    theme(legend.position = "top") +
    scale_fill_manual(values = cores_estat)
  
  ggsave(nome_arquivo, plot = grafico, width = 158, height = 93, units = "mm")
}

# Criando gráficos para cada categoria
for (categoria in unique(dados$Categoria)) {
  criar_grafico_setor(dados, categoria, cores_graficos, paste0("setor_", categoria, ".pdf"))
}

# Lendo o Banco de Dados
dados_webscraping <- read.table("dataframe_webscraping.txt", sep = "|", header = TRUE)

# Preparando o dataset
dados_webscraping %>%
  mutate(variacao_padronizada = as.numeric(gsub("%", "", gsub(",", ".", VARIACAO))),
         tipo_de_variacao = case_when(
           variacao_padronizada > 0 ~ "POSITIVA",
           variacao_padronizada == 0 ~ "SEM VARIAÇÃO",
           TRUE ~ "NEGATIVA"
         ),
         analise_laboratorial = ifelse(variacao_padronizada > -22, "APROVADO", "REPROVADO")) %>%
  select(tipo_de_variacao, analise_laboratorial, variacao_padronizada) -> dados_analise

# Frequência e gráfico de tipo_de_variacao
dados_analise %>%
  count(tipo_de_variacao) %>%
  mutate(freq = round(100 * (n / sum(n)), 2)) %>%
  ggplot(aes(x = fct_reorder(tipo_de_variacao, n, .desc = T), y = n, fill = tipo_de_variacao)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste(n, "(", freq, "%)", sep = "")), vjust = -0.5) +
  labs(x = "Tipo de Variação", y = "Frequência") +
  scale_fill_manual(values = cores_graficos) +
  theme_minimal()

# Salvando o gráfico
ggsave("barra_variacao.pdf", width = 158, height = 93, units = "mm")

# Função para converter colunas e calcular médias
processarDados <- function(df) {
  df %>%
    mutate(across(contains("L."), ~ as.numeric(gsub("%", "", gsub(",", ".", .))))) %>%
    mutate(media = round((`variacao_L.Glicina` + `variacao_L.Treonina`) / 2, 2),
           media_absoluta = round(sqrt((`variacao_L.Glicina`^2) + (`variacao_L.Treonina`^2)) / sqrt(2), 2))
}

# Função para gerar quadro resumo
gerarQuadroResumo <- function(df, coluna) {
  df %>%
    summarise(Média = round(mean({{coluna}}), 2),
              `Desvio Padrão` = round(sd({{coluna}}), 2),
              `Variância` = round(var({{coluna}}), 2),
              `Mínimo` = round(min({{coluna}}), 2),
              `1º Quartil` = round(quantile({{coluna}}, probs = .25), 2),
              Mediana = round(quantile({{coluna}}, probs = .5), 2),
              `3º Quartil` = round(quantile({{coluna}}, probs = .75), 2),
              `Máximo` = round(max({{coluna}}), 2)) %>%
    t() %>%
    as.data.frame()
}

# Carregamento dos dados
aprovadas <- read.table("proteinas_aprovadas_2.txt", sep = "|", header = TRUE)
reprovadas <- read.table("proteinas_reprovadas_2.txt", sep = "|", header = TRUE)

# Processamento dos dados
aprovadas <- processarDados(aprovadas)
reprovadas <- processarDados(reprovadas)

# Ordenado e exportação com xtable
aprovadas_latex <- aprovadas[, c(1, 2, 5, 6, 11)]
df_ordenado_aprovadas <- aprovadas_latex[order(aprovadas_latex$media_absoluta), ]
print(xtable(df_ordenado_aprovadas))

reprovadas_latex <- reprovadas[, c(1, 2, 5, 6, 11)]
df_ordenado_reprovadas <- reprovadas_latex[order(reprovadas_latex$media_absoluta, decreasing = TRUE), ]
print(xtable(df_ordenado_reprovadas))

# Quadros resumo
quadro_resumo_aprovadas_treonina <- gerarQuadroResumo(aprovadas, variacao_L.Treonina)
print(xtable(quadro_resumo_aprovadas_treonina))

quadro_resumo_aprovadas_L.Glicina <- gerarQuadroResumo(aprovadas, variacao_L.Glicina)
print(xtable(quadro_resumo_aprovadas_L.Glicina))

quadro_resumo_reprovadas_treonina <- gerarQuadroResumo(reprovadas, variacao_L.Treonina)
print(xtable(quadro_resumo_reprovadas_treonina))

quadro_resumo_reprovadas_L.Glicina <- gerarQuadroResumo(reprovadas, variacao_L.Glicina)
print(xtable(quadro_resumo_reprovadas_L.Glicina))

# Função para filtrar e gerar tabela LaTeX a partir de uma condição
gerar_tabela_latex <- function(dados, condicao, colunas, ordem_decrecente = TRUE) {
  dataset <- filter(dados, analise_laboratorial == condicao)
  subset_dados <- dataset[, colunas]
  df_ordenado <- subset_dados[order(subset_dados$variacao_padronizada, decreasing = ordem_decrecente), ]
  xtable(df_ordenado)
}

# Função para resumir dados e transpor
resumir_transpor <- function(dados) {
  resumo <- dados %>% 
    summarize(
      Média = round(mean(variacao_padronizada), 2),
      `Desvio Padrão` = round(sd(variacao_padronizada), 2),
      `Variância` = round(var(variacao_padronizada), 2),
      `Mínimo` = round(min(variacao_padronizada), 2),
      `1º Quartil` = round(quantile(variacao_padronizada, probs = .25), 2),
      Mediana = round(quantile(variacao_padronizada, probs = .5), 2),
      `3º Quartil` = round(quantile(variacao_padronizada, probs = .75), 2),
      `Máximo` = round(max(variacao_padronizada), 2)
    ) %>% 
    t() %>%
    as.data.frame()
  xtable(resumo)
}

# Aplicando as funções criadas para as diversas condições
gerar_tabela_latex(dados, "APROVADO", c(2,3,7))
gerar_tabela_latex(dados, "REPROVADO", c(2,3,7), FALSE)

# Resumo e transposição
quadro_resumo_aprovadas_creatina <- resumir_transpor(dataset_laboratorial_aprovado)
quadro_resumo_reprovadas_creatina <- resumir_transpor(dataset_laboratorial_reprovado)

# Função para criar gráficos de boxplot e salvar
criar_e_salvar_boxplot <- function(dataset, nome_arquivo) {
  ggplot(dataset) +
    aes(x=factor(""), y=variacao_padronizada) +
    geom_boxplot(fill="blue4", width = 0.5) +
    guides(fill=FALSE) +
    stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white") +
    labs(x="", y="Variação na concentração de Creatina") +
    ggsave(nome_arquivo, width = 158, height = 93, units = "mm")
}

# Aplicando da função de gráficos
criar_e_salvar_boxplot(dataset_laboratorial_aprovado, "box_creatina_apro.pdf")
criar_e_salvar_boxplot(dataset_laboratorial_reprovado, "box_creatina_repro.pdf")

# Função para aplicar transformações tanto em aprovadas quanto em reprovadas
processa_variacao <- function(df) {
  df %>%
    mutate(
      tipo_de_variacao_glicina = case_when(
        variacao_L.Glicina > 0 ~ "POSITIVA",
        variacao_L.Glicina == 0 ~ "SEM VARIAÇÃO",
        TRUE ~ "NEGATIVA"
      ),
      tipo_de_variacao_treonina = case_when(
        variacao_L.Treonina > 0 ~ "POSITIVA",
        variacao_L.Treonina == 0 ~ "SEM VARIAÇÃO",
        TRUE ~ "NEGATIVA"
      )
    )
}

# Aplicando a função aos dataframes
reprovadas <- processa_variacao(reprovadas)
aprovadas <- processa_variacao(aprovadas)

# Ajustando para leitura de dados e manipulação de porcentagens e legendas
dados <- read.table("gambiarra_reprovadas.txt", sep = "|", header = TRUE)

dados <- dados %>%
  mutate(
    freq_relativa = freq_relativa %>% as.numeric(),
    porcentagens = str_replace_all(as.character(freq_relativa), "\\.", ",") %>% paste0("%"),
    legendas = str_squish(paste(freq, " (", porcentagens, ")"))
  )

# Cores para o gráfico
cores_graficos <- c("blue4", "limegreen")

# Gráfico
ggplot(dados) +
  aes(
    x = fct_reorder(Aminoácido, freq, .desc = TRUE), 
    y = freq,
    fill = Tipo.de.Variação, 
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  scale_fill_manual(values = cores_graficos) +
  labs(x = "Aminoácido", y = "Frequência", fill = "Tipo de Variação")

ggsave("bi_freq_reprovadas.pdf", width = 158, height = 93, units = "mm")