# Carregando pacotes necessários
install.packages ("pacman")
pacman::p_load(lavaan, semPlot, BifactorIndicesCalculator, readxl)

# Carregando e organizando o banco
url <- "https://raw.githubusercontent.com/felipe-bigoto/banco-dass21/main/dados-dass-21.xlsx"
download.file(
  url,
  destfile = "dados-dass-21.xlsx",
  mode = "wb"
)
Dados_Bifactor <- read_excel("dados-dass-21.xlsx")

#Escrevendo o modelo
model_bifactor <- '
  PSICOPATOLOGIA  =~ Item_01 + Item_02 + Item_03 + Item_04 + Item_05 + Item_06 + Item_07 + Item_08 + Item_09 + Item_10 +
    Item_11 + Item_12 + Item_13 + Item_14 + Item_15 + Item_16 + Item_17 + Item_18 + Item_19 + Item_20 + Item_21

  DEPRESSAO =~ Item_03 + Item_05 + Item_10 + Item_13 + Item_16 + Item_17 + Item_21
  ANSIEDADE =~ Item_02 + Item_04 + Item_07 + Item_09 + Item_15 + Item_19 + Item_20
  ESTRESSE =~ Item_01 + Item_06 + Item_08 + Item_11 + Item_12 + Item_14 + Item_18

  PSICOPATOLOGIA ~~ 0*DEPRESSÃO; PSICOPATOLOGIA ~~ 0*ANSIEDADE; PSICOPATOLOGIA ~~ 0*ESTRESSE
  DEPRESSAO ~~ 0*ANSIEDADE; DEPRESSÃO ~~ 0*ESTRESSE
  ANSIEDADE ~~ 0*ESTRESSE
  '

# Ajuste do modelo
fit <- cfa(
  model_bifactor,
  data = Dados_Bifactor,
  estimator = "WLSMV",
  ordered = TRUE,
  std.lv = TRUE,
)

# Avaliação do ajuste
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Ômega Hierárquico, ECV e PUC
bifactorIndices(fit)

#Figura
semPaths(
  fit, what = "std",
  layout = "tree3",
  bifactor = "PSICOPATOLOGIA",
  intercepts = FALSE, thresholds = FALSE, residuals = FALSE,
  edge.label.cex = 1.1, label.cex = 1.2, edge.label.position = 0.7,
  mar = c(3,0.8,3,1),
  weighted = FALSE, edge.color = "black"
)


