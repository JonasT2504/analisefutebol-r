library(readxl)
library(ggplot2)
library(dplyr)

#Importando a base de dados
dados_aed <- read_excel("C:/Users/Jonas/Downloads/FIFA23_official_data (100 melhores)-1 AED.xlsx")

#Limpeza de dados

#Relatório inicial
str(dados_aed)     # Estrutura dos dados
summary(dados_aed) # Resumo estatístico
head(dados_aed)    # Primeiras linhas

#1. VISUALIZAÇÃO DE DADOS
salario <- dados_aed$Wage

#1.1 Histograma do Salário
ggplot(data = dados_aed, aes(x = salario)) +
  geom_histogram(binwidth = 10000, fill = "purple", color = "gray") +
  labs(title = "Histograma do Salário", x = "Salário", y = "Frequência")

#1.2 Boxplot das idades
idade <- dados_aed$Age

#Criação do boxplot
boxplot(idade,
        main = "Boxplot de Idades",    # Título do boxplot
        xlab = "Idades",             # Rótulo do eixo x
        ylab = "Valores",              # Rótulo do eixo y
        col = "purple",                  # Cor dos pontos
        border = "black",              # Cor da borda das caixas
        horizontal = TRUE,            # Exibir na horizontal
        notch = FALSE,                 # Nãoch (fenda) nas caixas
        outline = TRUE)                # Exibir outliers como pontos individuais

#1.3 Gráfico de Pizza da preferencia de pés
tabela_preferencia <- table(dados_aed$`Preferred Foot`)
cores <- c("orange", "purple")

pie(tabela_preferencia,
    main = "Preferência dos Jogadores por Pé",
    labels = c("Esquerdo", "Direito"),
    col = cores)

#1.4 Boxplot de Overral

boxplot(overall,
        main = "Boxplot de Overall",    # Título do boxplot
        xlab = "Overall",             # Rótulo do eixo x
        ylab = "Valores",              # Rótulo do eixo y
        col = "purple",                  # Cor dos pontos
        border = "black",              # Cor da borda das caixas
        horizontal = TRUE,            # Exibir na horizontal
        notch = FALSE,                 # Nãoch (fenda) nas caixas
        outline = TRUE)                # Exibir outliers como pontos individuais

#1.5 Clubes mais frequebres
tabela_clubes <- table(dados_aed$Club)
print(tabela_clubes)

tabela_frequencia_ordenada <- sort(tabela_clubes, decreasing = TRUE)
clubes_frequentes <- head(tabela_frequencia_ordenada, 10)
tabela_top_10 <- as.data.frame(clubes_frequentes)
print(tabela_top_10)

ggplot(data = tabela_top_10, aes(x = Freq, y = Var1)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Top 10 Clubes Mais Frequentes", x = "Frequência", y = "Clubes") +
  theme(axis.text.y = element_text(hjust = 0))

#1.6 Países nais frequentes
tabela_paises <- table(dados_aed$Nationality)
print(tabela_paises)

tabela_frequencia_ordered <- sort(tabela_paises, decreasing = TRUE)
paises_frequentes <- head(tabela_frequencia_ordered, 10)
tabela_toppaises_10 <- as.data.frame(paises_frequentes)
print(tabela_toppaises_10)

ggplot(data = tabela_toppaises_10, aes(x = Freq, y = Var1)) +
  geom_bar(stat = "identity", fill = "darkmagenta") +
  labs(title = "Top 10 Países Mais Frequentes", x = "Frequência", y = "Países") +
  theme(axis.text.y = element_text(hjust = 0))

#1.7 Dispersão Salário

# Crie o gráfico de dispersão com cores
ggplot(data = dados_aed, aes(x = salario, y = overall, color = factor(overall))) +
  geom_point() +
  labs(title = "Gráfico de Dispersão de Salário por Overall", x = "Salário", y = "Overall")
#############################################################################################

#NORMALIDADE DAS VARIÁVEIS NUMÉRICAS 
# Teste de normalidade de Shapiro- para o salário
resultado_teste <- shapiro.test(salario)

# Exibir o resultado do teste
print(resultado_teste)

# Interpretar o resultado do teste
if (resultado_teste$p.value > 0.05) {
  cat("A variável segue uma distribuição normal (p-value >", resultado_teste$p.value, ")\n")
} else {
  cat("A variável NÃO segue uma distribuição normal (p-value <", resultado_teste$p.value, ")\n")
}

# Teste de normalidade de Shapiro- para o salário
resultado_teste <- shapiro.test(age)

# Exibir o resultado do teste
print(resultado_teste)

# Interpretar o resultado do teste
if (resultado_teste$p.value > 0.05) {
  cat("A variável segue uma distribuição normal (p-value >", resultado_teste$p.value, ")\n")
} else {
  cat("A variável NÃO segue uma distribuição normal (p-value <", resultado_teste$p.value, ")\n")
}
