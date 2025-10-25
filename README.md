# Resseguro-shiny-app
Aplicativo actuarial em R Shiny para cálculo e simulação de resseguro proporcional e não-proporcional, com suporte a múltiplas moedas, relatórios dinâmicos, gráficos interativos e exportação em Excel. Desenvolvido por Meque da Barca para análise e gestão de risco.

# AML-REINSUR

Aplicativo actuarial em **R Shiny** para cálculo e simulação de **resseguro proporcional e não-proporcional**, com suporte a múltiplas moedas, relatórios dinâmicos, gráficos interativos e exportação em Excel. Desenvolvido por **Meque da Barca** para análise e gestão de risco.

---

## Funcionalidades

- Cálculo de **resseguro proporcional** (Quota-Share)  
- Cálculo de **resseguro não-proporcional** (Risk XL, Cat XL, Stop Loss)  
- Visualização de **sinistros e apólices** carregados  
- Gráficos interativos de perdas (brutas, cedidas e retidas)  
- **Exportação de resultados** em Excel (.xlsx) ou CSV (.zip)  
- Suporte a múltiplas moedas: Metical (MZN), Dólar (USD), Euro (EUR) e Rand (ZAR)  
- Notificações e validação de dados carregados  

---

## Requisitos

O aplicativo utiliza os seguintes pacotes R:

```r
shiny, shinydashboard, DT, dplyr, data.table, readr, readxl,
stringr, purrr, lubridate, ggplot2, plotly, openxlsx,
shinyvalidate, shinycssloaders, rmarkdown, zip


required_pkgs <- c(
  "shiny","shinydashboard","DT","dplyr","data.table","readr","readxl",
  "stringr","purrr","lubridate","ggplot2","plotly","openxlsx",
  "shinyvalidate","shinycssloaders","rmarkdown","zip"
)
missing <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if(length(missing)) install.packages(missing, dependencies = TRUE)

## Estrutura de Dados

O aplicativo requer duas bases principais (para cada tipo de resseguro):

Apólices (Policies)

Colunas típicas: policy_id, gross_premium, start_date, end_date, sum_insured

Sinistros (Claims)

Colunas típicas: claim_id, policy_id, loss_amount, date_of_loss





