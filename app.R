required_pkgs <- c(
  "shiny","shinydashboard","DT","dplyr","data.table","readr","readxl",
  "stringr","purrr","lubridate","ggplot2","plotly","openxlsx",
  "shinyvalidate","shinycssloaders",
  "rmarkdown","zip"
)
missing <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing, dependencies = TRUE)

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(data.table)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(lubridate)
library(ggplot2)
library(plotly)
library(openxlsx)
library(shinyvalidate)
library(shinycssloaders)
library(rmarkdown)
library(zip)

get_currency_symbol <- function(currency_code){
  switch(currency_code,
         USD = "$",
         EUR = "€",
         MZN = "MTn",
         ZAR = "R",
         "")
}

calc_quota_share <- function(policies, claims, share, comm_rate, overrider){
  stopifnot(is.data.frame(policies), is.data.frame(claims))
  stopifnot(is.numeric(share), between(share, 0,1))
  stopifnot(is.numeric(comm_rate), between(comm_rate, 0, 1))
  stopifnot(is.numeric(overrider), between(overrider, 0,1))
  claims <- claims %>%
    mutate(
      ceded_loss = loss_amount * share,
      retained_loss = loss_amount * (1 - share)
    )
  list(policies=policies, claims=claims)
}

calc_surplus <- function(policies, claims, retention, lines, comm_rate){
  stopifnot(is.data.frame(policies), is.data.frame(claims))
  stopifnot(is.numeric(retention), retention >= 0)
  stopifnot(is.numeric(lines), lines >= 0)
  stopifnot(is.numeric(comm_rate), between(comm_rate, 0, 1))
  limit <- retention * lines
  claims <- claims %>%
    mutate(
      ceded_loss = pmax(0, loss_amount - limit),
      retained_loss = pmin(loss_amount, limit)
    )
  list(policies=policies, claims=claims)
}

calc_risk_xl <- function(claims, retention, limit){
  stopifnot(is.data.frame(claims))
  stopifnot(is.numeric(retention), retention >= 0)
  stopifnot(is.numeric(limit), limit >= 0)
  claims <- claims %>%
    mutate(
      ceded_loss = pmin(pmax(0, loss_amount - retention), limit),
      retained_loss = pmin(loss_amount, retention)
    )
  claims
}

calc_cat_xl <- function(claims, retention, limit){
  stopifnot(is.data.frame(claims))
  stopifnot(is.numeric(retention), retention >= 0)
  stopifnot(is.numeric(limit), limit >= 0)
  claims <- claims %>%
    mutate(
      ceded_loss = pmin(pmax(0, loss_amount - retention), limit),
      retained_loss = pmin(loss_amount, retention)
    )
  claims
}

calc_stop_loss <- function(policies, claims, attachment_pct, limit, use_pct){
  stopifnot(is.data.frame(policies), is.data.frame(claims))
  stopifnot(is.numeric(attachment_pct), attachment_pct >= 0)
  stopifnot(is.numeric(limit), limit >= 0)
  stopifnot(is.logical(use_pct))
  total_loss <- sum(claims$loss_amount, na.rm = TRUE)
  attachment <- ifelse(use_pct, total_loss * attachment_pct, attachment_pct)
  claims <- claims %>%
    mutate(
      ceded_loss = pmax(0, pmin(loss_amount - attachment, limit)),
      retained_loss = loss_amount - ceded_loss
    )
  list(policies=policies, claims=claims)
}

ui <- dashboardPage(
  dashboardHeader(
    title = "AML-REINSUR"
  ),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Proporcional", tabName = "prop", icon = icon("percent")),
                menuItem("Não-Proporcional", tabName = "nprop", icon = icon("percent")),
                menuItem("Resultados", tabName = "results", icon = icon("calculator")),
                menuItem("Instrução de Uso", tabName = "instrucao", icon = icon("info-circle")),
                menuItem("Relatórios", tabName = "rel", icon = icon("file-export")),
                menuItem("Moeda", icon = icon("money-bill"),
                         menuSubItem("Metical (MZN)", tabName = "moeda_mzn"),
                         menuSubItem("Dólar (USD)", tabName = "moeda_usd"),
                         menuSubItem("Euro (EUR)", tabName = "moeda_eur"),
                         menuSubItem("Rand (ZAR)", tabName = "moeda_zar")
                )
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .small-note{font-size:10px;color:#666}
      .form-section{border:1px solid #eee;padding:8px;border-radius:6px;margin-bottom:8px;font-size:12px}
      .main-footer { background-color: #004584; color: #ffffff; font-size: 12px; padding: 8px 15px; text-align: center; font-weight: 600; }
      .main-footer a { color: #f0a500; text-decoration: none; }
      .main-footer a:hover { text-decoration: underline; }
      .dataTables_wrapper { font-size: 11px; }
      .plotly { height: 300px !important; }
    "))),
    tabItems(
      tabItem(tabName = "prop",
              fluidRow(
                box(width = 6, title = "Parâmetros Proporcionais", status = "primary", solidHeader = TRUE,
                    sliderInput("qs_share", "Quota-Parte (share)", min = 0, max = 1, value = 0.3, step = 0.01),
                    sliderInput("qs_comm", "Comissão (QS)", min = 0, max = 0.4, value = 0.2, step = 0.01),
                    sliderInput("qs_over", "Overrider", min = 0, max = 0.1, value = 0.00, step = 0.005)
                ),
                box(width = 6, title = "Dados Carregados", status = "info", solidHeader = TRUE,
                    fileInput("file_policies_prop", "Carregar Apólices (.csv/.xlsx)", accept = c(".csv", ".xlsx")),
                    fileInput("file_claims_prop", "Carregar Sinistros (.csv/.xlsx)", accept = c(".csv", ".xlsx")),
                    span(class = "small-note", "Os resultados só serão exibidos após carregamento dos dados e execução do cálculo."),
                    DTOutput("tbl_policies_prop") %>% withSpinner(),
                    DTOutput("tbl_claims_prop") %>% withSpinner(),
                    actionButton("run_prop", "Executar Cálculos", icon = icon("play"))
                )
              )
      ),
      tabItem(tabName = "nprop",
              fluidRow(
                box(width = 6, title = "Parâmetros Não-Proporcionais", status = "primary", solidHeader = TRUE,
                    numericInput("rxl_ret", "Risk XL – Retenção", value = 50000, min = 0),
                    numericInput("rxl_lim", "Risk XL – Limite", value = 200000, min = 0),
                    numericInput("cxl_ret", "Cat XL – Retenção por Evento", value = 500000, min = 0),
                    numericInput("cxl_lim", "Cat XL – Limite por Evento", value = 2000000, min = 0),
                    checkboxInput("sl_usepct", "Stop Loss – Anexo como % do Prêmio", TRUE),
                    sliderInput("sl_attach", "Stop Loss – Anexo (% ou valor)", min = 0, max = 1, value = 0.75, step = 0.01),
                    numericInput("sl_limit", "Stop Loss – Limite", value = 1000000, min = 0)
                ),
                box(width = 6, title = "Dados Carregados", status = "info", solidHeader = TRUE,
                    fileInput("file_policies_nprop", "Carregar Apólices (.csv/.xlsx)", accept = c(".csv", ".xlsx")),
                    fileInput("file_claims_nprop", "Carregar Sinistros (.csv/.xlsx)", accept = c(".csv", ".xlsx")),
                    span(class = "small-note", "Os resultados só serão exibidos após carregamento dos dados e execução do cálculo."),
                    DTOutput("tbl_policies_nprop") %>% withSpinner(),
                    DTOutput("tbl_claims_nprop") %>% withSpinner(),
                    actionButton("run_nprop", "Executar Cálculos", icon = icon("play"))
                )
              )
      ),
      tabItem(tabName = "results",
              fluidRow(
                valueBoxOutput("vb_prem", width = 3),
                valueBoxOutput("vb_loss", width = 3),
                valueBoxOutput("vb_ceded", width = 3),
                valueBoxOutput("vb_retained", width = 3)
              ),
              fluidRow(
                box(width = 6, title = "Resultados – Apólices", status = "info", solidHeader = TRUE,
                    DTOutput("out_policies") %>% withSpinner()
                ),
                box(width = 6, title = "Resultados – Sinistros", status = "info", solidHeader = TRUE,
                    DTOutput("out_claims") %>% withSpinner()
                )
              ),
              fluidRow(
                box(width = 12, title = "Gráficos de Perdas", status = "info", solidHeader = TRUE,
                    plotlyOutput("plt_losses") %>% withSpinner()
                )
              )
      ),
      tabItem(tabName = "instrucao",
              fluidRow(
                box(width = 12, title = "Instrução de Uso", status = "primary", solidHeader = TRUE,
                    tags$p("Este aplicativo calcula provisões técnicas para resseguro proporcional e não proporcional."),
                    tags$p("A lógica dos cálculos baseia-se nas fórmulas actuariais tradicionais, conforme definida nas abas Proporcional e Não-Proporcional."),
                    tags$p("Para cada tipo de resseguro, carregue os dados, ajuste os parâmetros e execute os cálculos."),
                    tags$p("Use a seleção de moedas para converter valores financeiros em Metical, Dólar, Euro ou Rand."),
                    tags$p("Os relatórios Excel incluem um resumo dos parâmetros utilizados e os resultados detalhados."),
                    tags$p("Na aba Relatórios, escolha quais cálculos deseja exportar em Excel ou CSV."),
                    tags$p("Os ícones de moeda estão fixos no menu lateral para fácil acesso.")
                )
              )
      ),
      tabItem(tabName = "rel",
              fluidRow(
                box(width = 12, title = "Selecione os cálculos para exportar", status = "primary", solidHeader = TRUE,
                    checkboxGroupInput("rel_type", "Tipos de Cálculos:",
                                       choices = c("Proporcional", "Não-Proporcional", "Simulações"), selected = NULL),
                    downloadButton("dl_excel", "Baixar Excel (.xlsx)"),
                    downloadButton("dl_csv", "Baixar CSV (.zip)")
                )
              )
      )
    ),
    tags$footer(class = "main-footer", "Aplicativo Profissional em desenvolvimento por Meque da Barca, Actuário, todos os direitos reservados @2025.")
  )
)

server <- function(input, output, session){
  rv <- reactiveValues(
    policies_prop = NULL,
    claims_prop = NULL,
    policies_nprop = NULL,
    claims_nprop = NULL,
    last_calc_policies = NULL,
    last_calc_claims = NULL,
    currency = "MZN"
  )
  
  observeEvent(input$tabs, {
    switch(input$tabs,
           moeda_mzn = rv$currency <- "MZN",
           moeda_usd = rv$currency <- "USD",
           moeda_eur = rv$currency <- "EUR",
           moeda_zar = rv$currency <- "ZAR"
    )
  })
  
  observeEvent(input$file_policies_prop, {
    req(input$file_policies_prop)
    ext <- tools::file_ext(input$file_policies_prop$name)
    df <- if (tolower(ext) == "csv") read_csv(input$file_policies_prop$datapath, show_col_types = FALSE) else readxl::read_excel(input$file_policies_prop$datapath)
    rv$policies_prop <- df
  })
  
  observeEvent(input$file_claims_prop, {
    req(input$file_claims_prop)
    ext <- tools::file_ext(input$file_claims_prop$name)
    df <- if (tolower(ext) == "csv") read_csv(input$file_claims_prop$datapath, show_col_types = FALSE) else readxl::read_excel(input$file_claims_prop$datapath)
    rv$claims_prop <- df
  })
  
  output$tbl_policies_prop <- renderDT({ req(rv$policies_prop); datatable(rv$policies_prop, options=list(scrollX=TRUE, pageLength=5), rownames=FALSE) })
  output$tbl_claims_prop <- renderDT({ req(rv$claims_prop); datatable(rv$claims_prop, options=list(scrollX=TRUE, pageLength=5), rownames=FALSE) })
  
  observeEvent(input$run_prop,{
    req(rv$policies_prop, rv$claims_prop)
    res <- calc_quota_share(rv$policies_prop, rv$claims_prop, input$qs_share, input$qs_comm, input$qs_over)
    rv$last_calc_policies <- res$policies
    rv$last_calc_claims   <- res$claims
    showNotification("Cálculos proporcionais concluídos!", type="message")
  })
  
  observeEvent(input$file_policies_nprop, {
    req(input$file_policies_nprop)
    ext <- tools::file_ext(input$file_policies_nprop$name)
    df <- if (tolower(ext) == "csv") read_csv(input$file_policies_nprop$datapath, show_col_types = FALSE) else readxl::read_excel(input$file_policies_nprop$datapath)
    rv$policies_nprop <- df
  })
  
  observeEvent(input$file_claims_nprop, {
    req(input$file_claims_nprop)
    ext <- tools::file_ext(input$file_claims_nprop$name)
    df <- if (tolower(ext) == "csv") read_csv(input$file_claims_nprop$datapath, show_col_types = FALSE) else readxl::read_excel(input$file_claims_nprop$datapath)
    rv$claims_nprop <- df
  })
  
  output$tbl_policies_nprop <- renderDT({ req(rv$policies_nprop); datatable(rv$policies_nprop, options=list(scrollX=TRUE, pageLength=5), rownames=FALSE) })
  output$tbl_claims_nprop <- renderDT({ req(rv$claims_nprop); datatable(rv$claims_nprop, options=list(scrollX=TRUE, pageLength=5), rownames=FALSE) })
  
  observeEvent(input$run_nprop,{
    req(rv$policies_nprop, rv$claims_nprop)
    clm_rxl <- calc_risk_xl(rv$claims_nprop, input$rxl_ret, input$rxl_lim)
    clm_cxl <- calc_cat_xl(clm_rxl, input$cxl_ret, input$cxl_lim)
    clm_sl  <- calc_stop_loss(rv$policies_nprop, clm_cxl, input$sl_attach, input$sl_limit, input$sl_usepct)$claims
    rv$last_calc_policies <- rv$policies_nprop
    rv$last_calc_claims   <- clm_sl
    showNotification("Cálculos não-proporcionais concluídos!", type="message")
  })
  
  output$vb_prem <- renderValueBox({
    req(rv$last_calc_policies)
    valueBox(
      paste0(get_currency_symbol(rv$currency), " ", formatC(sum(rv$last_calc_policies$gross_premium %||% 0, na.rm=TRUE), big.mark=",")),
      "Prêmio Bruto", icon=icon("money-bill"), color="teal"
    )
  })
  
  output$vb_loss <- renderValueBox({
    req(rv$last_calc_claims)
    valueBox(
      paste0(get_currency_symbol(rv$currency), " ", formatC(sum(rv$last_calc_claims$loss_amount %||% 0, na.rm=TRUE), big.mark=",")),
      "Perdas Brutas", icon=icon("fire"), color="red"
    )
  })
  
  output$vb_ceded <- renderValueBox({
    req(rv$last_calc_claims)
    valueBox(
      paste0(get_currency_symbol(rv$currency), " ", formatC(sum(rv$last_calc_claims$ceded_loss %||% 0, na.rm=TRUE), big.mark=",")),
      "Perdas Cedidas", icon=icon("hand-holding-usd"), color="orange"
    )
  })
  
  output$vb_retained <- renderValueBox({
    req(rv$last_calc_claims)
    valueBox(
      paste0(get_currency_symbol(rv$currency), " ", formatC(sum(rv$last_calc_claims$retained_loss %||% 0, na.rm=TRUE), big.mark=",")),
      "Perdas Retidas", icon=icon("shield-alt"), color="green"
    )
  })
  
  output$out_policies <- renderDT({ req(rv$last_calc_policies); datatable(rv$last_calc_policies, options=list(scrollX=TRUE, pageLength=5), rownames=FALSE) })
  output$out_claims <- renderDT({ req(rv$last_calc_claims); datatable(rv$last_calc_claims, options=list(scrollX=TRUE, pageLength=5), rownames=FALSE) })
  
  output$plt_losses <- renderPlotly({
    req(rv$last_calc_claims)
    df <- rv$last_calc_claims
    p <- ggplot(df, aes(x=claim_id)) +
      geom_col(aes(y=loss_amount, fill="Bruto"), alpha=0.6) +
      geom_col(aes(y=ceded_loss, fill="Cedido"), alpha=0.6) +
      geom_col(aes(y=retained_loss, fill="Retido"), alpha=0.6) +
      labs(x="ID do Sinistro", y="Montante", fill="Legenda") +
      theme_minimal(base_size=10)
    ggplotly(p)
  })
  
  output$dl_excel <- downloadHandler(
    filename = function() paste0("resseguro_", Sys.Date(), ".xlsx"),
    content = function(file){
      wb <- createWorkbook()
      addWorksheet(wb, "Parâmetros Escolhidos")
      param_list <- data.frame(
        Parâmetro = c("Moeda","Quota-Parte","Comissão QS","Overrider",
                      "Risk XL - Retenção","Risk XL - Limite",
                      "Cat XL Retenção","Cat XL Limite",
                      "Stop Loss - Uso %","Stop Loss - Anexo","Stop Loss - Limite"),
        Valor = c(rv$currency,input$qs_share,input$qs_comm,input$qs_over,
                  input$rxl_ret,input$rxl_lim,input$cxl_ret,input$cxl_lim,
                  input$sl_usepct,input$sl_attach,input$sl_limit)
      )
      writeData(wb, sheet = "Parâmetros Escolhidos", param_list)
      
      if("Proporcional" %in% input$rel_type & !is.null(rv$last_calc_policies)){
        addWorksheet(wb, "Proporcional_Policies")
        writeData(wb, "Proporcional_Policies", rv$last_calc_policies)
        addWorksheet(wb, "Proporcional_Claims")
        writeData(wb, "Proporcional_Claims", rv$last_calc_claims)
      }
      if("Não-Proporcional" %in% input$rel_type & !is.null(rv$last_calc_policies)){
        addWorksheet(wb, "NProp_Policies")
        writeData(wb, "NProp_Policies", rv$last_calc_policies)
        addWorksheet(wb, "NProp_Claims")
        writeData(wb, "NProp_Claims", rv$last_calc_claims)
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("resseguro_", Sys.Date(), ".zip"),
    content = function(file){
      tmp <- tempdir()
      files <- c()
      if("Proporcional" %in% input$rel_type & !is.null(rv$last_calc_policies)){
        write_csv(rv$last_calc_policies, file.path(tmp,"Proporcional_Policies.csv"))
        write_csv(rv$last_calc_claims, file.path(tmp,"Proporcional_Claims.csv"))
        files <- c(files, file.path(tmp,"Proporcional_Policies.csv"), file.path(tmp,"Proporcional_Claims.csv"))
      }
      if("Não-Proporcional" %in% input$rel_type & !is.null(rv$last_calc_policies)){
        write_csv(rv$last_calc_policies, file.path(tmp,"NProp_Policies.csv"))
        write_csv(rv$last_calc_claims, file.path(tmp,"NProp_Claims.csv"))
        files <- c(files, file.path(tmp,"NProp_Policies.csv"), file.path(tmp,"NProp_Claims.csv"))
      }
      zip(zipfile = file, files = files, flags = "-j")
    }
  )
}

shinyApp(ui, server)

