# app.R: Registro Odontograma ICDAS II con rhandsontable (versión con botón para desmarcar permanentes)

# Verificar e instalar automáticamente las librerías necesarias
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("rhandsontable", quietly = TRUE)) {
  install.packages("rhandsontable")
}

library(shiny)
library(rhandsontable)

# 1) Listas de dientes FDI
todos_dientes <- c(
  permanentes <- as.character(c(11:18,21:28,31:38,41:48)),
  temporales  <- as.character(c(51:55,61:65,71:75,81:85))
)
permanentes <- todos_dientes[1:length(permanentes)]; temporales <- todos_dientes[(length(permanentes)+1):length(todos_dientes)]

# 2) Definir niveles de cada codificación
opts_resta <- c(
  "R0: Sin restauración","R1: Sellante en mal estado","R2: Sellante en buen estado",
  "R3: Obturación de color del diente","R4: Amalgama","R5: Corona de acero",
  "R6: Restauración en porcelana","R7: Obturación perdida o fracturada","R8: Restauración temporal"
)
opts_caries <- c(
  "C0: Sin caries","C1: Primer cambio visual en el esmalte","C2: Cambio visual marcado en el esmalte",
  "C3: Ruptura localizada del esmalte","C4: Sombra oscura subyacente",
  "C5: Cavidad detectable","C6: Cavidad extensa"
)
opts_estado <- c(
  "NA","0: Presente sano","91: Implante realizado por caries","96: No revisable",
  "97: Pérdida por caries","98: Pérdida no cariosa","99: No erupcionado"
)
opts_act   <- c("NA","Activa","Detenida")
opts_rad   <- c("NA","No cavitada","Cavitada")

# 3) Crear esqueleto de un paciente
tooth_columns <- c("M","O","D","V","P","Raiz")
make_base_df <- function(fecha, odontologo, curso, ID, dientes_presentes){
  df <- expand.grid(
    tooth = todos_dientes,
    stringsAsFactors = FALSE
  )
  df$fecha           <- fecha
  df$odontologo      <- odontologo
  df$curso           <- curso
  df$ID              <- ID
  df$Presente        <- df$tooth %in% dientes_presentes
  df$EstadoGeneral   <- "NA"
  for(s in tooth_columns){
    df[[paste0("Rest_",s)]] <- "R0: Sin restauración"
    df[[paste0("Car_", s)]] <- "C0: Sin caries"
  }
  df$ActividadCaries  <- "NA"
  df$CariesRadicular  <- "NA"
  df$Notas            <- ""
  df[, c(
    "fecha","odontologo","curso","ID","tooth","Presente","EstadoGeneral",
    unlist(lapply(tooth_columns, function(s) paste0("Rest_", s))),
    unlist(lapply(tooth_columns, function(s) paste0("Car_",  s))),
    "ActividadCaries","CariesRadicular","Notas"
  )]
}

ui <- fluidPage(
  titlePanel("Registro Odontograma ICDAS II - Sonrisas Sanas, Futuros Brillantes"),
  sidebarLayout(
    sidebarPanel(
      dateInput("fecha","Fecha", value = Sys.Date()),
      textInput("odontologo","Odontólogo / Código", ""),
      selectInput("curso","Curso", choices = 1:3, selected = 1),
      numericInput("ID","ID Estudiante", value = NA, min = 1),
      hr(),
      h4("Dientes permanentes"),
      checkboxGroupInput("dientes_permanentes","", choices = permanentes, selected = NULL),
      actionButton("clear_permanentes","Desmarcar permanentes", icon = icon("square")),
      h4("Dientes temporales"),
      checkboxGroupInput("dientes_temporales","", choices = temporales, selected = temporales),
      hr(),
      actionButton("gen_table","Generar tabla de trabajo", icon = icon("table")),
      actionButton("reset_all","Limpiar registro y formulario", icon = icon("eraser")),
      hr(),
      downloadButton("download","Descargar CSV")
    ),
    mainPanel(
      rHandsontableOutput("hot_table")
    )
  )
)

server <- function(input, output, session) {
  # Data reactiva: base generada y actualizada
  rv <- reactiveVal(NULL)
  
  # 1) Generar tabla base
  observeEvent(input$gen_table, {
    req(input$odontologo, input$ID)
    dientes_sel <- c(input$dientes_permanentes, input$dientes_temporales)
    rv(make_base_df(
      fecha      = as.character(input$fecha),
      odontologo = input$odontologo,
      curso      = input$curso,
      ID         = input$ID,
      dientes_presentes = dientes_sel
    ))
  })
  
  # 2) Desmarcar permanentes
  observeEvent(input$clear_permanentes, {
    updateCheckboxGroupInput(session,
                             "dientes_permanentes",
                             selected = character(0))
  })
  
  # 3) Mostrar editable con dropdowns
  output$hot_table <- renderRHandsontable({
    df <- rv()
    req(df)
    rhandsontable(df, stretchH = "all") %>%
      hot_col("Presente", type = "checkbox") %>%
      hot_col("EstadoGeneral", type = "dropdown", source = opts_estado) %>%
      { for(s in tooth_columns){
        . <- hot_col(., paste0("Rest_", s), type = "dropdown", source = opts_resta)
        . <- hot_col(., paste0("Car_",  s), type = "dropdown", source = opts_caries)
      }; . } %>%
      hot_col("ActividadCaries", type = "dropdown", source = opts_act) %>%
      hot_col("CariesRadicular", type = "dropdown", source = opts_rad)
  })
  
  # 4) Sincronizar cambios de rhandsontable
  observeEvent(input$hot_table, {
    rv(hot_to_r(input$hot_table))
  })
  
  # 5) Reset completo
  observeEvent(input$reset_all, {
    rv(NULL)
    updateDateInput(session,   "fecha", value = Sys.Date())
    updateTextInput(session,   "odontologo", value = "")
    updateSelectInput(session, "curso", selected = 1)
    updateNumericInput(session, "ID", value = NA)
    updateCheckboxGroupInput(session, "dientes_permanentes", selected = NULL)
    updateCheckboxGroupInput(session, "dientes_temporales", selected = temporales)
  })
  
  # 6) Download CSV
  output$download <- downloadHandler(
    filename = function() paste0(input$curso, "_", input$ID, "_icdas.csv"),
    content = function(file) write.csv(rv(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
