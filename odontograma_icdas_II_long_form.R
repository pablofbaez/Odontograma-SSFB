# app.R: Registro Odontograma ICDAS II con rhandsontable (versión con botón para desmarcar permanentes)

# Verificar e instalar automáticamente las librerías necesarias
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("rhandsontable", quietly = TRUE)) {
  install.packages("rhandsontable")
}
if (!requireNamespace("shinyalert", quietly = TRUE)) {
  install.packages("shinyalert")
}

library(shinyalert)
library(shiny)
library(rhandsontable)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Listas de dientes FDI
todos_dientes <- c(
  permanentes <- as.character(c(11:18,21:28,31:38,41:48)),
  temporales  <- as.character(c(51:55,61:65,71:75,81:85))
)
permanentes <- todos_dientes[1:length(permanentes)]
temporales <- todos_dientes[(length(permanentes)+1):length(todos_dientes)]

# Definir niveles de cada codificación
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

tooth_columns <- c("M","O","D","V","P","R")

# Crear esqueleto de un paciente
make_base_df <- function(fecha, odontologo, curso, ID) {
  df <- expand.grid(
    tooth = todos_dientes,
    stringsAsFactors = FALSE
  )
  df$fecha <- fecha
  df$odontologo <- odontologo
  df$curso <- curso
  df$ID <- ID
  df$Presente <- df$tooth %in% temporales  # Marca solo los dientes temporales como presentes
  df$EstadoGeneral <- "NA"
  for(s in tooth_columns) {
    df[[paste0("Rest_", s)]] <- "R0: Sin restauración"
    df[[paste0("Car_", s)]] <- "C0: Sin caries"
  }
  df$ActividadCaries <- "NA"
  df$CariesRadicular <- "NA"
  df$Notas <- ""
  df[, c("fecha", "odontologo", "curso", "ID", "tooth", "Presente", "EstadoGeneral",
         unlist(lapply(tooth_columns, function(s) paste0("Rest_", s))),
         unlist(lapply(tooth_columns, function(s) paste0("Car_", s))),
         "ActividadCaries", "CariesRadicular", "Notas")]
}

ui <- fluidPage(
  useShinyalert(),  # Habilita shinyalert
  titlePanel("Registro Odontograma ICDAS II - Sonrisas Sanas, Futuros Brillantes"),
  sidebarLayout(
    sidebarPanel(
      dateInput("fecha", "Fecha", value = Sys.Date()),
      textInput("odontologo", "Odontólogo", ""),
      selectInput("curso", "Curso", choices = 1:3, selected = 1),
      numericInput("ID", "ID Estudiante", value = NA, min = 1),
      hr(),
      actionButton("gen_table", "Generar tabla de trabajo", icon = icon("table")),
      actionButton("reset_all", "Limpiar registro y formulario", icon = icon("eraser")),
      hr(),
      downloadButton("download", "Descargar CSV")
    ),
    mainPanel(
      rHandsontableOutput("hot_table")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveVal(NULL)
  
  observeEvent(input$gen_table, {
    req(input$odontologo, input$ID)
    rv(make_base_df(
      fecha = as.character(input$fecha),
      odontologo = input$odontologo,
      curso = input$curso,
      ID = input$ID
    ))
  })
  
  output$hot_table <- renderRHandsontable({
    df <- rv()
    req(df)
    rownames(df) <- df$tooth
    rhandsontable(df, stretchH = "all") %>%
      hot_col("Presente", type = "checkbox") %>%
      hot_col("EstadoGeneral", type = "dropdown", source = opts_estado) %>%
      { for(s in tooth_columns) {
        . <- hot_col(., paste0("Rest_", s), type = "dropdown", source = opts_resta)
        . <- hot_col(., paste0("Car_", s), type = "dropdown", source = opts_caries)
      }; . } %>%
      hot_col("ActividadCaries", type = "dropdown", source = opts_act) %>%
      hot_col("CariesRadicular", type = "dropdown", source = opts_rad)
  })
  
  observeEvent(input$hot_table, {
    rv(hot_to_r(input$hot_table))
  })
  
  observeEvent(input$reset_all, {
    shinyalert(
      title = "Confirmación",
      text = "¿Está seguro de que desea limpiar el registro y el formulario?",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Sí",
      cancelButtonText = "No",
      callbackR = function(x) {
        if(x) {
          # Solo limpia si el usuario confirma
          rv(NULL)
          updateDateInput(session, "fecha", value = Sys.Date())
          updateTextInput(session, "odontologo", value = "")
          updateSelectInput(session, "curso", selected = 1)
          updateNumericInput(session, "ID", value = NA)
        }
      }
    )
  })
  
  output$download <- downloadHandler(
    filename = function() paste0(input$curso, "_", input$ID, "_icdas.csv"),
    content = function(file) write.csv(rv(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
