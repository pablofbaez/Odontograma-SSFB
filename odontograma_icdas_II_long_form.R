# app.R: Registro Odontograma ICDAS II con rhandsontable (versión con botón para desmarcar permanentes)

# Verificar e instalar automáticamente las librerías necesarias

for (pkg in c("shiny","rhandsontable","shinyalert","bslib")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Datos constantes
todos_dientes <- c(
  permanentes <- as.character(c(11:18,21:28,31:38,41:48)),
  temporales  <- as.character(c(51:55,61:65,71:75,81:85))
)
permanentes <- todos_dientes[1:length(permanentes)]
temporales  <- todos_dientes[(length(permanentes)+1):length(todos_dientes)]

opts_resta <-  c(
  "R0","R1","R2","R3",
  "R4","R5","R6","R7","R8"
)
# "R0: Sin rest","R1: Sell mal","R2: Sell bien","R3: Obs color",
# "R4: Amalg","R5: Corona ac","R6: Porcelana","R7: Perd/fract","R8: Temp"

opts_caries <- c(
  "C0","C1","C2","C3",
  "C4","C5","C6"
)

#"C0: Sin car","C1: Camb vis I","C2: Camb vis II","C3: Rupt esm",
#  "C4: Sombra sub","C5: Cav detect","C6: Cav ext"

opts_estado <- c(
  "NA","0: Sano","91: Impl car","96: No rev","97: Pérd car","98: Pérd no car","99: No erup"
)
opts_act   <- c("NA","Activa","Detenida")
# opts_rad   <- c("NA","No cavitada","Cavitada")
tooth_columns <- c("M","O","D","V","P","R")

make_base_df <- function(fecha, odontologo, curso, ID) {
  df <- expand.grid(tooth = todos_dientes, stringsAsFactors = FALSE)
  df$fecha      <- fecha
  df$odontologo <- odontologo
  df$curso      <- curso
  df$ID         <- ID
  df$Presente   <- df$tooth %in% temporales
  df$EstadoGeneral <- "NA"
  for(s in tooth_columns) {
    df[[paste0("Rest_", s)]] <- "R0"
    df[[paste0("Car_",  s)]] <- "C0"
  }
  df$ActividadCaries <- "NA"
  # df$CariesRadicular <- "NA"
  df$Notas <- ""
  df[, c("fecha","odontologo","curso","ID","tooth","Presente","EstadoGeneral",
         unlist(lapply(tooth_columns, function(s) paste0("Rest_",s))),
         unlist(lapply(tooth_columns, function(s) paste0("Car_",s))),"Notas")] #,
        # "ActividadCaries","CariesRadicular","Notas")]
}

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyalert(),
  tags$head(tags$style(HTML("
    #top-panel {
      position: fixed; top: 0; left: 0; right: 0;
      background: #f8f9fa; padding: 10px; z-index: 1000;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    #main-content { margin-top: 110px; }
  "))),
  
  # panel superior fijo
  div(id = "top-panel",
      fluidRow(
        column(3, dateInput("fecha", "Fecha", value = Sys.Date())),
        column(3, textInput("odontologo", "Odontólogo", "")),
        column(1, selectInput("curso", "Curso", choices = 1:3, selected = 1)),
        column(2, numericInput("ID", "ID Estudiante", value = 1, min = 1)),
        column(1, actionButton("gen_table", "", icon = icon("table"))),
        column(1, downloadButton("download", "")),
        column(1, actionButton("reset_all","", icon = icon("eraser")))
        
      ),
  ),
  
  # contenido principal con scroll interno (tabla fija encabezado)
  div(id = "main-content",
      rHandsontableOutput("hot_table", height = "600px")
  )
)

server <- function(input, output, session) {
  rv <- reactiveVal(NULL)
  hidden_cols <- c("fecha","odontologo","curso","ID","tooth")
  
  observeEvent(input$gen_table, {
    req(input$odontologo, input$ID)
    base <- make_base_df(
      fecha      = as.character(input$fecha),
      odontologo = input$odontologo,
      curso      = input$curso,
      ID         = input$ID
    )
    rv(base)
  })
  
  output$hot_table <- renderRHandsontable({
    df_full <- rv(); req(df_full)
    # Mostrar sólo las columnas editables:
    df_disp <- df_full[, setdiff(names(df_full), hidden_cols)]
    rownames(df_disp) <- df_full$tooth
    rhandsontable(df_disp, stretchH = "all", height = 450) %>%
      hot_col("Presente",      type = "checkbox") %>%
      hot_col("EstadoGeneral", type = "dropdown", source = opts_estado) %>%
      { for(s in tooth_columns) {
        . <- hot_col(., paste0("Rest_",s), type = "dropdown", source = opts_resta)
        . <- hot_col(., paste0("Car_",s),  type = "dropdown", source = opts_caries)
        . 
      }; .
      } %>%
      hot_col("ActividadCaries", type = "dropdown", source = opts_act)#  %>%
      # hot_col("CariesRadicular", type = "dropdown", source = opts_rad)
  })
  
  observeEvent(input$hot_table, {
    new_disp <- hot_to_r(input$hot_table)
    full <- rv()
    # Sobrescribimos en full sólo las columnas que vinieron de la tabla
    cols_edit <- intersect(names(new_disp), names(full))
    full[cols_edit] <- new_disp[cols_edit]
    rv(full)
  })
  
  observeEvent(input$reset_all, {
    shinyalert(
      title = "Confirmación",
      text  = "¿Desea limpiar registro y formulario?",
      type  = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Sí", cancelButtonText = "No",
      callbackR = function(x) if (x) {
        rv(NULL)
        updateDateInput(session, "fecha", value = Sys.Date())
        updateTextInput(session, "odontologo", value = "")
        updateSelectInput(session, "curso", selected = 1)
        updateNumericInput(session, "ID", value = 1)
      }
    )
  })
  
  output$download <- downloadHandler(
    filename = function() paste0(input$curso, "_", input$ID, "_icdas.csv"),
    content = function(file) {
      write.csv(rv(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
