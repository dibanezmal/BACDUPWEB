# carga de librerias


library(shiny)
library(DT)
library(rapportools)
library(BioCircos)
library(colorspace)
library(dplyr)




# carga de librerias externas creadas por equipo bacdup Jose Francisco Sanchez y alumni Alba Garces para Circos

#source("./www/ext_funcions.R")
source("ext_funcions.R")


# Declaracion de constantes iniciales

usr_settings <- c(80,3,1,1,1,0) # valores por defecto de los filtros
hebras <- c("pos","neg","both") # definir las tres opciones de hebras para el selector
v_campos <- c(2,5,8,9,10,15,16) # campos que se mostraran por defecto en la tabla



# Las aplicaciones shiny son aplicaciones web que lo que hacen es facilitar la crreacion de codigo html para la parte de 
# interfaz de usuario sin conocer html. Eso no quiere decir que no se pueda luego enriquecer mediante codigo html y css, incluso javascript.

# Shiny esta organizada en 3 partes:
#       UI: parte interfaz de usuario, lo que vemos, llamada UI,esta parte se divide en dos:
#             UI inputs: bloque de elementos para interactuar, variables que modificamos.
#             UI outputs: bloque de resultados en pantalla.
#       SERVER: parte de manipulacion de datos en servidor que prepara los datos para su visualizacion.
#             SERVER inputs: Parte que permite ampliar o modificar los inputs de UI en funcion de condiciones
#             SERVER outputs: Parte que permite preparar los datos para las salidas en pantalla
#             SERVER data: Parte que sirve de creacion de variables que se usan en distintos outputs o inputs
#       RUNAPP: Shinyapp es la rutina que lanza la aplicacion uniendo UI con SERVER y que dispone de un localhost para ver los resultados


## Bloque UI (User Interface) que engloba inputs y outputs, entrada de datos o acciones y presentacion de datos y graficos


ui <- fluidPage(


    img(src = "header_bdw.jpg", height = 240, width = 1800), # carga imagen de cabecera de la aplicacion y estilos JS
    
    tags$style(HTML(".js-irs-0 .irs-single , .js-irs-0  .irs-bar-edge , .js-irs-0 .irs-bar {background: #66CDAA}")),
    
    

    
## Bloque UI: Inputs en la parte izquierda de la pantalla
    

    br(),
    br(),

    sidebarLayout(
        
        sidebarPanel(

#           Inputs que aparecen por defecto en la franja lateral izquierda:          
          
            tags$head(tags$style(".progress-bar{background-color:#66CDAA;}")),  
          

            fileInput("file_dup", "Choose annot_dup file:", # selector de archivo de duplicados dup_annot
                      multiple = FALSE,
                     accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            tags$hr(),
            
            fileInput("file_len", "Choose length file:", # selector de archivo de plasmidos length
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            tags$hr(),

            width=2,
            
            actionButton("go", "Go"), # boton accionador de la aplicacion con todos los apartados y filtros


#           Los inputs a continuacion aparecen una vez se ha pulsado el boton de go.
#           Parte de los inputs aparecen en funcion del archivo cargado, en concreto plasmidos y n_dups
#           El resto de selectores tienen unos valores por defecto marcados en la cabecera del archivo 
            
 
           
            conditionalPanel(
                
                
                condition = "input['go']", # accion que activa el panel condicional siguiente
              
                br(),
                tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                
                br(),
                uiOutput("plasmidos"), # selector de plasmidos
            
                br(),
                uiOutput("n_dups"), # selector de rango de numero de duplicados
            
                br(),
                selectInput("strand","select strand",hebras,hebras[usr_settings[2]]), # selector de hebra
 
               
                checkboxInput("pseudo","include pseudogenes",usr_settings[3]), # seleccionar si se incluyen pseudogenes
            
                checkboxInput("mobile","include mobile elements",usr_settings[4]), # seleccionar sis e incluyen fagos
                
                
                
                br(),
                tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"),
                br(),
                checkboxInput("vista","compact view",usr_settings[5]), # seleccionar vista reducida segun campos seleccionados o todos           
                br(),
                uiOutput("n_campos") # selector de campos de la tabla
                
            
                
            ) # cierre conditional panel
            
#            checkboxInput("v_circos","mostrar circos?",usr_settings[6]),
        
        ), # cierre sidebar panel
        
        

## BLOQUE OUTPUTS de UI construido sobre el bloque mainpanel

        

        mainPanel(
            
            tags$br(),
            tags$br(),

            # Carga de la parte superior de la aplicacion sobre menu con tablas, datos y links            
                        
            fluidRow( column(width = 5,
                             align = "center",
                             tags$style("table { border-radius: 15px; color: black; background:#66CDAA; font-size: 25px}"),
                             p(style={"color:black;font-size:30px"},"Original Data"), 
                             tableOutput("t_values")), # tabla de totales
                      
                      column(width = 5,
                             align = "center",
                             tags$style("table { border-radius: 15px; color: black; background:#66CDAA; font-size: 25px}"),
                             p(style={"color:black;font-size:30px"},"Filtered Data"), 
                             tableOutput("f_values")), # tabla de datos filtrados
 
                      column(width = 2,
                             align = "center",
                             
                             br(),
                             textOutput("g_link2"),
                             h2(p("LINK NCBI: "),htmlOutput("g_link"),
                             style='padding:8px; font-size:150%; color:white; background:#FF7256'), # link a ncbi
                                              
                             br(),
                             h2(downloadButton("Report","REPORT",
                                              style='padding:4px; font-size:100%; color:white; background:#FF7256'))) # donwload report
                      ),
            
  
            # A partir de aqui la parte principal de la aplicacion con los 4 apartados principales
            # se trata de TABLA DINAMICA, GRAFICO CIRCOS, TABLA DE PLASMIDOS y DATOS GENES DUPLICADOS          
           
            

            tags$br(),
            tags$br(),
            

            tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray"), # linea separadora con CSS

            tags$style("table { border-radius: 15px; color: black; background:#66CDAA; font-size: 20px}"), # estilos tablas CSS
  
            navbarPage(
                
                tags$style(HTML(" 
                        .navbar { background-color: black;}
                        .navbar-default .navbar-nav > li > a {color:white;}
                        .navbar-default .navbar-nav > .active > a,
                        .navbar-default .navbar-nav > .active > a:focus,
                        .navbar-default .navbar-nav > .active > a:hover {color: #66CDAA;background-color: white;}
                        .navbar-default .navbar-nav > li > a:hover {color: black;background-color:#66CDAA}
                        .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: #66CDAA;}
                        .navbar-default .navbar-nav > li > a {font-size:2em ;padding:20px 20px}
                        ")), # estilos JS para la barra de menus
                       
                tabPanel("Table", # apartado tabla dinamica que reacciona segun los filtros
                         
                         tags$br(),
                         DT::dataTableOutput("main_table")
                         ),
                
                tabPanel("Circos", # apartado del grafico circos que llama a la funcion externa funcion_ext.R
                        
                         BioCircosOutput("circos",height="700px", width="80%")
                         ),
                
                tabPanel("Plasmids", # apartado que captura informacion de la cepa en el repositorio github de BacDup y muestra tabla plasmidos
                         br(),
                         h1("Taxonomy and Chromosome table",align="center"),
                         br(),
                         fluidRow( column( width = 6,align="center", h3(tableOutput("taula_info"))),
                                   column( width = 6,align="center", h3(tableOutput("table_plasmids")))),
                         ),
                
                tabPanel("Data", # apartado datos de duplicados y grafico de distribucion con barplot

                         br(),
                         br(),
                         fluidRow( column( width = 5,align = "center", tableOutput("plot_dups3")),
                                   column( width = 7,align = "center",  p(style={"color:black;font-size:30px"},"Distribution of duplicate genes"), plotOutput("plot_dups1"))
                                   )
                         ) # cierre cuarto tabpanel
                         
                
            ) # cierre navbarpage
        
        ) # cierre mainpanel
    
    ) # cierre de sidebarlayout

)# cierre fluidPage y bloque UI USER INTERFACE




# =================================================================================================================
# =================================================================================================================
# =================================================================================================================




# Segundo bloque de una aplicacion Shiny, el bloque server. Vamos a organizarlo en 2 partes.
# Primera parte para declaracion de funciones generando variables que se van a usar en distintos outputs.
# Segunda parte con todos los datos de los outputs que vamos a pasar al bloque UI para ser renderizados en pantalla



server <- function(input, output) {

  
  
## PRIMERA PARTE SERVER DECLARACION DE FUNCIONES Y VARIABLES ## =================================================== 


    # Carga de los datos del archivo de duplicados en un dataframe eliminando datos NA
  
    dades_dup <- eventReactive(input$go, {
        validate(
            need(input$file_dup, "No files selected!!!")
        )
    
        inFile <- input$file_dup
    
        nom_gen <<- substr(inFile$name,1,13)
    
        df_aux <- read.csv(inFile$datapath, header = TRUE, sep = ",", quote = "")
    
        df_aux[is.na(df_aux)] <- ""
    
        df_aux
    })

  
    # Carga de los datos del archivo de plasmidos en un dataframe
  
    dades_len <- eventReactive(input$go, {
        validate(
            need(input$file_len, "No files selected!!!")
        )
    
        inFile <- input$file_len
    
        read.csv(inFile$datapath, header = FALSE, sep = ",", quote = "", row.names=1)
    
    })

    
    # Carga de los datos del repositorio BacDup en GitHub en un dataframe
    
    dades_info <- eventReactive(input$go, {
    
    
        file_info <- paste0("https://raw.githubusercontent.com/JFsanchezherrero/BacDup/main/developer/data_output/data/",nom_gen)
    
        file_info <- paste0(file_info,".1/input/info.csv")
    
        read.csv(file_info, header = FALSE, sep = ";")
    
    })
  

    # Creacion de la tabla de datos filtrada en tiempo real aplicando los filtros 
  
    df_filtered <- reactive({
    
        df_complete <- dades_dup()
        v_pseudo <- if(input$pseudo==1){c(TRUE,FALSE)}else{c(TRUE)}
        v_mobile <- if(input$mobile==1){c(TRUE,FALSE)}else{c(TRUE)}
        v_dupmin <- as.integer(input$slider_dups[1])
        v_dupmax <- as.integer(input$slider_dups[2])
    
        v_seq    <- as.vector(input$g_bacteris)
        v_strand <- if(input$strand=="both"){c("pos","neg")}else{input$strand}
    
    
    
        df_aux <- subset(df_complete,df_complete$strand %in% v_strand   & 
                       df_complete$count_dups >= v_dupmin &
                       df_complete$count_dups <= v_dupmax &
                       df_complete$rec_id %in% v_seq      &
                       is.empty(df_complete$dup_id_pseudo_free) %in% v_pseudo &
                       is.empty(df_complete$dup_id_mobile_free) %in% v_mobile)
        return(df_aux)
    
    })

      
    # funcion que construye un dataframe con los principales datos de los plasmidos 
    
    df_plasmids <- reactive({
      
      l_aux <- c()
      df_seq <- dades_len()
      
      df_aux <- cbind(rownames(df_seq),df_seq)
      nlen <- nrow(df_aux)
      nmax <- max(df_aux[2])
      for (i in 1:nlen){
        l_aux[i] <- "Plasmid"
        if (df_aux[i,2] == nmax){
          l_aux[i] <- "Chromosome"
        }
        i<i+1
      }
      
      df_aux <- cbind(df_aux,l_aux)
      colnames(df_aux) <- c("Id sequence","Size","plasmid/Chr")
      df_seq <- df_aux[order(-df_aux$Size),]
      
    }) 
    
 
    # funcion que construye una tabla con los principales datos del dataset original  
    
    t_values <- reactive({
      
      df_complete <- dades_dup()
      
      data.frame(
        Name = c("Records :","max dups  :","min_dups  :"),
        Value = as.character( c(nrow(df_complete),max(df_complete$count_dups),min(df_complete$count_dups)) ),
        stringsAsFactors = FALSE)
      
    })
    
    
    # funcion que extrae los datos principales del dataset filtrado
    
    f_values <- reactive({
      
      df_aux <- df_filtered()
      
      data.frame(
        Name = c("Records :","max dups  :","min_dups  :"),
        Value = as.character( c(nrow(df_aux),max(df_aux$count_dups),min(df_aux$count_dups)) ),
        stringsAsFactors = FALSE)
      
    })
    
 
    # funcion que genera el link de ncbi para consulta de datos externos    
    
    gen_link <- eventReactive(input$go,{
      n_gen <- a(nom_gen,  target="_blank", href =  paste0("https://www.ncbi.nlm.nih.gov/data-hub/genome/",nom_gen))                  
    })
    
    
    
    
    
    
    
       
  ## SEGUNDA PARTE SERVER: DATOS OUTPUT para pasar a UI ## ==========================================================
  
 
 
    
          
    # ==================  OUTPUT GENERAL SUPERIOR CON TABLAS Y LINKS =================================================
    
    
    # datos de salida de totales de tabla original   
    
    output$t_values <- renderTable({
      
      tags$style("table { border-radius: 15px; color: black; background:#FF7F50; font-size: 25px}")
      t_values()
    })
    
    
    # datos de salida de totales de tabla filtrada
    
    output$f_values <- renderTable({
      
      f_values()
    })
    
    
    
  
    
    # ========== OUTPUT TABPANEL 1: Datos de la tabla principal pasada por los filtros y con el selector de campos ====
  
    output$main_table <- DT::renderDataTable(options = list(pageLength=25),{       
    
        v_columns <- if(input$vista==1){input$v_campos2}else{c(1:20)} 
        df_aux <- df_filtered()
        df_aux <- df_aux[v_columns]
    
    })
  
    
    
    
  
    # ============ OUTPUT TABPANEL 2: Grafico circos pasado por los filtros ============================================
  
    output$circos <- renderBioCircos({
    
        df_aux <- df_filtered()
        df_plasmid <- dades_len()
    
    
        bed_info_file <- df_aux
        bed_info_file <- bed_info_file[,c("dup_id", "rec_id", "start", "end", "locus_tag", "product", "strand")]
        
        # # keep only real duplicated groups
        bed_info_file <- bed_info_file %>% group_by(dup_id) %>% filter(n() >1)
         
        return(create_BioCircos(seq_lengths = df_plasmid,bed_info_file = bed_info_file)) 
    
    })
  

    
    
    
    # ============== OUTPUT TAB PANEL 3: taxonomia leida en Github y datos plasmidos ===================================    
  
  
    # funcion que lee el archivo raw de BacDup sobre taxonomia y extrae los datos directos del txt
  
    output$taula_info <- renderTable({
    
        df_bacinfo <- dades_info()
    
        v_taxo <- df_bacinfo[4,1]
        v_taxo <- c(v_taxo, df_bacinfo[5,])
        df_taxo <- cbind(v_taxo)
    
        colnames(df_taxo) = c("Taxonomy")
        df_taxo[1,1] <- substring(df_taxo[1,1],9)
        df_taxo[2,1] <- substring(df_taxo[2,1],10)
        df_taxo
 
    })  
  
 
    # datos de salida de secuencias plasmidos y cromosoma   
    
    output$table_plasmids <- renderTable({
      
      df_plasmids()
    })
    
    
    # datos del link de salida a ncbi  
    
    output$g_link <- renderUI({
      
      gen_link()
    })
    
  
    
     
    
    # =============== OUTPUT TAB PANEL 4: Tabla frecuencial de duplicados y grafico barplot =============================    
    
    
    # generacion de tabla de duplicados segun frecuencia 
    
    output$plot_dups3 <- renderTable({
      l_aux <- c()
      df_aux <- df_filtered()
      df_aux2 <- table(df_aux$count_dups)
      df_aux2 <- data.frame(df_aux2)
      v_dups <- levels(df_aux2[,1])
      v_dups <- as.integer(v_dups)
      
      for (i in 1:nrow(df_aux2)){
        l_aux[i] <- (as.integer(df_aux2[i,2])/v_dups[i])
        i <- i +1
      }
      df_aux2 <- cbind(df_aux2,as.integer(l_aux))
      colnames(df_aux2) <- c("N_Dups","Total","Real Dups")
      df_aux2
    })

        
    # paso de grafico de duplicados
    
    output$plot_dups1 <- renderPlot({
      df_aux <- df_filtered()
      barplot(table(df_aux$count_dups), col="aquamarine")
    })        
    
    
    
    
    
    
    
    
    # PARTE SERVER PARA COMPLETAR EL SIDEBARPANEL CON DATOS DE LOS ARCHIVOS CARGADOS =========================================
    
    
    
    # Esta parte es la relacion de plasmidos que sirven a su vez de filtros en el sidebar mediante check box
    
    output$plasmidos <- renderUI({
      
        df_aux <- dades_len()
        v_seq <- unique(rownames(df_aux))
        checkboxGroupInput("g_bacteris","Select Sequence / plasmid:",v_seq,selected=v_seq)
      
    })    

    
    # Esta parte construye un slideInput clasico de shiny pero con los extremos min / max definidos por los datos que se cargan
    
    output$n_dups <- renderUI({
      
      df_complete <- dades_dup()
      
      min_dup <- min(as.integer(df_complete$count_dups))
      max_dup <- max(as.integer(df_complete$count_dups))
      
      sliderInput("slider_dups", label = "n_dups Range:", min = min_dup, 
                  max = max_dup, value = c(min_dup, max_dup), step = 1)
      
    })   
    
    
    # Aqui se van seleccionando los campos de la tabla en funcion de un bloque checkboxgroup
     
    output$n_campos <- renderUI({
    
    
        df_aux <- dades_dup()
        v_aux <- colnames(df_aux)
        checkboxGroupInput("v_campos2","Select Fields:",v_aux,selected=v_aux[v_campos])
  
    })
  
  
} # fin del bloque server


#=====================================================================================================================
#=====================================================================================================================
#=====================================================================================================================


# Lanzamos la aplicacion final montando UI con SERVER 

shinyApp(ui = ui, server = server)

# si queremos trabajar directamente en html, podemos cargar una pagina index.html en el bloque ui con la sintaxis inferior
# shinyApp(ui = htmlTemplate("./www/index.html"), server)
# esto anula el ui anterior, es decir, o trabajamos con un UI programado en shiny o trabajamos con un ui externo en formato html

