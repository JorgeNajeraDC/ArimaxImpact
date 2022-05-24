
#' @title Perform impact evaluations for time series.
#' @description Using a web application in the format of a dashboard, it performs an impact assessment
#' on any time series using the ARIMAX model methodology in a simple way.
#' @export Dashboard_ARIMAX
#' @author Jorge Nájera
#' @examples
#' Dashboard_ARIMAX()

Dashboard_ARIMAX <- function() {
  # Main login screen
  loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                   wellPanel(
                     tags$h2("Iniciar Sesion", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Usuario", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                       style = "text-align: center;",
                       actionButton("login", "Iniciar Sesion", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                       shinyjs::hidden(
                         div(id = "nomatch",
                             tags$p("Oops! Incorrect username or password!",
                                    style = "color: red; font-weight: 600;
                                            padding-top: 5px;font-size:16px;",
                                    class = "text-center"))),
                       br(),
                       br()
                     ))
  )

  credentials = data.frame(
    username_id = c("userTS"),
    passod   = sapply(c("passTS"),password_store),
    permission  = c("advanced"),
    stringsAsFactors = F
  )

  header <- dashboardHeader( title = " ", uiOutput("logoutbtn"))

  sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
  body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
  ui<-dashboardPage(header, sidebar, body, skin = "blue")

  server <- function(input, output, session) {

    login = FALSE
    USER <- reactiveValues(login = login)

    observe({
      if (USER$login == FALSE) {
        if (!is.null(input$login)) {
          if (input$login > 0) {
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            if(length(which(credentials$username_id==Username))==1) {
              pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
              pasverify <- password_verify(pasmatch, Password)
              if(pasverify) {
                USER$login <- TRUE
              } else {
                shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          }
        }
      }
    })

    output$logoutbtn <- renderUI({
      req(USER$login)
      tags$li(a(icon("fa fa-sign-out"), "Cerrar Sesion",
                href="javascript:window.location.reload(true)"),
              class = "dropdown",
              style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })

    output$sidebarpanel <- renderUI({
      if (USER$login == TRUE ){
        sidebarMenu(
          h3("Panel de Control:"),
          fileInput("file1", label=h5("Para empezar sube el archivo en formato CSV con la serie de tiempo que se desea analizar:"),
                    accept = c(
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          prettySwitch("header", "Con encabezado:", TRUE),
          prettyRadioButtons("dist3",label = h5(div(style="text-align:justify","1.- Selecciona el tipo de Periodicidad de la serie:")),
                             list(
                               "Semanal" = 52,
                               "Mensual" = 12,
                               "Trimestral" =4),animation = "tada",
                             selected = 12),
          numericInput("num1", label = h5("2.- Escribe el anio de inicio de la serie:"), value = 2000),
          numericInput("num2", label = h5("3.- Escribe el Dia/Mes/Trimestre de inicio del periodo de la serie:"), value = 1),
          prettyCheckboxGroup("dist55",label =h5(div(style="text-align:justify","4.- Selecciona el tipo de variable que se ocupara para calcular los Outliers:")),
                              list(
                                "Additive outliers" = "AO",
                                "Level shifts" = "LS",
                                "Temporary changes" ="TC",
                                "Innovational outliers" ="IO"),animation = "tada",
                              selected = c("AO","LS","TC","IO")),
          br(),
          h5(div(style="text-align:center","5.- Generar cambios en la serie: ")),
          fluidRow(
            column(3, offset = 0,actionButton("descarga", label = "Cargar/Recargar cambios  ", style = "unite", color="primary")))

        )

      }
    })

    output$body <- renderUI({
      if (USER$login == TRUE ) {
        fluidRow(
          valueBoxOutput("value1")
          ,valueBoxOutput("value2")
          ,valueBoxOutput("value3"),
          box(width = 8,dygraphOutput("contents1",height = "400px")),
          box(width = 4,plotlyOutput("plotly",height = "400px")),
          column(width = 8,
                 box(width = 12,dygraphOutput("contents2",height = "150px")),
                 box(width = 12,dygraphOutput("contents3",height = "150px"))),
          box(width = 4, DT::dataTableOutput('results',height = "345px" ))
        )
      }
      else {
        loginpage
      }
    })



    observeEvent(input$descarga, {

      Outliers<- shiny::reactive({
        library(forecast)
        inFile <- isolate(input$file1)
        if (is.null(inFile))
          return(NULL)

        data<-read.csv(inFile$datapath)
        data<-ts(data, frequency = as.numeric(isolate(input$dist3)), start = c(isolate(input$num1), isolate(input$num2)))
        set.seed(666)
        lambda<- BoxCox.lambda(data)
        Outliers<-tso(BoxCox(data,
                             lambda=lambda),
                      types = c(isolate(input$dist55)))
      })





      output$results <-  DT::renderDataTable({
        Outliers<-Outliers()
        Outliers_Datos<-data.frame(Outliers[["outliers"]]) %>%
          rename(Tipo=type) %>%
          rename(Indice=ind)%>%
          rename(Periodo=time)%>%
          rename(Coeficiente=coefhat)%>%
          rename(Estadistico_T_student=tstat)
        datatable(Outliers_Datos, options = list(autoWidth = TRUE,
                                                 searching = FALSE,pageLength = 5,
                                                 initComplete = JS(
                                                   "function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                   "}")))
      })


      output$value1 <- renderValueBox({
        Outliers<-Outliers()

        BoxTest<- Box.test(Outliers[["fit"]][["residuals"]], lag=20, type="Ljung-Box")

        valueBox(
          round(BoxTest[["p.value"]],4)
          ,paste('Valor del Pvalue para Ljung-Box')
          ,icon = icon("stats",lib='glyphicon')
          ,color = ifelse(BoxTest[["p.value"]]<0.05,"red","blue"))
      })
      output$value2 <- renderValueBox({
        Outliers<-Outliers()

        ArchTest<- ArchTest(Outliers[["fit"]][["residuals"]], lags=12, demean = FALSE)
        valueBox(
          round(ArchTest[["p.value"]][["Chi-squared"]],4)
          ,'Valor del Pvalue para Arch-Test'
          ,icon = icon("bar-chart-o")
          ,color = ifelse(ArchTest[["p.value"]][["Chi-squared"]]<0.05,"red","blue"))
      })
      output$value3 <- renderValueBox({
        Outliers<-Outliers()

        JarqueBera<- jarque.bera.test(Outliers[["fit"]][["residuals"]])
        valueBox(
          round(JarqueBera[["p.value"]],4)
          ,paste('Valor del Pvalue para Jarque-Bera')
          ,icon = icon("table")
          ,color = ifelse(JarqueBera[["p.value"]]<0.05,"red","blue"))
      })



      output$contents1 <- renderDygraph({
        Outliers<-Outliers()

        library(forecast)
        inFile <- isolate(input$file1)
        if (is.null(inFile))
          return(NULL)

        data<-read.csv(inFile$datapath)
        data<-ts(data, frequency = as.numeric(isolate(input$dist3)), start = c(isolate(input$num1), isolate(input$num2)))
        set.seed(666)
        lambda<- BoxCox.lambda(data)

        TS<- shiny::reactive({
          if (input$dist3==52)
          {
            SerieOriginal<- InvBoxCox(Outliers[["y"]],lambda=lambda)
            SerieAjustado<- InvBoxCox(Outliers[["yadj"]],lambda=lambda)
            SeriesTime <- cbind(SerieOriginal, SerieAjustado)
            SerieEfectos<-ts(SeriesTime)
            SerieEfectos
          }
          else
          {
            SerieOriginal<- InvBoxCox(Outliers[["y"]],lambda=lambda)
            SerieAjustado<- InvBoxCox(Outliers[["yadj"]],lambda=lambda)
            SerieEfectos<- cbind(SerieOriginal, SerieAjustado)
            SerieEfectos
          }
        })
        SerieEfectos<- TS()

        dygraph(SerieEfectos, main = "Comparacion entre serie Original y Simulada.",group = "Resultados") %>%
          dySeries("SerieOriginal", color = "grey",strokePattern = "dashed") %>%
          dySeries("SerieAjustado", color = "blue") %>%
          dyOptions(fillGraph = TRUE, fillAlpha = 0.4)%>%
          dyHighlight(highlightCircleSize = 3,highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyCrosshair(direction = "vertical")
      })


      output$contents2 <- renderDygraph({
        Outliers<-Outliers()

        library(forecast)
        inFile <- isolate(input$file1)
        if (is.null(inFile))
          return(NULL)

        data<-read.csv(inFile$datapath)
        data<-ts(data, frequency = as.numeric(isolate(input$dist3)), start = c(isolate(input$num1), isolate(input$num2)))
        set.seed(666)
        lambda<- BoxCox.lambda(data)



        TS<- shiny::reactive({
          if (input$dist3==52)
          {
            SerieOriginal<- InvBoxCox(Outliers[["y"]],lambda=lambda)
            SerieAjustado<- InvBoxCox(Outliers[["yadj"]],lambda=lambda)
            SerieEfectos<- SerieOriginal-SerieAjustado
            SerieEfectos<-ts(SerieEfectos)
            SerieEfectos
          }
          else
          {
            SerieOriginal<- InvBoxCox(Outliers[["y"]],lambda=lambda)
            SerieAjustado<- InvBoxCox(Outliers[["yadj"]],lambda=lambda)
            SerieEfectos<- SerieOriginal-SerieAjustado
            SerieEfectos
          }
        })
        SerieEfectos<- TS()

        dygraph(SerieEfectos, main = "Diferencias entre la Serie Original y la Serie Simulada (con valores originales).", group = "Resultados") %>%
          dyBarChart()
      })


      output$contents3 <- renderDygraph({
        Outliers<-Outliers()

        library(forecast)
        inFile <- isolate(input$file1)
        if (is.null(inFile))
          return(NULL)

        data<-read.csv(inFile$datapath)
        data<-ts(data, frequency = as.numeric(isolate(input$dist3)), start = c(isolate(input$num1), isolate(input$num2)))
        set.seed(666)
        lambda<- BoxCox.lambda(data)

        TS<- shiny::reactive({
          if (input$dist3==52)
          {
            SerieOriginal<- InvBoxCox(Outliers[["y"]],lambda=lambda)
            SerieAjustado<- InvBoxCox(Outliers[["yadj"]],lambda=lambda)
            SerieEfectos<- Outliers[["effects"]]
            SerieEfectos<-ts(SerieEfectos)
            SerieEfectos
          }
          else
          {
            SerieOriginal<- InvBoxCox(Outliers[["y"]],lambda=lambda)
            SerieAjustado<- InvBoxCox(Outliers[["yadj"]],lambda=lambda)
            SerieEfectos<- Outliers[["effects"]]
            SerieEfectos
          }
        })
        SerieEfectos<- TS()

        dygraph(SerieEfectos, main = "Efecto Real en Conjuto de los Outliers en la Serie (con valores transformados).", group = "Resultados") %>%
          dySeries(color="red") %>%
          dyHighlight(highlightCircleSize = 3,highlightSeriesOpts = list(strokeWidth = 3)) %>%
          dyCrosshair(direction = "vertical")
      })

      output$plotly <- renderPlotly({
        Outliers<-Outliers()

        Residuos<- data.frame(Residuales=Outliers[["fit"]][["residuals"]])
        p <- ggplot(Residuos, aes(sample = Residuales)) +
          stat_qq_line(color="black", size=1.5) +
          stat_qq(color="red",alpha=0.4) +
          labs(title="Q-Q plot de los residuos del modelo generado.",
               x=" ",y= " ")+
          theme_linedraw()

        ggplotly(p)

      })

    })
  }

  shinyApp(ui, server)
}
