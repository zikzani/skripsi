library(shiny)
library(shinythemes)
shinyUI(fluidPage(
  theme=shinytheme("lumen"),
  titlePanel(title="FINAL"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var1","select period",choices = c("triwulan1"=1,"triwulan2"=2,"triwulan3"=3,"triwulan4"=4)),
      sliderInput("var2","select Range ",min = 1,max = 10,value = 5),
   radioButtons("var3","select Association",choices = c("2","3","4"),inline = TRUE),
   radioButtons("type","format type:",
                   choices = c("excel(CSV)","Text(TSV)","Text","Docx")),
   downloadButton("down1", "Download"),
   h5(textOutput("Kunjungan"))
    ),
      mainPanel(
        (navbarPage(title="",selected = "Barang Terlaris",position = "fixed-top",
          tabPanel("Barang Terlaris",withSpinner(plotOutput("myhist")),tableOutput('table1')) ,
           tabPanel("Association Rule",withSpinner(plotOutput("plot1")),tableOutput("table2")),
          tabPanel("Pendapatan",withSpinner(plotOutput("pdp")),tableOutput("table4")),
           tabPanel("Golongan",withSpinner(plotOutput("gol")),tableOutput("table3"))
      
                    )
        )
      )
))
)