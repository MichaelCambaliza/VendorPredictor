library(tidyverse)
library(caret)
library(gbm)
library(reshape2)
library(dplyr)
library(shinythemes)
library(DT)
library(shiny)

model = readRDS("model.rds")
data = readRDS("Final.rds")
vendor_data = readRDS("vendor.rds")
Area1 = readRDS("Area1.rds")
Molecule1 = readRDS("Molecule1.rds")
Technology1 = readRDS("Technology1.rds")
Indication1 = readRDS("Indication1.rds")

ui =shiny::fluidPage(theme = shinytheme("superhero"),
                shiny::pageWithSidebar(
                    
                    shiny::headerPanel('Vendor Predictor'),
                    
                    shiny::sidebarPanel(
                        tags$label(h3('Input parameters')),
                        tags$style(type='text/css', ".selectize-input { font-size: 32px; line-height: 32px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
                        div(style = "font-size:20px;",
                            shiny::selectInput(inputId = "Area", label = div(style = "font-size:28px", "Select the Area"), 
                                        choices = Area1, selectize = TRUE
                            )
                        ),
                        div(style = "font-size:20px;",
                            shiny::selectInput(inputId = "Molecule", label = div(style = "font-size:28px", "Select the Molecule"), 
                                        choices = Molecule1, selectize = TRUE
                            )
                        ),
                        div(style = "font-size:20px;",
                            shiny::selectInput(inputId = "Technology", label = div(style = "font-size:28px", "Select the Technology"), 
                                        choices = Technology1, selectize = TRUE
                            )
                        ),
                        div(style = "font-size:20px;",
                            shiny::selectInput(inputId = "Indication", label = div(style = "font-size:28px", "Select the Indication"), 
                                        choices = Indication1, selectize = TRUE
                            )
                        ),
                        shiny::actionButton("submitbutton", "Submit", 
                                     class = "btn btn-primary",style='padding:5px; font-size:18px'),
                        width = 6
                    ),
                    
                    shiny::mainPanel(
                        tags$style(type='text/css', ".selectize-input { font-size: 28px; line-height: 28px;} .selectize-dropdown { font-size: 28px; line-height: 28px; }"),
                        shiny::verbatimTextOutput('contents'),
                        div(DT::dataTableOutput('tabledata'), style = "font-size:28px"),
                        width = 12
                    )
                    
                )
)


server = function(input, output, session) {
    
    datasetInput = shiny::reactive({  
        
        df = data.frame(
            Name = c("Area",
                     "Molecule",
                     "Indication",
                     "Technology"),
            Value = as.character(c(input$Area,
                                   input$Molecule,
                                   input$Indication,
                                   input$Technology)),stringsAsFactors = TRUE)
        
        Vendor = ""
        df = rbind(df, Vendor)
        input = t(df)
        write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
        test = read.csv(paste("input", ".csv", sep=""), header = TRUE)
        Output = round(predict(model,test,type="prob"),3)
        probs = reshape2::melt(Output,id.vars = NULL)
        colnames(probs) = c("Vendor", "Probability")
        rownames(probs) = NULL
        data3 = data[,c("Vendor","Status")]
        data4 = distinct(data3)
        probs = merge(x = probs, y = data4[,c("Vendor","Status")], by = "Vendor")
        probs = merge(probs, vendor_data, all = TRUE)
        probs[is.na(probs)] = "NA"
        probs = probs[1:152,]
        probs = probs[order(probs$Probability, decreasing = TRUE), ]
        probs$Probability[probs$Probability >= .25] = "High"
        probs$Probability[probs$Probability < .1] = "Low"
        probs$Probability[probs$Probability >= .1 & probs$Probability < .25] = "Medium"
        print(probs[1:4,], row.names = FALSE)
        
    })

    
    # Prediction results table
    output$tabledata = DT::renderDataTable({
        if (input$submitbutton>0) { 
            DT::datatable(datasetInput(),rownames = FALSE, filter="none", selection="none", escape=FALSE,
                      options = list(headerCallback = DT::JS(
                          "function(thead) {",
                          "  $(thead).css('font-size', '3em');
                          $(thead).closest('thead').find('th').eq(0).css('color', 'white');
      $(thead).closest('thead').find('th').eq(1).css('color', 'white');
      $(thead).closest('thead').find('th').eq(2).css('color', 'white');
      $(thead).closest('thead').find('th').eq(3).css('color', 'white');",
                          "}"
                      ),searching = FALSE,lengthChange = FALSE,pageLength = 4, dom = "t")) %>%
                formatStyle(names(datasetInput()),fontSize = '28px')
        } 
    }, width = "600px")
    
}

shiny::shinyApp(ui = ui, server = server)
