library(shiny)
library(rhandsontable)

shinyServer(
  pageWithSidebar(
    headerPanel("CMSC 150 Project"),
    
    sidebarPanel(
      selectInput("methodToUse", "Please Select the Method to Use", 
                  choices = c("Polynomial Regression", "Quadratic Spline Interpolation", "Simplex")),
      conditionalPanel(condition = "input.methodToUse == 'Polynomial Regression'", 
                       fileInput("file1", "Choose CSV File", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                       numericInput("Order", "Please Enter the Order of the Polynomial", 0),
                       numericInput("NumberToEstimate1", "Please Enter the Number That You Want to Estimate", 0)),
                   
      conditionalPanel(condition = "input.methodToUse == 'Quadratic Spline Interpolation'", 
                       fileInput("file2", "Choose CSV File", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                       numericInput("NumberToEstimate2", "Please Enter the Number That You Want to Estimate", 0)),
      
      conditionalPanel(condition = "input.methodToUse == 'Simplex'", 
                       checkboxGroupInput("checkedOptions", "Options", choices = c("Show Initial Tableau", "Show Values Per Iteration"), selected = c("Show Initial Tableau", "Show Values Per Iteration"))),
      
      actionButton("solveButton", "SOLVE")
    
      ),
    
    mainPanel(

        verbatimTextOutput("chosenMethod"),
        verbatimTextOutput("results"),
        
        conditionalPanel(condition = "input.methodToUse == 'Quadratic Spline Interpolation'", 
                         tableOutput("printTable")),
        conditionalPanel(condition = "input.methodToUse == 'Simplex'",
                         verbatimTextOutput("inputNumToShipInst"),
                         rHandsontableOutput("simplexDemandNumToShip"),
                         verbatimTextOutput("inputCostInst"),
                         rHandsontableOutput("simplexCost"),
                         verbatimTextOutput("inputSupplyInst"),
                         rHandsontableOutput("simplexSupply"),
                         verbatimTextOutput("initTableau"),
                         tableOutput("printTableau"),
                         verbatimTextOutput("solutionsPerIteration"),
                         tableOutput("solutionsPerIterationTable"),
                         verbatimTextOutput("simplexAnswer"))
      
    )
  )
)