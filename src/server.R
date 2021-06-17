source("Functions_Project_Mutia.r")

shinyServer(
  function(input, output, session){
    
    answer <- reactiveValues(estimatedNum = 0, poly_function = "" , functionsTable = matrix(), 
                             simplexDemandNumToShip = matrix(c(1, 1, 1, 1, 1), nrow = 1, dimnames = list(1, c("Sacramento", "Salt Lake","Albuquerque", "Chicago", "New York"))),
                             simplexCost = matrix(c(1:15), nrow = 3, dimnames = list(c("Denver", "Phoenix", "Dallas"),
                             c("Sacramento", "Salt Lake","Albuquerque", "Chicago", "New York"))),
                             simplexSupply = matrix(c(5, 5, 5), nrow = 1, dimnames = list(1, c("Denver", "Phoenix", "Dallas"))),
                             simplexInitialT = matrix(),
                             simplexSolutionPerIter = matrix(),
                             simplexAnswer = 0,
                             
                             aM = matrix(),
                             aS = c(),
                             aD = c())
    
    
    output$chosenMethod <- renderText(input$methodToUse)
    
    observeEvent(input$solveButton, {
       if(input$methodToUse == 'Polynomial Regression'){
        results = PolynomialRegression(input$Order, input$NumberToEstimate1, input$file1$datapath)
        answer$poly_function = results$polynomial_function_string
        answer$estimatedNum = results$answer
       }else if(input$methodToUse == 'Quadratic Spline Interpolation'){
        results = QuadraticSpline(input$file2$datapath, input$NumberToEstimate2)
        answer$functionsTable = results$functionsMatrix
        answer$estimatedNum = results$answer
       }else if(input$methodToUse == 'Simplex'){

        answer$simplexCost = as.data.frame(hot_to_r(input$simplexCost))
        answer$simplexSupply = as.data.frame(hot_to_r(input$simplexSupply))
        answer$simplexDemandNumToShip = as.data.frame(hot_to_r(input$simplexDemandNumToShip))

        results = minSimplex(answer$simplexCost, answer$simplexSupply, answer$simplexDemandNumToShip)
        if(is.null(results)) answer$simplexAnswer = "No feasible Solution"
        else{
          answer$simplexInitialT = results$initTableau
          answer$simplexAnswer = results$answer
          answer$simplexSolutionPerIter = results$tableOfSolutions
        }
       }
    })
    
    output$simplexCost <- renderRHandsontable ({
      if(input$methodToUse == 'Simplex'){
        rhandsontable(answer$simplexCost, width = 600, height = 100 )
      }
    })
    
    output$simplexDemandNumToShip <- renderRHandsontable ({
      if(input$methodToUse == 'Simplex'){
        rhandsontable(answer$simplexDemandNumToShip, width = 600, height = 70)
      }
    })
    
    output$simplexSupply <- renderRHandsontable ({
      if(input$methodToUse == 'Simplex'){
        rhandsontable(answer$simplexSupply, width = 600, height = 70 )
      }
      
    })
    
    output$printTable <- renderTable({
      if(input$methodToUse == 'Quadratic Spline Interpolation'){
        answer$functionsTable
      }
    })
    
    output$printTableau <- renderTable({
      if(input$methodToUse == 'Simplex'){
        answer$simplexInitialT
      }
    })
    
    output$solutionsPerIterationTable <- renderTable({
      if(input$methodToUse == 'Simplex'){
        answer$simplexSolutionPerIter
      }
    })
    
    output$results <- renderText({
      if(input$methodToUse == 'Polynomial Regression'){
        paste("Function that will model the data: ","\n",answer$poly_function, "\n\n" ,"Estimated Number:", answer$estimatedNum)
      }else if(input$methodToUse == 'Quadratic Spline Interpolation'){
        paste("Estimated Number:", answer$estimatedNum, "\n\n ", "List of functions per iteration: ")
      }
     
    })
    
    output$inputNumToShipInst <- renderText({
      if(input$methodToUse == 'Simplex'){
        paste("Enter the number of products to ship from plant to warehouse: ")
      }
    })
    
    output$inputCostInst <- renderText({
      if(input$methodToUse == 'Simplex'){
        paste("Enter the shipping costs from plant to warehouse (Enter positive values): ")
      }
    })
    
    output$inputSupplyInst <- renderText({
      if(input$methodToUse == 'Simplex'){
        paste("Enter the amount of total supply from every plant (Enter positive values): ")
      }
    })
    
    output$initTableau <- renderText({
      if(input$methodToUse == 'Simplex'){
        paste("INITIAL TABLEAU ")
      }
    })


    output$solutionsPerIteration <- renderText({
      if(input$methodToUse == 'Simplex'){
        paste("SOLUTIONS PER ITERATION ")
      }
    })

    
    output$simplexAnswer <- renderText({
      if(input$methodToUse == 'Simplex'){
        paste("MINIMUM COST:  ", answer$simplexAnswer)
      }
    })

     
    
    
  }
  
)