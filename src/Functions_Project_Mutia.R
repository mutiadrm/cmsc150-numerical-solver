######################################################################################################################
#                                             Denise Reign M. Mutia                                                  #
#                                                   2018-01345                                                       #
#                                                CMSC 150    B-4L                                                     #
######################################################################################################################



######################################################################################################################
#                                               GAUSS-JORDAN                                                         #
######################################################################################################################
GaussJordan <- function(system){
  result1 = system
  for (i in 1:nrow(result1$augcoeffmatrix)){
    pivot_row = result1$augcoeffmatrix[i,]
    j=i
    pivot_element = pivot_row[[j]]
    
    a = 0
    #looks for the largest value in the column and assigns it as pivot element
    for(k in i:nrow(result1$augcoeffmatrix)){
      a = max(abs(a), abs(result1$augcoeffmatrix[[k,j]]))
    }
    
    if(a != pivot_element){ #current pivot element is not the largest in the column. pivoting will be done
      for(k in i:nrow(result1$augcoeffmatrix)){
        if(abs(result1$augcoeffmatrix[[k,j]]) == a){
          # SWAP 2 ROWS
          temp_vector = result1$augcoeffmatrix[i,]
          result1$augcoeffmatrix[i,] = result1$augcoeffmatrix[k,]
          result1$augcoeffmatrix[k, ] = temp_vector
          break;
        }
      }
    }
    
    #Normalization
    pivot_element = result1$augcoeffmatrix[i,i]#pivot element is the element in current row that is in the main diagonal
    result1$augcoeffmatrix[i, ] = result1$augcoeffmatrix[i, ]/pivot_element #divide values in row by the pivot element so that the PE will become 1
    
    for (m in 1:nrow(result1$augcoeffmatrix)){
      if(m>nrow(result1$augcoeffmatrix)){
        break;
      }
      
      if(m!=i){
        if(result1$augcoeffmatrix[m,i] != 0){
          pivot_row = c(result1$augcoeffmatrix[i,])
          
          pivot_element = result1$augcoeffmatrix[i,i]#pivot element is the element in current row that is in the main diagonal
          
          ve = result1$augcoeffmatrix[m,i] #assigns the values under the pivot element as values to be eliminated
          
          multiplier = ve/pivot_element
          
          vector = result1$augcoeffmatrix[i,]*multiplier #multiplies the values in pivot row with multiplier
          
          result1$augcoeffmatrix[m,] =  result1$augcoeffmatrix[m,] - vector #subtracts values of vector from the values in the row where 'value to be eliminated' is located 
        }
      }
    }
  }
  
  solutionV = c()
  #append all the values in the RHS column of the matrix to vector of solutions
  for(i in 1:nrow(result1$augcoeffmatrix)){
    solutionV = c(solutionV, result1$augcoeffmatrix[i, ncol(result1$augcoeffmatrix)])
  }
  return (solutionV)
}


######################################################################################################################
#                                             POLYNOMIAL  REGRESSION                                                 #
######################################################################################################################
PolynomialRegression<-function(inputNumber, numberToEstimate, inputCSV){
  #getwd() #get working directory
  inputList <- read.csv(inputCSV, FALSE, ",") # reads the csv file and transforms it into a data frame
  #inputList = inputCSV
  print(inputList)
  # assigns the first vector from the csv as x values and the second vector as y values
  x = inputList[[1]]
  y = inputList[[2]]
  variables = c(x, y)
  
   # CREATE MATRIX
  if(inputNumber<1){
    return(NA)
  }else{
  
    augcoeffmatrix = matrix(nrow =(inputNumber+ 1), ncol = (inputNumber + 2),  byrow = T) # creates an empty matrix
  
    augcoeffmatrix[1, 1] = length(x)
    augcoeffmatrix[1, ncol(augcoeffmatrix)] = sum(y)
  
  
    for(i in 2:nrow(augcoeffmatrix)){
  
      # values in the first row
      for(j in 2: ncol(augcoeffmatrix)-1){
        newCoeff = sum(x^(j-1))
        augcoeffmatrix[1, j] = newCoeff
      }
      # degree of x increases as it moves in every row/column
      for(j in 1:(ncol(augcoeffmatrix)-1)){
        newCoeff = sum(x^((i-1) + (j-1)))
        augcoeffmatrix[i, j] = newCoeff
     }
  
      # values in the last columm
      newCoeff = sum((x^(i-1))*y)
     augcoeffmatrix[i,ncol(augcoeffmatrix)] = newCoeff
    }
  }
  
  system = list("augcoeffmatrix" = augcoeffmatrix, "variables" = variables)
  unknowns = GaussJordan(system) # calls the GaussJordan function to solve for the unknowns
  
  stringV = c("function(x)")
  for(i in 1:length(unknowns)){ # appends new coefficients multiplied by their variables to the function
    a = as.character(unknowns[i])
    stringV = c(stringV, a)
    if(i >=2){
      stringV = c(stringV, "*x^", as.character(i-1))
    }
    if(i < length(unknowns)){ # + is only appended while the term is not in the end of the function
      stringV = c(stringV, "+")
    }
  }
  
  polynomial_string = paste(stringV, collapse = " ")
  polynomial_function = eval(parse(text = polynomial_string)) # converts the string into an R function
  
  answer = polynomial_function(numberToEstimate)
  # groups all the answers in a single list and returns it to the user
  result = list("polynomial_function_string" = polynomial_string, "answer" = answer)
  print(result)
  return(result)
  
}

######################################################################################################################
#                                               QUADRATIC SPLINE                                                     #
######################################################################################################################
QuadraticSpline <- function(inputCSV, numberToEstimate){
  inputList <- read.csv(inputCSV, FALSE, ",") # reads the csv file and transforms it into a data frame
  
 
  # assigns the first vector from the csv as x values and the second vector as y values
  x = inputList[[1]]
  y = inputList[[2]]

  splineV = c()
  splineMatrix = matrix(c(0), nrow = 3*(length(x)-1), ncol = (3*(length(x)-1))+1) # creates an empty matrix for 3*n-1 equations
  listOfFunctions = list()
  #print(splineMatrix)
  
  # sort the data points in an increasing order
  for(i in 1:length(x)){
    for (j in 1:length(x)){
      if (x[i] < x[j]){
        tempx = x[i]
        x[i] = x[j]
        x[j] = tempx
        
        tempy = y[i]
        y[i] = y[j]
        y[j] = tempy
      }
    }
  }

  ######################################## SET UP THE MATRIX ##########################################
  i = 0
  # ax^2 + bx + c = 0
  for (counter in 1:(length(x)-1)){
    i = (2*(counter-1))+1
    j = (3*(counter-1))+1 

    splineMatrix[i,j] = (x[counter])^2
    splineMatrix[i,j+1] = x[counter]
    splineMatrix[i,j+2] = 1
    splineMatrix[i, ncol(splineMatrix)] = y[counter]
    
    splineMatrix[i+1,j] = (x[counter+1])^2
    splineMatrix[i+1,j+1] = x[counter+1]
    splineMatrix[i+1,j+2] = 1
    splineMatrix[i+1, ncol(splineMatrix)] = y[counter+1]
    
    counter = counter + 1
  }

  i = i+2
  index=2
  for (counter in i:(nrow(splineMatrix)-1)){
    
    j = (3*(index-2))+1 
    
    # 2a1(x)+b1-2a2(x)-b2 = 0 starting from the 2nd point
    splineMatrix[counter,j] = 2*(x[index])
    splineMatrix[counter,j+1] = 1
    
    splineMatrix[counter,j+3] = 0-(2*(x[index]))
    splineMatrix[counter,j+4] = -1
    index = index+1
  }
  #a1 = 0
  splineMatrix[nrow(splineMatrix), 1] = 1
  
  system = list("augcoeffmatrix" = splineMatrix)
  unknowns = GaussJordan(system) # calls the GaussJordan function to solve for the unknowns
  
  functionsMatrix = matrix(c(0), nrow = length(listOfFunctions), ncol = 3)
  colnames(functionsMatrix) = c("Lower Bound", "Upper Bound", "Function")

  ######################################## GET THE FUNCTIONS PER INTERVAL ##########################################
  indexInUnknowns = 1
  for(k in 1:(length(x)-1)){
    polynomial_string = c("function(x) ")
    polynomial_string = c(polynomial_string, as.character(unknowns[indexInUnknowns]), "*(x^2) + ",
                         as.character(unknowns[indexInUnknowns+1]), "*x + ",
                         as.character(unknowns[indexInUnknowns+2]))

    polynomial_string = paste(polynomial_string, collapse = "")
    functionsMatrix = rbind(functionsMatrix, c(x[k], x[k+1], polynomial_string))
    
    polynomial_function = eval(parse(text = polynomial_string)) # convert string to R function

    listOfFunctions[[k]] = polynomial_function
    indexInUnknowns = indexInUnknowns+3
 }

 ######################################## GET THE ESTIMATE OF THE NUMBER ##########################################
    for(l in 1:(length(x)-1)){
      if((numberToEstimate>=x[l]) && (numberToEstimate <= x[l+1])){
       answer = listOfFunctions[[l]](numberToEstimate) 
      }
    }
  
  results = list("listOfFunctions" = listOfFunctions, "answer" = answer, "functionsMatrix" = functionsMatrix)
  print(results)
  return(results)
}




######################################################################################################################
#                                                   SIMPLEX                                                          #
######################################################################################################################
minSimplex <- function(matrixShippingCost, inputVSupply, inputVDemands){
  vSupply = c()
  vDemands = c()
  for(i in 1:length(inputVSupply)){
    vSupply = c(vSupply, inputVSupply[[i]])
  }
  for(i in 1:length(inputVDemands)){
    vDemands = c(vDemands, inputVDemands[[i]])
  }
  
  if(sum(vDemands) > sum(vSupply)) return(NULL)
                                                                                         # x1 - x15                  #Solution
  simplexMatrix = matrix(c(0), nrow=(length(vSupply)+length(vDemands)+1) , ncol=(length(vSupply)*length(vDemands)  + 1))
  
  pivotColumn = simplexMatrix[,1]
  pivotElement = 0

  ######################################## SET UP THE SIMPLEX MATRIX ##########################################
  
  #Constraints for the supplies from the plants
  for (i in 1:length(vSupply)){
    for (j in (length(vDemands)*(i-1)+1):(length(vDemands)*i)){  #p1 = x1+x2+x3+x4+x5
      simplexMatrix[i, j] = -1
    }
    simplexMatrix[i, ncol(simplexMatrix)] = -vSupply[i]
  }
  
  #Constraints for the demands from the warehouses
  for (i in 1:length(vDemands)){
    for (j in 1:length(vSupply)){
      simplexMatrix[length(vSupply)+i, (length(vDemands)*(j-1))+i] = 1
    }
    simplexMatrix[length(vSupply)+i, ncol(simplexMatrix)] = vDemands[i]
  }
  
  #Z
  for (i in 1:nrow(matrixShippingCost)){
    for (j in 1:ncol(matrixShippingCost)){
      simplexMatrix[nrow(simplexMatrix), (ncol(matrixShippingCost)*(i-1) + j)] = (matrixShippingCost[i, j])
    }
  }
  ############################################# DUAL ###################################################
  simplexMatrix = t(simplexMatrix)
  
  for (i in 1:ncol(simplexMatrix)){
    simplexMatrix[nrow(simplexMatrix), i] = -(simplexMatrix[nrow(simplexMatrix), i])
  }

  numColOldMatrix = ncol(simplexMatrix)
  
  for(i in 1:nrow(simplexMatrix)){
    simplexMatrix = cbind(simplexMatrix, c(0)) # add new columns for the slack variables
  }
  
  # move the column for solutions to the rightmost column
  simplexMatrix[,ncol(simplexMatrix)] = simplexMatrix[, numColOldMatrix]
  simplexMatrix[,numColOldMatrix] = c(0)
  
  for(i in 1:nrow(simplexMatrix)){
    for(j in 1:nrow(simplexMatrix)){
      if(i == j){
        simplexMatrix[i, numColOldMatrix + (j-1)] = 1
      }
    }
  }
  
  tableOfSolutions = matrix( c(0), nrow = 1, ncol = 15)
  colnames(tableOfSolutions) = c("De->S", "De->S.L.", "De->Al", "De->Chi", "De->N.Y.",
                                 "P->S", "P->S.L.", "P->Al", "P->Chi", "P->N.Y.",
                                 "Da->S", "Da->S.L.", "Da->Al", "Da->Chi", "Da->N.Y.")
  
  initTableau = simplexMatrix
  simplexAnswer = maxSimplex(simplexMatrix , tableOfSolutions)
  simplexMatrix = simplexAnswer$simplexMatrix
  tableOfSolutions = simplexAnswer$tableOfSolutions
  answer = simplexMatrix[nrow(simplexMatrix), ncol(simplexMatrix)]

  results = list("initTableau" = initTableau, "answer" = answer, "tableOfSolutions" = tableOfSolutions)
  return(results)
}

maxSimplex <- function(simplexMatrix, tableOfSolutions){

  numOfIter = 0
  while(TRUE){
    ###################################### GET PIVOT COLUMN  ###############################################
    pivotColumn = simplexMatrix[,1]
    pivotRow = simplexMatrix[1,]
    pivotElement = 0
    rowIndexOfPivotElement = 1
    colIndexOfPivotElement = 1
    ve = 0# value to be eliminated
    
    # get the largest magnitude
    maxNeg = 0
    for(i  in 1:ncol(simplexMatrix)){
      if((0 - simplexMatrix[nrow(simplexMatrix), i]) > maxNeg){
        maxNeg = 0-simplexMatrix[nrow(simplexMatrix), i]
      }
    }
    
    if(maxNeg == 0) break # there are no negative numbers left in the bottom row
    
    # assign column with largest negative number as the pivot column
    for(i in 1:ncol(simplexMatrix)){
      if(0-simplexMatrix[nrow(simplexMatrix), i] == maxNeg){
        pivotColumn = simplexMatrix[,i]
        colIndexOfPivotElement = i
        break
      }
    }
    
    testRatio = 10000
    for(i in 1:(nrow(simplexMatrix)-1)){
      if(is.finite(simplexMatrix[i, ncol(simplexMatrix)] / pivotColumn[i]) && # not Inf
         (pivotColumn[i] != 0) &&
         # value in the RHS or solution column               #value in the pivot column
         ((simplexMatrix[i, ncol(simplexMatrix)] / pivotColumn[i]) > 0) && # test ratio is positive
         ((simplexMatrix[i, ncol(simplexMatrix)] / pivotColumn[i]) < testRatio)){
        testRatio = simplexMatrix[i, ncol(simplexMatrix)] / pivotColumn[i]
        pivotElement = pivotColumn[i]
        pivotRow = simplexMatrix[i, ]
        rowIndexOfPivotElement = i
      }
    }
    
    if(pivotElement == 0) break
    
    #normalize 
    simplexMatrix[rowIndexOfPivotElement, ] = simplexMatrix[rowIndexOfPivotElement, ] /pivotElement
    
    pivotElement = simplexMatrix[rowIndexOfPivotElement, colIndexOfPivotElement]
    pivotColumn = simplexMatrix[, colIndexOfPivotElement]
    
    for (m in 1:nrow(simplexMatrix)){
      if(m!=rowIndexOfPivotElement){
        if(simplexMatrix[m, colIndexOfPivotElement] != 0){
          ve = simplexMatrix[m, colIndexOfPivotElement] #assigns the values under the pivot element as values to be eliminated
          multiplier = ve/pivotElement
          vector = simplexMatrix[rowIndexOfPivotElement,]*multiplier #multiplies the values in pivot row with multiplier
          simplexMatrix[m, ] =   simplexMatrix[m, ] - vector #subtracts values of vector from the values in the row where 'value to be eliminated' is located 
          
        }
      }
    }
    
    # save the basic solutions per iteration in a table
    if(numOfIter == 0){
      tableOfSolutions[1,] = c(simplexMatrix[nrow(simplexMatrix), 9], simplexMatrix[nrow(simplexMatrix), 10], simplexMatrix[nrow(simplexMatrix), 12],
                               simplexMatrix[nrow(simplexMatrix), 12], simplexMatrix[nrow(simplexMatrix), 13], simplexMatrix[nrow(simplexMatrix), 14],
                               simplexMatrix[nrow(simplexMatrix), 15], simplexMatrix[nrow(simplexMatrix), 16], simplexMatrix[nrow(simplexMatrix), 17],
                               simplexMatrix[nrow(simplexMatrix), 18], simplexMatrix[nrow(simplexMatrix), 19], simplexMatrix[nrow(simplexMatrix), 20],
                               simplexMatrix[nrow(simplexMatrix), 21], simplexMatrix[nrow(simplexMatrix), 22], simplexMatrix[nrow(simplexMatrix), 23])
    }else{
      tableOfSolutions = rbind(tableOfSolutions, c(simplexMatrix[nrow(simplexMatrix), 9], simplexMatrix[nrow(simplexMatrix), 10], simplexMatrix[nrow(simplexMatrix), 12],
                                                 simplexMatrix[nrow(simplexMatrix), 12], simplexMatrix[nrow(simplexMatrix), 13], simplexMatrix[nrow(simplexMatrix), 14],
                                                 simplexMatrix[nrow(simplexMatrix), 15], simplexMatrix[nrow(simplexMatrix), 16], simplexMatrix[nrow(simplexMatrix), 17],
                                                 simplexMatrix[nrow(simplexMatrix), 18], simplexMatrix[nrow(simplexMatrix), 19], simplexMatrix[nrow(simplexMatrix), 20],
                                                 simplexMatrix[nrow(simplexMatrix), 21], simplexMatrix[nrow(simplexMatrix), 22], simplexMatrix[nrow(simplexMatrix), 23]))
 
    }
       numOfIter = numOfIter + 1
  }
  results = list("simplexMatrix" = simplexMatrix, "tableOfSolutions" = tableOfSolutions)
  return(results)
}