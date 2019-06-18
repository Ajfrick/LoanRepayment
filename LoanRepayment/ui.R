
library(tidyverse)
library(stringr)
library(shiny)

l_labs = str_c("Remaining Balance on Loan #", 1:6)
r_labs = str_c("Interest Rate % for Loan #", 1:6)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Loan Repayment"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("P1", label = l_labs[1], value = 3000, min = 0, max = 1e6),
      numericInput("r1", label = r_labs[1], value = 5, min = 0, max = 10),
      
      numericInput("P2", label = l_labs[2], value = 0, min = 0, max = 1e6),
      numericInput("r2", label = r_labs[2], value = 0, min = 0, max = 10),
      
      numericInput("P3", label = l_labs[3], value = 0, min = 0, max = 1e6),
      numericInput("r3", label = r_labs[3], value = 0, min = 0, max = 10),
      
      numericInput("P4", label = l_labs[4], value = 0, min = 0, max = 1e6),
      numericInput("r4", label = r_labs[4], value = 0, min = 0, max = 10),
      
      numericInput("pmt", label = "Monthly Payment", value = 100, min = 0, max = 1e5),
      
      actionButton("go", "Go")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("loantext"),
      plotOutput("payplot"),
      plotOutput("balplot")
    )
  )
))
