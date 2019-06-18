
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  calcLoan = function(P,r,pmt){
    ind = (P != 0) #Remove empty balances
    r = r/100 # Convert to decimal percentage
    P = P[ind]
    r = r[ind]
    I = P * ((1 + r/12)-1) # Overall Interest 1st month
    
    props = P/sum(P) # proportion of each payment to each loan
    pays = pmt * props
    
    #Initialize each list for Principle, Balance, Interest, payment to each
    Ps = list()
    Ps[[1]] = P
    Bals = list()
    Bals[[1]] = sum(P)
    Is = list()
    Is[[1]] = I
    if(pmt < sum(Is[[1]])){return(NULL)}
    Pays = list()
    Pays[[1]] = pays
    Pays2I = list()
    Pays2I[[1]] = min(sum(Is[[1]]),sum(Pays[[1]]))
    Pays2P = list()
    Pays2P[[1]] = max(0, sum(Pays[[1]]) - sum(Is[[1]]))
    
    # Loop through balance calculations until balance is under 0
    for(i in 2:1e5){
      Ps[[i]] = Ps[[i-1]] + Is[[i-1]] - Pays[[i-1]]
      Bals[[i]] = sum(Ps[[i]])
      if(sum(Ps[[i]] < 0)) break
      Is[[i]] = Ps[[i]] * ((1 + r/12)-1)
      Pays[[i]] = pmt * Ps[[i]]/sum(Ps[[i]])
      Pays2I[[i]] = min(sum(Is[[i]]),sum(Pays[[i]]))
      Pays2P[[i]] = min(max(0,  sum(Pays[[i]]) - sum(Is[[i]])), Bals[[i]])
    }
    
    I_tot = sum(unlist(lapply(Pays2I, sum))) # Total Interest
    P_tot = sum(unlist(lapply(Pays2P, sum))) # Total Principle
    
    out = list("months" = ceiling(i),
               "I_tot"   = I_tot,
               "Bals"    = Bals,
               "P_tot"   = I_tot + P_tot,
               "Pays" = list("P" = Pays2P,
                             "I" = Pays2I))
    out
  }
  dispLoan = function(P,r,pmt){
    out = calcLoan(P, r, pmt)
    if(is.null(out)){
      return(str_c("Your desired monthy payment is lower than accrued interest, please",
                   " enter a larger monthly payment"))
    }
    return(str_c("Total Amount paid: $", sprintf(paste0('%.',2,'f'),out$P_tot),
                 "\nInterest Paid: $", sprintf(paste0('%.',2,'f'),out$I_tot),
                 "\nMonths to pay off: ", out$months))
  }
  plotLoan = function(P, r, pmt){
    out = calcLoan(P, r, pmt)
    if(is.null(out)){
      return(NULL)
    }
    payTib = tibble(
      Ps = unlist(out$Pays$P),
      Is = unlist(out$Pays$I)
    ) %>% rowid_to_column(var = 'n') %>% 
      gather(type, pay, -n)
    
    balTib = tibble(
      bal = unlist(out$Bals),
      type = "Principal"
    ) %>% rowid_to_column(var = 'n') %>% 
      filter(bal > 0)
    
    p_pay = ggplot(data = payTib, aes(x = n, y = pay)) + 
      geom_bar(aes(fill = type), stat = 'identity', position = 'stack') + 
      scale_fill_manual(values = c("Red", "Blue"), labels = c("Interest", "Principal")) + 
      coord_cartesian(ylim = c(0, pmt)) +
      labs(x = "Months", y = "Payment", title = "Payment to Interest vs Principle",
           fill = "") +
      
      theme_bw()
    
    b_pay = ggplot(data = balTib, aes(x = n, y = bal)) + 
      geom_bar(aes(fill = type), stat = 'identity' ) + 
      scale_fill_manual(values = "Blue", labels = "Principal") +
      labs(x = "Months", y = "Remaining Balance", title = "Total Loan Balance Over time") +
      coord_cartesian(ylim = c(0, P)) +
      theme_bw()
    
    plots = list("bal_plot" = b_pay,
                 "pay_plot" = p_pay)
    return(plots)
  }
  P = eventReactive(input$go,{
    Ps = c(input$P1,input$P2,input$P3,input$P4)
  })
  r = eventReactive(input$go, {
    rs = c(input$r1,input$r2,input$r3,input$r4)
  })
  pmt = eventReactive(input$go,{
    pmt = input$pmt
  })
  # test = calcLoan(c(input$P1,input$P2,input$P3,input$P4,input$P5,input$P6),
  #                 c(input$I1,input$I2,input$I3,input$I4,input$I5,input$I6),
  #                 pmt = input$pmt)
  output$loantext <- renderText({
    dispLoan(P(), r(), pmt())
  })
  output$payplot = renderPlot({
    plotLoan(P(), r(), pmt())[[1]]
  })
  output$balplot = renderPlot({
    plotLoan(P(), r(), pmt())[[2]]
  })
})

