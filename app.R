#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shinyjs")

library(shiny)
library(shinyjs)

data = read.csv("data.csv", header = TRUE, sep=",")
cleanedData <- data[-which(is.na(data$loan_amnt) | is.na(data$term) | is.na(data$purpose) | is.na(data$dti) | is.na(data$addr_state) | is.na(data$home_ownership) | is.na(data$annual_inc) | is.na(data$fico_range_low) | is.na(data$open_acc) | is.na(data$revol_bal) | is.na(data$inq_last_6mths) | is.na(data$emp_length)),]

myvars <- c("int_rate", "loan_amnt","term", "fico_range_low", "inq_last_6mths", "open_acc", "purpose", "dti", "home_ownership", "emp_length", "addr_state", "member_id", "loan_status")
cleanedVals <- cleanedData[myvars]

sampleData <- head(cleanedVals, n=3)

# interest rate model
model.int_rate = lm(int_rate ~ fico_range_low + term + loan_amnt + inq_last_6mths + open_acc + purpose + dti + home_ownership + emp_length, data = cleanedVals)

# default risk model
model.default_risk = glm(loan_status ~ fico_range_low + term + loan_amnt + inq_last_6mths + open_acc + purpose + dti + home_ownership + emp_length, data = cleanedVals, family = binomial())

library(QuantPsyc)

coef.int_rate = lm.beta(model.int_rate)
coef.default_risk = lm.beta(model.default_risk)


calcImportance <- function(coef) {
  if (abs(coef) >= 0.5) {
    return("Very High")
  } else if (abs(coef) >= 0.2) {
    return("High")
  } else if (abs(coef) > 0.095) {
    return("Medium")
  } else {
    return("Low")
  }
}

getPurposeImp <- function (purpose, coef) {
  purposeList = c("credit_card", "debt_consolidation", "educational", "home_improvement", "house", "major_purchase", "medical", "moving", "other", "renewable_energy", "small_business", "vacation", "wedding")
  index = match(purpose, purposeList)
  if (is.na(index)) {
    return("Medium")
  } else {
    std_coef = coef[index]
    return(calcImportance(std_coef))
  }
}

getHomeOwnImp <- function (home_own_status, coef) {
  homeOwnList = c("NONE", "OTHER", "OWN", "RENT")
  index = match(home_own_status, homeOwnList)
  if (is.na(index)) {
    return("Medium")
  } else {
    std_coef = coef[index]
    return(calcImportance(std_coef))
  }
}

getEmpLengthImp <- function (emp_length, coef) {
  empLenghtList = c("1 year", "10+ years", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "n/a")
  index = match(emp_length, empLenghtList)
  if (is.na(index)) {
    return("Medium")
  } else {
    std_coef = coef[index]
    return(calcImportance(std_coef))
  }
}

calcFicoStatus <- function(ficoScore) {
  if (ficoScore < 580) {
    return("Very Poor")
  } else if (ficoScore < 670) {
    return("Poor")
  } else if (ficoScore < 740) {
    return("Good")
  } else if (ficoScore < 800) {
    return("Very Good")
  } else {
    return("Excellent")
  }
}

calcTermStatus <- function(term) {
  if (term == "60 months") {
    return("Poor")
  } else {
    return("Good")
  }
}

calcLoanAmtStatus <- function(loanAmount) {
  if (loanAmount < 5000) {
    return("Excellent")
  } else if (loanAmount < 10000) {
    return("Very Good")
  } else if (loanAmount < 15000) {
    return("Good")
  } else if (loanAmount < 24000) {
    return("Poor")
  } else {
    return("Very Poor")
  }
}

calcOpenAccStatus <- function(numAcc) {
  if (numAcc < 4) {
    return("Very Poor")
  } else if (numAcc < 8) {
    return("Poor")
  } else if (numAcc < 12) {
    return("Good")
  } else if (numAcc < 20) {
    return("Very Good")
  } else {
    return("Excellent")
  }
}

calcDTIStatus <- function(dti) {
  if (dti < 8) {
    return("Very Poor")
  } else if (dti < 13) {
    return("Poor")
  } else if (dti < 18) {
    return("Good")
  } else if (dti < 24) {
    return("Very Good")
  } else {
    return("Excellent")
  }
}

calcInquiryStatus <- function(numInquiries) {
  if (numInquiries == 0) {
    return("Excellent")
  } else if (numInquiries == 1) {
    return("Very Good")
  } else if (numInquiries == 2) {
    return("Good")
  } else if (numInquiries < 4) {
    return("Poor")
  } else {
    return("Very Poor")
  }
}

calcStatus <- function(index, coef) {
  std_coef = 0
  if (is.na(index)) {
    std_coef = 0
  } else {
    std_coef = coef[index]
  }
  if (std_coef < -0.1) {
    return("Excellent")
  } else if (std_coef < 0) {
    return("Very Good")
  } else if (std_coef < 0.2) {
    return("Good")
  } else if (std_coef < 0.5) {
    return("Poor")
  } else {
    return("Very Poor")
  }
}

calcPurposeStatus <- function(purpose, coef) {
  purposeList = c("credit_card", "debt_consolidation", "educational", "home_improvement", "house", "major_purchase", "medical", "moving", "other", "renewable_energy", "small_business", "vacation", "wedding")
  index = match(purpose, purposeList)
  return(calcStatus(index, coef))
}

calcHomeOwnStatus <- function(home_own_status, coef) {
  homeOwnList = c("NONE", "OTHER", "OWN", "RENT")
  index = match(home_own_status, homeOwnList)
  return(calcStatus(index, coef))
}

calcEmpLenStatus <- function(emp_length, coef) {
  empLenghtList = c("1 year", "10+ years", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "n/a")
  index = match(emp_length, empLenghtList)
  return(calcStatus(index, coef))
}

calcFactors <- function(userData, coef.int_rate) {
  factors = c("FICO Credit Score", "Loan Amount", "Loan Duration", "Purpose", "Home Ownership", "Employment Duration", "Open Accounts", "Debt to Income Ratio", "Inquiries in last 6 months")
  importance = rep("Medium", 9)
  importance[1] = calcImportance(coef.int_rate[1])
  importance[2] = calcImportance(coef.int_rate[3])
  importance[3] = calcImportance(coef.int_rate[2])
  importance[4] = getPurposeImp(userData$purpose, coef.int_rate[6:18])
  importance[5] = getHomeOwnImp(userData$home_ownership, coef.int_rate[20:23])
  importance[6] = getEmpLengthImp(userData$emp_length, coef.int_rate[24:34])
  importance[7] = calcImportance(coef.int_rate[5])
  importance[8] = calcImportance(coef.int_rate[19])
  importance[9] = calcImportance(coef.int_rate[4])
  status = rep("Medium", 9)
  status[1] = calcFicoStatus(userData$fico_range_low)
  status[2] = calcLoanAmtStatus(userData$loan_amnt)
  status[3] = calcTermStatus(as.character(userData$term))
  status[4] = calcPurposeStatus(as.character(userData$purpose), coef.int_rate[6:18])
  status[5] = calcHomeOwnStatus(as.character(userData$home_ownership), coef.int_rate[20:23])
  status[6] = calcEmpLenStatus(as.character(userData$emp_length), coef.int_rate[24:34])
  status[7] = calcOpenAccStatus(userData$open_acc)
  status[8] = calcDTIStatus(userData$dti)
  status[9] = calcInquiryStatus(userData$inq_last_6mths)
  values = c(userData$fico_range_low, userData$loan_amnt, as.character(userData$term), as.character(userData$purpose), as.character(userData$home_ownership), as.character(userData$emp_length), userData$open_acc, userData$dti,  userData$inq_last_6mths)
  factorSummary = data.frame(Factor=factors, Status = status, Imp = importance, Value = values)
  return(factorSummary)
}

#data_train <- read.csv("/Users/priyankakulkarni/Downloads/trainData.csv",header=TRUE)
#data_test <-  read.csv("/Users/priyankakulkarni/Downloads/testData.csv",header=TRUE)
#data.lm <- lm(int_rate ~ loan_amnt + term + fico_range_low + inq_last_6mths + open_acc + purpose + dti + home_ownership + emp_length + addr_state, data = data_train)
#myvars <- c("int_rate", "loan_amnt","term", "fico_range_low", "inq_last_6mths", "open_acc", "purpose", "dti", "home_ownership", "emp_length", "addr_state", "member_id")
#cleanedData <- data_train[myvars]


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
   # Application title
   titlePanel("Enter loan information"),
   dataTableOutput("intRate"),
   
  
   
   # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #    sidebarPanel(
      #   sliderInput("bins",
               #      "Number of bins:",
     #                min = 1,
     #                max = 50,
     #                value = 30)
   #   ),
      
      # Show a plot of the generated distribution
    #  mainPanel(
        
        mainPanel(
          
          actionButton("printData","Print data"),
          
          textInput("member_id", "Enter member id", ""),
          actionButton("login","Login"),
          actionButton("register","Register"),
          conditionalPanel(
            condition = "input.login > 0",
            actionButton("logout","Logout")
          ),
          conditionalPanel(
            condition = "input.register > 0",
            actionButton("back","Back")
          )

          
        ),
  
  div(id = "reg_panel",
 # conditionalPanel(
 #   condition = "input.register > 0",
    sidebarPanel(
      textInput('new_member_id', "Enter new member id",""),
      numericInput("new_fico_range_low", "Enter your FICO credit score", 0, min = 1),
      selectInput("new_home_ownership", "Type of home ownership:",
                  c("Own" = "own",
                    "Rent" = "rent",
                    "Mortgage" = "mortgage",
                    "None" = "none",
                    "Other" = "other")),
      selectInput("new_emp_length", "Employment duration:",
                  c("< 1 Year" = "< 1 year",
                    "1 year" = "1 year",
                    "2 years" = "2 years",
                    "3 years" = "3 years",
                    "4 years" = "4 years",
                    "5 years" = "5 years",
                    "6 years" = "6 years",
                    "7 years" = "7 years",
                    "8 years" = "8 years",
                    "9 years" = "9 years",
                    "10+ years" = "10+ years",
                    "N/A" = "n/a")),
      numericInput("new_inq_last_6mths", "Enter number of inquiries in last 6 months", 0, min = 1),
      numericInput("new_open_acc", "Enter number of open accounts", 0, min = 1),
      numericInput("new_dti", "Enter debt to income ratio", 0, min = 1)
      ),
      
      # textInput('x', "enter X value here",""),
      # textInput('y', "enter Y value here",""),
      actionButton("new_user","Create account")
      
  #  )
  ),


  #  conditionalPanel(
  #    condition = "input.login > 0",
  #    sidebarPanel(
  #    tableOutput("member_info")
  #  )
#  ),
  
      conditionalPanel(
        condition = "input.login > 0",
        sidebarPanel(
        selectInput("purpose", "Purpose of loan:",
                    c("Credit Card" = "credit_card",
                      "Education" = "educational",
                      "Home" = "home",
                      "Car" = "car",
                      "Home" = "house",
                      "Business" = "small_business",
                      "Vacation" = "vacation",
                      "Wedding" = "wedding",
                      "Major purchase" = "major_purchase",
                      "Medical" = "medical",
                      "Moving" = "moving",
                      "Other" = "Other")),
        #  textInput("loan_amt", "Enter loan amount", "Amount in $"),
        numericInput("loan_amt", "Enter loan amount", 10, min = 1),
       # textInput("loan_duration", "Enter loan duration in months", ""),
       selectInput("loan_duration", "Enter loan duration in months",
                   c("36 months" = "36 months", "60 months" = "60 months")
                   ),
        
        # textInput('x', "enter X value here",""),
        # textInput('y', "enter Y value here",""),
        actionButton("submit","Predict loan credibility")
        
      )
      
     ),
  
  conditionalPanel(
    condition = "input.submit > 0",
    width = "500px",
    sidebarPanel(
    
    uiOutput('table'),
    uiOutput('int_rate'),
    uiOutput('default_risk')
   
    ),
 uiOutput('factors')
  )
        
  
        
       #  plotOutput("distPlot"),
         #Loan input

         
      #   textOutput("txtOutput"),
         
      #   tableOutput("intRate")
      
    #  )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # cleanedVals <- reactiveValues(df_data = sampleData)
   
  # print(cleanedVals$df_data)
  
   output$txtOutput = renderText("Enter loan duration")
   
   shinyjs::hide("reg_panel")
   
   sampleVals = reactive ({
     sampleData
   })
   
   Data = reactive({
     if (input$submit > 0) {
       df <- data.frame(Purpose=input$purpose,Amount=input$loan_amt,Duration=input$loan_duration)
       #print(df)
       return(list(df=df))
     }
   })
   
   observeEvent(input$printData, {
     print(sampleData)
   })
   
   observeEvent(input$new_user, {
     df <- data.frame(member_id=input$new_member_id,fico_range_low=input$new_fico_range_low,open_acc=input$new_open_acc, dti=input$new_dti,emp_length=input$new_emp_length, home_ownership=input$new_home_ownership, inq_last_6mths=input$new_inq_last_6mths,int_rate=NA, loan_amnt = NA, term = NA, purpose=NA, addr_state=NA, loan_status =NA)
     sampleData <- rbind(sampleData, df)
     print(sampleData)
     shinyjs::hide("reg_panel")
     shinyjs::enable("login")
   #  session = getDefaultReactiveDomain()
   #  session$reload()
   })
   
   observeEvent(input$register, {
     shinyjs::disable("login")
     shinyjs::show("reg_panel")
   })
   
   observeEvent(input$back, {
   #  session = getDefaultReactiveDomain()
   #  session$reload()
     shinyjs::enable("login")
   })
   
#   NewMember = reactive({
 #    if (input$new_user > 0) {
  #     df <- data.frame(member_id=input$new_member_id,fico_range_low=input$new_fico_range_low,open_acc=input$new_open_acc, dti=input$new_dti,emp_length=input$new_emp_length, home_ownership=input$new_home_ownership, inq_last_6mths=input$inq_last_6mths)
  #     sampleData <- rbind(sampleData, df)
  #       return(list(df=df))
 #     }
#   })
   
   
   
   
   MemberData = reactive({
     if (input$member_id != '') {
       df <- data.frame(sampleVals[sampleVals$member_id==input$member_id,])
       print(input$member_id)
       print(sampleData)
       return(list(df=df))
     }
   })
   
  
   
   
   observeEvent(input$login, {
   #  shinyjs::disable("login")
     shinyjs::disable("register")
   #  shinyjs::disable("member_id")
   })
   
   observeEvent(input$logout, {
    # session = getDefaultReactiveDomain(); session$reload()
   }) 
   
   output$member_info <- renderTable({
     if (is.null(MemberData())) {return()}
     details <- data.frame(MemberData()$df$member_id)
     print(MemberData()$df$member_id)
   },
   'include.colnames' = FALSE,
   caption = "Member ID", 
   caption.placement = getOption("xtable.caption.placement", "top"), 
   caption.width = getOption("xtable.caption.width", NULL))
   
   observeEvent(input$submit, {
     
     
     output$table <- renderTable({
       if (is.null(Data())) {return()}
       print("table")
       print(Data()$df)
     }, 'include.rownames' = FALSE
     , 'include.colnames' = TRUE
     , caption = "Loan details", caption.placement = getOption("xtable.caption.placement", "top"), 
     caption.width = getOption("xtable.caption.width", NULL)
     #, 'sanitize.text.function' = function(x){x}
     )
     
     output$int_rate <- renderTable({
       if (is.null(Data())) {return()}
       pred <- predict(model.int_rate, newdata=data.frame(loan_amnt=Data()$df$Amount,term=Data()$df$Duration,fico_range_low=MemberData()$df$fico_range_low, inq_last_6mths=MemberData()$df$inq_last_6mths, open_acc=MemberData()$df$open_acc,purpose=Data()$df$Purpose,dti=MemberData()$df$dti,home_ownership=MemberData()$df$home_ownership,emp_length=MemberData()$df$emp_length,addr_state=MemberData()$df$addr_state, loan_status=MemberData()$df$loan_status))
       # val <- as.numeric(Data()$df$y)
       print(pred)
     }, 
     'include.colnames' = FALSE,
     caption = "Predicted Interest Rate", 
     caption.placement = getOption("xtable.caption.placement", "top"), 
     caption.width = getOption("xtable.caption.width", NULL),
     width = "500px"
     )
     
     output$default_risk <- renderTable({
       if (is.null(Data())) {return()}
       def_pred <- predict(model.default_risk, newdata=data.frame(loan_amnt=Data()$df$Amount,term=Data()$df$Duration,fico_range_low=MemberData()$df$fico_range_low, inq_last_6mths=MemberData()$df$inq_last_6mths, open_acc=MemberData()$df$open_acc,purpose=Data()$df$Purpose,dti=MemberData()$df$dti,home_ownership=MemberData()$df$home_ownership,emp_length=MemberData()$df$emp_length,addr_state=MemberData()$df$addr_state), type="response")
       # val <- as.numeric(Data()$df$y)
       print(def_pred)
     }, 
     'include.colnames' = FALSE,
     caption = "Predicted Default Risk", 
     caption.placement = getOption("xtable.caption.placement", "top"), 
     caption.width = getOption("xtable.caption.width", NULL),
     width = "500px"
     )
     
     output$factors <- renderTable({
       if (is.null(Data())) {return()}
       abcd <- data.frame(loan_amnt=Data()$df$Amount,term=Data()$df$Duration,fico_range_low=MemberData()$df$fico_range_low, inq_last_6mths=MemberData()$df$inq_last_6mths, open_acc=MemberData()$df$open_acc,purpose=Data()$df$Purpose,dti=MemberData()$df$dti,home_ownership=MemberData()$df$home_ownership,emp_length=MemberData()$df$emp_length,addr_state=MemberData()$df$addr_state)
       factor_df <- calcFactors(abcd, coef.int_rate)
       print(factor_df)
     }, 'include.rownames' = FALSE
     , 'include.colnames' = TRUE
     , caption = "Loan details", caption.placement = getOption("xtable.caption.placement", "top"), 
     caption.width = getOption("xtable.caption.width", NULL),
     width = "500px"
     #, 'sanitize.text.function' = function(x){x}
     )
     
    
    
     
   }) 
   
   
   
  
  
   
  # output$intRate <- renderDataTable({pred})
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

