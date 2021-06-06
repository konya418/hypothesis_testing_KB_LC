library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
library(shinyBS)
library(shiny)
library(shinyjs)
library(latex2exp)
library(boastUtils)
library(shinydashboard)

playerdata <- read.csv("NBA1617E.csv", header = TRUE)
APP_TITLE <- "Hypothesis Testing with NBA data"

# Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
index1 <- which(((playerdata$FTA >= 1) * (playerdata$FTA <= 1000)) == 1)
playerdata2 <- playerdata[index1, ]

# create a list of just the players names to be selected from later
PlayerNames <- playerdata2[, 1]

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "Hypothesis Testing with NBA data", 
      titleWidth = 250,
      tags$li(
        class = "dropdown", 
        actionLink("info",icon("info"))
      ),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Hypothesis_Testing"
        )
      ),
      tags$li(class = "dropdown", 
              tags$a(href='https://shinyapps.science.psu.edu/', 
                     icon("home", lib = "font-awesome")
              )
      )
    ),
    
    ## Create the sidebar navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", 
                 tabName = "overview", 
                 icon = icon("dashboard")
        ),
        menuItem("Filtering", 
                 tabName = "filtering", 
                 icon = icon("wpexplorer")
        ),
        menuItem("Hypothesis Testing", 
                 tabName = "hypothesis_testing", 
                 icon = icon("cogs")
        ),
        menuItem("Data", 
                 tabName = "data", 
                 icon = icon("table")
        )
      )
    ),
    
    ### Overview page ----
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "Feature.css")
      ),
      tabItems(
        tabItem(
          tabName = "overview",
          tags$a(href = "http://stat.psu.edu/", tags$img(src = "PS-HOR-RGB-2C.png", align = "left", width = 180)),
          br(), br(), br(),
          
          h3(strong("About:")),
          h4("In this app the goal is to learn about the reasoning of a hypothesis test about proportions."),
          h4("This app uses 2016-2017 data for the NBA regular season."),
          br(),
          h3(strong("Instructions:")),
          h4(tags$li("In Part 1 you will look at how the population distribution of all the players' free throw percentages is affected by filtering (restricting attention to a subpopulation).")),
          tags$head(
            tags$style(HTML("#go{background-color: #367fa9}"))
          ),
          div(
            style = "text-align: center;",
            bsButton("go1", "Go to filtering exploration", icon = icon("bolt"))
          ),
          h4(tags$li("In Part 2 you will explore hypothesis tests about an individual players' free throw percentages.")),
          tags$head(
            tags$style(HTML("#go{background-color: #367fa9}"))
          ),
          div(
            style = "text-align: center;",
            bsButton("go2", "Go to hypothesis tester", icon = icon("bolt"))
          ),
          br(),
          h3(strong("Acknowledgements:")),
          h4("This app was developed and programmed in 2017 by David Robinson. The hypothesis testing features in part 2 were edited and improved with additional programming in 2018 by Ryan Voyack."),
          
          br(),
          br(),
          img(src = "fthrow2.png", height = 250, width = 650, algin = "middle")
        ),
        
        ### Filtering page ----
        tabItem(
          tabName = "filtering",
          div(
            style = "display: inline-block;vertical-align:top;",
            tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
          ),
          fluidRow(
            # Include LaTeX functioality although I don't think I used it for this
            withMathJax(),
            
            # Column 1 has inputs for how to filter and is of width 4
            column(
              4,
              
              selectInput("filtertype", h2("Select how you would like to filter."), choices = c(GamesPlayed = "games", FreeThrowAttempts = "FTA")),
              
              conditionalPanel(
                "input.filtertype == 'games'",
                sliderInput(
                  inputId = "gamesplayed",
                  "Filter on number of games played:",
                  min = 0,
                  max = 82,
                  value = c(0, 85)
                )
              ),
              conditionalPanel(
                "input.filtertype == 'FTA'",
                sliderInput(
                  inputId = "FTA1",
                  "Filter on number of free throws attempted:",
                  min = 0,
                  max = 881,
                  value = c(0, 881)
                )
              ),
              img(src = "Giannis.png", height = 219, width = 300, align = "middle")
            ),
            
            # Column two displays the Histogram of the distrubition of the free throw attempts
            column(
              8,
              h1("Histogram"),
              
              plotOutput("histogramNBA"),
              # Add rollover for Histogram of Free Throw Proportion Plot
              bsPopover(id = "histogramNBA", title = " ", content = "This histogram filters the NBA players based on games played or free throw attempts. Changing the slider will adjust the number of players that fit the selected criteria.", placement = "left", trigger = "hover", options = NULL)
            ),
            
            # A box with information to get students thinking and transitioning into part 2
            box(width = 12, background = "blue", h4("Try to think about what the median and mean of FT% are and what range you might expect most of the players to fall in. "))
          )
        ),
        
        
        ### Hypothesis Testing page ----
        tabItem(
          tabName = "hypothesis_testing",
          div(
            style = "display: inline-block;vertical-align:top;",
            tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
          ),
          fluidRow(
            column(
              12,
              h4("Here, we will create a sample 'p-hat' for any player's free throw percentage and test it against a particular null hypothesis. By default, we test it against the NBA average percentage of 74%. Also, we will only be selecting from all NBA players that played no less than half of the games their teams played in during the 2016-2017 season.")
            )
          ),
          fluidRow(
            # This is a text output that displays what the hypothesis is they are testing and who the player is
            column(
              4,
              # Conditional based on how the user would like to select a player for the hypothesis test
              selectInput(inputId = "howToChooseNBA", "Would you like to select a random player, or a player of your choice?", choices = c(Random = "rand", Select = "sel")),
              conditionalPanel(
                "input.howToChooseNBA == 'sel'",
                selectizeInput(inputId = "player", "Select your player from the drop down list below:", choices = PlayerNames, multiple = FALSE, options = list(placeholder = "Select Your Player"), selected = NULL)
              ),
              
              # Random button
              actionButton(inputId = "rand", label = "Choose"),
              
              
              # after the user selects generate, we pull up option to choose null and sample size
              conditionalPanel(
                condition = "input.rand",
                
                
                # The H0 value the user would like to test against
                numericInput("null.valNBA", "Select a value for the null hypothesis. ", min = 0, max = 1, value = 0.74, step = 0.01),
                textOutput("text3NBA"),
                
                tags$head(tags$style("#text3NBA{color: black;font-style: bold;}")),
                br(),
                
                ### User now selects what their sample size would be ie how many shots they are simulating for the player
                # simulates shots based on the players actual FT%
                # h4("Simulate your player shooting free throws and guess whether or not we can reject the null hypothesis"),
                sliderInput("samp.sizeNBA", ("Input the number of shots in the sample:  "), min = 5, max = 60, value = 30, step = 1),
                
                actionButton(inputId = "resample", label = "Sample!"),
                
                conditionalPanel(
                  condition = "input.resample",
                  checkboxInput("iftestNBA", h5("Show Hypothesis Test Output")),
                  checkboxInput("significancebounds", h5("Plot significance bounds"))
                )
                
                # Conditional using checkbox if they want to see what the true population proportion is for their player
                # checkboxInput("trueNBA", h6("Plot the true free throw percentage")),
                # conditionalPanel("input.trueNBA==true",
                #                textOutput("text1NBA")
                # ),
              )
              
              
              # include an image
              # img(src = "fthrow.png", height = 150, width =100)
            ),
            
            column(
              8,
              
              h4("CHALLENGE: Simulate your player shooting free throws and determine whether or not we can reject the null hypothesis based on the plot."),
              h4("CHALLENGE: Does increasing the sample size make it easier or harder to get a significantly low p-value?"),
              
              plotOutput("proportion2NBA"),
              bsPopover(id = "proportion2NBA", title = " ", content = "This bar plot shows us the sampled proportion and the hypothesized proportion that are being tested in our hypothesis test.", placement = "left", trigger = "hover", options = NULL),
              # Output some info about the graphs and the conditional part
              # h4("The red line shows the proportion from the null hypothesis"),
              # h4("The purple line shows the sample proportion"),
              # conditionalPanel("input.true==true",
              #                  h4("The blue line shows the players actual free throw proportion from the 2016-17 season")
              # )
              conditionalPanel(
                "input.resample",
                uiOutput("text1NBA"),
                uiOutput("text2NBA")
              ),
              conditionalPanel(
                "input.iftestNBA==true",
                h4("Normal approximation hypothesis test"),
                tableOutput("testtableNBA"),
                h4("Exact hypothesis test"),
                tableOutput("exactTesttableNBA")
              )
              
              # ),
              # column(8,
              #       conditionalPanel("input.iftestNBA==true",
              #                        tableOutput("exactTesttableNBA")
              #       )
            )
          )
        ),
        
        
        ### Data page ----
        tabItem(
          tabName = "data",
          div(
            style = "display: inline-block;vertical-align:top;",
            tags$a(href = "https://shinyapps.science.psu.edu/", tags$img(src = "homebut.PNG", width = 15))
          ),
          fluidRow(
            column(
              6,
              h3("Data"),
              tableOutput("samp.table")
            )
          )
        )
      )
    )
  )
)


# Define server logic ----
server <- function(input, output, session) {
  
  # Setup locker configuration
  config <- list(
    base_url = "https://learning-locker.stat.vmhost.psu.edu/",
    auth = "Basic YWVlMjQ2ZDJmMzk2OWMwYTk0NTY3ZTQ0ZThiMDU3NDI3MjhhNWFiYjpmYWU4NDkwNTVlMzNiMDEyNzY0OGIyOGI5YzliZjI2NjMyYzFhYzJk",
    agent = rlocker::createAgent()
  )
  
  # Initialize Learning Locker connection
  connection <- rlocker::connect(session, config)
  
  .generateStatement <- function(session, verb = NA, object = NA, description = NA, value = NA) {
    if(is.na(object)){
      object <- paste0("#shiny-tab-", session$input$tabs)
    } else {
      object <- paste0("#", object)
    }
    
    stmt <- list(
      verb =  verb,
      object = list(
        id = paste0(boastUtils::getCurrentAddress(session), object),
        name = paste0(APP_TITLE),
        description = description
      )
    )
    
    if(!is.na(value)){
      stmt$result <- list(
        response = value
      ) 
    }
    
    statement <- rlocker::createStatement(stmt)
    response <- rlocker::store(session, statement)
    
    return(response)   
  }
  
  dataFilter <- reactive({
    games <- input$gamesplayed
    gameindex <- which(((playerdata$G >= games[1]) * (playerdata$G <= games[2])) == 1)
    
    fta <- input$FTA1
    index <- which(((playerdata$FTA >= fta[1]) * (playerdata$FTA <= fta[2])) == 1)
    playerdata <- playerdata[index, ]
    bballdata <- playerdata[gameindex, ]
  })
  
  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = "This app is designed to practice hypothesis testing using NBA data.",
      type = NULL
    )
  })
  
  observeEvent(input$filtertype, {
    .generateStatement(
      session = session, 
      object = "filtertype", 
      verb = "interacted", 
      description = "Changed histogram filter.", 
      value = input$filtertype)
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input$gamesplayed, {
    .generateStatement(
      session = session, 
      object = "gamesplayed", 
      verb = "interacted", 
      description = "Filtering data on GamesPlayed.", 
      value = paste(input$gamesplayed, 
                    collapse = ", ")
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input$FTA1, {
    .generateStatement(
      session = session, 
      object = "FTA1", 
      verb = "interacted", 
      description = "Filtering data on FreeThrowAttempts.", 
      value = paste(input$FTA1,
                    collapse = ", ")
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input$go1, {
    updateTabItems(
      session = session, 
      inputId = "tabs", 
      selected = "filtering")
  })
  
  observeEvent(input$go2, {
    updateTabItems(
      session = session, 
      inputId = "tabs", 
      selected = "hypothesis_testing")
  })
  
  observeEvent(input$howToChooseNBA, {
    .generateStatement(
      session = session, 
      object = "howToChooseNBA", 
      verb = "interacted", 
      description = "Would you like to select a random player, or a player of your choice?", 
      value = input$howToChooseNBA
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input$player, {
    .generateStatement(
      session = session, 
      object = "player", 
      verb = "interacted", 
      description = "Select your player from the drop down list below:", 
      value = input$player
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input$rand, {
    .generateStatement(
      session = session, 
      object = "rand", 
      verb = "interacted", 
      description = "Selection made.", 
      value = paste(input$howToChooseNBA, 
                    input$player, collapse = ", ")
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input[['null.valNBA']], {
    .generateStatement(
      session = session, 
      object = "null.valNBA", 
      verb = "interacted", 
      description = "Select a value for the null hypothesis.", 
      value = input[['null.valNBA']]
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input[['samp.sizeNBA']], {
    .generateStatement(
      session = session, 
      object = "samp.sizeNBA", 
      verb = "interacted", 
      description = "Input the number of shots in the sample:", 
      value = input[['samp.sizeNBA']]
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input$resample, {
    .generateStatement(
      session = session, 
      object = "resample", 
      verb = "interacted", 
      description = "Selection made."
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input$iftestNBA, {
    .generateStatement(
      session = session, 
      object = "iftestNBA", 
      verb = "interacted", 
      description = "Show Hypothesis Test Output", 
      value = input$iftestNBA
    )
  }, 
  ignoreInit = TRUE)
  
  observeEvent(input$significancebounds, {
    .generateStatement(
      session = session, 
      object = "significancebounds", 
      verb = "interacted", 
      description = "Plot significance bounds", 
      value = input$significancebounds
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$tabs, {
    .generateStatement(
      session = session, 
      verb = "experienced", 
      description = paste0("Navigated to ", 
                           input$tabs, 
                           " tab.")
    )
  }, 
  ignoreInit = TRUE)
  
  player.select <- reactive({
    # Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
    # will be used in first app
    index1 <- which(((playerdata$FTA >= 1) * (playerdata$FTA <= 1000)) == 1)
    playerdata2 <- playerdata[index1, ]
    
    # Randomly select a player if it is random
    decision <- input$howToChooseNBA
    if (decision == "rand") {
      s1 <- playerdata2[sample(nrow(playerdata2), 1), ]
      name <- s1$Player
    }
    else {
      name <- input$player
    }
    
    # Random Button
    input$rand
    
    # If it is not random use the player that the user selected
    index <- which(playerdata2$Player == name)
    namedata <- playerdata2[index, ]
  })
  
  player.select2 <- reactive({
    # Filter the player data so that it only chooses players who played more than half of the games
    # will be used in second app
    playerdata2 <- playerdata %>% filter(G >= max(G) / 2)
    
    # Randomly select a player if it is random
    decision <- input$howToChooseNBA
    if (decision == "rand") {
      s1 <- playerdata2[sample(nrow(playerdata2), 1), ]
      name <- s1$Player
    }
    else {
      name <- input$player
    }
    
    # Random Button
    input$rand
    
    # If it is not random use the player that the user selected
    index <- which(playerdata2$Player == name)
    namedata <- playerdata2[index, ]
  })
  
  
  # This is a reactive element for how many shots will be simulated
  nNBA <- reactive({
    return(input$samp.sizeNBA)
  })
  
  #### This is a reactive element for what the user chooses for the null value####
  hNBA <- reactive({
    return(input$null.valNBA)
  })
  
  truepropNBA <- reactive({
    return(input$trueNBA)
  })
  
  
  ######
  # Output text for what the free throw percentage is for the player
  output$text1NBA <- renderUI({
    namedata <- player.select2()
    ftp <- namedata$FT / namedata$FTA
    
    p <- "p"
    withMathJax(h4(sprintf(
      "The true free throw proportion for %s is %f.", namedata$Player, round(ftp, 2)
    )))
  })
  
  # Output text for what the sampled free throw percentage is for the player
  output$text2NBA <- renderUI({
    namedata <- player.select2()
    phat <- phat()
    
    withMathJax(h4(sprintf(
      "The sampled free throw proportion ( \\(\\hat{p}\\) ) for %s is %f.", namedata$Player, round(phat, 2)
    )))
  })
  
  # Output text for the null hypothesis
  output$text3NBA <- renderText({
    namedata <- player.select2()
    h1 <- hNBA()
    paste("Test the hypothesis that the free throw percentage for ", namedata$Player, "is equal to", h1, "against a two-sided alternative.")
  })
  
  #### Output plot, the histogram for "filtering", part 1####
  output$histogramNBA <- renderPlot({
    validate(
      need(input$gamesplayed > 0,
           message = "Please input a valid number of games played"
      )
    )
    
    bballdata <- dataFilter()
    n <- nrow(bballdata)
    y <- numeric(length = n)
    
    # Calculates the free throw percentages for all the players and puts it into a variable
    # Produces NAN's for players that haven't taken any free throws
    # Doesn't matter for the Histogram though because the Histogram won't display the NaN's
    # I take out the NaN's in a different part where it is needed
    for (i in 1:n) {
      y[i] <- bballdata$FT[i] / bballdata$FTA[i]
    }
    
    # The actual histogram
    par(bg = "lightsteelblue")
    hist(y, xlab = "Free Throw Proportion", main = "Histogram of Free Throw Proportion", col = "firebrick")
  })
  
  
  #### make phat ####
  phat <- eventReactive(
    {
      input$resample
      input$samp.sizeNBA
    },
    {
      h0 <- hNBA()
      namedata <- player.select2()
      ftp <- namedata$FT / namedata$FTA
      n1 <- nNBA()
      true1 <- truepropNBA()
      phat <- 0
      sim1 <- rbinom(n = n1, size = 1, prob = ftp)
      
      for (i in 1:n1) {
        if (sim1[i] == 1) {
          phat <- phat + 1
        }
        else {
          phat <- phat
        }
      }
      phat <- phat / n1
    }
  )
  
  #### making event reactive to create non changing samp dist####
  temp2 <- eventReactive(input$rand, {
    h0 <- hNBA()
    namedata <- player.select2()
    ftp <- namedata$FT / namedata$FTA
    n1 <- nNBA()
    true1 <- truepropNBA()
    
    # sampling distribuion
    phat <- 0
    phats <- c()
    j <- 1
    for (j in 1:2000) {
      i <- 1
      sim1 <- rbinom(n = 40, size = 1, prob = ftp)
      phat <- 0
      for (i in 1:40) {
        if (sim1[i] == 1) {
          phat <- phat + 1
        }
        else {
          phat <- phat
        }
        i <- i + 1
      }
      phat <- phat / 40
      phats[j] <- phat
      j <- j + 1
    }
    # phats = rnorm(n=500, mean=ftp, sd=sqrt(ftp*(1-ftp)))
    
    data.frame(length = phats)
  })
  
  #### make conditional for significance bounds####
  # true2 <- eventReactive(input$resample, {
  #
  # })
  
  #### output plot of the plot in part 2, "Three proportions"####
  output$proportion2NBA <- renderPlot({
    validate(
      need(input$resample,
           message = 'Please finish choosing options, and click the "Sample!" button.'
      )
    )
    
    # input$resample
    h0 <- hNBA()
    namedata <- player.select2()
    ftp <- namedata$FT / namedata$FTA
    n1 <- nNBA()
    true1 <- truepropNBA()
    
    phat <- phat()
    options(digits = 6)
    dat <- round(playerdata$FT / playerdata$FTA, 6)
    dat <- as.numeric(na.omit(dat))
    
    stanerr1 <- sqrt(h0 * (1 - h0) / n1)
    z1 <- (phat - h0) / stanerr1
    z1 <- round(z1, digits = 3)
    
    # lower=ifelse(z1<0,max(dat[which(dat<(z1*stanerr1+h0))]),max(dat[which(dat<(h0-z1*stanerr1))]))
    # upper=ifelse(z1>0,min(dat[which(dat>(z1*stanerr1+h0))]),min(dat[which(dat>(h0-z1*stanerr1))]))
    if (input$significancebounds) {
      lower <- max(dat[which(dat < (h0 - 1.96 * stanerr1))])
      upper <- min(dat[which(dat > (h0 + 1.96 * stanerr1))])
      upper <- ifelse(upper == 1, .98, upper)
      lower <- ifelse(lower <= .25, .251, lower)
    }
    
    data <- melt(data.frame(
      p_hat = c(phat), p0 = c(h0),
      hypothesis = c("%made")
    ),
    variable_name = "p"
    )
    
    g <- ggplot(data, aes(x = hypothesis, y = value, fill = variable)) + geom_bar(position = "dodge", stat = "identity") + ylab("Proportion")
    g <- g + theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold")
    )
    g <- g + if (input$significancebounds) {
      geom_hline(aes(yintercept = lower), color = "black")
    } else {
      NULL
    }
    g <- g + if (input$significancebounds) {
      geom_hline(aes(yintercept = upper), color = "black")
    } else {
      NULL
    }
    g + scale_fill_discrete(
      name = "variables",
      breaks = c("p_hat", "p0"),
      labels = unname(TeX(c("\\hat{p}", "p_0")))
    )
  })
  
  
  #### hypothesis test output in part 2####
  output$testtableNBA <- renderTable({
    validate(
      need(input$samp.sizeNBA > 0,
           message = "Please input a valid number of shots"
      )
    )
    
    namedata <- player.select2()
    h1 <- hNBA()
    ftp <- namedata$FT / namedata$FTA
    n4 <- nNBA()
    phat <- phat()
    
    
    stanerr1 <- sqrt(h1 * (1 - h1) / n4)
    z1 <- (phat - h1) / stanerr1
    z1 <- round(z1, digits = 3)
    # paste(round(z1,digits = 3))
    # dat <- round(playerdata$FT / playerdata$FTA, 6)
    # dat <- as.numeric(na.omit(dat))
    
    # sd(dat) is # .141861
    
    if (phat > h1) {
      p1 <- pnorm(z1, lower.tail = FALSE) * 2
      # p1 = (sum(dat>min(dat[which((dat-h1)/.141861*sqrt(n4)>z1)]))/438) # one sided probability because the distribution is not symmetric (?) #this isnt right tho im pretty sure
      # p1 = sum(dat>min(dat[which(dat>phat)]))/438
      # actually, these are wrong because they arent affected by null changing
    } else {
      p1 <- pnorm(z1, lower.tail = TRUE) * 2
      # p1 = (sum(dat<max(dat[which((dat-h1)/.141861*sqrt(n4)<z1)]))/438) # one sided probability because the distribution is not symmetric (?)
      # p1 = sum(dat<max(dat[which(dat<phat)]))/438
      # these are wrong because they arent affected by null changing
    }
    
    if (input$iftestNBA) {
      ctable <- matrix(c(z1, p1), nrow = 1)
      colnames(ctable) <- c("z-statistic", "p-value")
      ctable
    }
  })
  
  # exact hypothesis test using binomial
  output$exactTesttableNBA <- renderTable({
    validate(
      need(input$samp.sizeNBA > 0,
           message = "Please input a valid number of shots"
      )
    )
    
    namedata <- player.select2()
    h1 <- hNBA()
    ftp <- namedata$FT / namedata$FTA
    n4 <- nNBA()
    phat <- phat()
    
    p <- binom.test(phat * n4, n4, h1)$p.value
    p <- round(p, digits = 4)
    if (input$iftestNBA) {
      ctable <- matrix(c(phat, p), nrow = 1)
      colnames(ctable) <- c("p-hat", "p-value")
      ctable
    }
  })
  
  output$samp.table <- renderTable({
    sample1 <- playerdata
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
