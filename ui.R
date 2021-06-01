setwd("/Users/rogercusco/Documents/ShinyMobileGaming")

library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)
library(rhandsontable)


header <- dashboardHeader(title = HTML("A <strong>Product Manager</strong>'s Data Lab for Mobile Gaming"), titleWidth=550)
  
sidebar <-   dashboardSidebar(
    sidebarMenu(
     menuItem("Welcome", tabName = "welcome", icon = icon("gamepad")),
     menuItem("Basics", tabName = "basics", icon = icon("user-graduate")),
     menuItem("Marketing", tabName = "marketing", icon = icon("money")),
     menuItem("Revenue", tabName = "revenue", icon = icon("gamepad")),
     menuItem("Business Model", tabName = "model", icon = icon("chart-bar")),
     menuItem("About", tabName = "about", icon = icon("info-circle"))
   ), width=150)
  
body <-  dashboardBody(
            tabItems(
               tabItem(tabName = "welcome",
                            box(width=12, background='light-blue', column(11, box(width=12, br(),br(),
                                 h1("Welcome to the Product Manager's Data Lab", style = "font-family: 'Source Sans Pro'; 
                                                                                            font-style: normal; 
                                                                                            font-weight: 230;
                                                                                            font-size: 50px"),br(),
                                 h4("Creating successful games is both an art and a science. An extremely creative endeavor, but also 
                                 a highly analytical and data-driven process. This Shiny application aims at unravelling the science, so that you can focus on the art. The goal is to 
                                 provide an intuition for the major dynamics behind the core KPI in the industry. ", style="font-family: 'Source Sans Pro'; 
                                                                                            font-style: normal; 
                                                                                            font-weight: 230;
                                                                                            font-size: 20px"), br(),                                                
                                 background='light-blue', tags$div(tags$ul(
                                   tags$li("What KPI do I need to turn my game into a profitable business?"),
                                   tags$li("How do I interpret the data from my marketing campaigns?"),
                                   tags$li("How reliable are my KPI?")),  style = "font-family: 'Source Sans Pro'; 
                                                                                            font-style: normal; 
                                                                                            font-weight: 230;
                                                                                            font-size: 20px"),
                                 
                                 br()
                                  ))),
                 fluidRow(column(6, box(width=12, background='light-blue',
                                    div(img(src="mobileplay.png", height='51%', width='51%'), style="text-align: center;"), br(),br())), 
                          column(6, box(width=12, status = "primary", title="App Developer", solidHeader = TRUE, 
                                        column(6,h3("Roger Cuscó", style="font-weight: 600; font-size: 20px"), h4("I'm an Economist turned Data Scientist turned Product Manager, 
                                                                       currently working at Socialpoint. You can find me on:", style="font-size: 16px"),
                                               actionLink(inputId='ghlink', label= tags$a(href="https://www.linkedin.com/in/roger-cusc%C3%B3-939b278b/", "Linkedin", style='font-size: 16px'), icon=icon("linkedin", style='font-size: 16px')),br(),
                                               actionLink(inputId='ghlink', label= tags$a(href="https://github.com/rogercusco", "Github", style='font-size: 16px'), icon=icon("github", style='font-size: 16px')
                                               )),br(),
                                        column(6, div(img(src="profile_pic.png", height='70%', width='70%'), style="text-align: center;" ) , style='padding-bottom:15px; padding-right:0px;'))),
                          box(width=12, h5("check it out")))
                            #try renderImage on the server size for better auto resizing
                           ),
               tabItem(tabName = "basics",
                       fluidRow(column(12, box(width=12, background='purple', column(11, box(width=12,h1("The Basics", style = "font-family: 'Source Sans Pro'; 
                                                                                            font-style: normal; 
                                                                                            font-weight: 230;
                                                                                            font-size: 40px"),
                                                                                                 br(),
                                h4(
                                   "Modern mobile gaming companies are essentially SaaS companies with a freemium product strategy. The customer journey is simple:",br(),br(),
                                   tags$div(tags$ul(
                                     tags$li("Potential customers download a game for free."),
                                     tags$li("They use it for a while."),
                                     tags$li("If they are enjoying it, they might upgrade their experience by paying for premium content."),
                                     tags$li("And at some point they stop playing.")),  style = "font-family: 'Source Sans Pro'; 
                                   font-style: normal; 
                                   font-weight: 230;
                                   font-size: 20px"),
                                   "All the KPI (Key Performanc Indicators) 
                                   we will discuss in this page are the numbers used in the industry to synthesize how successful a particular game is in maximizing 
                                   the value of all potential customers that go through that journey.", br(), br(),
                                   "Now, there are a few key features of a FTP gaming business that have subtle but profound implications in how we model 
                                   and interpret its KPI: ",br(), br(),tags$div(tags$ul(
                                     tags$li("Customer spending is normally uncapped."),
                                     tags$li("Only a small minority of players end up spending money in the game"),
                                     tags$li("Only a small minority of players end up playing a game for more than a few days or weeks.")),  style = "font-family: 'Source Sans Pro'; 
                                                                                            font-style: normal; 
                                                                                            font-weight: 230;
                                                                                            font-size: 20px"),
                                    style = "font-family: 'Source Sans Pro'; 
                                             font-style: normal; 
                                             font-weight: 280;
                                             font-size: 19px"), background='purple'))),
                                       style='padding:0px;'))
                       ),
               tabItem(tabName = "revenue",
                  fluidRow(
                      column(4, box(width = 12, status = "primary", solidHeader = TRUE, title= "Input Marketing Metrics",
                               column(6, numericInput("Marketing investment", label = h4("D1"), value = 1)),
                               column(6, numericInput("CPI - Cost per install", label = h4("D7"), value = 1))
                               )),
                              ),
                  # fluidRow(column(2, align = T, box(width = 12, status = "primary", solidHeader = TRUE, title= "Input LTV",
                  #                        rHandsontableOutput("hot_ltv")                ),
                                 # ),
                           
                         ),
               tabItem(tabName = "model",
                       fluidRow(column(12, box(width=12, background='light-blue', column(11, box(width=12,h1("A Simple Business Model", style = "font-family: 'Source Sans Pro'; 
                                                                                            font-style: normal; 
                                                                                            font-weight: 230;
                                                                                            font-size: 40px"),
                                                                                                 br(),
                                                h4("Input your business metrics on the left to obtain your business model on the right. This is a very simplified model that has some severe limitations, namely:",  style = "font-family: 'Source Sans Pro'; 
                                                                                            font-style: normal; 
                                                                                            font-weight: 250;
                                                                                            font-size: 16px"),
                                                tags$div(tags$ul(
                                                  tags$li("CPI does not change dynamically with the volume of Marketing investment."),
                                                  tags$li("LTV and Retention are modeled independently. Updating retention data does not affect LTV."),
                                                  tags$li("In general, all inputs are static and do not change over time.")),  style = "font-family: 'Source Sans Pro'; 
                                                                                            font-style: normal; 
                                                                                            font-weight: 250;
                                                                                            font-size: 16px"), background='light-blue'))),
                                                style='padding:0px;')),
                       fluidRow(
                       # INPUTS COLUMN
                       column(6, box(width=12, status = "warning", solidHeader= TRUE, title="Inputs",
                                     box(width = 12, status="warning", title= "Input Your Lifetime Value",
                                         column(4,
                                             rHandsontableOutput("hot_ltv")),
                                         column(8, plotOutput('ltvPlot', height=220))),
                                    box(width = 12, status="warning", title= "Input Your Retention Numbers",
                                        column(4,
                                               rHandsontableOutput("hot")),
                                        column(8, plotOutput('retPlot', height=220))),
                                    box(width = 12, status="warning", tite="Input Your Marketing Numbers", 
                                        column(6, numericInput("mkg_inv", label = h4("How much will you invest in Marketing per Month?"), value = 20000)),
                                        column(6, numericInput("cpi_inpu", label = h4("What is your CPI (Cost per install ratio)?"), value = 2)),
                                        column(6, numericInput("kfactor", label = h4("What is your k-factor (ratio of organic to marketing installs)?"), value = 1.5))),
                                    box(width = 12, status="warning", tite="Input Your Financial Numbers", 
                                        column(6, numericInput("init_capital", label = h4("How much money do you have in the bank?"), value = 10000)),
                                        column(6, numericInput("opex_m", label = h4("How much cash do you spend every month (aside from Marketing?"), value = 30000)))
                                ), style='padding:0px;'),
                       # OUTPUT COLUMN
                       column(6, box(width=12, status = "primary", solidHeader= TRUE, title="Outputs",
                                     box(width = 12, status="primary", title= "Projected Income Statement for the first 2 Years:", plotlyOutput("PLTable", height = "180px")),
                                     box(width = 12, status="primary", title= "Projected Cash-flow evolution",textOutput("months"),br(), textOutput("capital"),
                                                                                                              plotlyOutput("cashPlot", height=350)),
                                     box(width = 12, status="primary", tite="",
                                         column(6,plotlyOutput("dausPlot", height=220), style='padding:0px;'),
                                         column(6,plotlyOutput("nguPlot", height=220), style='padding:0px;')
                               )), style='padding:0px;')
               )),
              tabItem(tabName = "marketingl",
                      h3("hello nanu"),
                  fluidRow(
                    plotlyOutput("p"),
                    verbatimTextOutput("event")
                          )),
    tabItem(tabName = "about",
            fluidPage(
              tags$iframe(src = "about.html", 
                          width = '100%', height = '800px',
                          frameborder = 0, scrolling = 'auto'
              )))))

ui <- dashboardPage(header, sidebar, body, skin = "purple")