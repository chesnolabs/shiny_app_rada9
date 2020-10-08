library(shinyWidgets)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(data.table)
library(tidyr)
library(DT)
library(splitstackshape)
library(purrr)

library(extrafont)
library(extrafontdb)
library(forcats)
library(networkD3)

# Additional info and datasets
source("helper.R", encoding = "UTF-8")

# 1. UI ####
ui <- navbarPage("Як нардепи Ради-9 подають поправки і як їх приймають", position = "fixed-top",
                 theme=shinytheme("united"),
                 # tabPanel-1 ####
                 tabPanel(title = "Графік",
                          br(),
                          br(),
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('faction', 'Вибрати фракцію чи групу', 
                                          choices = unique(amends_by_mps_WO$factions), 
                                          #multiple = TRUE, 
                                          selected = "Батьківщина"),
                              # CSV UI 
                              downloadButton("downloadData", "Завантажити дані таблиці і графіка"),
                              br(),
                              br(),
                              h4("Оновлено 6 жовтня 2020 року"),
                              a(href = "https://www.rada.gov.ua/", 
                                "Дані персональних сторінок законодавчої активності з сайту Верховної Ради", target = "_blank")),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Топ-20 подавачів поправок, пофракційно",
                                                   br(),
                                                   plotly::plotlyOutput("plot", height = "800px")
                                                   # br(),   br(),
                                          ),
                                          tabPanel("Топ-10 нардепів, чиї правки найбільше приймали",
                                                   br(),
                                                   plotly::plotlyOutput("positive_amends_plot", height = "800px")
                                          ))))),
                 # tabPanel-2 ####
                 tabPanel(title = "Табличні дані",
                          br(),
                          br(),
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('faction_tab2', 
                                          'Вибрати фракцію чи групу', 
                                          choices = unique(amends_by_mps_WO$factions), 
                                          selected = "Батьківщина"),     
                              # CSV UI 
                              downloadButton("downloadData_table", "Завантажити дані таблиці і графіка"),
                              br(),
                              br(),
                              h4("Оновлено 6 жовтня 2020 року"),
                              a(href = "https://www.rada.gov.ua/",
                                "Дані персональних сторінок законодавчої активності з сайту Верховної Ради",
                                target = "_blank")),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Хто як подає поправки",
                                                   br(),
                                                   DT::DTOutput("table")),
                                          tabPanel("Розширена таблиця з № законопроектів, до яких подали поправки",
                                                   br(),
                                                   DT::DTOutput("table_zakon")
                                          ))))),
                 # tabPanel-3 ####
                 tabPanel(title = "Голосування",
                          br(),
                          br(),
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              # CSV UI 
                              downloadButton("downloadData_table_zakon226", "Завантажити дані таблиці"),
                              br(),
                              br(),
                              h4("Оновлено 6 жовтня 2020 року"),
                              a(href = "https://www.rada.gov.ua/",
                                "Дані персональних сторінок законодавчої активності з сайту Верховної Ради",
                                target = "_blank")),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          # 3.1 226 votes ####
                                          tabPanel("Поправки 226 голосів",
                                                   h4(strong(paste("Усього відбулося", length(act_amends$id_event), 
                                                                   "голосувань за поправки, які набрали 226 голосів"))),
                                                   br(),
                                                   plotOutput('plot_amends', width = "950px", height = "450px"),
                                                   # br(),   br(),
                                                   DT::DTOutput("table_voting_226")),
                                          # 3.2 All votings ####
                                          tabPanel("Усі голосування",
                                                   h4(strong(paste("Усього відбулося", length(amendments_voting$id_event),
                                                                   "голосувань за поправки"))),
                                                   br(),
                                                   plotOutput("plot_amends_all", width = "950px", height = "450px"),
                                                   DT::DTOutput("table_voting")
                                          ),
                                          # 3.3 Top bills by votes ####
                                          tabPanel("Законопроекти-лідери",
                                                   h4(strong(paste("Поправки, які набрали 226 голосів, відповідно до № законопроекту"))),
                                                   br(),
                                                   plotly::plotlyOutput("top_per_bill_226", width = "950px", height = "550px"),
                                                   br(),
                                                   br(),
                                                   h4(strong(paste("Голосування за всі поправки відповідно до № законопроекту"))),
                                                   plotly::plotlyOutput("top_per_bill", width = "950px", height = "550px")
                                          ))))),
                 
                 # tabPanel-4 ####
                 tabPanel(title = "Мережа",
                          br(),
                          br(),
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("amends_connection",
                                          "Кількість поданих спільно поправок",
                                          min = 1, 
                                          max = 15,
                                          value = 5),
                              # CSV UI 
                              br(),
                              br(),
                              h4("Оновлено 6 жовтня 2020 року"),
                              a(href = "https://www.rada.gov.ua/", 
                                "Дані персональних сторінок законодавчої активності з сайту Верховної Ради", target = "_blank")),
                            mainPanel(
                              forceNetworkOutput("network_amends")
                            ))),
                 
                 # tabPanel-5 ####
                 tabPanel(title = "Про інструмент",
                          br(),
                          br(),
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              h4("Оновлено 6 жовтня 2020 року"),
                              a(href = "https://www.rada.gov.ua/",
                                "Дані персональних сторінок законодавчої активності з сайту Верховної Ради",
                                target = "_blank")),
                            mainPanel(h4(strong("Інструмент моніторингу поправок народних депутатів Ради 9 скликання")),
                                      h5("Ви знаходитеся на сторінці бета-версії інструменту Руху ЧЕСНО по відстеженню поправок нардепів"),
                                      h5('З початку 2020 року Рух ЧЕСНО займається моніторингом поправок нардепів та роботою в комітетах'),
                                      h5("Для зручності ми створили цей інструмент, де кожен аналітик і журналіст"), 
                                      h5("може побачити у зручному форматі, як приймаються поправки нардепів, фракцій та груп"),
                                      h5("Якщо у Вас є побажання з приводу поліпшення інструменту, напишіть на o.stavniichuk@gmail.com"))
                          )))

