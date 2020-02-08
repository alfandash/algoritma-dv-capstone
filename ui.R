

#sidebar
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Home", icon = icon("home"),
                 tabName = "home"),
        menuItem(text = "League", icon = icon("align-justify"),
                 tabName = "league"),
        menuItem(text = "Team", icon = icon("users"),
                 tabName = "team"),
        menuItem(text = "Player Data", icon = icon("user-plus"),
                 tabName = "player_data")
    )
)

#tabPlayerData
fluidPlayerData <- fluidRow(
    box(width = 12, title = "Player Database", solidHeader = T, status = "primary",
        tabsetPanel(
            tabPanel("Summary",
                     dataTableOutput(outputId = "playerDatabase")
                     )
        )
    )
)

#tabHome
fluidHome <- fluidRow(
    valueBoxOutput("totalPlayersBox"),
    valueBoxOutput("totalClubBox"),
    valueBoxOutput("totalNationalityBox"),
    box( width = 12, title = "Summary", solidHeader = T, status = "primary",
        tabBox( width = 12,
                tabPanel("Best Player Overall Skill",
                         plotlyOutput(outputId = "playerOverall")
                ),
                tabPanel("Player Nationality",
                         plotlyOutput(outputId = "playersNationality"),
                ),
                tabPanel("Distribution Skill With Age",
                         plotlyOutput(outputId = "playerOverallByAge"),
                ),
                tabPanel("Distribution Player Position",
                         plotlyOutput(outputId = "playerByPosition")
                )
        )
    )
)

fluidLeague <- fluidRow(
    box( width = 12, title = "League Status & Data", solidHeader = T, status = "primary",
         box( width = 12,
              column(width = 6,
                     selectInput(
                         inputId = "selectLeague",
                         label = "Select League",
                         choices = lapply(1:nrow(player_league), function (x) {
                             return(player_league$league[x])
                         }))),
              column(width = 6,
                     sliderInput(inputId = "age",
                                 label = "Player Age",
                                 min = min(data$age),
                                 max = max(data$age),
                                 value = max(data$age))
                     )
         ),
         box(width = 12,
            ),
         valueBoxOutput("totalPlayerByLeague"),
         valueBoxOutput("totalNationalityByLeague"),
         valueBoxOutput("totalClubByLeague"),
         box( width = 6,
              plotlyOutput(outputId = "averageSkillPositionByLeague")
         ),
         box( width = 6,
              leafletOutput(outputId = "distributionNationalityByLeague")
         ),
         tabBox(width = 12,
                tabPanel("Goalkeeper",
                         plotlyOutput(outputId = "BestGkByLeague")
                         ),
                tabPanel("Defender",
                         plotlyOutput(outputId = "BestDefenderByLeague")
                         ),
                tabPanel("Midfielder",
                         plotlyOutput(outputId = "BestMidfielderByLeague")
                ),
                tabPanel("Forward",
                         plotlyOutput(outputId = "BestForwardByLeague")
                )
         )
    )
)

fluidTeam <- fluidRow(
    box(width = 12, title = "Team", solidHeader = T, status = "primary",
        box(width = 12,
            selectInput(
                inputId = "selectLeagueTeam",
                label = "Select League",
                choices = lapply(1:nrow(player_league), function (x) {
                    return(player_league$league[x])
                })),
            uiOutput("selectTeam"),
            actionButton("buttonSelectTeam", label = "Select")
            ),
        
        tabBox(width = 12, id = "tabBoxTeam",
               tabPanel("Player Skill Distribution",
                        plotlyOutput(outputId = "playersDistributionPlot")
                        ),
               tabPanel("Best kicker",
                        column(width=6,
                               h3("Best freekick players"),
                               tableOutput('freeKick')
                               ),
                        column(width=6,
                               h3("Best penalty players"),
                               tableOutput('penaltyKick')
                               )
                        )
               ),
        box(id = "boxPlayerListTeam",width = 12,
                h3("Team Players"),
                tableOutput('teamPlayers')
            )
    )
)

#body
body <- dashboardBody(
    useShinyjs(),
    tabItems(tabItem(tabName = "home", fluidHome),
             tabItem(tabName = "league", fluidLeague),
             tabItem(tabName = "player_data", fluidPlayerData),
             tabItem(tabName = "team", fluidTeam)
             )
)

page <- dashboardPage(
    dashboardHeader(title = "FIFA 2020"),
    sidebar,
    body
)


shinyUI(page)
