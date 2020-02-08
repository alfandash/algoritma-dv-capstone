shinyServer(function(input, output) {
    
    output$playerByPosition <- renderPlotly({
        data <- player_data %>%
            group_by(position) %>%
            summarise(total = n())
        
        data %>%
            plot_ly(labels = ~position, values = ~total, type = "pie") %>% 
            layout(title = "Distribution player position in FIFA 2020",
                   xaxis = list(showgrid = F, zeroline = F, showticklabels= F),
                   yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
    })
    
    output$playerOverall <- renderPlotly({
        player <- player_data %>% 
            select(short_name, overall, potential, club) %>% 
            arrange(desc(overall)) %>% 
            head(20)
        
        plot <- player %>% 
            ggplot(aes(x = reorder(short_name, overall), y = overall)) +
            geom_col(aes(fill = overall,
                         text = glue("Name: {short_name}
                           Club: {club}
                           Overall: {overall}
                           Potential: {potential}"))) +
            scale_fill_gradient(low = "pink",
                                high = "red") +
            coord_flip() +
            theme_minimal() +
            labs(y = "Overall Skill Index",
                 fill = "Skill Index",
                 title = "Top 20 Overall Skill FIFA 2020") +
            theme(axis.title.y = element_blank()) 
        
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$playerOverallByAge <- renderPlotly({
        player <- player_data %>% 
            select(overall) %>% 
            group_by(overall) %>% 
            summarise(total = n())
        plot <- player %>% 
            ggplot(aes(x = overall, y = total)) +
            geom_area(fill="#69b3a2", alpha=0.5) +
            geom_line(col="skyblue",
                      alpha=0.5,
                      aes(text = glue("Overall Skill: {overall}
                                      Total Players: {total}"))) +
            xlab("Overall Skill Index") +
            ylab("Total Players") +
            theme_minimal() +
            labs(title = "Distribution Skill Player By Age FIFA 2020")
        
        
        
        plotly <- ggplotly(plot, tooltip = "text")
        
    })
    
    output$leagueOverall <- renderPlotly({
        overallLeague <- player_data %>% 
            filter(international_reputation > 1) %>% 
            group_by(league) %>% 
            summarise(mean_overall = mean(overall)) %>% 
            arrange(desc(mean_overall)) %>% 
            head(10)
        
        plot <- overallLeague %>% 
            ggplot(aes(x = reorder(league, mean_overall), y = mean_overall)) +
            geom_col() +
            coord_flip() +
            theme_minimal()
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$playersNationality <- renderPlotly({
        playersNationality <- player_data %>% 
            group_by(nationality) %>% 
            summarise(total = n(),
                      average = as.integer(mean(overall))) %>% 
            arrange(desc(total)) %>% 
            head(20)
        
        plot <- playersNationality %>% 
            ggplot(aes(x = reorder(nationality, total), y = total)) +
            geom_col(aes(fill = nationality,
                         text = glue("Total Players: {total}
                                     Average Skill: {average}"))) +
            coord_flip() +
            theme_minimal() +
            theme(axis.title.y = element_blank(),
                  legend.position = "none") +
            labs(title = "Top 20 Nationality Player FIFA 2020")
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    
    output$totalPlayersBox <- renderValueBox({
        valueBox(
            nrow(player_data), "Total Players", icon = icon("users"),
            color = "blue"
        )
    })
    
    output$totalClubBox <- renderValueBox({
        valueBox(
            NROW(unique(player_data$club)), "Total Clubs", icon = icon("futbol"),
            color = "green"
        )
    })
    
    output$totalNationalityBox <- renderValueBox({
        valueBox(
            NROW(unique(player_data$nationality)), "Total Nationality", icon = icon("flag"),
            color = "red"
        )
    })
    
    output$totalPlayerByLeague <- renderValueBox({
        data <- player_data %>%
            filter(league == input$selectLeague & age <= input$age) %>%
            summarise(n())
        
        valueBox(
            data, "Total Players In League", icon = icon("users"),
            color = "blue"
        )
    })
    
    output$totalNationalityByLeague <- renderValueBox({
        data <- player_data %>%
            filter(league == input$selectLeague & age <= input$age) %>%
            distinct(nationality) %>% 
            summarise(n())
        
        valueBox(
            data, "Total Nationality In League", icon = icon("flag"),
            color = "red"
        )
    })
    
    output$totalClubByLeague <- renderValueBox({
        data <- player_data %>%
            filter(league == input$selectLeague & age <= input$age) %>%
            distinct(club) %>% 
            summarise(n())
        
        valueBox(
            data, "Total Club In League", icon = icon("futbol"),
            color = "green"
        )
    })
    
    output$averageSkillPositionByLeague <- renderPlotly({
        data <- player_data %>% 
            filter(league == input$selectLeague & age <= input$age)
        
        plot <- data %>% 
            ggplot(aes(x = reorder(position, overall), y = overall)) +
            geom_boxplot(fill="skyblue", alpha=0.5) +
            theme_minimal() +
            labs(title = "Overall Distribution",
                 x = "Position")
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$BestGkByLeague <- renderPlotly({
        data <- player_data %>% 
            filter(position == "goalkeeper" & league == input$selectLeague & age <= input$age) %>% 
            head(5)
        
        plot <- data %>% 
            ggplot(aes(x = reorder(short_name, overall), y = overall)) +
            geom_col(aes(fill = overall,
                         text = glue("Name: {short_name}
                           Club: {club}
                           Overall: {overall}
                           Potential: {potential}"))) +
            coord_flip(ylim = c(50,95)) +
            scale_fill_gradient(low = "skyblue",
                                high = "navy") +
            theme_minimal() +
            theme(axis.text.y=element_text(hjust=2)) +
            labs(title = "Best Goalkeeper",
                 y = "",
                 x = "")
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$BestDefenderByLeague <- renderPlotly({
        data <- player_data %>% 
            filter(position == "defender" & league == input$selectLeague & age <= input$age) %>% 
            head(5)
        
        plot <- data %>% 
            ggplot(aes(x = reorder(short_name, overall), y = overall)) +
            geom_col(aes(fill = overall,
                        text = glue("Name: {short_name}
                           Club: {club}
                           Overall: {overall}
                           Potential: {potential}"))) +
            coord_flip(ylim = c(50,95)) +
            scale_fill_gradient(low = "skyblue",
                                high = "navy") +
            theme_minimal() +
            theme(axis.text.y=element_text(hjust=2)) +
            labs(title = "Best Defender",
                 y = "",
                 x = "")
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$BestMidfielderByLeague <- renderPlotly({
        data <- player_data %>% 
            filter(position == "midfielder" & league == input$selectLeague & age <= input$age) %>% 
            head(5)
        
        plot <- data %>% 
            ggplot(aes(x = reorder(short_name, overall), y = overall)) +
            geom_col(aes(fill = overall,
                         text = glue("Name: {short_name}
                           Club: {club}
                           Overall: {overall}
                           Potential: {potential}"))) +
            coord_flip(ylim = c(50,95)) +
            scale_fill_gradient(low = "skyblue",
                                high = "navy") +
            theme_minimal() +
            theme(axis.text.y=element_text(hjust=2)) +
            labs(title = "Best Midfielder",
                 y = "",
                 x = "")
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$BestForwardByLeague <- renderPlotly({
        data <- player_data %>% 
            filter(position =="forward" & league == input$selectLeague & age <= input$age) %>% 
            head(5)
        
        plot <- data %>% 
            ggplot(aes(x = reorder(short_name, overall), y = overall)) +
            geom_col(aes(fill = overall,
                         text = glue("Name: {short_name}
                           Club: {club}
                           Overall: {overall}
                           Potential: {potential}"))) +
            coord_flip(ylim = c(50,95)) +
            scale_fill_gradient(low = "skyblue",
                                high = "navy") +
            theme_minimal() +
            theme(axis.text.y=element_text(hjust=2)) +
            labs(title = "Best Forward",
                 y = "",
                 x = "")
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    output$distributionNationalityByLeague <- renderLeaflet({
        df <- player_data %>% 
            filter(league == input$selectLeague & age <= input$age) %>%
            mutate(nationality = ifelse(nationality == "England", "United Kingdom",as.character(nationality))) %>% 
            group_by(nationality) %>% 
            summarise(total_player = n()) %>% 
            mutate(GEO = as.character(nationality),
                   VALUE = total_player,) %>% 
            select(GEO, VALUE)
        
        WorldCountry <- geojson_read("./data/GeoData/countries.geo.json", what = "sp")
        data_map <- WorldCountry[WorldCountry$name %in% df$GEO, ]
        data_map$value <- df$VALUE[match(data_map$name, df$GEO)]
        
        #set bin and color for choropleth map
        bins <- c(0,10,20,30,40,50,60,70,80,90,100,Inf)
        pal <- colorBin("YlOrRd", domain = df$VALUE, bins = bins)
        
        leaflet(data_map) %>%
            addTiles() %>%
            addPolygons(stroke = TRUE,
                        smoothFactor = 0.5,
                        fillOpacity=0.8,
                        color = "white",
                        fillColor = ~pal(data_map$value),
                        opacity = 0.7,
                        weight = 1,
                        popup = paste(data_map$name, "<br>",
                                      data_map$value, "Players"
                        )
            ) %>% 
            addLegend("topright", pal = pal, values = df$VALUE,
                      title = "Total Players",
                      labels = c(min(df$VALUE), max(df$VALUE)),
                      opacity = 0.6
            )
            
        
        
    })
    
    output$playerDatabase <- renderDataTable(player_data, options = list(
        pageLength = 20,
        scrollX = TRUE,
        scrollCollapse = TRUE
    ))
    
    
    filtered_club <- reactive({
        club_filltered <- player_data %>% 
            select(league, club) %>%
            filter(league == input$selectLeagueTeam)
        
        club_filltered
    })
    
    output$selectTeam <- renderUI({
        club_fill <- filtered_club()
        if (!is.null(club_fill)) {
            selectInput(
                inputId = "selectTeamTeams",
                label = "Select Team",
                choices = lapply(1:nrow(club_fill), function (x) {
                    return(club_fill$club[x])
                }))
        }
    })
    
    output$freeKick <- renderTable(width = '100%', {
        kicker <- player_data %>% 
            filter(club == input$selectTeamTeams) %>% 
            select(short_name, skill_fk_accuracy, power_shot_power, skill_curve, power_long_shots) %>% 
            rename("Name" = short_name,
                   "Accuracy" = skill_fk_accuracy,
                   "Power" = power_shot_power,
                   "Curve" = skill_curve,
                   "Long Shot" = power_long_shots) %>% 
            arrange(desc(Accuracy))
         head(kicker, n = 5)
    })
    output$penaltyKick <- renderTable(width = '100%', {
        kicker <- player_data %>% 
            filter(club == input$selectTeamTeams) %>% 
            select(short_name, mentality_penalties, power_shot_power, skill_curve, power_long_shots) %>% 
            rename("Name" = short_name,
                   "Penalties" = mentality_penalties,
                   "Power" = power_shot_power,
                   "Curve" = skill_curve,
                   "Long Shot" = power_long_shots) %>% 
            arrange(desc(Penalties))
        head(kicker, n = 5)
    })
    
    output$teamPlayers <- renderTable(width = '100%',{
        teamPlayers <- player_data %>% 
            filter(club == input$selectTeamTeams) %>% 
            select(team_jersey_number, short_name, overall, age, player_positions) %>% 
            rename("Jersey Number" = team_jersey_number,
                   Name = short_name,
                   Overall = overall,
                   Age = age,
                   Position = player_positions)
    
        head(teamPlayers, n = 20)
    })
    
    output$playersDistributionPlot <- renderPlotly({
        data <- player_data %>% 
            select(short_name,overall,age,club) %>% 
            filter(club == input$selectTeamTeams)
        
        plot <- data %>% 
            ggplot(aes(y = age, x = overall)) +
            geom_point(alpha=0.7, aes(text = glue("Name: {short_name},
                                                   Overall Skill: {overall}
                                                   Age: {age}"))) +
            scale_color_viridis(discrete=TRUE, guide=FALSE) +
            theme_minimal() +
            labs(title = "Player Skill and Age Distribution",
                 x = "Overall Skill",
                 y = "Age")
        
        plotly <- ggplotly(plot, tooltip = "text")
    })
    
    ## observe the button being pressed
    shinyjs::hide(id = "boxPlayerListTeam")
    shinyjs::hide(id = "tabBoxTeam")
    shinyjs::hide("playersDistributionPlot")
    observeEvent(input$buttonSelectTeam, {
        if(!input$selectTeamTeams == "") {
            shinyjs::show(id = "boxPlayerListTeam")
            shinyjs::show(id = "tabBoxTeam")
            shinyjs::show("playersDistributionPlot")
        }
    })
    
})
