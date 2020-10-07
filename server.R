

# 2.SERVER ####
server <- function(input, output) {

  # Tab 1.1 Reactive ####
  #Reactive table for a plot 
  reactive_plot <- reactive({ 
    
    amends_by_mps_WO%>% 
      group_by(factions) %>% 
      top_n(20, totally) %>%
      ungroup()%>% 
      #gather(type, number, accepted:no_conclusion, factor_key = TRUE) %>% 
      mutate(factions=as.factor(factions)) %>% 
      pivot_longer(-factions:-totally, names_to = "type", values_to = "number") %>%
      ungroup()%>% 
      mutate(type=as.factor(type)) %>% 
      arrange(desc(totally, factions))%>%
      filter(factions==input$faction) %>% 
      mutate(type=recode(type,
                         "accepted"="Враховані",
                         "partly_accepted"="Частково враховані",
                         "rejected"="Відхилені",
                         "redakciyno_accepted"="Редакційно враховані",
                         "others"="Інше",
                         "no_conclusion"="Немає висновку"))
    
    
  })
  # Tab 1.1 Plot ####
  
  output$plot <-  plotly::renderPlotly ({
    
    # Plot ####
    reactive_plot() %>% 
      ggplot(aes(x=reorder(short_name, number),y=number, fill=type))+
      geom_col(position = position_stack(reverse = TRUE))+
      #ggtitle("Топ-20 подавачів поправок, пофракційно")+
      coord_flip()+
      scale_fill_manual(labels = c("Враховані", 
                                   "Частково враховані", 
                                   "Відхилені", 
                                   "Редакційно враховані", 
                                   "Інше", 
                                   "Немає висновку"),
                        values = c("Враховані" = "#452571", # #583479 #452571
                                   "Частково враховані" = "#e5e500", # #f39200  #e5e500 #E5E5E5
                                   "Відхилені" = "#FC3F1B", #firebrick 
                                   "Редакційно враховані" = "#E5E5E5", #
                                   "Інше" = "#999999",
                                   "Немає висновку"="#333333"))+ # #333333
      theme(text = element_text(family = "OpenSans", size=30),
            plot.caption = element_text(hjust = 1, 
                                        size=18,
                                        colour = "darkgrey",
                                        family = "OpenSans"),
            panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title=element_blank(),
            legend.text = element_text( family = "OpenSans", size=12),
            legend.background = element_blank(),
            strip.background=element_blank(),
            axis.text.x = element_text(size = 12, colour = "black", family = "OpenSans"), #цифри внизу #201d41
            axis.text.y = element_text(size = 12, colour = "black", family = "OpenSans"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            #plot.margin = margin(1, 1, 1, 1, "cm"),
            plot.background =  element_rect(fill = "white"),
            panel.background= element_rect(fill = "white", color = "white"))
    
    
    
  })
  
  #  # Tab 1.2 Reactive ####
  # Чиї поправки найбільше приймалися (не у відсотках)
  rea_positive <- reactive({
    
    amends_by_mps_WO %>%
      select(-rejected,-others,-no_conclusion) %>% 
      group_by(factions) %>% 
      top_n(20, accepted) %>%
      ungroup()%>% 
      #gather(type, number, accepted:no_conclusion, factor_key = TRUE) %>% 
      mutate(factions=as.factor(factions)) %>% 
      filter(factions==input$faction) %>% 
      arrange(desc(accepted, factions))%>%
      pivot_longer(-factions:-totally, names_to = "type", values_to = "number") %>%
      ungroup()%>% 
      mutate(type=as.factor(type)) %>% 
      mutate(type=recode(type,
                         "accepted"="Враховані",
                         "partly_accepted"="Частково враховані",
                         "redakciyno_accepted"="Редакційно враховані"))
  })
  
  #  # Tab 1.2 Plot ####
  output$positive_amends_plot <-  plotly::renderPlotly ({
    
    rea_positive() %>% 
      ggplot(aes(x=reorder(short_name, number),y=number, fill=type))+
      geom_col(position = position_stack(reverse = TRUE))+
      coord_flip()+
      scale_fill_manual(
        values = c("Враховані" = "#452571", # #583479 #452571
                   "Частково враховані" = "#e5e500", # #f39200  #e5e500 #E5E5E5
                   "Редакційно враховані" = "#E5E5E5"))+ 
      theme(text = element_text(family = "OpenSans", size=30),
            plot.caption = element_text(hjust = 1, 
                                        size=18,
                                        colour = "darkgrey",
                                        family = "OpenSans"),
            panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title=element_blank(),
            legend.text = element_text( family = "OpenSans", size=12),
            legend.background = element_blank(),
            strip.background=element_blank(),
            axis.text.x = element_text(size = 12, colour = "black", family = "OpenSans"), #цифри внизу #201d41
            axis.text.y = element_text(size = 12, colour = "black", family = "OpenSans"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            #plot.margin = margin(1, 1, 1, 1, "cm"),
            plot.background =  element_rect(fill = "white"),
            panel.background= element_rect(fill = "white", color = "white"))
  })
  
  
  #  Tab 2.1 Reactive ####
  reactive_table <- reactive({ 
    
    amends_by_mps_WO %>%
      filter(factions==input$faction_tab2) %>%  # where to put this
      rename(
        "Враховані"="accepted",
        "Частково враховані"= "partly_accepted",
        "Відхилені"="rejected",
        "Редакційно враховані"="redakciyno_accepted",
        "Інше"="others",
        "Немає висновку"="no_conclusion",
        "Усього"="totally",
        "Ім'я і прізвище"="short_name",
        "Фракція"="factions",
        "По-батькові"="fathername",
        #"№ ЗП"="number",
        "Область"="region_name") 
    
  })
  
  # Tab 2.1 Table ####
  output$table <- DT::renderDT({
    
    reactive_table() %>% 
      # Change names for buttons
      DT::datatable(filter = 'top',
                    extensions = "Buttons", 
                    options = list( language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
    
  })
  
  # Tab 2.2 Reactive  ####
  reactive_table_zakon <- reactive({
    
    amends_by_mps %>% 
      filter(factions==input$faction_tab2) %>% 
      rename(
        "Враховані"="accepted",
        "Частково враховані"= "partly_accepted",
        "Відхилені"="rejected",
        "Редакційно враховані"="redakciyno_accepted",
        "Інше"="others",
        "Немає висновку"="no_conclusion",
        "Усього"="totally",
        "Ім'я і прізвище"="short_name",
        "Фракція"="factions",
        "По-батькові"="fathername",
        "№ ЗП"="number",
        "Область"="region_name") 
  })
  
  # Tab 2.2 Table ####
  output$table_zakon <- DT::renderDT({
    
    reactive_table_zakon() %>% 
      #formatStyle( color = 'red', backgroundColor = 'orange', fontWeight = 'bold') %>% 
      # Change names for buttons
      # https://rstudio.github.io/DT/004-i18n.html
      DT::datatable(filter = 'top',
                    style = 'bootstrap',
                    options = list( 
                      language = list(fixedHeader = TRUE, url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
  })
  
  
  #
  
  
  
  
  
  # Tab 3-1 reactive ####
  # Reactive graph for amends personal voting 
  reactive_plot_amends_voting <- reactive({ 
    
    out_amends%>%
      filter(for.>225) %>% 
      group_by(factions)%>%
      summarise(
        # У відсотках
        vote_for_perc = round(mean(vote_status == "За")*100, 1), 
        vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
        vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
        vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
        vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
        vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
      mutate(factions=as.factor(factions))%>%
      arrange(vote_for_perc, factions) %>% 
      mutate(factions = fct_reorder(factions, levels(factions))) %>% 
      gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
      filter(!status=="vote_present_perc") %>% 
      mutate(status=recode(status,
                           "vote_for_perc"="За",
                           "vote_abstain_perc"="Утрималися",
                           "vote_against_perc"="Проти",
                           "vote_not_voting_perc"="Не голосували",
                           "vote_absent_perc"="Відсутні"))
    
    
  })
  
  # Tab 3-1 Plot ####
  
  output$plot_amends <-  renderPlot ({
    
    reactive_plot_amends_voting() %>% 
      ggplot(aes(x=factions, y=n_vote, fill=status))+
      geom_col(position = position_stack(reverse = TRUE))+
      coord_flip()+
      ggtitle("Розподіл голосувань за поправки до законопроектів, у % ")+
      scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                        values = c("За" = "#452571", 
                                   "Утрималися" = "#e5e500",
                                   "Проти" = "#FC3F1B", 
                                   "Не голосували" = "#e8e8e8",
                                   "Відсутні" = "#333333"))+
      theme(text = element_text(family = "OpenSans", size=24, colour = "black" ),
            plot.caption = element_text(hjust = 1, 
                                        #face = "italic", 
                                        color="grey", 
                                        size=14, family = "OpenSans"),
            panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title=element_blank(),
            legend.text = element_text(family = "OpenSans", size = 18, colour = "black"),
            legend.background = element_blank(),
            strip.background=element_blank(),
            #strip.text.x =element_text(size = 22),  # Facets names
            axis.text.x = element_text(size = 18,  family = "OpenSans", colour = "black"), #цифри внизу#201d41
            axis.text.y = element_text(size = 18,  family = "OpenSans", colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(1, 1, 1, 1, "cm"),
            plot.background =  element_rect(fill = "white"),
            panel.background= element_rect(fill = "white", color = "white"))
    
  })
  
  
  # Tab 3-2 reactive ####
  reactive_plot_amends_all <- reactive({    
    
    out_amends %>% # ######################################################################################
    filter(for.>225) %>% 
      group_by(factions)%>%
      summarise(
        # У відсотках
        vote_for_perc = round(mean(vote_status == "За")*100, 1), 
        vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
        vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
        vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
        vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
        vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
      mutate(factions=as.factor(factions))%>%
      arrange(vote_for_perc, factions) %>% 
      mutate(factions = fct_reorder(factions, levels(factions))) %>% 
      gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
      filter(!status=="vote_present_perc") %>% 
      mutate(status=recode(status,
                           "vote_for_perc"="За",
                           "vote_abstain_perc"="Утрималися",
                           "vote_against_perc"="Проти",
                           "vote_not_voting_perc"="Не голосували",
                           "vote_absent_perc"="Відсутні"))
    
    
    
  })
  
  # Tab 3-2 plot all amends ####
  
  output$plot_amends_all <-  renderPlot ({
    
    reactive_plot_amends_all() %>% 
      ggplot(aes(x=factions, y=n_vote, fill=status))+
      geom_col(position = position_stack(reverse = TRUE))+
      coord_flip()+
      ggtitle("Усі голосування за поправки у %")+
      scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                        values = c("За" = "#452571", 
                                   "Утрималися" = "#e5e500",
                                   "Проти" = "#FC3F1B", 
                                   "Не голосували" = "#e8e8e8",
                                   "Відсутні" = "#333333"))+
      theme(text = element_text(family = "OpenSans", size=24, colour = "black" ),
            plot.caption = element_text(hjust = 1, 
                                        #face = "italic", 
                                        color="grey", 
                                        size=14, family = "OpenSans"),
            panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title=element_blank(),
            legend.text = element_text(family = "OpenSans", size = 18, colour = "black"),
            legend.background = element_blank(),
            strip.background=element_blank(),
            #strip.text.x =element_text(size = 22),  # Facets names
            axis.text.x = element_text(size = 18,  family = "OpenSans", colour = "black"), #цифри внизу#201d41
            axis.text.y = element_text(size = 18,  family = "OpenSans", colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(1, 1, 1, 1, "cm"),
            plot.background =  element_rect(fill = "white"),
            panel.background= element_rect(fill = "white", color = "white"))
    
    
  })
  
  # Tab 3-3 reactive ####
  
  table_amends226_mps <- reactive({
    
    out_amends %>%
      filter(for.>225) %>% 
      group_by(fullname, factions)%>%
      summarise(vote_for = sum(vote_status == "За"), 
                vote_abstain = sum(vote_status == "Утримався"),
                vote_against_ = sum(vote_status == "Проти"), 
                vote_present = sum(vote_status == "Присутній"),
                vote_not_voting = sum(vote_status == "Не голосував"),
                vote_absent = sum(vote_status == "Відсутній")) %>% 
      arrange(vote_for, fullname) %>% 
      #dplyr::ungroup() %>% 
      select(-vote_present) %>% 
      filter(!is.na(fullname)) %>% 
      separate(fullname, c("surname","name","fathername"), sep = " ")%>%
      unite(short_name, c("name","surname"), sep = " ")  %>%  
      #filter(!vote_status=="vote_present") %>%  
      #filter(factions==input$faction) %>% 
      rename(
        "За"="vote_for",
        "Утрималися"="vote_abstain",
        "Проти"="vote_against_",
        #"Не голосували"="vote_present",
        "Не голосували"="vote_not_voting",
        "Відсутні"="vote_absent",
        "Ім'я"="short_name",
        "По-батькові"="fathername",
        "Фракція"="factions")
    
  })
  
  
  # Tab 2 TABLE 226 ####
  output$table_voting_226 <- DT::renderDT({
    
    table_amends226_mps() %>% 
      # Change names for buttons
      # https://rstudio.github.io/DT/004-i18n.html
      DT::datatable(filter = 'top',
                    style = 'bootstrap',
                    #extensions = "Buttons", 
                    options = list( 
                      language = list(fixedHeader = TRUE, url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
    
    
  })
  
  # Tab 4   Reactive data prep all amends TABLE ####
  
  table_amends_mps <- reactive({
    
    out_amends %>% 
      group_by(fullname, factions)%>%
      summarise(vote_for = sum(vote_status == "За"), 
                vote_abstain = sum(vote_status == "Утримався"),
                vote_against_ = sum(vote_status == "Проти"), 
                vote_present = sum(vote_status == "Присутній"),
                vote_not_voting = sum(vote_status == "Не голосував"),
                vote_absent = sum(vote_status == "Відсутній")) %>% 
      arrange(vote_for, fullname) %>% 
      #dplyr::ungroup() %>% 
      select(-vote_present) %>% 
      filter(!is.na(fullname)) %>% 
      separate(fullname, c("surname","name","fathername"), sep = " ")%>%
      unite(short_name, c("name","surname"), sep = " ")  %>%  
      #filter(!vote_status=="vote_present") %>%  
      #filter(factions==input$faction) %>% 
      rename(
        "За"="vote_for",
        "Утрималися"="vote_abstain",
        "Проти"="vote_against_",
        #"Не голосували"="vote_present",
        "Не голосували"="vote_not_voting",
        "Відсутні"="vote_absent",
        "Ім'я"="short_name",
        "По-батькові"="fathername",
        "Фракція"="factions")
  })
  
  
  # Reactive table for plot plot_amends_all ####
  reactive_plot_amends_226 <- reactive({    
    
    out_amends %>%
      group_by(factions)%>%
      summarise(
        # У відсотках
        vote_for_perc = round(mean(vote_status == "За")*100, 1), 
        vote_abstain_perc = round(mean(vote_status == "Утримався")*100, 1),
        vote_against_perc = round(mean(vote_status == "Проти")*100, 1), 
        vote_present_perc = round(mean(vote_status == "Присутній")*100, 1),
        vote_not_voting_perc = round(mean(vote_status == "Не голосував")*100, 1),
        vote_absent_perc = round(mean(vote_status == "Відсутній")*100, 1)) %>% 
      mutate(factions=as.factor(factions))%>%
      arrange(vote_for_perc, factions) %>% 
      mutate(factions = fct_reorder(factions, levels(factions))) %>% 
      gather(status, n_vote, vote_for_perc:vote_absent_perc, factor_key = TRUE)%>%
      filter(!status=="vote_present_perc") %>% 
      mutate(status=recode(status,
                           "vote_for_perc"="За",
                           "vote_abstain_perc"="Утрималися",
                           "vote_against_perc"="Проти",
                           "vote_not_voting_perc"="Не голосували",
                           "vote_absent_perc"="Відсутні"))
    
    
    
  })
  
  
  # Render plot all amends #### vote_status
  
  output$plot_amends_all <-  renderPlot ({
    
    reactive_plot_amends_226()%>% 
      ggplot(aes(x=factions, y=n_vote, fill=status))+
      geom_col(position = position_stack(reverse = TRUE))+
      coord_flip()+
      ggtitle("Усі голосування за поправки у %")+
      scale_fill_manual(labels = c("За", "Утрималися", "Проти", "Не голосували", "Відсутні"),
                        values = c("За" = "#452571", 
                                   "Утрималися" = "#e5e500",
                                   "Проти" = "#FC3F1B", 
                                   "Не голосували" = "#e8e8e8",
                                   "Відсутні" = "#333333"))+
      theme(text = element_text(family = "OpenSans", size=24, colour = "black" ),
            plot.caption = element_text(hjust = 1, 
                                        #face = "italic", 
                                        color="grey", 
                                        size=14, family = "OpenSans"),
            panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title=element_blank(),
            legend.text = element_text(family = "OpenSans", size = 18, colour = "black"),
            legend.background = element_blank(),
            strip.background=element_blank(),
            #strip.text.x =element_text(size = 22),  # Facets names
            axis.text.x = element_text(size = 18,  family = "OpenSans", colour = "black"), #цифри внизу#201d41
            axis.text.y = element_text(size = 18,  family = "OpenSans", colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(1, 1, 1, 1, "cm"),
            plot.background =  element_rect(fill = "white"),
            panel.background= element_rect(fill = "white", color = "white"))
    
  })
  
  
  
  # Tab 4.2.b Render table amends ####
  
  output$table_voting <- DT::renderDT({
    
    table_amends_mps() %>% 
      # Change names for buttons
      # https://rstudio.github.io/DT/004-i18n.html
      DT::datatable(filter = 'top',
                    style = 'bootstrap',
                    #extensions = "Buttons", 
                    options = list( 
                      language = list(fixedHeader = TRUE, 
                                      url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json')))
    
  })
  
  # tab 5 ####
  # Лідери за к-стю поправок
  # Reactive prep for топ-голосування за закони
  top_bill_reac <- reactive({
    #n_amends
    # Фільтруємо більше 25 поправок ####
    amendments_voting %>% 
      group_by(number_question, name_question) %>% 
      summarise(n=n()) %>% 
      arrange(desc(n))%>% 
      filter(n>25)  # Прибрати фільтр, аби побачити повну картинку 
  })
  
  
  # Plot based on reactive prepareation top_bill_reac
  
  output$top_per_bill <- plotly::renderPlotly({
    
    top_bill_reac() %>% 
      ggplot(aes(x=reorder(number_question,n), n))+
      geom_col(fill="#452571", width = 0.7)+
      coord_flip()+
      #ggtitle("Кількість голосувань за поправки за всю каденцію Ради-9
      #Відповідно до законопроекту")+
      theme(text = element_text(family = "OpenSans", size=20  ),
            plot.caption = element_text(hjust = 1, 
                                        color="grey", 
                                        size=18,
                                        family = "OpenSans"),
            panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.major.y = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title=element_blank(),
            legend.text = element_text(family = "OpenSans", size = 14, colour = "black"),
            legend.background = element_blank(),
            strip.background=element_blank(),
            #strip.text.x =element_text(size = 22),  # Facets names
            axis.text.x = element_text(size = 14,  family = "OpenSans", colour = "black"), #цифри внизу#201d41
            axis.text.y = element_text(size = 14,  family = "OpenSans", colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(1, 1, 1, 2, "cm"),
            plot.background =  element_rect(fill = "white"),
            panel.background= element_rect(fill = "white", color = "white"))
    
    
    #ggplotly(p, tooltip = "number_question")
    
  })
  
  
  
  # tab 6 ####
  # Лідери за к-стю поправок
  # Reactive prep for топ-голосування за закони
  top_bill_226_reac <- reactive({
    
    amendments_voting %>%
      filter(for.>225)%>% 
      group_by(number_question, name_question) %>% 
      summarise(n=n())%>% 
      filter(n>=10) # Змінювати відповідно до ситуації
  })
  
  
  # Plot based on reactive prepareation top_bill_reac
  
  output$top_per_bill_226 <- plotly::renderPlotly({
    
    top_bill_226_reac() %>% 
      ggplot(aes(x=reorder(number_question, n), n))+
      geom_col(fill="#452571", width = 0.7)+
      coord_flip()+
      #ggtitle("Кількість голосувань за поправки за всю каденцію Ради-9
      # Відповідно до законопроекту")+
      theme(text = element_text(family = "OpenSans", size=20  ),
            plot.caption = element_text(hjust = 1, 
                                        color="grey", 
                                        size=18,
                                        family = "OpenSans"),
            panel.grid.major.x = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.major.y = element_line(colour = "antiquewhite4",linetype="dashed",size=0.5),
            panel.grid.minor.x = element_blank(),#Прибрала сітку грідс
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.title=element_blank(),
            legend.text = element_text(family = "OpenSans", size = 14, colour = "black"),
            legend.background = element_blank(),
            strip.background=element_blank(),
            #strip.text.x =element_text(size = 22),  # Facets names
            axis.text.x = element_text(size = 14,  family = "OpenSans", colour = "black"), #цифри внизу#201d41
            axis.text.y = element_text(size = 14,  family = "OpenSans", colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.margin = margin(1, 1, 1, 2, "cm"),
            plot.background =  element_rect(fill = "white"),
            panel.background= element_rect(fill = "white", color = "white"))
    
  })
  
  
  # Network Prep ####
  
  list_initiators <- read.csv("https://raw.githubusercontent.com/Okssana/shiny_app_network/master/nodes_amends_29_09_2020.csv", 
                              fileEncoding = "Windows-1251") %>%
    select(-X)
  
  edges_for_gephy <- read.csv("https://raw.githubusercontent.com/Okssana/shiny_app_network/master/edges_amends_29_09_2020.csv") %>% 
    select(-X)
  


    # Preparation 2 ####
  
  list.function <- reactive({
    
    d1 <- as.data.frame(list_initiators)
    d2 <- as.data.frame(edges_for_gephy)
    
    sapply(paste('d', seq(2,1), sep=''), get, environment(), simplify = FALSE) 
    
  })
  
  
  # ####
  
  # Filtering connections ####
  # 
  # Edges\links
  links1 <-reactive({

    my.list1_new <- as.data.frame(list.function()$d2)%>%
      filter(Value >= input$amends_connection) # %in% causes an error

  })
  # 
  # 
 
  # Network Render #### 
  output$network_amends <- renderForceNetwork({
    
    
    links <- links1()
    
    links_nodes <- list.function()$d1
    
    ## These three lines have to solve problem with zero-indexing, but it doesn't work 
    links$Source <- match(links$Source, links_nodes$ID_mps)-1 # Працює, але чомусь помилка з"являється 
    links$Target <- match(links$Target, links_nodes$ID_mps)-1
    
    
    forceNetwork(Links = links, # source target   value
                       Nodes = links_nodes, # name
                       Source = "Source",
                       Target = "Target", 
                       Value = "Value",
                       Group = "factions", # Colors
                       NodeID = "names_mps",
                       #linkDistance = networkD3::JS("function(d) { return 2*d.value; }"), # mess with the 5 to mess with the distances
                       Nodesize = "weight_name",
                       opacity = 1, 
                       fontSize = 18, 
                       zoom = T,  
                       #linkDistance = 5,
                       #height = "700px", 
                       #width = "600px",
                       legend = TRUE,
                       #radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"),
                       #bounded = TRUE,
                       charge=-10)
    
    
   # networkD3::saveNetwork(pp, "amends_network.html", selfcontained = TRUE)
    
  })
  
  
  # Down button 1 ####
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("amends_by_mps_WO-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(amends_by_mps_WO, file)
    }
  )
  
  # Down button 2 ####
  
  output$downloadData_table <- downloadHandler(
    
    filename = function() {
      paste("amends_by_mps-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(amends_by_mps, file)
    }
  )
  
  # Down button 3 ####
  
  output$downloadData_table_zakon <- downloadHandler(
    
    filename = function() {
      paste("amends_by_mps-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(amends_by_mps, file)
    }
  )
  
  # Down button 4 ####
  
  output$downloadData_plot_amends <- downloadHandler(
    
    filename = function() {
      paste("amends_all_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_amends_mps(), file)   ###
    }
  )
  
  # Down button 5 ####
  
  output$downloadData_table_zakon226 <- downloadHandler(
    
    filename = function() {
      paste("amends_226_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_amends226_mps(), file) ####
    }
  )
  
  
}



# Run app ####
shinyApp(ui=ui, server=server)
