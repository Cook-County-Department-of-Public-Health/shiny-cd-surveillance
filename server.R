server <- function(input, output, session) {
  #make data frame for just selected disease
  disease <- reactive({
    select_disease_crosstabs[as.numeric(input$disease)] %>%
      as.data.frame()
  })
  
  #Get column numbers of first and last years of data
  year1 <- reactive({
    1
  })
  
  year2 <- reactive({
    ncol(disease())
  })
  


  #Calculate average incidence per 100,000 population
  disease_av_per_thousand <- reactive({
    req(year2())
    req(year1())

    if(year2()-year1() > 0){
      disease_sum = rowSums(disease()[,year1():year2()])
      disease_av = disease_sum/(year2()-year1()+1)
    }
    else{
      disease_av = disease()[,year1()]
    }
    disease_av_per_thousand = (disease_av/town_size_district$Population)*100000
    return(disease_av_per_thousand)
  })
  
  # #Selected Town for label (if any)
  town_index <- reactive({
    #as.numeric(input$town)
    0
  })
  # 
  # town_name <- reactive({
  #   names(CC_towns_vector)[town_index()+1]
  # })


  #Disease and social index and year names
  disease_name <- reactive({
    disease_list[as.numeric(input$disease)]
  })
  
  
  year1_name <- reactive({
    gsub("^.*\\.", "",colnames(disease())[year1()])
  })
  
  year2_name <- reactive({
    gsub("^.*\\.", "",colnames(disease())[year2()])
  })
  
  year <- reactive({
    cur_year
  })
  #Selected social data
  selected_social <- reactive({
    social_data[,as.numeric(input$social)]
  })
  
  social_name <- reactive({
    social_list[as.numeric(input$social)]
  })

  district_colors = reactive({c("#b55681", "#f7986f","#95c47b","#29a7c6")})

############################# Scatter plot of disease by social index ########################################## 
  
  #Selected social data formatted
  selected_social_print <- reactive({
    if(as.numeric(input$social) == 1){
      paste0("$",selected_social() %>% round(0) %>% prettyNum(big.mark = ","))
    }
    else{
      paste0(selected_social() %>% round(1), "%")
    }
    
  })
  
  output$ScatterPlot <- renderPlotly({
    #If a town is selected, define annotation
    if(town_index() > 0)
    {
      a <- list(
        x = selected_social()[town_index()],
        y = disease_av_per_thousand()[town_index()],
        text = town_name(),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 4,
        ax = 20,
        ay = -40
      )
    }

    else{a <- NULL} #If not, set annotation to null

    #suppress warnings
    options(warn = -1)

    #Produce scatter plot
    plot_ly(x=selected_social(), y=disease_av_per_thousand(),
            type = "scatter", mode = "markers", 
            #text = rownames(disease()),
            text = paste0("<b>",rownames(disease()),"</b>","<br>2010 Population: ",prettyNum(town_size_district$Population, big.mark = ","),  "<br>", social_name(), ": ", selected_social_print(),
                          "<br>", disease_name(), " Incidence: ", round(disease_av_per_thousand(),1), " per 100,000"),
            color = town_size_district$District, size = town_size_district$Population,
            colors = district_colors(), 
            sizes = c(5,150), hoverinfo = "text") %>%
      layout(
        yaxis = list(title=paste("Mean Annual ", disease_name(), "\nIncidence per 100,000 (",
                                 year1_name(), "-", year2_name(), ")",
                                 rep("&nbsp;", 20), #adding space under axis title
                                 rep("\n&nbsp;", 3),
                                 sep = ""),
                     titlefont = list(size = 16),
                     tickfont = list(size = 16)),
        xaxis = list(title=social_name(),
                     titlefont = list(size = 16),
                     tickfont = list(size = 16)),
        title = paste(disease_name(),"Incidence and\n", social_name(), "\nby Suburban Cook County Municipality"),
        margin = list(b = 120, l = 90, r = 0, t = 100),
        annotations = a, cex = 2
      ) 


  })
  
############################# Boxplot of disease incidence by district ###############################################
  output$BoxPlotDisease = renderPlotly({

      #Plot boxplot
      plot_ly(y = disease_av_per_thousand(), type = "box", boxpoints = "all",
              jitter = 0.3, pointpos = 0, color = town_size_district$District,
              colors = district_colors(),
              text = paste(names(CC_towns_vector[2:length(CC_towns_vector)]),"<br>", disease_name(), " Incidence: ",
                           round(disease_av_per_thousand()), " per 100,000", sep = ""), hoverinfo = "text")%>%
        layout(
          yaxis = list(title=paste("Mean Annual ", disease_name(), " Incidence per 100,000 (",
                                   year1_name(), "-", year2_name(), ")", sep = "")),
          xaxis = list(title="Suburban Cook County District"),
          title = paste(disease_name(),"Incidence by District"),
          showlegend = FALSE
        )#%>% config(collaborate = FALSE)

  })


  
############################# Map showing the SCC districts ########################################## 
  output$districtMap <- renderLeaflet({
    
    colors = c(district_colors(), "#999999", "#CECECE",  "#4B5F96")
    
    leaflet(options = leafletOptions(minZoom = 8, zoomSnap = .25)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.8, lat = 41.81, zoom = 9.5) %>%
      addPolygons(data = muni,
                  fillColor = colors[6],
                  stroke = F,
                  dashArray = "3",
                  fillOpacity = 0.5,
                  label = "Unincorporated Area",
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto")
      ) %>%
      addPolygons(data = chicago,
                  fillColor = colors[5],
                  stroke = F,
                  dashArray = "3",
                  fillOpacity = 0.7,
                  #label = "Chicago",
                  label = sprintf("<strong>%s</strong><br/>%s", "Chicago", "Out of Jurisdiction") %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto")
      ) %>%
      addPolygons(data = cc2,
                  fillColor = as.character(colors[cc2$DIST]),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = FALSE),
                  label = sprintf("<strong>%s</strong><br/>%s", cc2$CITY, c("North District", "West District", "Southwest District", "South District", "Out of Jurisdiction")[as.numeric(cc2$DIST)]) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto")
      )%>%
      addLegend("topright", colors = c(colors[1:6]), 
                labels = c("North District","West District", "Southwest District", "South District", "Out of Jurisdiction", "Unincorporated"),
                title = "CCDPH Jurisdiction",
                opacity = 0.7)%>%
      addScaleBar(position = "bottomleft") %>%
    
    onRender(
      "function(el, x) {
      L.easyPrint({
      sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
      filename: 'DistrictMap',
      exportOnly: true,
      hideClasses: ['leaflet-control-easyPrint'],
      hideControlContainer: false,
      
      }).addTo(this);
  }"
        )
      
  })
  
########### Map showing the selected disease incidence rates in all municipalities ################################## 
  output$DiseaseMap = renderLeaflet({
    pal <- colorNumeric("inferno", 0:max(disease_av_per_thousand()+1), reverse = T)
    
    leaflet(options = leafletOptions(minZoom = 8)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
      addPolygons(data = muni,
                  fillColor = "#CECECE",
                  stroke = F,
                  dashArray = "3",
                  fillOpacity = 0.5
      ) %>%
      addPolygons(data = cc,
                  fillColor = pal(disease_av_per_thousand()),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = FALSE),
                  #label = paste(cc$CITY, "<br/> Chlamydia Cases:", cl_2017),
                  label = sprintf("<strong>%s</strong><br/>%.1f", cc$CITY, disease_av_per_thousand()) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px", direction = "auto")
      ) %>%
      addLegend("topright", pal = pal, values = disease_av_per_thousand(),
                title = paste0(strwrap(paste0("Mean Annual ", disease_name(), " Incidence per 100,000 (",
                                              year1_name(), "-", year2_name(), ")"), 20),collapse = "<br>"),
                #labFormat = labelFormat(prefix = "$"),
                opacity = 1)%>%
      addScaleBar(position = "bottomleft")%>%
      
      onRender(
        "function(el, x) {
        L.easyPrint({
        sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
        filename: 'SurveillanceMap',
        exportOnly: true,
        hideClasses: ['leaflet-control-easyPrint'],
        hideControlContainer: false,
        
        }).addTo(this);
  }")
  })
  

  
  
################################ Year disease rates graph #############################################
  output$year_rates <- renderPlotly({
    case_by_year = disease_by_year %>% filter(Disease == disease_name()) %>% filter(Year > (year()-5)) %>%
      filter(Jurisdiction %in% input$jurisdiction) %>%
      mutate(Jurisdiction = ifelse(Jurisdiction == "Suburban Cook County", "Suburban Cook", Jurisdiction)) %>%
      mutate(Jurisdiction = factor(Jurisdiction, levels = c("Suburban Cook", "Chicago", "Illinois", "United States")))
    
    ymax = max(case_by_year$Rate, na.rm = T) + max(case_by_year$Rate, na.rm = T)*.3
    
    # plot_ly(data = case_by_year, x = ~Year, y = ~Rate, linetype = ~Jurisdiction, type = "scatter", mode = "lines+markers",
    #         line = list(color = "#0b3954"), marker = list(color = "#0b3954")) %>%
    
    jurcolors = c("#002B4F", "#094E87", "#4F7DA3", "#91ABC1")  
    
    plot_ly(data = case_by_year, x = ~Year, y = ~Rate, color = ~Jurisdiction, linetype = ~Jurisdiction, type = "scatter", mode = "lines+markers",
            colors = jurcolors) %>%
      layout(yaxis = list(title = "Incidence Rate per 100,000 Population", range = c(0,ymax), showgrid = FALSE),
             title = paste(disease_name(), "Trends"),
             xaxis = list(dtick = 1, showgrid = FALSE),
             margin = list(b = 100, l = 100, r = 100, t = 100), 
             showlegend = T,
             hovermode = 'compare')#%>% config(collaborate = FALSE)
  })
  
################################ Sex disease rates graph #############################################
  output$sex_rates <- renderPlotly({
    case_this_year = disease_by_sex %>%
      filter(Disease == disease_name())  %>% 
      filter(Year == year()) %>%
      mutate(Pop = ccdph_demo[,as.character(Sex)] %>% as.numeric()) %>% 
      mutate(Rate = Count/Pop*100000) %>% 
      mutate(Rate = round(Rate, 2)) 
    
    yvar = case_this_year$Rate
    ylab = "Rate per 100,000 Population"
    ytext = c(paste("The", disease_name(), "rate in females was\n", case_this_year$Rate[case_this_year$Sex == "Female"], "cases per 100,000 population in", year()),
              paste("The", disease_name(), "rate in males was\n", case_this_year$Rate[case_this_year$Sex == "Male"], "cases per 100,000 population in", year()))
    if(input$count_rate_input == "Counts"){
      yvar = case_this_year$Count
      ylab = "Number of Cases"
      ytext = c(paste("There were", case_this_year$Count[case_this_year$Sex == "Female"], "cases of\n", disease_name(), "in females in", year()),
                paste("There were", case_this_year$Count[case_this_year$Sex == "Male"], "cases of\n", disease_name(), "in males in", year()))
    }
    
    plot_ly(data = case_this_year, x = yvar, y = ~Sex, marker = list(color = c("#938ba1", "#a3b1ce")),
            type = "bar", orientation = 'h', text = yvar, textposition = 'auto', hovertext = ytext,
            hoverinfo = "text") %>%
      layout(xaxis = list(title=ylab,
                          zeroline = TRUE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE),
             yaxis = list(title= ""),
             title = paste(disease_name(),"Incidence by Sex\nin Suburban Cook County,", year()),
             margin = list(b = 100, l = 100, r = 100, t = 100))#%>% config(collaborate = FALSE)
  })
  
################################ Race disease rates graph #############################################
  output$race_rates <- renderPlotly({
    case_by_year = disease_by_race %>% filter(Disease == disease_name())  %>% 
      mutate(RaceEth = factor(RaceEth, levels = c("Asian", "Hispanic/Latino", "NH Black", "NH White"))) %>%
      complete(RaceEth, Disease, Year) %>%
      mutate(Pop = ccdph_demo[,as.character(RaceEth)] %>% as.numeric()) %>% 
      mutate(Rate = Count/Pop*100000) %>%
      filter(Year == year()) %>% 
      mutate(Rate = round(Rate, 2)) %>%
      replace_na(list(Count = 0, Rate = 0))
      
    yvar = case_by_year$Rate
    ylab = "Rate per 100,000 Population"
    ytext = apply(case_by_year, 1, function(x){
      race = gsub("NH", "Non-Hispanic", x["RaceEth"]) 
      race = gsub("White", "white", race) %>% paste0("s")
      return(
        paste(
          "The", disease_name(), "rate in", race,"was\n", x["Rate"], "cases per 100,000 population in", year()
        )
      )
    })
    if(input$count_rate_input == "Counts"){
      yvar = case_by_year$Count
      ylab = "Number of Cases"
      ytext = apply(case_by_year, 1, function(x){
        race = gsub("NH", "Non-Hispanic", x["RaceEth"]) 
        race = gsub("White", "white", race) 
        return(
          paste(
            "There were", x["Count"], "cases of", disease_name(), "\nin",race,"individuals in", year()
          )
        )
      })
      
    }
    
    plot_ly(data = case_by_year, x = yvar, y = ~RaceEth, 
            type = "bar", marker = list(color = c( "#348aa7", "#5dd39e","#bce784",  "#d3fad6")),
            showlegend = FALSE, text = yvar, textposition = 'auto', orientation = 'h', hovertext = ytext,
            hoverinfo = "text")%>%
      layout(xaxis = list(title=ylab,
                          zeroline = TRUE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE),
             yaxis = list(title = ""),
             title = paste(disease_name(),"Incidence by Race/Ethnicity\nin Suburban Cook County,", year()),
             margin = list(b = 100, l = 100, r = 100, t = 100))#%>% config(collaborate = FALSE)
    
  })


################################ Age disease rates graph #############################################
  output$age_rates <- renderPlotly({
    age_cuts = c(seq(0,80,5))
    case_by_age = disease_by_age %>% filter(Disease == disease_name()) %>%
      filter(Year == year()) %>%
      mutate(agecat = findInterval(Age, age_cuts))
    agecat_names = sapply(age_cuts, function(x){
      if(x == max(age_cuts)){
        return(paste0(x,"+"))
      }
      return(paste(x, "to", x+4))
    })
    case_by_age$agecat_name = agecat_names[case_by_age$agecat] %>% factor(levels = agecat_names)
    case_by_age = case_by_age %>%
      complete(agecat_name) %>%
      group_by(agecat_name) %>%
      dplyr::summarise(count = sum(Count))
    case_by_age[is.na(case_by_age)] <- 0
    case_by_age = case_by_age %>%
      mutate(pop = ccdph_demo[,as.character(agecat_name)] %>% as.numeric()) %>%
      mutate(rate = (count/pop*100000) %>% round(2))
    
    yvar = case_by_age$rate
    ylab = "Rate per 100,000 Population"
    ytext = paste(case_by_age$rate, "cases per 100,00")
    ytext = gsub("1 cases per 100,000", "1 case per 100,000", ytext)
    if(input$count_rate_input == "Counts"){
      yvar = case_by_age$count
      ylab = "Number of Cases"
      ytext = paste(case_by_age$count, "cases")
      ytext = gsub("1 cases", "1 case", ytext)
    }
    
    
    plot_ly(x=~agecat_name, y =yvar, data = case_by_age, marker = list(color ="#c2efeb"),
            type = "bar", text = ytext, hoverinfo = 'text+x')%>%
      layout(yaxis = list(title=ylab, showgrid = FALSE),
             title = paste(disease_name(),"Age Distribution in Suburban Cook County,", year()),
             xaxis = list(title = "Age", tickangle = -25),
             margin = list(b = 100, l = 100, r = 100, t = 100),
             hovermode = 'compare')#%>% config(collaborate = FALSE)
  })
  
  ################################ District disease rates graph #############################################
  output$district_rates <- renderPlotly({
    case_by_year = disease_by_district %>% filter(Disease == disease_name())  %>% 
      mutate(District = factor(District, levels = c("North", "West", "Southwest", "South")[4:1])) %>%
      complete(District, Disease, Year) %>%
      mutate(Pop = ccdph_demo[,as.character(District)] %>% as.numeric()) %>% 
      mutate(Rate = Count/Pop*100000) %>%
      filter(Year == year()) %>% 
      mutate(Rate = round(Rate, 2)) %>%
      replace_na(list(Count = 0, Rate = 0))
    
    yvar = case_by_year$Rate
    ylab = "Rate per 100,000 Population"
    ytext = apply(case_by_year, 1, function(x){
      district = x["District"] 
      return(
        paste(
          "The", disease_name(), "rate in the", district,"district of Suburban Cook County was\n", x["Rate"], "cases per 100,000 population in", year()
        )
      )
    })
    if(input$count_rate_input == "Counts"){
      yvar = case_by_year$Count
      ylab = "Number of Cases"
      ytext = apply(case_by_year, 1, function(x){
        district = x["District"] 
        return(
          paste(
            "There were", x["Count"], "cases of", disease_name(), "\nin the",district,"district of Suburban Cook County in", year()
          )
        )
      })
      
    }
    
    plot_ly(data = case_by_year, x = yvar, y = ~District, 
            type = "bar", marker = list(color = district_colors()[4:1]),
            showlegend = FALSE, text = yvar, textposition = 'auto', orientation = 'h', hovertext = ytext,
            hoverinfo = "text")%>%
      layout(xaxis = list(title=ylab,
                          zeroline = TRUE,
                          showline = FALSE,
                          showticklabels = FALSE,
                          showgrid = FALSE),
             yaxis = list(title = ""),
             title = paste(disease_name(),"Incidence by \nSuburban Cook County District,", year()),
             margin = list(b = 100, l = 100, r = 100, t = 100))#%>% config(collaborate = FALSE)
    
  })
  
  
  
################################ Month disease rates graph #############################################
  output$month_rates <- renderPlotly({
  case_by_month = disease_by_month %>%
    filter(Year == year()) %>%
    filter(Disease == disease_name()) %>%
    complete(Month, Disease, Year) %>%
    merge(disease_by_month %>%
            filter(Year %in% (year()-5):(year()-1)) %>%
            filter(Disease == disease_name()) %>%
            complete(Month) %>%
            group_by(Month) %>%
            dplyr::summarise(count_5yr_av = mean(Count, na.rm = T),
                      rate_5yr_av = mean(Rate, na.rm = T))
    ) %>%
    replace_na(list(Count = 0, Rate = 0, count_5yr_av = 0, rate_5yr_av = 0)) %>%
    arrange(Month) %>%
    mutate(count_5yr_av = round(count_5yr_av,2)) %>%
    mutate(rate_5yr_av = round(rate_5yr_av,2)) %>%
    unique()
  
  num_years = 5
  if(disease_name() %in% c("Chlamydia", "Gonorrhea")){
    num_years = year() - 2015
  }
  if(disease_name() == "Campylobacteriosis"){
    num_years = year() - 2016
  }
  
  yvar = case_by_month$Rate
  yvar2 = case_by_month$rate_5yr_av
  ylab = "Rate per 100,000 Population"
  if(input$count_rate_input == "Counts"){
    yvar = case_by_month$Count
    yvar2 = case_by_month$count_5yr_av
    ylab = "Number of Cases"
  }
  
  plot_ly(x = ~Month, y = yvar, data = case_by_month, type = "bar", name = year(),
          marker = list(color = "#f4a261")) %>%
    add_trace(x = ~Month, y = yvar2, data = case_by_month, type = "scatter", mode = "lines+markers", name = paste(num_years,"Year Average"),
              marker = list(color = "#264653"), line = list(color = "#264653")) %>%
    layout(yaxis = list(title = ylab, showgrid = FALSE),
           hovermode = 'compare',
           margin = list(b = 100, l = 100, r = 100, t = 100),
           title = paste(disease_name(),"Incidence by Month\nin Suburban Cook County,", year()))#%>% config(collaborate = FALSE)
  })

  
################################ Disease-specific risk factors #############################################
  output$risk_factor <- renderPlotly({
    
    #For enterics, plot travel history
    if(disease_name() %in% enterics){
      
    #format data  
    enteric_risks2 = enteric_risks %>%
        filter(Year == year()) %>%
        filter(Disease == disease_name())
      
    risk = enteric_risks2 %>%
        ungroup() %>%
        select(travel_in_IL_perc, travel_in_US_perc, travel_out_US_perc, any_travel_perc, no_travel_perc) %>%
        data.table::transpose() %>%
        mutate(Travel = c("Travel in IL", "Travel in US", "Travel Abroad", "Any Travel", "No Travel")) %>%
        mutate(Travel = factor(Travel, levels = c("Travel in IL", "Travel in US", "Travel Abroad", "Any Travel", "No Travel"))) %>%
        set_colnames(c("Percent of Cases", "Travel History")) %>%
        mutate(Count = c(enteric_risks2$travel_in_IL[1], enteric_risks2$travel_in_US[1], enteric_risks2$travel_out_US[1],
                         enteric_risks2$any_travel[1], enteric_risks2$no_travel[1]))
    
    #hover text
    description = c("traveled within\nIllinois", "traveled to a state outside of\nIllinois", 
              "traveled outside\nthe United States", "reported travel\n", "did not\ntravel")
    ytext = sapply(1:5, function(i){
      return(paste0(round(risk$`Percent of Cases`[i],1), "% of ", disease_name(), " cases ", description[i],
                    " during their possible exposure period."))
    })
    
    #horizontal bar chart
    plot_ly(y=risk$`Travel History`, x = risk$`Percent of Cases`, data = risk, type = "bar",
            marker = list(color =c(rep("#a2d6b3",3), "#419990", "#a9afd1")),
            text = paste0(round(risk$`Percent of Cases`, 1), "%"), textposition = 'auto',
            hovertext = ytext, hoverinfo = "text") %>%
      layout(xaxis = list(showgrid = TRUE, range = c(0,100),
                          showticklabels = FALSE,
                          tick0 = 0, dtick = 99, gridwidth = 1, gridcolor = "#8e8e8e",
                          zeroline = TRUE),
             margin = list(b = 100, l = 100, r = 100, t = 100),
             title = paste("Travel History in\n", disease_name(), "Cases,", year())
             )#%>% config(collaborate = FALSE)
    }
    
    #for VPDs, plot vaccination history
    else if(disease_name() %in% vpd){
      #select/format data
      risk = vpd_risks %>%
        filter(Disease == disease_name()) %>%
        filter(Year == year()) %>%
        mutate(Disease = gsub( "Chronic| Invasive Disease| \\(.*$", "", Disease)) %>%
        mutate(Disesae = trimws(Disease))

      if("" %in% risk$Vaccinated & "Unknown" %in% risk$Vaccinated){
        risk$Count[risk$Vaccinated == "Unknown"] = sum(risk$Count[risk$Vaccinated %in% c("", "Unknown")])
        risk = risk %>% filter(Vaccinated != "")
      } else if ("" %in% risk$Vaccinated){
        risk$Vaccinated = ifelse(risk$Vaccinated == "", "Unknown", risk$Vaccinated)
      }
      
      risk = risk %>%
        mutate(Vaccinated = factor(Vaccinated, levels = c("Unknown", "Yes", "No"))) %>%
        mutate(Percent = (Count/sum(Count)*100) %>% round(1)) %>%
        arrange(Vaccinated)
      
      #hover text
      ytext = c(paste0(risk$Percent[1], "% of ", disease_name(), "\ncases have an unknown vaccination status."), 
                paste0(risk$Percent[2], "% of ", disease_name(),"\ncases received at least one dose of ", risk$Disease[1], " containing vaccine."), 
                paste0(risk$Percent[3], "% of ", disease_name(), "\ncases were not vaccinated against ", risk$Disease[1], ".")
                )
      
      #horizontal bar chart
      plot_ly(y=risk$Vaccinated, x = risk$Percent, data = risk, type = "bar",
              marker = list(color =c( "#cccccc",  "#a2d6b3","#a9afd1")),
              text = paste0(round(risk$Percent, 1), "%"), textposition = 'auto',
              hovertext = ytext, hoverinfo = "text") %>%
        layout(xaxis = list(showgrid = TRUE, range = c(0,100),
                            showticklabels = FALSE,
                            tick0 = 0, dtick = 99, gridwidth = 1, gridcolor = "#8e8e8e",
                            zeroline = TRUE),
               margin = list(b = 100, l = 100, r = 100, t = 100),
               title = paste(risk$Disease[1], "Vaccination History of\n", disease_name(), "Cases,", year())
        )#%>% config(collaborate = FALSE)
      
      #for Lyme disease, plot tick habitat exposures
    } else if(disease_name() == "Lyme Disease"){
      #select and format data
      total_lyme = disease_by_year$Count[disease_by_year$Disease == "Lyme Disease" & disease_by_year$Year == year() & 
                                           disease_by_year$Jurisdiction == "Suburban Cook County"]
      risk = lyme_risk %>%
        filter(Year == year()) %>%
        mutate(Percent = (Count/total_lyme*100) %>% round(1)) %>%
        arrange(Percent) %>%
        mutate(Habitat = factor(Habitat, levels = Habitat))
      
      #horizontal bar chart
      plot_ly(y=risk$Habitat, x = risk$Percent, data = risk, type = "bar",
              marker = list(color =c( "#a2d6b3")),
              text = paste0(round(risk$Percent, 1), "%"), textposition = 'auto', hoverinfo = "text") %>%
        layout(xaxis = list(showgrid = TRUE, 
                            showticklabels = FALSE,
                            tick0 = 0, dtick = 99, gridwidth = 1, gridcolor = "#8e8e8e",
                            zeroline = TRUE),
               margin = list(b = 100, l = 100, r = 100, t = 100),
               title = paste("Tick Habitats Visited during Exposure\nPeriod by Lyme Disease Cases,", year())
        )#%>% config(collaborate = FALSE)
      
      #For WNV, plot percent neuroinvasive
    } else if(disease_name() == "West Nile Virus"){
      #select and clean data
      risk = wnv_risk %>%
        filter(Year == year()) %>%
        mutate(Percent = (Count/sum(Count)*100) %>% round(1)) 
      
      #horizontal bar chart
      plot_ly(y=risk$Type, x = risk$Percent, data = risk, type = "bar",
              marker = list(color =c("#a2d6b3","#a9afd1")),
              text = paste0(round(risk$Percent, 1), "%"), textposition = 'auto', hoverinfo = "text") %>%
        layout(xaxis = list(showgrid = TRUE, range = c(0,100),
                            showticklabels = FALSE,
                            tick0 = 0, dtick = 99, gridwidth = 1, gridcolor = "#8e8e8e",
                            zeroline = TRUE),
               margin = list(b = 100, l = 100, r = 100, t = 100),
               title = paste("Neuroinvasive and Non-Neuroinvasive\nWest Nile Virus Cases,", year())
        )#%>% config(collaborate = FALSE)
    }
    })
  
  
  #Explanatory text for each disease- defined in "load data" file
  output$disease_description <- renderText({
    text = paste0(disease_info[disease_name()],
          "</br></br> There were <b>",
          disease_by_year$Count[disease_by_year$Disease == disease_name() & disease_by_year$Year == cur_year][1] %>%
            prettyNum(big.mark = ","),
          " reported cases</b> of ", disease_name(), " in suburban Cook County in ", cur_year, "."
    )
    return(text)
  })
  
  #CDC syndicated info
  # output$cdc <- renderUI({
  #   input$disease
  #   return(tags$div(HTML(disease_cdc[disease_name()])))
  # })
  
  #Selected disease name to be used in UI
  output$disease <- renderText({
    return(disease_name())
  })
  
  #Link to CDC website for each disease- defined in "load data" file
  output$diseaseLink <- renderUI({
    h3(a(href = as.character(disease_link[disease_name()]), disease_name(), target = "_blank")) 
  })
  

  ############################## OUTBREAKS TABLE #############################################
  output$outbreaks <- function(){
    outbreaks%>%
      kable(escape = F, align = "c") %>%
      kable_styling(c("hover", "condensed"), full_width = F) %>%
      add_header_above(c(" "=3, "Person-to-Person" = 3, " " = 2)) %>%
      add_header_above(c("Communicable Disease Outbreaks in Suburban Cook County by Year and Mode of Transmission" = 8))
  }
  
  output$outbreaksGraph <- renderPlotly({
    temp = outbreaks_month %>%
      filter(Year %in% input$outbreaksYear & Mode %in% input$outbreaksMode)
    
    plot_cols = c("Foodborne"= "#8CCECE", "Waterborne" = "#68A5A5", 
                  "Person-to-Person (Non VPD)" ="#E0B1B6", "Person-to-Person (VPD)" = "#CE8C8C", 
                  "Person-to-Person (Influenza)" = "#B27070", "Unknown" = "#B3BCBC")
    
    p <- ggplot(temp, 
                aes(x=Month, y=n, fill=Mode,
                    text = paste0("</br>", Month, " ", Year, "</br>",
                                  Mode, ": <b>", n, "</b>"
                    )
                )) +
      geom_bar(stat='identity')+
      scale_fill_manual(values = plot_cols[temp$Mode]) +
      xlab("") + ylab("") +
      facet_wrap(~Year, strip.position = "bottom", nrow=1) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90),
            panel.spacing = unit(0, "lines"),
            panel.grid.major.x = element_blank(),
            strip.background = element_rect(
              color="white", fill="#CCDDDD", size=1.5, linetype="solid"),
            strip.text.x = element_text(color = "black")
      ) +
      labs(fill = "")
    
    
    
    ggplotly(p, tooltip = "text") %>%
      layout(title = "Suburban Cook County Communicable Disease Outbreaks",
             margin = list(t = 100, l = 100),
             yaxis = list(title = "Number of Outbreaks"),
             xaxis = list(title = "")
      )
    
  })
  
}




