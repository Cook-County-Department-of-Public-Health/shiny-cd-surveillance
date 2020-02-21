ui <- fluidPage(

  #Bring in extra CSS to style application
  includeCSS("app.css"),
  
  #Add Google analytics global tracking code
  tags$head(HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-131221855-4"></script>')),
  tags$head(tags$script(HTML(" window.dataLayer = window.dataLayer || [];
                              function gtag(){dataLayer.push(arguments);}
                              gtag('js', new Date());
                              gtag('config', 'UA-131221855-4')"))),
  tags$head(tags$script(src = "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js")),
  
  
  #Building the header 
  fluidRow(class = "header",
           column(class = "headimg", 2, align = "center", img(class = "imggen", src="https://www.cookcountypublichealth.org/wp-content/uploads/2018/12/CookCountyLogo.png", alt="CCDPH Logo")), 
           column(class = "headtitle", 10, HTML('
                                                <h1 style="font-weight: 700; font-size: 40px">Communicable Disease Surveillance in Suburban Cook County, IL</h1>
                                                '))
  ),
  
  
  br(),

  navbarPage("", id = "menu", windowTitle = "CCDPH CD Surveillance",
             tabPanel("About",
                      fluidRow(column(width = 10, offset = 1,
                                      h3(strong("Cook County Department of Public Health Communicable Disease Survellance")),
                                      br(),
                                      p("The Cook County Department of Public Health (CCDPH) continuously collects data
                                        on communicable diseases in order to better understand their burden
                                        in our community, detect outbreaks, and prevent further infections.
                                        Click on the ", em("Disease Data"), " tab to explore some of this surveillance data
                                        for select communicable diseases, and the ", em("Outbreak Data"), 
                                        "tab to see outbreaks that CCDPH has investigated."),
                                      br(),
                                      
                                      fluidRow(column(width = 6,
                                                      p(strong("Cook County Department of Public Health Jurisdiction")),
                                      p("The jurisdiction of CCDPH 
                                        includes all municipalities and portions of municipalities within
                                        Cook County, except for Chicago, Evanston, Oak Park, Skokie and 
                                        Stickney Township, each of which has its own state-certified local 
                                        health department. Throughout this site, the term,", 
                                        em("suburban Cook County"), 
                                        "refers to CCDPH's jurisdiction. Additionally, suburban Cook County can 
                                        be divided into four geographic districts, which are used to stratify data 
                                        and look at sub-county trends."), br(),
                                      p(strong("Reportable Diseases and Conditions")),
                                      p("There are over sixty reportable diseases and conditions in Illinois. 
                                        Medical providers and laboratories within suburban Cook County are 
                                        required by law to report positive cases of these reportable diseases 
                                        to CCDPH. These case reports are 
                                        then entered into a secure database and forwarded to the Illinois 
                                        Department of Public Health, providing the basis for the information 
                                        presented here. "), br(),
                                      # actionButton(inputId='reportable', 
                                      #              label=HTML("Click here to see a full list of <br/><b>reportable diseases and conditions in IL</b> "), 
                                      #              onclick ="window.open('https://www.cookcountypublichealth.org/communicable-diseases/reportable-diseases/', '_blank')",
                                      #              style = "display:block;
                                      #              height: 80px;
                                      #              width: 350px;
                                      #              padding: 20px;
                                      #              border-radius: 25px;
                                      #              border: 3px solid #6E9DC9;
                                      #              background-color: #F4F4F4;"), br(),
                                      p(a(href = 'https://www.cookcountypublichealth.org/communicable-diseases/reportable-diseases/',
                                          "Click here ", target = "_blank"), "to see a full list of reportable diseases and conditions in Illinois."), br(),
                                      p("The CCDPH Communicable Disease Unit puts out additional surveillance reports for
                                        tuberculosis (TB), sexually transmitted infections (STIs), HIV/AIDS, influenza,
                                        and West Nile virus, which can be found on the ",
                                        a(href = "https://www.cookcountypublichealth.org/data-reports/communicable-disease-data-reports/",
                                          "CCDPH website.", target = "_blank")
                                      ),
                                      
                                      br(), br()),
                                      
                                      
                                      column(width = 6, offset = 0,
                                             leafletOutput("districtMap", height = "550px", width = "600px"), br())),
                                      p(strong("Data Limitations")),
                                      p("Data on this site include reported cases of reportable diseases, but do 
                                        not represent the entire population of persons infected because not all 
                                        infected persons have been tested or reported. Frequently, there is a 
                                        considerable lag between the time a person is diagnosed with an infection 
                                        and the time the local health department receives the report. Additionally, 
                                        persons with asymptomatic infections may be underrepresented in surveillance 
                                        reports because many such individuals may not seek care, may remain undiagnosed, 
                                        and, consequently, unreported."), br(), 
                                      p("This application is currently in beta testing. Please click ", 
                                        a(href = "mailto:hannah.steinberg@cookcountyhhs.org?Subject=Shiny%20Surveillance%20App", "here"), 
                                        " to send comments, feedback, or technical questions. Source code for this application can be found ",
                                        a(href = "https://github.com/hsteinberg/CCDPH-CD-Surveillance", "here.", target="_blank"), 
                                        "If you like this app, click", a(href = "https://ccdphcd.shinyapps.io/home/", "here", target="_blank"), 
                                        "to see more interactive data applications from the Cook County Department of Public Health
                                        Communicable Disease Unit!",
                                        align = "justify", style = "padding-bottom: 10px;padding-top: 10px;text-align: center; background:  #EAF5FF;border: 3px solid #D5E2EF;font-size: 12px; padding-left: 10px; padding-right: 10px;"),
                                      br(),br(), br()))
                                      
                                      
                                      
                                      
                                      
                               
                      ),
             tabPanel("Disease Data", br(), br(),
                   fluidRow(column(width = 11, offset = 1,
                     column(width = 5,
                                   #select dropdown for disease
                            h4(strong("Select a disease to view surveillance data")),
                                   selectizeInput(inputId = "disease", "", 
                                                  choices = disease_choices,
                                                  selected = 1)),
                            column(width = 6, align = "right",
                                   actionButton(inputId='reportable', 
                                                label=HTML("Click here to see <b>communicable <br/> disease</b> and <b>health equity</b> data"), 
                                                onclick ="window.open('https://ccdphcd.shinyapps.io/healthequity/', '_blank')",
                                                style = "display:block;
                                                height: 80px;
                                                width: 350px;
                                                padding: 20px;
                                                border-radius: 25px;
                                                border: 3px solid #6E9DC9;
                                                background-color: #F4F4F4;"))
                     )
                                   
                                   
                   ), hr(),
  
  
  #------------------------ SURVEILLANCE DATA ---------------------------------#
  
  fluidRow(column(width = 10, offset = 1, tabsetPanel(type = "pills",
                                                      
    # tabPanel("Disease Information",
    #          uiOutput("cdc")
    #          
    #          ),
    
    tabPanel("Surveillance Data",
            br(),
            uiOutput("diseaseLink"),
            #h3(textOutput("disease")),
            fluidRow(
                 column(width = 9, align = "left", htmlOutput("disease_description", style = "font-size: 16px; text-align: justify;letter-spacing: .05em;"))),
          hr(),
            
            #------------------------ JURISDICTION 5 YEAR TRENDS ---------------------------------#
            fluidRow(
              column(width = 9, plotlyOutput("year_rates", height = "500px")),
              column(width = 3, 
                     br(),
                     br(),
                     br(),
                     br(),
                     checkboxGroupInput(inputId = "jurisdiction", "Display data from:",
                                        choices = c("Suburban Cook County", "Chicago", "Illinois", "United States"),
                                        selected = c("Suburban Cook County", "Chicago", "Illinois", "United States")),
                     h5(em("Data may not be available from each jurisdiction for each year."))
                     )
                 
                  ),
            hr(),
            
            #------------------------ COUNT/RATE SELECT ---------------------------------#
            fluidRow(
                 column(width = 10, offset = 0, radioButtons(inputId = "count_rate_input", "Plot rates or counts", 
                                                 choices = c("Counts","Rates"),
                                                 selected = "Rates",
                                                 inline = T)),
                 
                 #------------------------ INCIDENCE BY SEX ---------------------------------#
                 column(width = 6, plotlyOutput("sex_rates", height = "500px")),
                 
                 #------------------------ INCIDENCE BY RACE ---------------------------------#
                 column(width = 6, plotlyOutput("race_rates", height = "500px"))
                   ),
            br(),
            
            #------------------------ AGE DISTRIBUTION ---------------------------------#
            fluidRow(
              column(width = 12, plotlyOutput("age_rates", height = "500px"))
                     
            ),
            br(),
            br(),
            fluidRow(
              #------------------------ INCIDENCE BY MONTH ---------------------------------#
              column(width = 6, plotlyOutput("month_rates", height = "500px")),
              
              #------------------------ INCIDENCE BY DISTRICT ---------------------------------#
              column(width = 6, plotlyOutput("district_rates", height = "500px"))
            ),
            # fluidRow(
              #------------------------ DISEASE-SPECIFIC RISK FACTORS ---------------------------------#
            #   column(width = 6, plotlyOutput("risk_factor", height = "500px"))
            # ),

            hr(),
            #------------------------ HEALTH EQUITY SCATTER PLOT (TAKEN OUT) ---------------------------------#
            # fluidRow(column(width = 3,
            #                 selectizeInput(inputId = "social", "Select a social indicator to compare to disease incidence", 
            #                   choices = social_choices,
            #                   selected = 1)
            #     ),
            # (column(width = 9,
            #       plotlyOutput("ScatterPlot", height = "720px", width = "100%")
            #     ))),
            # br(),
            # hr(),
            fluidRow(
              column(width = 12, 
                     h5(em("All rates are calculated with populations from the 2010 United States Census."), align = "center"),
                     br(),
                     br())
              
            )
            ),
    
    #------------------------ INCIDENCE MAP ---------------------------------#

    tabPanel("Map",
             fluidRow(column(width = 10, offset = 1,
                             br(),
                             leafletOutput("DiseaseMap", height = "700px", width = "100%"),
                             fluidRow(column(width=12, align = "right")),
                             hr(),
                             h5(em("All rates are calculated with populations from the 2010 United States Census."), align = "center"),
                             h5(em("Municipalities with (pt.) after their names are only partially contained within 
                                   Cook County. Their population sizes represent the portion of the municipality 
                                   in Cook County. Disease rates for these municipalities may be over or under 
                                   estimated due to reporting discrepencies and small population sizes."), align = "center"),
                             
                             br())       ))))))
          ,
 
 #------------------------ OUTBREAKS ---------------------------------#
tabPanel("Outbreak Data",
         fluidRow(column(width = 4, offset = 0,
                         h4(strong("Communiable Disease Outbreaks")),
                         p("An outbreak is when we see more cases of a particular illness
                           in a group of people than we would expect. All suspected outbreaks are
                           reportable to public health. CCDPH investigates dozens of communicable
                           disease outbreaks each year. These outbreaks can be spread by food (foodborne),
                           water (waterborne), or from person to person.")),
                  column(width = 7, align = "left",
                         tableOutput("outbreaks"),
                         h6(em("VPD = Vaccine Preventable Disease"), style = "text-align: center"))),
                  
         hr(),
         fluidRow(column(width = 12, offset = 0, br(),
                         sidebarLayout(sidebarPanel(width = 3,
                                                    h4("Monthly Outbreak Data Display Options"), br(),
                                                    checkboxGroupInput(
                                                      inputId = "outbreaksMode",
                                                      label = "Mode of Transmission",
                                                      choices = levels(outbreaks_month$Mode),
                                                      selected = levels(outbreaks_month$Mode)
                                                    ),
                                                    h6(em("VPD = Vaccine Preventable Disease")), br(),
                                                    checkboxGroupInput(
                                                      inputId = "outbreaksYear",
                                                      label = "Years",
                                                      choices = unique(outbreaks_month$Year),
                                                      selected = (cur_year-2):cur_year
                                                    ), 
                                                    h6(em("Outbreaks are counted by the month of onset
                                                          for the first patient in the outbreak."))
                                                    ),
                                       mainPanel(width = 9,
                                                 plotlyOutput("outbreaksGraph", width = "100%", height = "600px"),
                                                 br(), br(), br())
                                       )
                         ))

 )
)


  
)#end fluid page

