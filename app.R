#' Roxygen style comments
#' This structure supports later package documentation


library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(viridis)
library(shinydashboard)
library(ggplot2)
library(tigris)
library(rsconnect)
library(scales)

options(tigris_use_cache = TRUE)

#bring in SBA data
SBA <- read_csv("./data/150K_slim.csv")
SBA%>%mutate(Lender=str_replace(Lender, "\\bNational Association\\b", "NA"))->SBA

#bring in NAICS data
naics <- read_csv("./data/naics.csv") 

nat_banks_list <- read_csv("./data/nat_banks_final.csv")
nat_banks_list%>%select(-findrsLender, -cert)->nat_banks
nat_banks%>%mutate(Lender=str_replace(Lender, "\\bNational Association\\b", "NA"))->nat_banks
nat_banks%>%inner_join(SBA, by="Lender")->nat_banks

#bring in census population data estimates by state for 2019
#source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage
pop <- read_csv("./data/pop_by_state.csv")

#import census population data estimates by county for 2019
pop_cty <- read_csv("./data/pop_by_county.csv")

# obtain population/10000
pop%>%  mutate(pop_10000=pop_est/10000)->pop

#replace naics code with two-digit industry categories
SBA%>%mutate(Sector=as.numeric(str_extract(NAICSCode, "\\d\\d")))%>%
  inner_join(naics, by="Sector")->SBA

#prepare for plot of top lenders in each state
SBA %>%group_by(State, Lender) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  group_by(State)%>%
  top_n(n=10)->top_banks

#prepare for plot of top lenders in each state - nat banks
nat_banks%>%group_by(State, Lender) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  group_by(State)%>%
  top_n(n=10)->top_banks_nat

#prepare for plot of top lenders in all state - nat banks
nat_banks%>%mutate(State="National")%>%
  group_by(State, Lender) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  group_by(State)%>%
  top_n(n=10)->top_banks_nat_all

top_banks_nat%>%mutate(State="National")->top_banks_nat_nat

#prepare for use in state plot
SBA%>%mutate(LoanRange1=factor(LoanRange, levels=c("e $150,000-350,000", 
                                                   "d $350,000-1 million", 
                                                   "c $1-2 million", 
                                                   "b $2-5 million",
                                                   "a $5-10 million")))->SBA_state_plot 

#Prepare map plot data for nat banks
#asset cat and state selected
nat_banks_list%>%mutate(Lender=str_replace(Lender, "\\bNational Association\\b", "NA"))%>%
  inner_join(SBA_state_plot, by="Lender")->SBA_state_plot_nat 
#state selected and no asset selected
SBA_state_plot_nat%>%mutate(asset_cat="All")->SBA_state_plot_nat_all 
#no state selected and asset selected
SBA_state_plot_nat%>%mutate(State="National")->SBA_state_plot_nat_all1
#when no asset cat or state select
SBA_state_plot_nat_all%>%mutate(State="National")->SBA_state_plot_nat_all2 

#Prepare count of loans by state
SBA %>%
  group_by(State)%>%
  summarize(total = n())%>%
  inner_join(pop, by="State")%>%
  mutate(total=(total/pop_10000))%>% #obtain number of loans per 10,000
  rename(state=State)->loans_by_state

#Prepare count of loans by state for national banks
nat_banks %>%
  group_by(State)%>%
  summarize(total = n())%>%
  inner_join(pop, by="State")%>%
  mutate(total=(total/pop_10000))%>% #obtain number of loans per 10,000
  rename(state=State)->loans_by_state_nat

# Use the states shapefile and the loans_by_states dataframe 
# STUSPS and state are the two columns they'll be joined by
states <- states(cb=T)
states_merged_SBA <- geo_join(states, loans_by_state, "STUSPS", "state", how="inner")
states_merged_SBA_nat <- geo_join(states, loans_by_state_nat, "STUSPS", "state", how="inner")

# Create a color palette based on the number range in the total column
pal <- colorNumeric("Oranges", domain=loans_by_state$total)
palnat <- colorNumeric("Oranges", domain=loans_by_state_nat$total)

ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("SBA Paycheck Protection Program Loan Level Data - Loans $150K and Above"),
                tabsetPanel(
                  tabPanel("All Loans",
                           (tags$style(type="text/css",
                                        ".shiny-output-error { visibility: hidden; }",
                                        ".shiny-output-error:before { visibility: hidden; }")),
                           
                           br(),
                           fluidRow(column(12, textOutput("clickText"))),
                           br(),
                           fluidRow(column(12,
                                           leafletOutput("map"))),
                           br(),
                           fluidRow(column(5, offset=1, plotOutput("stateLoanRangePlot")),
                                    column(5, plotOutput("stateJobsPlot"))),
                           fluidRow(column(10, offset=1, plotOutput("stateIndustriesPlot"))),
                           
                           br(),
                           fluidRow(column(10, offset=1, plotOutput("stateLenderRangeplot"))),
                           
                           
                           
                           br(),
                           fluidRow(column(12, textOutput("warningText"))),
                           fluidRow(column(12, textOutput("dataSource")))
                           ),#end All Loans tab

                tabPanel("OCC-Regulated Banks",
                        br(),
                        fluidRow(column(12, textOutput("clickTextNat"))),
                        br(),
                        selectInput("assetCat", "Choose OCC-regulated bank asset size category", 
                                    choices=c("All", "Less than $1bn", "$1bn - 2.49bn","$2.5bn - 9.9bn",
                                               "$10bn - 99.9bn","$100bn and above"), width='25%'),
                         fluidRow(column(12,leafletOutput("mapNatBank"))),
                         br(),
                         fluidRow(column(5, offset=1, plotOutput("bankLoanRangePlot")),
                                  column(5, plotOutput("bankJobsPlot"))),
                          fluidRow(column(10, offset=1, plotOutput("bankIndustriesPlot"))),
                          br(),
                          fluidRow(column(10, offset=1, plotOutput("bankLenderRangeplot"))),
                          br(),
                          fluidRow(column(12, textOutput("warningTextNat"))),
                          fluidRow(column(12, textOutput("dataSourceNat")))
                
                ),#end OCC regulated banks tab

                tabPanel("Jobs Retained",
                         br(),
                         fluidRow(column(8, offset=2, plotOutput("corrJobsAll"))),
                         br(),
                         fluidRow(column(8, offset=2, plotOutput("corrJobsNat")))                      
                         )#End jobs Retained panel
                
            )#end tabsetPanel
      )#end ui

server <- function(input, output, session) {
  
  #click text
  output$clickText <- renderText({
    "Please click on a state to view plots. Zoom out to view Alaska and Hawaii."
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles("CartoDB.Positron") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)%>% #Open map so it centers on US
      addPolygons(data = states_merged_SBA, 
                  fillColor = ~pal(states_merged_SBA$total), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                       popup=~NAME,
                       layerId = ~state
                  )%>%
      addLegend(pal = pal, 
                values = states_merged_SBA$total, 
                position = "bottomright", 
                title = "No. of Loans<br />per 10,000<br/>residents")
  })
  
  # generate data in reactive
  ggplot_data <- reactive({
    state_click <- input$map_shape_click
    SBA_state_plot[SBA_state_plot$State %in% state_click,]
      })
  
  #Plot of number of loans at each loan range level for a selected state
  output$stateLoanRangePlot <- renderPlot({
    
    if(nrow(ggplot_data())>0){
      my_data <- ggplot_data()
    } else {
      my_data <- SBA_state_plot[SBA_state_plot$State %in% "CA",]
    }
    
     ggplot(data=my_data,aes(x=LoanRange1))+
      geom_bar(color="gray48", fill="cadetblue3")+
      theme_bw()+
      theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8))+
      xlab("Loan Range")+
      ylab("Number of Loans")+
      scale_x_discrete(breaks=c("e $150,000-350,000", 
                                "d $350,000-1 million", 
                                "c $1-2 million", 
                                "b $2-5 million",
                                "a $5-10 million"),
                       labels=c("$150k-350k", 
                                "$350k-1m", 
                                "$1m-2m", 
                                "$2m-5m",
                                "$5m-10m"))+
      scale_y_continuous(labels=scales::comma, limits=c(0,50000))+
       geom_text(stat='count', aes(label=comma(..count..)), vjust=-.25)+
       ggtitle(paste0("Number of Loans in Each Loan Range for ",unique(my_data$State)))
     
  })
 
  
  #Plot of number of jobs saved at each loan range level for selected state
  output$stateJobsPlot <- renderPlot({
    
    if(nrow(ggplot_data())>0){
      my_data <- ggplot_data()
    } else {
      my_data <- SBA_state_plot[SBA_state_plot$State %in% "CA",]
    }
    
      ggplot(data=my_data, aes(x=LoanRange1, y=JobsRetained))+
      geom_jitter(color="cadetblue3")+
      theme_bw()+
      theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=1, hjust=1))+
      xlab("Loan Range")+
      ylab("Number of Jobs Retained")+
      scale_x_discrete(breaks=c("e $150,000-350,000", 
                                "d $350,000-1 million", 
                                "c $1-2 million", 
                                "b $2-5 million",
                                "a $5-10 million"),
                       labels=c("$150k-350k", 
                                "$350k-1m", 
                                "$1m-2m", 
                                "$2m-5m",
                                "$5m-10m"))+
      scale_y_continuous(labels=scales::comma, limits=c(0,600))+
        ggtitle(paste0("Number of Jobs Retained in Each Loan Range for ",unique(my_data$State)))
  })
  
  #Plot of number of jobs saved in each industry for selected state
  output$stateIndustriesPlot <- renderPlot({
    
    if(nrow(ggplot_data())>0){
      my_data <- ggplot_data()
    } else {
      my_data <- SBA_state_plot[SBA_state_plot$State %in% "CA",]
    }
    
    #Prepare data for use
    my_data%>%select(State, IndustryName, JobsRetained)%>%
      group_by(State, IndustryName)%>%
      summarise(statejobsInd = sum(JobsRetained, na.rm=TRUE))%>%
      group_by(State)%>%top_n(n=10)%>%
      ggplot(aes(reorder(statejobsInd, x=IndustryName), y=statejobsInd))+
      geom_col(color="gray48", fill="cadetblue3")+
      theme_bw()+
      theme(axis.text.x=element_text(color = "black", size=10, angle=30,vjust=1, hjust=1))+
      xlab("Industries")+
      ylab("Number of Jobs Retained")+
      scale_y_continuous(labels=scales::comma, limits=c(0,600000))+
      geom_text(aes(label=comma(statejobsInd)), vjust=-.25, hjust=.25)+
      ggtitle(paste0("Number of Jobs Retained for ", unique(my_data$State),
                     "\nTen Industries with Largest No. of Jobs Retained" ))
  })
  
 
  #Plot of number of loans for top ten lenders in state
  output$stateLenderRangeplot <- renderPlot({
    
    if(nrow(ggplot_data())>0){
      my_data <- ggplot_data()
    } else {
      my_data <- SBA_state_plot[SBA_state_plot$State %in% "CA",]
    }
    
    #Prepare data for use
    my_data%>%
      inner_join(top_banks, by=c("State", "Lender"))%>%
      select(State, Lender, LoanRange1)%>%  
      group_by(Lender)%>%
      mutate(tot_range_lender=n())%>%
      ungroup()%>%
      group_by(Lender, LoanRange1)%>%
      ggplot(aes(x=reorder(Lender, -tot_range_lender)))+
      geom_bar(aes(fill=LoanRange1))+
      theme_bw()+
      theme(axis.text.x=element_text(color = "black", size=10, angle=60, vjust=1, hjust=1))+
      xlab("Lenders and Loan Ranges")+
      ylab("Number of Loans by Loan Range")+
      scale_y_continuous(labels=scales::comma, limits=c(0,8000))+
      geom_text(stat='count', aes(label=comma(..count..)), vjust=-.25)+
      ggtitle(paste0("Number of Loans - Ten Largest Lenders in ", unique(my_data$State)))+
      scale_fill_manual(values=c("cadetblue2", 
                                 "cadetblue1",
                                 "cadetblue3",
                                 "cadetblue4",
                                 "gray48"),
                        breaks=c("e $150,000-350,000",
                                 "d $350,000-1 million",
                                 "c $1-2 million",
                                 "b $2-5 million",
                                 "a $5-10 million"),
                        labels=c("$150k-350k",
                                 "$350k-1m",
                                 "$1m-2m",
                                 "$2m-5m",
                                 "$5m-10m"),
                        name="Loan Ranges")
    
  })
  
  #Show correlation between jobs retained and state population
  output$corrJobsAll <- renderPlot({
    SBA%>%
      inner_join(pop, by='State') %>%
      select(State, pop_est, JobsRetained) %>%
      group_by(State, pop_est) %>%
      summarise(jobs_state = sum(JobsRetained, na.rm = TRUE)) %>%
      ggplot(aes(x = pop_est, y = jobs_state)) +
      geom_point() +
      scale_x_log10() +
      scale_y_log10() +
      geom_smooth(method = lm) +
      labs(x = "State Population (log)", y= "No. of Jobs Retained (log)") +
      geom_text(aes(label=State), check_overlap = TRUE, size=3, vjust=-1)+
      theme_bw()+
      ggtitle("Number of Jobs Retained and State Population - All Banks")
    
  })
  
  #Show correlation between jobs retained and county population
  output$corrJobsNat <- renderPlot({
    nat_banks%>%
      inner_join(pop, by='State') %>%
      select(State, pop_est, JobsRetained) %>%
      group_by(State, pop_est) %>%
      summarise(jobs_state = sum(JobsRetained, na.rm = TRUE)) %>%
      ggplot(aes(x = pop_est, y = jobs_state)) +
      geom_point() +
      scale_x_log10() +
      scale_y_log10() +
      geom_smooth(method = lm) +
      labs(x = "State Population (log)", y= "No. of Jobs Retained (log)") +
      geom_text(aes(label=State), check_overlap = TRUE, size=3, vjust=-1)+
      theme_bw()+
      ggtitle("Number of Jobs Retained and State Population - OCC-Regulated Banks")
  })
  
  
  #click text
  output$clickTextNat <- renderText({
    "Please select an asset category and/or a state to view data for OCC-regulated banks. Please note, only OCC-regulated banks that could be conclusively matched with SBA Lenders are included in this analysis."
  })
  
  output$mapNatBank <- renderLeaflet({
    leaflet() %>%
      addTiles("CartoDB.Positron") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)%>% #Open map so it centers on US
      addPolygons(data = states_merged_SBA_nat,
                  fillColor = ~palnat(states_merged_SBA_nat$total),
                  fillOpacity = 0.7,
                  weight = 0.2,
                  smoothFactor = 0.2,
                  popup=~NAME,
                  layerId = ~state
      )%>%
      addLegend(pal = palnat,
                values = states_merged_SBA_nat$total,
                position = "bottomright",
                title = "No. of Loans<br />per 10,000<br/>residents<br />All OCC Banks")
  })
  
  #generate data in reactive
  ggplot_data_nat <- reactive({
    state_click_nat <- input$mapNatBank_shape_click
    SBA_state_plot_nat[SBA_state_plot_nat$State %in% state_click_nat,]
  })
  
  #generate data in reactive
  ggplot_data_nat1 <- reactive({
    state_click_nat <- input$mapNatBank_shape_click
    SBA_state_plot_nat_all[SBA_state_plot_nat_all$State %in% state_click_nat,]
  })
  
  output$bankLoanRangePlot <- renderPlot({
      #no selections
      if ((input$assetCat=="" | input$assetCat=="All") & (nrow(ggplot_data_nat())==0)){ 
      my_data_nat <- SBA_state_plot_nat_all2
    } #asset selection & no state selected
      else if ((input$assetCat!="" | input$assetCat!="All") & (nrow(ggplot_data_nat())==0)){ 
      my_data_nat <- SBA_state_plot_nat_all1
      #no asset selection & state selected
    } else if ((input$assetCat=="" | input$assetCat=="All") & (nrow(ggplot_data_nat())!=0)){ 
      my_data_nat <- ggplot_data_nat1()
      #asset and state selected
    } else if ((input$assetCat!="" | input$assetCat!="All") & (nrow(ggplot_data_nat())!=0)){
      my_data_nat <- ggplot_data_nat() 
    }

    my_data_nat%>%
      filter(asset_cat==!!input$assetCat)%>%
      ggplot(aes(x=LoanRange1))+
      geom_bar(color="gray48", fill="cadetblue3")+
      theme_bw()+
      theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8))+
      xlab("Loan Range")+
      ylab("Number of Loans")+
      scale_x_discrete(breaks=c("e $150,000-350,000",
                                "d $350,000-1 million",
                                "c $1-2 million",
                                "b $2-5 million",
                                "a $5-10 million"),
                       labels=c("$150k-350k",
                                "$350k-1m",
                                "$1m-2m",
                                "$2m-5m",
                                "$5m-10m"))+
      #scale_y_continuous(labels=scales::comma, limits=c(0,150000))+
      geom_text(stat='count', aes(label=comma(..count..)), vjust=-.25)+
      ggtitle(paste0("Number of Loans in Each Loan Range for ",unique(my_data_nat$State)))
  })
  
   #Plot of number of jobs saved at each loan range level for selected state
  output$bankJobsPlot <- renderPlot({
    #no selections
    if ((input$assetCat=="" | input$assetCat=="All") & (nrow(ggplot_data_nat())==0)){ 
      my_data_nat <- SBA_state_plot_nat_all2
    } #asset selection & no state selected
    else if ((input$assetCat!="" | input$assetCat!="All") & (nrow(ggplot_data_nat())==0)){ 
      my_data_nat <- SBA_state_plot_nat_all1
      #no asset selection & state selected
    } else if ((input$assetCat=="" | input$assetCat=="All") & (nrow(ggplot_data_nat())!=0)){ 
      my_data_nat <- ggplot_data_nat1()
      #asset and state selected
    } else if ((input$assetCat!="" | input$assetCat!="All") & (nrow(ggplot_data_nat())!=0)){
      my_data_nat <- ggplot_data_nat() 
    }
    
    my_data_nat%>%
      filter(asset_cat==!!input$assetCat)%>%
      ggplot(aes(x=LoanRange1, y=JobsRetained))+
        geom_jitter(color="cadetblue3")+
        theme_bw()+
        theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=1, hjust=1))+
        xlab("Loan Range")+
        ylab("Number of Jobs Retained")+
        scale_x_discrete(breaks=c("e $150,000-350,000", 
                                  "d $350,000-1 million", 
                                  "c $1-2 million", 
                                  "b $2-5 million",
                                  "a $5-10 million"),
                         labels=c("$150k-350k", 
                                  "$350k-1m", 
                                  "$1m-2m", 
                                  "$2m-5m",
                                  "$5m-10m"))+
        #scale_y_continuous(labels=scales::comma, limits=c(0,600))+
      ggtitle(paste0("Number of Jobs Retained in Each Loan Range for ",unique(my_data_nat$State)))
    })
  

  output$bankIndustriesPlot <- renderPlot({
    #no selections
    if ((input$assetCat=="" | input$assetCat=="All") & (nrow(ggplot_data_nat())==0)){ 
      my_data_nat <- SBA_state_plot_nat_all2
    } #asset selection & no state selected
    else if ((input$assetCat!="" | input$assetCat!="All") & (nrow(ggplot_data_nat())==0)){ 
      my_data_nat <- SBA_state_plot_nat_all1
      #no asset selection & state selected
    } else if ((input$assetCat=="" | input$assetCat=="All") & (nrow(ggplot_data_nat())!=0)){ 
      my_data_nat <- ggplot_data_nat1()
      #asset and state selected
    } else if ((input$assetCat!="" | input$assetCat!="All") & (nrow(ggplot_data_nat())!=0)){
      my_data_nat <- ggplot_data_nat() 
    }
    
    my_data_nat%>%
      filter(asset_cat==!!input$assetCat)%>%
      select(State, IndustryName, JobsRetained)%>%
      group_by(State, IndustryName)%>%
      summarise(statejobsInd = sum(JobsRetained, na.rm=TRUE))%>%
      group_by(State)%>%top_n(n=10)%>%
      ggplot(aes(reorder(statejobsInd, x=IndustryName), y=statejobsInd))+
      geom_col(color="gray48", fill="cadetblue3")+
      theme_bw()+
      theme(axis.text.x=element_text(color = "black", size=10, angle=30,vjust=1, hjust=1))+
      xlab("Industries")+
      ylab("Number of Jobs Retained")+
      #scale_y_continuous(labels=scales::comma, limits=c(0,1500000))+
      geom_text(aes(label=comma(statejobsInd)), vjust=-.25, hjust=.25)+
      ggtitle(paste0("Number of Jobs Retained for ", unique(my_data_nat$State),
                     "\nTen Industries with Largest No. of Jobs Retained" ))
  })
  

  output$bankLenderRangeplot <- renderPlot({
    #no asset and no state selected
    if ((input$assetCat=="" | input$assetCat=="All") & (nrow(ggplot_data_nat())==0)){ 
      my_data_nat <- SBA_state_plot_nat_all2%>%
        inner_join(top_banks_nat_all, by=c("State", "Lender"))%>%
        select(State, Lender, LoanRange1)%>%  
        group_by(Lender)%>%
        mutate(tot_range_lender=n())%>%
        ungroup()%>%
        group_by(Lender, LoanRange1)
    } #asset selection & no state selected
    else if ((input$assetCat!="" | input$assetCat!="All") & (nrow(ggplot_data_nat())==0)){ 
      my_data_nat <- SBA_state_plot_nat_all1%>%
        filter(asset_cat==!!input$assetCat)%>%
        group_by(State, Lender)%>%
        summarize(tot_range_lender=n())%>% 
        top_n(n=10)%>%
        inner_join(SBA_state_plot_nat_all1, by=c("State","Lender"))%>%
        select(State, Lender, LoanRange1, tot_range_lender)%>% 
        group_by(Lender, LoanRange1)
      #no asset selection & state selected
    } else if ((input$assetCat=="" | input$assetCat=="All") & (nrow(ggplot_data_nat())!=0)){ 
      my_data_nat <- ggplot_data_nat1() %>%
        inner_join(top_banks_nat, by=c("State", "Lender"))%>%
        select(State, Lender, LoanRange1, asset_cat)%>%  
        group_by(Lender)%>%
        mutate(tot_range_lender=n())%>%
        ungroup()%>%
        group_by(State, Lender)%>%
        top_n(n=10)
      #asset and state selected
    } else if ((input$assetCat!="" | input$assetCat!="All") & (nrow(ggplot_data_nat())!=0)){
      my_data_nat <- ggplot_data_nat() %>% 
        filter(asset_cat==!!input$assetCat)%>% 
        select(State, Lender, LoanRange1, asset_cat)%>%  
        group_by(State, Lender)%>%
        summarize(tot_range_lender=n())%>%
        top_n(n=10)%>%
        inner_join(SBA_state_plot_nat, by=c("State", "Lender"))%>%
        select(State, Lender, LoanRange1, tot_range_lender)%>%  
        group_by(Lender, LoanRange1)
    }
    
    my_data_nat%>%
      ggplot(aes(x=reorder(Lender, -tot_range_lender)))+
      geom_bar(aes(fill=LoanRange1))+
      theme_bw()+
      theme(axis.text.x=element_text(color = "black", size=10, angle=60, vjust=1, hjust=1))+
      xlab("Lenders and Loan Ranges")+
      ylab("Number of Loans by Loan Range")+
      #scale_y_continuous(labels=scales::comma, limits=c(0,37000))+
      geom_text(stat='count', aes(label=comma(..count..)), vjust=-.25)+
      ggtitle(paste0("Number of Loans - Ten Largest Lenders in ", unique(my_data_nat$State)))+
      scale_fill_manual(values=c("cadetblue2", 
                                 "cadetblue1",
                                 "cadetblue3",
                                 "cadetblue4",
                                 "gray48"),
                        breaks=c("e $150,000-350,000",
                                 "d $350,000-1 million",
                                 "c $1-2 million",
                                 "b $2-5 million",
                                 "a $5-10 million"),
                        labels=c("$150k-350k",
                                 "$350k-1m",
                                 "$1m-2m",
                                 "$2m-5m",
                                 "$5m-10m"),
                        name="Loan Ranges")

  })

  #warning text
  output$warningText <- renderText({
    "Note: Of the 661,218 loans reported by SBA, 48,922 loans reported 0 jobs retained; an 
    additional 40,506 observations did not report job-retention data. Additionally, some companies included in this database
    have indicated the information reported by SBA is inaccurate or incomplete."
  })
  
  #data source
  output$dataSource <- renderText({
    "The data used in these plots was downloaded from
     https://home.treasury.gov/policy-issues/cares-act/assistance-for-small-businesses/sba-paycheck-protection-program-loan-level-data."
  })
  
  #warning text nat
  output$warningTextNat <- renderText({
    "Note: Of the 208,690 loans made by national banks that could be conclusively matched and reported by SBA, 14,252 
    loans reported 0 jobs retained; an addition 24,632 observations did not report job-retention data."
  })
  
  #data source nat
  output$dataSourceNat <- renderText({
    "The data used in these plots was downloaded from
     https://home.treasury.gov/policy-issues/cares-act/assistance-for-small-businesses/sba-paycheck-protection-program-loan-level-data."
  })

  # # output$SBA_df <- renderDataTable({
  # #   SBA_orig
  # # }, options = list(pageLength = 10))
  # 
  # thedata <- reactive(SBA_orig)
  # 
  # output$SBA_df <- renderDataTable({
  #   SBA_orig
  # }, options = list(pageLength = 10))
  # 
  # output$download <- downloadHandler(
  #   filename = function(){"SBA.csv"}, 
  #   content = function(fname){
  #     write.csv(thedata(), fname)
  #   }
  # )
  
  }
shinyApp(ui, server)