#' Roxygen style comments
#' This structure supports later package documentation
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(shinydashboard)
library(ggplot2)
library(tigris)
options(shiny.maxRequestSize = 30*1024^2)


SBA <- get(load(file = "../data/SBA.Rdata"))
SBA_orig<-SBA #create copy of SBA dataset with all fields for use in table tab

#elmininate all fields we don't need for visualization and statistics tabs to
#make loading faster
SBA%>%select(-BusinessName, -Address, -BusinessType, -RaceEthnicity, -Gender, 
             -Veteran, -DateApproved, -NonProfit)->SBA
naics <- read_csv("../data/naics.csv") #bring in industry data
#bring in census population data estimates for 2019
#source: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage
pop <- read_csv("../data/pop_census_2019.csv")
# obtain population/10000
pop%>%  mutate(pop_10000=pop_est/10000)->pop

state <- unique(SBA$State)

#replace naics code with two-digit industry categories
SBA%>%mutate(Sector=as.numeric(str_extract(NAICSCode, "\\d\\d")))%>%
  inner_join(naics, by="Sector")->SBA
# Subsample df for statistical analysis
#SBA_samp <- SBA %>% sample_frac(0.1) 

#Find Top Lender
lend_vec <- vector(mode = "character", length = length(state))
n_vec <- vector(mode = "double", length = length(state))
for (i in seq_along(n_vec))
{
  SBA %>%
    filter(State == state[[i]]) %>%  ##CHANGED
    group_by(Lender) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    slice(1) -> temp
  
  lend_vec[[i]] <- temp[[1]]
  n_vec[[i]] <- temp[[2]]
  
}

top_lend <- data.frame(state, lend_vec, n_vec) ##CHANGED
top_lend %>%
  filter(state != "XX") -> top_lend

#Find Total Num of Loans given in each range by state
loan_range_summary <- function(state){
  SBA %>%
    filter(State == state) %>%
    group_by(LoanRange) %>%
    mutate(n = n()) %>%
    arrange(desc(n)) %>%
    select(State,LoanRange, n) %>%
    distinct()
}

loan_range_df <- map_df(unique(SBA$State), loan_range_summary)

#Combine and clean data for mapping
inner_join(top_lend,loan_range_df,by = c("state"= "State")) %>%
  pivot_wider(names_from = LoanRange, values_from = n) %>%
  rename("Loan_150K_350K" = `e $150,000-350,000`,
         "Loan_350K_1_mil"= `d $350,000-1 million`,
         "Loan_1_2_mil" = `c $1-2 million`,
         'Loan_2_5_mil' = `b $2-5 million`,
         "Loan_5_10_mil" = `a $5-10 million`) -> geocode
#prepare for use in state plot
SBA%>%mutate(LoanRange1=factor(LoanRange, levels=c("e $150,000-350,000", 
                                                   "d $350,000-1 million", 
                                                   "c $1-2 million", 
                                                   "b $2-5 million",
                                                   "a $5-10 million")))->SBA_state_plot #look into relabeling
#Prepare count of loans by state
SBA %>%
group_by(State)%>%
  summarize(total = n())%>%
  inner_join(pop, by="State")%>%
  mutate(total=(total/pop_10000))%>% #obtain number of loans per 10,000
  rename(state=State)->loans_by_state

top_lend%>%inner_join(loans_by_state, by="state")->loans_by_state1
# Now we use the Tigris function geo_join to bring together 
# the states shapefile and the sb_states dataframe -- STUSPS and state 
# are the two columns they'll be joined by
states <- states(cb=T)
states_merged_SBA <- geo_join(states, loans_by_state1, "STUSPS", "state", how="inner")
# Creating a color palette based on the number range in the total column
pal <- colorNumeric("Oranges", domain=loans_by_state$total)

source("dashboard.R",local = TRUE)
ui <- fluidPage(theme = shinytheme("sandstone"),
                titlePanel("2020 SBA Loan Explorer"),
                tabsetPanel(
                  tabPanel("Visualization",
                           fluidRow(column(12,
                                           leafletOutput("map"))),
                           br(),
                           fluidRow(column(4, plotOutput("stateLoanRangePlot")),
                                    column(4, plotOutput("stateJobsPlot")),
                                    column(4, plotOutput("stateIndustriesPlot"))),
                  ),#end Visualization tab
                  tabPanel("Statistical Modeling",
                           dashboardPage(
                             skin = "black",
                             dashboardHeader(title = "SBA: Job Retention"),
                             sidebar,
                             mainpage
                           )
                  ),#end Statistical Modeling tab
                  
                  tabPanel("Data",
                           dataTableOutput("SBA_df")
                  )#end Data tab
                )#end titlePanel
)#end ui
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Wikimedia") %>%
      setView(lng = -93.85, lat = 37.45,zoom = 4) %>% #Open map so it centers on US
      addPolygons(data = states_merged_SBA, 
                  fillColor = ~pal(states_merged_SBA$total), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup= ~paste("State:",STUSPS,"<br>",
                               "Top Lender:",lend_vec,"<br>",
                               "Num of Loans from Top Lender:",n_vec,"<br>"),
                  layerId = ~STUSPS,
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
                     labels=c("$150,000-350,000",
                              "$350,000-1 million",
                              "$1-2 million",
                              "$2-5 million",
                              "$5-10 million"))+
    scale_y_continuous(labels=scales::comma, limits=c(0,50000))+
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
    theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8))+
    xlab("Loan Range")+
    ylab("Number of Jobs Retained")+
    scale_x_discrete(breaks=c("e $150,000-350,000",
                              "d $350,000-1 million",
                              "c $1-2 million",
                              "b $2-5 million",
                              "a $5-10 million"),
                     labels=c("$150,000-350,000",
                              "$350,000-1 million",
                              "$1-2 million",
                              "$2-5 million",
                              "$5-10 million"))+
    scale_y_continuous(labels=scales::comma, limits=c(0,600))+
    ggtitle(paste0("Number of Jobs Retained in Each Loan Range for ",unique(my_data$State)))
})

#Plot of number of jobs saved in each industry for selected state
output$stateIndustriesPlot <- renderPlot({
  #Prepare data for use
  if(nrow(ggplot_data())>0){
    my_data <- ggplot_data()
  } else {
    my_data <- SBA_state_plot[SBA_state_plot$State %in% "CA",]
  }
  my_data%>%select(State, IndustryName, JobsRetained)%>%
    group_by(State, IndustryName)%>%
    summarise(statejobsInd = sum(JobsRetained, na.rm=TRUE))%>%
    group_by(State)%>%top_n(n=10)%>%
    ggplot(aes(reorder(statejobsInd, x=IndustryName), y=statejobsInd))+
    geom_col(color="gray48", fill="cadetblue3")+
    theme_bw()+
    theme(axis.text.x=element_text(color = "black", size=10, angle=30, vjust=.8, hjust=0.8))+
    xlab("Industries")+
    ylab("Number of Jobs Retained")+
    scale_y_continuous(labels=scales::comma, limits=c(0,600000)) +
    ggtitle(paste0("Number of Jobs Retained for ", unique(my_data$State),
                   "\nTen Industries with Largest No. of Jobs Retained" ))
})
output$SBA_df <- renderDataTable({
  SBA_orig
}, options = list(pageLength = 10))


#########################
# State vs Jobs Retained
#########################
#SBA_samp <- SBA %>% sample_frac(0.1)

# Plot
output$stateplot <- renderPlot({
  SBA %>%
    sample_frac(0.1) %>%
    mutate_at(vars(Zip), funs(factor)) %>%
    filter(State == input$stateInput) -> st
  
  head(st)
  
  ggplot(st, aes(x = Zip, y = JobsRetained)) +
    geom_boxplot() +
    theme_classic() + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
})

# Model output
output$STsummary <- renderPrint({
  SBA %>%
    sample_frac(0.1) %>%
    mutate(LoanRange = recode(LoanRange, 
                              "a $5-10 million" = 5, 
                              "b $2-5 million" = 4,
                              "c $1-2 million" = 3,
                              "d $350,000-1 million" = 2,
                              "e $150,000-350,000" = 1)) %>%
    filter(State == input$stateInput) -> stmod
  
  st_df_lm <- lm(JobsRetained ~ Zip, data = stmod) # Fit model
  #summary(lr_df_lm)
  broom::tidy(st_df_lm)[c(1,2,5)]
  # {map(.$mod, summary)}
})
##############################
# Loan Range vs Jobs Retained
##############################
# Plot
output$loanplot <- renderPlot({
  SBA %>%
    sample_frac(0.1) %>%
    inner_join(pop, by='State') %>%
    select(State, pop_est, JobsRetained) %>%
    group_by(State) %>%
    summarise(pop_est_mn = mean(pop_est), jobs_mn = mean(JobsRetained, na.rm = TRUE)) %>%
    ggplot(aes(x = pop_est_mn, y = jobs_mn)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    geom_smooth(method = lm) +
    labs(x = "Population (avg per state)", y= "Jobs Retained (n)") +
    theme_classic()
})
# Model Output
output$Loansummary <- renderPrint({

  SBA %>%
    sample_frac(0.1) %>%
    mutate(LoanRange = recode(LoanRange, 
                              "a $5-10 million" = 1, 
                              "b $2-5 million" = 2,
                              "c $1-2 million" = 3,
                              "d $350,000-1 million" = 4,
                              "e $150,000-350,000" = 5)) %>%
    mutate(vars(State), funs(factor)) %>%
    mutate_if(is.factor, as.numeric)-> stmod
  
  lr_df_lm <- lm(JobsRetained ~ LoanRange + State, data = stmod) 
  
  jr <- predict(lr_df_lm, newdata = data.frame(LoanRange = as.numeric(input$LR), State = input$stateInput))
  jr
  
})

}
shinyApp(ui, server)