# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#
library(cowplot)
library(ggplot2)
library(tidyverse) # Includes ggplot and dplyr
library(ggalt)
library(ggpointdensity) # To address overplotting and color lines with density
library(ggrepel)
library(patchwork) # To combine plots
library(ggforce)
library(maps) # Map data for states and counties
library(mapproj) # For a range of map projections: mercator, albers, mollweide, gilbert...
library(usmap) # For 50 states
library(ggmap)
library(ggiraph)
library(sf)
library(sp)
library(maptools)
library(scales)
library(data.table)
library(sjmisc)
#load crimes data set
count_chicagocrime.df2 = read.csv(file = "small2.csv")






#load neighborhood names data set
neighborhood <- read.csv("Neigh.csv")





#left join crimes and neighborhood names dataset
count.crimes.df = left_join(count_chicagocrime.df2, neighborhood, by = "Community.Area")





#load population dataset and change and Community.Area to Neighborhood
pop = read.csv("population.csv")
colnames(pop)[2] = "Neighborhood"





#left join population dataset with names crimes dataset map
pop.chicagocrime.df = left_join(count.crimes.df, pop)





#make Neighborhood values lowercase for joining purposes
pop.chicagocrime.df$Neighborhood_lower = tolower(pop.chicagocrime.df$Neighborhood)





# #load map of neighborhood
chi_map <- read_sf("https://raw.githubusercontent.com/thisisdaryn/data/master/geo/chicago/Comm_Areas.geojson")
#
# #change community values to lowercase and change column name to "Neighborhood_lower" to join with "Neighborhood_lower" in count.crimes.df
chi_map$community_lower = tolower(chi_map$community)
chi_map = chi_map %>%
    rename(Neighborhood_lower = community_lower)





#PER CAPITA violent/nonviolent dataframe
percapita.df = pop.chicagocrime.df %>%
    group_by(Neighborhood) %>%
    summarise(percapita_violent = round(((sum_violent/X2010)/20)*100, 2), percapita_nonviolent = round(((sum_nonviolent/X2010)/20)*100, 2))





#make percapita values lowercase for joining purposes
percapita.df$Neighborhood_lower = tolower(percapita.df$Neighborhood)





#join sf object (chi_map) to another datafame (count.crimes.df) so we can plot
percapita_chi_map = left_join(percapita.df, chi_map)





#create dataframe of counties you want labeled for violent crimes
violent.labels.df = percapita_chi_map %>%
    filter(Neighborhood %in% c("Fuller Park", "Forest Glen"))





#centroid coordinates of violent neighborhoods we want to label
mid.violent = sf::st_centroid(violent.labels.df$geometry)
#Forest Glen = -87.75835 41.99393
#Fuller Park= -87.63242 41.80908






library(scales)
library(glue)





#violent crimes per capita, highest = Fuller Park, lowest = Forest Glen
violent<-
    ggplot(data = percapita_chi_map, aes(fill = percapita_violent, geometry = geometry)) +
    geom_sf_interactive(aes(tooltip=glue("Neighborhood: {Neighborhood}\n Violent Crimes Per 100 People: {percapita_violent }"),data_id=Neighborhood_lower)) +
    labs(title = "Risk of encountering violent crimes is highest in Fuller Park and lowest in Forest Glen.", size = 12, subtitle = "Top violent crime types include battery, assault, sex offense, and homicide.", fill = "Violent Crimes per 1000 people per year") +
    
    
    
    scale_fill_viridis_c(option = "viridis", direction = -1, labels = scales::label_number(accuracy = .1)) +
    theme_void() +
    theme(plot.title = element_text(color="black", size=16, face="bold"), plot.subtitle =element_text(color="black", size=14, face="bold"))





ggiraph(code = print(violent))





#create dataframe of counties you want labeled for nonviolent crimes
nonviolent.labels.df = percapita_chi_map %>%
    filter(Neighborhood %in% c("Fuller Park", "Edison Park"))





#centroid coordinates of non-violent neighborhoods we want to label
mid = sf::st_centroid(nonviolent.labels.df$geometry)
mid
#Edison Park = -87.81378 42.00761
#Fuller Park = -87.63242 41.80908





#non- violent crimes per capita, highest = Fuller Park, lowest = Edison Park
nonviolent<- ggplot(data = percapita_chi_map, aes(fill = percapita_nonviolent, geometry = geometry)) +
    geom_sf_interactive(aes(tooltip=glue("Neighborhood: {Neighborhood}\n Non-Violent Crimes Per 100 People: {percapita_nonviolent }"),data_id=Neighborhood_lower))+
    # labs(subtitle = expression(atop("Risk is highest in Fuller Park and lowest in Edison Park", atop(scriptstyle("Top non-violent crimes include theft, criminal damage, narcotics, and burglary.")))), fill = "Non-Violent Crimes Per 100 People") +
    labs(title = "Risk of encountering non-violent crimes is highest in Fuller Park and lowest in Edison Park.", size = 12, subtitle="Top non-violent crime types include theft, criminal damage, narcotics, and burglary.", fill = "Non-Violent Crimes per 1000 people per year") +
    
    scale_fill_viridis_c(option = "viridis", direction = -1, labels = scales::label_number(accuracy = .1)) +
    theme_void() +
    theme(plot.title = element_text(color="black", size=16, face="bold"), plot.subtitle =element_text(color="black", size=14, face="bold"))






ggiraph(code = print(nonviolent))






ggiraph(code = print(violent/nonviolent))





combined = violent + nonviolent





timeseries = read.csv("timeh8.csv")





timeseries2 <-timeseries %>%
    unique() %>%
    group_by(Hours) %>%
    mutate(refnonviolent = (sum(Nonviolentpercapita)/77)*10, refviolent = (sum(violentpercapita)/77)*10)






library(shiny)





ui <- fluidPage(
    
    # Application title
    titlePanel("Risk of Crime in Chicago Neighborhoods"),
    h3("Chicago per capita violent crime vs. per capita non-violent crime by Neighborhood"),
    
    girafeOutput("Map"),
    textOutput("caption"),
    
    h3("Select a neighborhood to compare violent and no-violent crimes per 1000 people per year by hour"),
    
    
    
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "Neighborhood", label = "Select Neighborhood:",
                        selected = "Loop",
                        choices = c( "Albany Park",
                                     "Archer Heights",
                                     "Armour Square",
                                     "Ashburn",
                                     "Auburn Gresham",
                                     "Austin",
                                     "Avalon Park",
                                     "Avondale",
                                     "Belmont Cragin",
                                     "Beverly",
                                     "Bridgeport",
                                     "Brighton Park",
                                     "Burnside",
                                     "Calumet Heights",
                                     "Chatham",
                                     "Chicago Lawn",
                                     "Clearing",
                                     "Douglas",
                                     "Dunning",
                                     "East Garfield Park",
                                     "East Side",
                                     "Edgewater",
                                     "Edison Park",
                                     "Englewood",
                                     "Forest Glen",
                                     "Fuller Park",
                                     "Gage Park",
                                     "Garfield Ridge",
                                     "Grand Boulevard",
                                     "Greater Grand Crossing",
                                     "Hegewisch",
                                     "Hermosa",
                                     "Humboldt Park",
                                     "Hyde Park",
                                     "Irving Park",
                                     "Jefferson Park",
                                     "Kenwood",
                                     "Lake View","Lincoln Park",
                                     "Lincoln Square",
                                     "Logan Square",
                                     "Loop",
                                     "Lower West Side",
                                     "McKinley Park",
                                     "Montclare",
                                     "Morgan Park",
                                     "Mount Greenwood",
                                     "Near North Side",
                                     "Near South Side",
                                     "Near West Side",
                                     "New City",
                                     "North Center",
                                     "North Lawndale",
                                     "North Park",
                                     "Norwood Park",
                                     "O'Hare",
                                     "Oakland",
                                     "Portage Park",
                                     "Pullman",
                                     "Riverdale",
                                     "Roseland",
                                     "South Chicago",
                                     "South Deering",
                                     "South Lawndale",
                                     "South Shore",
                                     "Uptown",
                                     "Washington Heights",
                                     "Washington Park",
                                     "West Elsdon",
                                     "West Englewood",
                                     "West Garfield Park",
                                     "West Lawn",
                                     "West Pullman",
                                     "West Ridge" ,
                                     "West Town",
                                     "Woodlawn",
                                     "Rogers Park")
            )),
        
        mainPanel(
            plotOutput("trend", height = 300)
        )
    )
)







# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$Map = renderGirafe({
        girafe(code=print(combined), width_svg = 20, height_svg = 15, height = 1200, width = 1550)
        
    })
    output$caption=renderText({
        paste("Source:2001-2021 data from Chicago Police Department's CLEAR (Citizen Law Enforcement Analysis and Reporting) System")}
    )
    
    
    #timeseries
    output$trend<-renderPlot({
        
        ggplot(timeseries%>%filter(Neighborhood == input$Neighborhood))+
            geom_line(mapping=aes(Hours,violentpercapita*10, color='Violent'), size = 1)+
            geom_line(mapping=aes(Hours,Nonviolentpercapita*10, color='Non-Violent'),size =1)+
            geom_line(timeseries2, mapping=aes(Hours,refviolent, color = "Violent"),linetype = "dotted", size=1, alpha = 0.5)+
            geom_line(timeseries2, mapping=aes(Hours,refnonviolent, color = "Non-Violent"), linetype = "dotted",size=1, alpha=0.5)+
            labs(title = "Hourly crimes per 1000 people per year", color="Type of Crime")+
            xlab("Hour (24-hour clock)")+
            ylab("Crimes per 1000 people per year")+
            coord_cartesian(ylim = c(0, 9)) +
            theme_bw()+
            scale_colour_manual(values=c("blue", "red"))
        
        
        
        
    })
    
    
    
}





# Run the application
shinyApp(ui = ui, server = server)