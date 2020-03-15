# -------------------------------------------------------------------------
# author  : Gabriel Erichson
# Publish : https://gabriel-erichson.shinyapps.io/lbb03_algoritma_gehm/
# -------------------------------------------------------------------------

# global ------------------------------------------------------------------

## Package -----------------------------------------------------------------
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(glue)
library(plotly)
library(scales)
library(lubridate)
library(echarts4r)
library(highcharter) 
library(stringr)
options(scipen = 123)


## Data
## load data
apps <- read.csv("data/gplaystore32.csv")

# Rename Column name cause it easiest to access
apps <- apps %>% 
    rename(
        app_name = App.Name,
        category = Category,
        rating = Rating,
        reviews = Reviews,
        installs = Installs,
        size = Size,
        price = Price,
        content_rating = Content.Rating,
        last_update = Last.Updated,
        min_version = Minimum.Version,   
        latest_version = Latest.Version,
    )


## adjust data type and remove duplicate data
apps <- apps %>% 
    mutate(
        app_name = as.character(app_name),
        category =  as.factor(gsub("_"," ",category)),
        rating = as.numeric(as.character(rating),2),
        rating = ifelse(is.na(rating),0,rating),
        reviews = as.numeric(reviews,2),
        installs = gsub("\\+", "", as.character(installs)),
        installs = as.numeric(gsub(",", "", installs),2),
        #Mengkonvert size menjadi numeric dalam satuan Megabyte
        #hapus spasi
        size = gsub(" ","", size), 
        #hilangkan karakter M
        size = gsub("M","",size), 
        # Convert Kilobyte menjadi Megabyte
        size = ifelse(grepl("k", size), round(as.numeric(gsub("k","",size))/1080,1), as.numeric(size)),
        size = as.numeric(size,2), #terdapat size `Varies with device` sehingga setelah diconvert menjadi NA Value
        price = as.numeric(gsub("\\$","",price),2),
        type = ifelse(price>0,"paid","free"),
        type = as.factor(type),
        content_rating = as.factor(content_rating),
        last_update = mdy(last_update),
        min_version = as.character(min_version),
        latest_version = as.character(latest_version),
        price_summary = installs*price
    ) %>% 
    distinct()

apps <- within(apps, category[category == ' Channel 2 News'] <- 'NEWS_AND_MAGAZINES')
apps <- within(apps, category[category == ' Podcasts'] <- 'ENTERTAINMENT')
apps <- within(apps, category[category == ')'] <- 'DATING')
apps <- within(apps, price[is.na(price)] <- 0)
apps <- within(apps, price_summary[is.na(price_summary)] <- 0)

apps$category <- droplevels(apps$category)
apps$type <- droplevels(apps$type)
apps$content_rating <- droplevels(apps$content_rating)

#KETIGA DATA INI MEMILIKI NA Value sehingga di exclude saja dari dataset
apps <- apps %>% 
    filter(!app_name == "ELer Japanese - NHK News" & !app_name == "Never have I ever 18+ " & !app_name == "Israel News")


apps.distribution <- apps %>% 
    group_by(category,type) %>% 
    summarise(
        count_app = n(),
        mean_rating = round(mean(rating),2),
        mean_review = round(mean(reviews),2),
        mean_price = round(mean(price),2),
        sum_install = sum(installs),
        sum_price = round(sum(price_summary),2) 
    ) %>%
    ungroup() %>% 
    arrange(desc(count_app))

# Region UI -----------------------------------------------------------------------------------------------------

header <- dashboardHeader(
    title = span(img(src = "icon_yellow_100.png", height = 35), "GPlaystore"),
    titleWidth = 300
)

sidebar <- dashboardSidebar(
    width = 300,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    tags$footer("Gabriel Erichson - ALGORITMA © Copyright 2020",  style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:40px;   /* Height of the footer */
              color: #9f9f9f;
              padding: 10px;
              background-color: #f9fafc;
              border-radius: 0px;
              border-color: transparent;
              font-size: 12px;
              box-shadow: 0 2px 2px 0 rgba(0,0,0,0.18), 0 1px 2px 0 rgba(0,0,0,0.15);
              z-index: 1000;"),
    sidebarMenu(
        menuItem("Application Distribution", tabName = "menu_dashboard", icon = icon("dashboard")),
        menuItem("Datatable", tabName = "menu_datatable", icon = icon("list")),
        menuItem("About", tabName = "menu_about", icon=icon("question"))
    )
)


body <- dashboardBody(
    tags$head(
        
        includeCSS(file.path('www', path = "mycss.css")),
        tags$head(
            tags$style(
                        HTML('.info-box {min-height: 60px;} .info-box-icon {height: 60px; width: 60px; line-height: 60px; font-size: 24px;} .info-box-content {font-size: 50%; padding-top: 10px; padding-bottom: 5px; margin-left: 60px;}')),
            tags$link(rel = "icon", 
                      type = "image/png", 
                      href = "icon_yellow_100.png")
            )
    ),
    useShinyjs(),
    tabItems(
        tabItem(
            tabName = "menu_dashboard",
            h2("Application Distribution in Google Play Store"),
            br(),
            fluidRow(
                column(
                    width = 6,
                    box(
                        width = 12,
                        radioButtons(
                            inputId = "rb_type",
                            label = "Purchase Type:",
                            choices = c("All" = "all", "Free" = "free","Paid" = "paid"),
                            inline = TRUE,
                            selected = "all"
                        )
                    )
                ),
                column(
                    width = 6,
                    sliderInput("slider_topn", label = h4("Top N Category based on Total Install:"), min = 5, max = 30, value = 10, width = 500)
                    
                )
                
            ),
            fluidRow(
                column(
                    width = 6,
                    infoBoxOutput("valbox_best_category",width = 6),
                    infoBoxOutput("valbox_best_install",width = 6),
              
                        highchartOutput("hc1",height = 420)
                    
                ),
                column(
                    width = 6,
                    plotlyOutput(outputId ="plot1")
                )
            )
            
        ),
        tabItem(
            tabName = "menu_datatable",
            h2("Application in Google Play Store"),
            br(),
            fluidRow(
                box(
                    column(
                        width = 6,
                       radioButtons(
                           inputId = "rb_type_datatable",
                           label = "Type:",
                           choices = c("All" = "all", "Free" = "free","Paid" = "paid"),
                           inline = TRUE,
                           selected = "all"
                           
                       )
                           
    
                    ),
                    column(
                        width = 6,
                        selectInput(
                                    "cmb_category",
                                    "Category:",
                                    c("All",
                                      unique(sort(levels(apps$category),decreasing = F))
                                      )
                                    )
                    )
                )
                
            ),
            DT::dataTableOutput("table_app")
        ),
        tabItem(
            tabName = "menu_about",
            includeMarkdown("README.md")
        )
        
    )
)

ui <- tagList(
    dashboardPage(
        title = "Gplaystore",
        skin = "black",
        header,
        sidebar,
        body
    )
)


# Region Server ----------------------------------------------------------------------------------------------
# Server Connection -------------------------------------------------------
server <- function(input, output) { 
    v_best_category <- ""
    v_best_rating <- ""
    v_best_install <- ""
    
    
    output$valbox_best_category <- renderValueBox({
        if(input$rb_type == "all"){
            v_best_category <- apps.distribution %>% 
                group_by(category) %>% 
                summarise(total = sum(count_app)) %>% 
                ungroup() %>% 
                arrange(desc(total)) %>% 
                select(category) %>% 
                head(1)
        }
        else{
       
            v_best_category <- apps.distribution %>%
                filter(type == input$rb_type) %>% 
                arrange(desc(count_app)) %>% 
                select(category) %>% 
                head(1)
        }
        
        
        infoBox(
            "Top 1 Category", 
            tags$p(v_best_category, style = "font-size: 80%;"), 
            icon = icon("thumbs-up"),
            color = "blue", fill = TRUE
        )
    })
    
    
    output$valbox_best_install <- renderValueBox({

        if(input$rb_type == "all"){
            v_best_install <- apps.distribution %>% 
                group_by(category) %>% 
                summarise(total = sum(count_app), sum_install = sum(sum_install)) %>% 
                ungroup() %>% 
                arrange(desc(total)) %>% 
                select(sum_install) %>% 
                head(1)
        }
        else{
            
            v_best_install <-  apps.distribution %>% 
                filter(type == input$rb_type) %>% 
                group_by(category) %>% 
                summarise(total = sum(count_app), sum_install = sum(sum_install)) %>% 
                ungroup() %>% 
                arrange(desc(total)) %>% 
                select(sum_install) %>% 
                head(1)
                
        }
        
        infoBox(
            "Total Install", format(round(as.numeric(v_best_install)), big.mark="."), icon = icon("users"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$hc1 <- renderHighchart({
        
        if(input$rb_type == "all"){
            
            apps.distribution <- apps %>% 
                group_by(category) %>% 
                summarise(
                    count_app = n(),
                    mean_rating = round(mean(rating),2),
                    mean_review = round(mean(reviews),2),
                    mean_price = round(mean(price),2),
                    sum_install = sum(installs),
                    sum_price = round(sum(price_summary),2) 
                ) %>%
                ungroup() %>% 
                arrange(desc(count_app)) %>% 
                hchart('treemap', hcaes(x = category, value = count_app, color = count_app)) %>%
                hc_add_theme(hc_theme_google()) %>% 
                hc_colorAxis(stops = color_stops(n = 10, colors = c("#0073b7", "#00a65a", "#f39c12"))) %>% 
                hc_title(text="<br><strong>Top Category based on Total Apps and Purchase Type</strong>") %>% 
                hc_tooltip(shared = TRUE,
                           borderColor = "black",
                           formatter=JS("function(){
                                        return '<strong>Category:  </strong>'+ this.series.category +'<br>'+
                                        '<strong>Total Apps:  </strong>'+ this.point.count_app +'<br>'+
                                        '<strong>Total Install:  </strong>' + Highcharts.numberFormat(this.point.sum_install,-1) +'<br>'+
                                        '<strong>Mean Rating:  </strong>' + this.point.mean_rating +'<br>'+
                                        '<strong>Mean Price:  </strong> $'+this.point.mean_price;}"))
                           
        }
        else{
            
            apps.distribution <- apps %>% 
                filter(type == input$rb_type) %>%
                group_by(category) %>% 
                summarise(
                    count_app = n(),
                    mean_rating = round(mean(rating),2),
                    mean_review = round(mean(reviews),2),
                    mean_price = round(mean(price),2),
                    sum_install = sum(installs),
                    sum_price = round(sum(price_summary),2) 
                ) %>%
                ungroup() %>% 
                arrange(desc(count_app)) %>% 
                hchart('treemap', hcaes(x = 'category', value = 'count_app', color = 'count_app')) %>%
                hc_add_theme(hc_theme_google()) %>% 
                hc_colorAxis(stops = color_stops(n = 10, colors = c("#f39c12","#00a65a","#0073b7"))) %>% 
                hc_title(text="Application Distribution based on Category") %>% 
                hc_tooltip(shared = TRUE,
                           borderColor = "black",
                           pointFormat = "<strong>Total Apps:</strong> {point.count_app}<br>
                            <strong>Total Install:</strong> {point.sum_install}<br>
                            <strong>Mean Rating:</strong> {point.mean_rating}<br>
                            <strong>Mean Price:</strong> ${point.mean_price}
                            ")
        }
        
        
       
    })
    
    output$plot1 <- renderPlotly({
        
        data <- apps.distribution %>% 
            arrange(desc(sum_install)) %>% 
            mutate(text = glue(
                "Category: {category}
                 Install: {sum_install}
                 Avg. Rating: {mean_rating}
                 Avg. Price: {mean_price}"
            )) %>% 
            head(input$slider_topn)
        
        data$category = reorder(data$category,data$sum_install)
        
        if(input$rb_type != "all"){
            data <- apps.distribution %>% 
                filter(type == input$rb_type) %>% 
                arrange(desc(sum_install)) %>% 
                mutate(text = glue(
                    "Category: {category}
                    Install: {comma(sum_install)}
                    Avg. Rating: {mean_rating}
                    Avg. Price: {mean_price}"
                )) %>% 
                head(input$slider_topn)
            
            data$category = reorder(data$category,data$sum_install)
            
        }
        
        data.plot <-ggplot(data = data, aes(x = category, y = sum_install, text = text)) +
            geom_col(aes(fill = category), show.legend = F) +
            coord_flip() +
            labs(y = "", x = "")+
            scale_y_continuous(labels = scales::unit_format(unit = "k",scale = 1e-3))+
            theme(
                  axis.text.x=element_text(size=9,margin = margin(b=10)),
                  axis.text.y.left = element_text(margin = margin(l=10)),
                  legend.position='none'
                  )
        
        ggplotly(data.plot, tooltip = "text", height=500)%>% 
            config(displayModeBar = F) # untuk menampilkan button di plotly
            
        
    })
    
    
    output$table_app <- DT::renderDataTable(DT::datatable({
        data <- apps %>% 
            mutate(
                "application name" = app_name,
                "installs * price" = price_summary,
                rating = as.numeric(round(rating,2))
                
            ) %>% 
            select("application name",category,type,rating,reviews,installs,price,"installs * price")
            
        
        
        if(input$rb_type_datatable != "all"){
            data <- data %>% 
                filter(data$type == input$rb_type_datatable)

        }
        
           
        if(input$cmb_category != "All"){
            data <- data %>% 
                filter(data$category == input$cmb_category)
        }
        
        data
        
    }))
    
    
}

shinyApp(ui, server)
