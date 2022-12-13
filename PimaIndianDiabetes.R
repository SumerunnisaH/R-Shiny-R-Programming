library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(dashboardthemes)
library(shinyalert)

db=read.csv("C:/Users/ELCOT/Desktop/Mini_project/diabetes.csv",header=T)
db$Outcome=as.factor(db$Outcome)

db=db %>% mutate(insulin_ml=case_when(Insulin<=5.00~"lowml",
                                      Insulin>5.00&Insulin<=55.00~"normalml",
                                      Insulin>55.00~"highml"))
db$insulin_ml=as.factor(db$insulin_ml)

db=db %>% mutate(bp=case_when(BloodPressure<=60~"lowbp",
                              BloodPressure>60&BloodPressure<=80~"normalbp",
                              BloodPressure>80~"highbp"))
db$bp=as.factor(db$bp)

db=db %>% mutate(level_glucose=case_when(Glucose<=90~"low",
                                         Glucose>90&Glucose<=110~"normal",
                                         Glucose>110~"high"))
db$level_glucose =as.factor(db$level_glucose )

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Diabetes"),
                    dashboardSidebar(sidebarMenu(id = "tab1", selected = "home",
                                                 menuItem("Home", tabName = "home"),
                                                 menuItem("Personal Info", tabName = "personal"),
                                                 menuItem("Medical Info", tabName = "medical"))),
                    dashboardBody(
                        shinyDashboardThemes(
                            theme = "flat_red"
                        ),
                        tabItems(
                            tabItem("home",
                                    fluidRow(column(12, h2("Welcome to the Presentation of Diabetes Data"))),
                                    fluidRow(column(12, box(radioButtons(inputId = "radio1",selected = character(0),
                                                                         label = "Please choose for Personal or Medical",
                                                                         choices = c("Personal Info", "Medical Info"))))),
                            ),
                            tabItem("personal",
                                    
                                    h1(textOutput("text")),
                                    fluidRow(h1(" ")),
                                    fluidRow(column(12,offset=13,fluidRow(selectizeInput("selectPer", "Select the type of variable",
                                                                                       choices = c("Pregnancies", "Age","DiabetesPedigreeFunction")))),column(6,uiOutput("us1"))),
                                    
                                    fluidRow(column(6,plotlyOutput("Pregnancies"))),
                                    fluidRow(dataTableOutput("Pregnancies_1")),
                                    fluidRow(column(1,offset = 8,downloadButton("download6", label = "Download")))
                                    
                                    
                            ),
                            
                            tabItem("medical",
                                    tabsetPanel(type = "tabs", selected = "Glucose", id = "intabset1",
                                                tabPanel("Glucose", value = "Glucose",
                                                         h1("Glucose vs Outcome"),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(12,plotlyOutput("Glucose"))),
                                                         fluidRow(dataTableOutput("Glucose_1")),
                                                         fluidRow(column(1,offset = 6,actionButton(inputId = "button1", label = "home"))),
                                                         fluidRow(column(1,offset = 11,actionButton(inputId = "button2", label = "Next"))),
                                                         fluidRow(column(1,offset = 8,downloadButton("download1", label = "Download")))
                                                ),
                                                tabPanel("BloodPressure", value = "BloodPressure",
                                                         h1("BloodPressure vs Outcome"),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(12,plotlyOutput("BloodPressure"))),
                                                         fluidRow(dataTableOutput("BloodPressure_1")),
                                                         fluidRow(column(1,offset = 6,actionButton(inputId = "button3", label = "home"))),
                                                         fluidRow(column(1,offset = 0, actionButton(inputId = "button4", label = "Back"))),
                                                         fluidRow(column(1,offset = 11,actionButton(inputId = "button5", label = "Next"))),
                                                         fluidRow(column(1,offset = 8,downloadButton("download2", label = "Download")))
                                                         
                                                ),
                                                tabPanel("Insulin", value = "Insulin",
                                                         h1("Insulin vs Outcome"),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(12,plotlyOutput("Insulin"))),
                                                         fluidRow(dataTableOutput("Insulin_1")),
                                                         fluidRow(column(1,offset = 6,actionButton(inputId = "button6", label = "home"))),
                                                         fluidRow(column(1,offset = 0, actionButton(inputId = "button7", label = "Back"))),
                                                         fluidRow(column(1,offset = 11,actionButton(inputId = "button8", label = "Next"))),
                                                         fluidRow(column(1,offset = 8,downloadButton("download3", label = "Download")))
                                                         
                                                ),
                                                tabPanel("SkinThickness", value = "SkinThickness",
                                                         h1("SkinThickness vs Outcome"),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(8,fluidRow(sliderInput(
                                                             "Slider2", "Please select the SkinThickness",
                                                             min = min(db$SkinThickness),
                                                             max = max(db$SkinThickness),
                                                             value = c(min = min(db$SkinThickness), max = max(db$SkinThickness)))
                                                         ))),
                                                         fluidRow(column(12,plotlyOutput("SkinThickness"))),
                                                         fluidRow(dataTableOutput("SkinThickness_1")),
                                                         fluidRow(column(1,offset = 6,actionButton(inputId = "button9", label = "home"))),
                                                         fluidRow(column(1,offset = 0,actionButton(inputId = "button10", label = "Back"))),
                                                         fluidRow(column(1,offset = 11,actionButton(inputId = "button11", label = "Next"))),
                                                         fluidRow(column(1,offset = 8,downloadButton("download4", label = "Download")))
                                                         
                                                ),
                                                tabPanel("BMI", value = "BMI",
                                                         h1("BMI vs Outcome"),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(h1(" ")),
                                                         fluidRow(column(7,fluidRow(sliderInput(
                                                             "Slider3", "Please select the BMI",
                                                             min = min(db$BMI),
                                                             max = max(db$BMI),
                                                             value = c(min = min(db$BMI), max = max(db$BMI)))
                                                         ))),
                                                         fluidRow(column(12,plotlyOutput("BMI"))),
                                                         fluidRow(dataTableOutput("BMI_1")),
                                                         fluidRow(column(1,offset = 0,actionButton(inputId = "button13", label = "Back"))),
                                                         fluidRow(column(1,offset = 6,actionButton(inputId = "button12", label = "home"))),
                                                         fluidRow(column(1,offset = 8,downloadButton("download5", label = "Download")))
                                                         
                                                         
                                                )
                                    )
                            )
                        )
                    )
                    
)


server <- function(input, output, session){
    observeEvent(input$radio1,{
        if(input$radio1 == "Personal Info"){
            updateTabItems(session = session, inputId = "tab1", selected = "personal")
        }
        if(input$radio1 == "Medical Info"){
            updateTabItems(session = session, inputId = "tab1", selected = "medical")
        }
    })
    output$text <- renderText({ paste0( input$selectPer," vs Outcome") })
    output$us1 <- renderUI({
        fluidRow(sliderInput(
            "Slider1", "Please select the range",
            min = floor(min(db[as.character(input$selectPer)])),
            max = ceiling(max(db[as.character(input$selectPer)])),
            value = c(min = min(db[as.character(input$selectPer)]), max = max(db[as.character(input$selectPer)])))
        )
        
    })
    output$Pregnancies <- renderPlotly({
        
        if(input$selectPer == "Pregnancies"){
            db <- db[which(db$Pregnancies %in% (input$Slider1[1]:input$Slider1[2])),]
            plot_ly(db, x = ~Pregnancies, type = 'histogram',color=~Outcome,colors=c(I("brown"),I('yellow')),stroke=I('blue'))
        } else
            if(input$selectPer == "Age"){
                db <- db %>% filter(Age>=input$Slider1[1] & Age<=input$Slider1[2])
                d1 <- db %>% filter(Outcome=="0") %>% droplevels()
                
                density1 <- density(d1$Age)
                
                d2 <- db %>% filter(Outcome=="1") %>% droplevels()
                density2 <- density(d2$Age)
                
                original_plot1 <- plot_ly(x = ~density1$x, 
                                          y = ~density1$y, 
                                          type = 'scatter',
                                          mode = 'lines',
                                          name = 'Outcome==0', 
                                          fill = 'tozeroy')
                final_plot <- original_plot1 %>% add_trace(x = ~density2$x,
                                                           y = ~density2$y,
                                                           name = 'Outcome==1',
                                                           fill = 'tozeroy')
                final_plot
            }
        else{
            db <- db %>% filter(DiabetesPedigreeFunction>=input$Slider1[1] & DiabetesPedigreeFunction<=input$Slider1[2])
            plot_ly(db, x = ~DiabetesPedigreeFunction, type = "histogram",color=~Outcome,colors=c(I("darkgreen"),I('orange')),stroke=I('black'))
            
        }
        
    })
    output$download1 <- downloadHandler(
        filename = function() {
            paste("Glucose", ".csv", sep = "")
        },
        content = function(file) {
            df = as.data.frame(db %>% 
                                   group_by(Outcome) %>% summarise(minimum=min(Glucose),
                                                                   maximum=max(Glucose),
                                                                   average=round(mean(Glucose))))
            
            write.csv(df, file, row.names = TRUE)
        }
        
    )
    output$download2 <- downloadHandler(
        filename = function() {
            paste("BloodPressure", ".csv", sep = "")
        },
        content = function(file) {
            df = as.data.frame(db %>% 
                                   group_by(Outcome) %>% summarise(minimum=min(BloodPressure),
                                                                   maximum=max(BloodPressure),
                                                                   average=round(mean(BloodPressure))))
            
            write.csv(df, file, row.names = TRUE)
        }
        
    )
    output$download3 <- downloadHandler(
        filename = function() {
            paste("Insulin", ".csv", sep = "")
        },
        content = function(file) {
            df = as.data.frame(db %>% 
                                   group_by(Outcome) %>% summarise(minimum=min(Insulin),
                                                                   maximum=max(Insulin),
                                                                   average=round(mean(Insulin))))
            
            write.csv(df, file, row.names = TRUE)
        }
        
    )
    output$download4 <- downloadHandler(
        filename = function() {
            paste("SkinThickness", ".csv", sep = "")
        },
        content = function(file) {
            df = as.data.frame(db %>% 
                                   group_by(Outcome) %>% summarise(minimum=min(SkinThickness),
                                                                   maximum=max(SkinThickness),
                                                                   average=round(mean(SkinThickness))))
            
            write.csv(df, file, row.names = TRUE)
        }
        
    )
    output$download5 <- downloadHandler(
        filename = function() {
            paste("BMI", ".csv", sep = "")
        },
        content = function(file) {
            df = as.data.frame(db %>% 
                                   group_by(Outcome) %>% summarise(minimum=min(BMI),
                                                                   maximum=max(BMI),
                                                                   average=round(mean(BMI))))
            
            write.csv(df, file, row.names = TRUE)
        }
        
    )
    output$download6 <- downloadHandler(
       
        filename = function(){
            paste(input$selectPer, ".csv", sep = "")
        },
        content = function(file){
            if(input$selectPer == "Pregnancies"){
                df = as.data.frame(db %>% 
                                        group_by(Outcome) %>% summarise(minimum=min(Pregnancies),
                                                                        maximum=max(Pregnancies),
                                                                        average=round(mean(Pregnancies)))) 
            } else
                if(input$selectPer == "Age"){
                    df = as.data.frame(db %>% 
                                            group_by(Outcome) %>% summarise(minimum=min(Age),
                                                                            maximum=max(Age),
                                                                            average=round(mean(Age))))
                    
                }
            else{df = as.data.frame(db %>% 
                                         group_by(Outcome) %>% summarise(minimum=min(DiabetesPedigreeFunction),
                                                                         maximum=max(DiabetesPedigreeFunction),
                                                                         average=round(mean(DiabetesPedigreeFunction))))
                
               
            }
            write.csv(df, file, row.names = TRUE)
        }
       
    )
    
    output$Pregnancies_1 <- renderDataTable({
        df1 = as.data.frame(db %>% 
                                group_by(Outcome) %>% summarise(minimum=min(Pregnancies),
                                                                maximum=max(Pregnancies),
                                                                average=round(mean(Pregnancies))))
        df2 = as.data.frame(db %>% 
                                group_by(Outcome) %>% summarise(minimum=min(Age),
                                                                maximum=max(Age),
                                                                average=round(mean(Age))))  
        df3 = as.data.frame(db %>% 
                                group_by(Outcome) %>% summarise(minimum=min(DiabetesPedigreeFunction),
                                                                maximum=max(DiabetesPedigreeFunction),
                                                                average=round(mean(DiabetesPedigreeFunction))))
        if(input$selectPer == "Pregnancies"){
            datatable(df1, options = list(searching = FALSE))
        } else
            if(input$selectPer == "Age"){
                datatable(df2, options = list(searching = FALSE))  
            }
        else{
            datatable(df3, options = list(searching = FALSE))
        }
    })
    
    output$Glucose_1 <- renderDataTable({
        df = as.data.frame(db %>% 
                               group_by(Outcome) %>% summarise(minimum=min(Glucose),
                                                               maximum=max(Glucose),
                                                               average=round(mean(Glucose))))
        datatable(df, options = list(searching = FALSE))
    })
    output$BloodPressure_1 <- renderDataTable({
        df = as.data.frame(db %>% 
                               group_by(Outcome) %>% summarise(minimum=min(BloodPressure),
                                                               maximum=max(BloodPressure),
                                                               average=round(mean(BloodPressure))))
        datatable(df, options = list(searching = FALSE))
    })
    output$Insulin_1 <- renderDataTable({
        df = as.data.frame(db %>% 
                               group_by(Outcome) %>% summarise(minimum=min(Insulin),
                                                               maximum=max(Insulin),
                                                               average=round(mean(Insulin))))
        datatable(df, options = list(searching = FALSE))
    })
    output$BMI_1 <- renderDataTable({
        df = as.data.frame(db %>% 
                               group_by(Outcome) %>% summarise(minimum=min(BMI),
                                                               maximum=max(BMI),
                                                               average=round(mean(BMI))))
        datatable(df, options = list(searching = FALSE))
    })
    output$SkinThickness_1 <- renderDataTable({
        df = as.data.frame(db %>% 
                               group_by(Outcome) %>% summarise(minimum=min(SkinThickness),
                                                               maximum=max(SkinThickness),
                                                               average=round(mean(SkinThickness))))
        datatable(df, options = list(searching = FALSE))
    })
    output$BMI <- renderPlotly({
        db <- db[which(db$Pregnancies %in% (input$Slider3[1]:input$Slider3[2])),]
        plot_ly(db, y = ~BMI, type = "box",color=~Outcome,colors=c(I("blue"),I('purple')),stroke=I('black'))
    })
    output$Glucose <- renderPlotly({
        db %>%
            count(Outcome,level_glucose) %>% 
            plot_ly(x=~level_glucose,y=~n,color=~Outcome,colors=c(I("violet"),I('brown')),type='bar')
    })
    output$BloodPressure <- renderPlotly({
        db %>%
            count(bp,Outcome) %>% 
            plot_ly(x=~bp,y=~n,color=~Outcome,colors=c(I("pink"),I('maroon')),type='bar')
    })
    output$Insulin <- renderPlotly({
        db %>%
            count(insulin_ml,Outcome) %>% 
            plot_ly(x=~insulin_ml,y=~n,color=~Outcome,colors=c(I("darkblue"),I('blue')),type='bar')
    })
    output$SkinThickness <- renderPlotly({
        db <- db[which(db$SkinThickness %in% (input$Slider2[1]:input$Slider2[2])),]
        plot_ly(db, x = ~SkinThickness, type = 'histogram',color=~Outcome,stroke=I('blue'))
    })
    
    observeEvent(input$button1,{
        updateTabsetPanel(session = session, "tab1", selected = "home")
    })
    observeEvent(input$button2,{
        updateTabItems(session = session, "intabset2", selected = "BloodPressure")
    })
    
    observeEvent(input$button3,{
        updateTabsetPanel(session = session, "tab1", selected = "home")
    })
    observeEvent(input$button4,{
        updateTabsetPanel(session = session, "intabset2", selected = "Glucose")
    })
    observeEvent(input$button5,{
        updateTabsetPanel(session = session, "intabset1", selected = "Insulin")
    })
    observeEvent(input$button6,{
        updateTabsetPanel(session = session, "tab1", selected = "home")
    })
    observeEvent(input$button7,{
        updateTabsetPanel(session = session, "intabset2", selected = "SkinThickness")
    })
    observeEvent(input$button8,{
        updateTabsetPanel(session = session, "intabset2", selected = "BloodPressure")
    })
    
    observeEvent(input$button9,{
        updateTabsetPanel(session = session, "tab1", selected = "home")
    })
    observeEvent(input$button10,{
        updateTabsetPanel(session = session, "intabset2", selected = "Insulin")
    })
    observeEvent(input$button11,{
        updateTabsetPanel(session = session, "intabset2", selected = "BMI")
    })
    observeEvent(input$button12,{
        updateTabsetPanel(session= session, "tab1", selected = "home")
    })
    observeEvent(input$button13,{
        updateTabsetPanel(session = session, "intabset2", selected = "SkinThickness")
    })
    
    
}

shinyApp(ui, server)