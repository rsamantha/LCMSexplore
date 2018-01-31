## Evaluating the effect of missing values and imputing strategies

#load the data
data(rubusNA)
data(rubusFilled)

server <- function(input, output){

  mydata <- reactive({

    #prepraring the data table without imputing (default) for the plot
    d <- rubusNA %>%
      as.tibble %>%
      filter(sampleName!="OM_11_DR_P_09_1501") %>%
      gather(feature,raw, -sampleName,-color,-location,-year,-variety,-variety.name,-temperature) %>%
      filter(feature==input$fname)

    #prepraring the data table with imputing for the plot
    check_imputing<-function(string_vect){
      imputing_strategies<-c("fixed","noise","fillpeaks","knn")
      out<-is.element(imputing_strategies,string_vect)
      return(out)
    }

    #fixed
    if(check_imputing(input$imputing_str)[1]){
      new<-d %>% rename(fixed=raw)
      new[is.na(new$fixed),"fixed"]<-min(new$fixed,na.rm = TRUE)/2
      d <- suppressMessages(full_join(d,new))
    }
    #noise
    if(check_imputing(input$imputing_str)[2]){
      new<-d %>% rename(noise=raw)
      new[is.na(new$noise),"noise"]<-runif(n=sum(is.na(new$noise)),
                                           min=0,
                                           max=min(new$noise,na.rm = TRUE))
      d <- suppressMessages(full_join(d,new))
    }
    #fillPeaks
    if(check_imputing(input$imputing_str)[3]){
      new<-rubusFilled %>%
        as.tibble %>%
        filter(sampleName!="OM_11_DR_P_09_1501") %>%
        gather(feature,fillPeaks, -sampleName,-color,-location,-year,-variety,-variety.name,-temperature) %>%
        filter(feature==input$fname)
      d <- suppressMessages(full_join(d,new))
    }
    #knn
    if(check_imputing(input$imputing_str)[4]){
      new<-suppressWarnings(rubusNA %>%
        as.tibble %>%
        filter(sampleName!="OM_11_DR_P_09_1501") %>%
        select(-color,-location,-year,-variety,-variety.name,-temperature) %>%
        gather(Feature,Value,-sampleName) %>%
        spread(sampleName, Value) %>%
        remove_rownames %>%
        column_to_rownames(var="Feature") %>%
        as.matrix)
      new_imputed<-suppressMessages(impute.knn(new,k=5))
      rubusKNN<-suppressWarnings(new_imputed$data %>%
        as.data.frame %>%
        rownames_to_column(var="feature") %>%
        gather(sampleName,knn,-feature) %>%
        filter(feature==input$fname))

      d <- suppressMessages(full_join(d,rubusKNN))
    }

    #preparing the data tabel for faceting
    plotdata<-d %>%
      gather(option,value, -sampleName,-color,-location,-year,-variety,-variety.name,-temperature,-feature) %>%
      #chosing nicer labels for faceting the plot
      mutate(option=fct_inorder(suppressWarnings(fct_recode(option,
                               "Original Data"="raw",
                               "Constant Small Value"="fixed",
                               "Small Uniform Noise"="noise",
                               "xcms fillPeaks"="fillPeaks",
                               "knn imputing"="knn"))))

    return(plotdata)
  })

  output$plotMat <- renderPlot({

    X <- mydata()

    mytheme<-function(...){
      theme(panel.background = element_rect(fill = "white",colour = "black"),
                   axis.line = element_line(colour = "black"),
                   panel.grid.major = element_line(color = "gray80",size = 0.5),
                   panel.grid.minor = element_line(color="gray80",size = 0.25),
                   strip.background = element_rect(fill="white"),
                   strip.text = element_text(face = "bold",size = 12),...)
    }

    #handling the class info at the plot level
    if(is.null(input$sclass)){
      p1 <- X %>%
        ggplot(aes(x=feature,y=value))+
        geom_boxplot(fill="seagreen3",alpha=0.7)+
        facet_wrap(~option,ncol=1)+
        coord_flip()+ #for having horizontal boxplot
        mytheme(legend.position="none")+ #removing the legend for the boxplot
        labs(x="",y="")
      p2 <- X %>%
        ggplot(aes(x=value))+
        geom_histogram(fill="seagreen3", alpha=0.7, bins=12,color="black")+
        facet_wrap(~option,ncol=1)+
        mytheme()+
        labs(x="")

      grid.arrange(p1,p2,ncol=2)
      }else{
      p1c <- X %>%
        ggplot(aes(x=feature,y=value))+
        geom_boxplot(aes(fill=color),alpha=0.7)+
        facet_wrap(~option,ncol=1)+
        coord_flip()+ #for having horizontal boxplot
        mytheme(legend.position="none")+ #removing the legend for the boxplot
        labs(x="",y="")
      p2c <- X %>%
        ggplot(aes(x=value))+
        geom_histogram(position="identity",aes(fill=color), alpha=0.5, bins=10,color="black")+
        facet_wrap(~option,ncol=1)+
        mytheme()+
        labs(x="")
      grid.arrange(p1c,p2c,ncol=2)
      }
  })
}


ui <- fluidPage(
  titlePanel("Evaluating missing values and imputing strategies"),

  sidebarLayout(
    sidebarPanel(
      #width = 4,
      ## select the variable to explore
      selectInput(inputId='fname', label=h4("Feature"),
                  choices=c("284.03/1258.17","58.03/1500.06","663.38/2431.68","108.02/845.92","1121.28/1304.53"),
                  multiple = FALSE,selected = NULL),

      ## show class lable information
      checkboxGroupInput(inputId='sclass', label=h4("Sample class"),
                         choices=c("Rubus color"=TRUE)),

      ## type of imputing strategy
      checkboxGroupInput(inputId='imputing_str', label=h4("Imputing strategy"),
                  choices=c("Constant Small Value" = "fixed",
                            "Small noise" = "noise",
                            "xcms fillPeaks"="fillpeaks",
                            "knn imputing"="knn")),
      submitButton("Update View")
    ),

    mainPanel(
      plotOutput("plotMat", width = "90%", height = "500px"),
      br(),
      includeMarkdown("NAimputing.md")
    )
  )
)

shinyApp(ui = ui, server = server)

