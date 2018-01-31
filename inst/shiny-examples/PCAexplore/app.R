## Exploring the data table with Principal Component Analysis (PCA)

#load the data
data(rubusFilled)

server <- function(input, output){

  check_imputing<-function(string_vect){
    imputing_strategies<-c("fixed","noise","fillpeaks","knn")
    out<-is.element(imputing_strategies,string_vect)
    return(out)
  }

  mydata <- reactive({

    #prepraring the data table without imputing (default) for the plot
    d <- rubusNA %>%
      as.tibble %>%
      filter(sampleName!="OM_11_DR_P_09_1501")

    #in parallel we prepare the matrix imputed with fillpeaks
    RF<-rubusFilled %>%
      as.tibble %>%
      filter(sampleName!="OM_11_DR_P_09_1501")


    #computing the percentage of NA
    NAmat<-d %>%
      gather(Feature,Value, -sampleName,-color,-location,-year,-variety,-variety.name,-temperature) %>%
      dplyr::select(-one_of("sampleName","color","location","year","variety","variety.name","temperature")) %>%
      group_by(Feature) %>%
      summarise_all(funs(sum(is.na(.)))) %>%
      mutate(Value=Value/length(unique(d$sampleName)))

    if(input$perc!="all"){
      threshold<-switch(input$perc,
                        "na5"=0.05,
                        "na25"=0.25,
                        "na50"=0.50,
                        "na75"=0.75,
                        "na95"=0.95)
      keepfeat<-NAmat %>% filter(Value<threshold) %>% pull(Feature)

      #subsetting the dataset with only the selected features
      d<-d %>%
        select(one_of(c("sampleName","color","location","year","variety","variety.name","temperature",keepfeat)))
      RF<-RF %>%
        select(one_of(c("sampleName","color","location","year","variety","variety.name","temperature",keepfeat)))
    }
    d<-d %>%
      gather(Feature,Value, -sampleName,-color,-location,-year,-variety,-variety.name,-temperature)


    #prepraring the data table with imputing for the plot
    #fixed
    if(check_imputing(input$imputing_str)[1]){
      d[is.na(d$Value),"Value"]<-min(d$Value,na.rm = TRUE)/2
    }
    #noise
    if(check_imputing(input$imputing_str)[2]){
      d[is.na(d$Value),"Value"]<-runif(n=sum(is.na(d$Value)),
                                       min=0,
                                       max=min(d$Value,na.rm = TRUE))
    }
    #fillPeaks
    if(check_imputing(input$imputing_str)[3]){
      d<-RF %>%
        gather(Feature,Value, -sampleName,-color,-location,-year,-variety,-variety.name,-temperature)
    }
    #knn
    if(check_imputing(input$imputing_str)[4]){
      new<-suppressWarnings(d %>%
        select(-color,-location,-year,-variety,-variety.name,-temperature) %>%
        spread(sampleName, Value) %>%
        remove_rownames %>%
        column_to_rownames(var="Feature") %>%
        as.matrix)
      new_imputed<-suppressWarnings(impute.knn(new,k=5))
      new<-new_imputed$data %>%
        as.data.frame %>%
        rownames_to_column(var="Feature") %>%
        gather(sampleName,Value,-Feature)
      d<-suppressMessages(full_join(d[,c("sampleName","color","location","year","variety","variety.name","temperature","Feature")],new))
    }

    #preparing the data with log10 transformation
    plotdata<-d %>%
      mutate(Value=log10(Value+1e-5))

    return(plotdata)
  })

  output$plotPCA <- renderPlot({

    X <- mydata() %>% spread(Feature,Value)

    #handling the class info at the plot level
    if(is.null(input$sclass)){
      dat<-as.data.frame(X)
      rownames(dat)<-X$sampleName
      dat<-dat[,-match(c("sampleName","variety.name"),colnames(dat))]

      mypca<-PCA(dat, scale.unit = TRUE,
                 quali.sup = match(c("color", "location", "year", "variety", "temperature"),colnames(dat)),
                 graph = FALSE)

      if(is.null(input$dclass)){
        myplot<-fviz_pca_ind(mypca,geom = "point",
                             pointsize=3,mean.point=FALSE)
      }else{
        if(input$dclass=="rcol"){
          myplot<-fviz_pca_ind(mypca,geom = "point",
                               habillage = match("color",colnames(dat)),
                               pointsize=3,mean.point=FALSE,legend.title = "Rubus\ncolor")
        }
        if(input$dclass=="temp"){
          myplot<-fviz_pca_ind(mypca,geom = "point",
                               habillage = match("temperature",colnames(dat)),
                               pointsize=3,mean.point=FALSE,legend.title = "Storage\ntemperature")
        }
        if(input$dclass=="loc"){
          myplot<-fviz_pca_ind(mypca,geom = "point",
                               habillage = match("location",colnames(dat)),
                               pointsize=3,mean.point=FALSE,legend.title = "Location\nof sampling")
        }
        if(input$dclass=="year"){
          myplot<-fviz_pca_ind(mypca,geom = "point",
                               habillage = match("year",colnames(dat)),
                               pointsize=3,mean.point=FALSE,legend.title = "Year of\nsampling")
        }
      }
    }
    return(myplot)
  })
}


ui <- fluidPage(
  titlePanel("Exploring the data table with Principal Component Analysis (PCA)"),

  sidebarLayout(
    sidebarPanel(

      ## type of imputing strategy
      radioButtons(inputId='perc', label=h4("Percentage of missing values"),selected="all",
                   choices=c("All features (6,365)"="all",
                             "features with < 95% NAs (6,312)"="na95",
                             "features with < 75% NAs (3,312)"="na75",
                             "features with < 50% NAs (2,090)"="na50",
                             "features with < 25% NAs (1,340)" = "na25",
                             "features with < 5% NAs (732)" = "na5"
                   )),

      ## type of imputing strategy
      radioButtons(inputId='imputing_str', label=h4("Imputing strategy"),selected = "fillpeaks",
                   choices=c("Constant Small Value" = "fixed",
                             "Small noise" = "noise",
                             "xcms fillPeaks"="fillpeaks",
                             "knn imputing"="knn")),

      ## type of imputing strategy
      radioButtons(inputId='dclass', label=h4("Experimental Design Information"),selected="NULL",
                   choices=c("Rubus color" = "rcol",
                             "Storage temperature" = "temp",
                             "Location of sampling"="loc",
                             "Year of sampling"="year")),
      submitButton("Update View")
    ),

    mainPanel(
      plotOutput("plotPCA", width = "90%", height = "500px"),
      br(),
      includeMarkdown("PCAexplore.md")
    )
  )
)

shinyApp(ui = ui, server = server)

