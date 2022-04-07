#------------------
# Loading Packages
#------------------
library(patchwork) 
library(ROCR)
library(ggrepel)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(gtsummary)
library(RColorBrewer)
library(reactable)
library(kableExtra)
library(plotly)
library(data.table)
library(stringi)
library(DT)
library(magrittr)

library(shinydashboard)
library(shinycssloaders)
library(shinybusy)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(shiny)

library(fastDummies)
library(discretization)
library(Factoshiny)
library(rpart)
library(randomForest)
library(caret)
library(rpart.plot)
library(broom)
library(GGally)
library(car)

#-------------------------------------------
#           USER INTERFACE AREA
#-------------------------------------------
ui <- shinyUI(
  
  dashboardPage(skin="blue",   # skin: for changing the dashboard body,
                
                ### Header ###
                dashboardHeader(title = "Visual Analytics"),
                
                ### Sidebar ###
                dashboardSidebar(tags$style(type = 'text/css',".badge{min-width: 200px;}"),
                  
                  ## Menu ##
                  sidebarMenu(
                    menuItem("Dev. in progress !", tabName = 'dashboard'),
                    menuItem(
                        fileInput("file1", "Upload CSV File",multiple = TRUE,
                                  accept = c("text/csv","text/comma-separated-values,text/plain", ".csv"))
                    ),
                    
                    ### wordcloud 
                    menuItem("Words cloud SliderInput",
                      sliderInput("freq","Minimum frequency:", min = 1,  max = 100, value = 15),
                      sliderInput("word","Maximum number of Words:",min = 1,  max = 400,  value = 100)
                    ),
                    menuItem(actionButton("btn_decretiz", "Discretization")),
                    menuItem(actionButton("btn_pca", "Start PCA")),
                    menuItem(actionButton("btn_train_dataset", "Training Data"))

                  ) #End sidebarMenu
                  
                  ), #End Sidebar
                
                ### Body ###
                dashboardBody(navbarPage(
                  titlePanel(" "),
                  
                  # # Page1-------------------------------------------------------------------------------------------------------------------------------
                  tabPanel("Database",icon = icon("database"),
                           add_busy_spinner(spin = "fading-circle"),
                           
                           fluidPage(
                             fluidRow(
                               tableOutput("contents"),column(1),column(10, reactableOutput("table1")),column(1)
                             )
                           )
                           
                  ), #End tabPanel
                  
                  # Page2-------------------------------------------------------------------------------------------------------------------------------
                  tabPanel("Overview",icon = icon("chart-line"),

                           fluidPage(
                             add_busy_spinner(spin = "fading-circle"),

                             fluidRow(column(1),
                                      valueBoxOutput(width=4,'valuebox1'),
                                      valueBoxOutput(width=2,'valuebox3'),
                                      valueBoxOutput(width=2,'valuebox2'),
                                      valueBoxOutput(width=2,'valuebox4')
                             ),
                             #------------------
                             fluidRow(column(1),
                                      box(width=5,plotOutput('plot1',height = 300)),
                                      box(width=5,plotOutput('plot3',height = 300))
                             ),

                             fluidRow(column(1),
                                      box(width=4,title = "Most frequent ENSEIGNE",plotOutput('plot5',height = 300)),
                                      box(width=6,plotlyOutput('plot7',height = 342))
                             ),

                             fluidRow(column(1),
                                      box(width = 10,title = "Diplay Table",solidHeader = FALSE,collapsible = TRUE,reactableOutput("table3"))
                             ),

                             fluidRow(column(1),
                                      box(width = 10,title = "Enseigne Table",solidHeader = FALSE,collapsible = TRUE,reactableOutput("table5"))
                             ),

                             fluidRow(column(1),
                                      box(width = 10,title = "Feature Table",solidHeader = FALSE,collapsible = TRUE,reactableOutput("table7"))
                             ),

                             fluidRow(column(1),
                                      box(width = 10,title = "Enseigne- Display Table",solidHeader = FALSE,collapsible = TRUE,reactableOutput("table9"))
                             )

                           )#End FluidPage
                  ),  #End tabPanel
                  
                  # Page3 -------------------------------------------------------------------------------------------------------
                  tabPanel("PCA",
                           fluidPage(
                             # add_busy_spinner(spin = "fading-circle"),
                             
                             fluidRow(column(1),
                                      box(width=5,title = "PCA Variables",solidHeader = FALSE,collapsible = TRUE, plotOutput('plot13',height = 500)), # Graphe des individus (ACP)
                                      box(width=5,title = "PCA Individuals",solidHeader = FALSE,collapsible = TRUE, plotOutput('plot15',height = 500)) 
                             ),
                             
                             fluidRow(column(1), box(title = "PCA Results",verbatimTextOutput(outputId = "summary_PCA"),width = 10,
                                                     solidHeader = FALSE,collapsible = TRUE, 
                                                     plotOutput('plt_eboulis',height = 320))
                                      ),
                             
                             fluidRow(column(1),
                                      box(width = 10,title = "Table by MDLP Method After Discretization",
                                          solidHeader = FALSE,collapsible = TRUE,reactableOutput("table11"))
                             )
                             
                           )#End fluidPage
                  ), #En tabPanel
                  
                  # Page4 -------------------------------------------------------------------------------------------------------
                  tabPanel("Training Datasets",#icon = icon("redo"),
                           fluidPage(
                             add_busy_spinner(spin = "fading-circle"),
                             fluidRow(column(1),
                                      box(width = 12,title = "Learning Dataset",
                                          solidHeader = FALSE,collapsible = TRUE,reactableOutput("table13"))
                             ),
                             
                             fluidRow(column(1),
                                      box(width = 12,title = "Test Dataset",
                                          solidHeader = FALSE,collapsible = TRUE,reactableOutput("table15"))
                             )
                             
                           )#End fluidPage
                  ), #En tabPanel
                  
                  # Page5 -------------------------------------------------------------------------------------------------------
                  tabPanel("Modeling",
                           fluidPage(
                             add_busy_spinner(spin = "fading-circle"),
                             
                             #--------------
                             # Decision Tree
                             # Print : Confusion Matrix 
                             fluidRow(column(1), 
                                      box(width = 2,title = "Decision Tree : Cnf Matrix",solidHeader = FALSE,collapsible = TRUE,
                                          verbatimTextOutput(outputId = "d_tree_cnf"),
                                          verbatimTextOutput(outputId = "d_tree_err")),
                                      
                                      box(width = 3,title = "Logistic Reg : Cnf Matrix",solidHeader = FALSE,collapsible = TRUE,
                                          verbatimTextOutput(outputId = "logit_cnf"),
                                          verbatimTextOutput(outputId = "logit_err")),
                                      
                                      box(width = 2,title = "Random Forest : Cnf Matrix",solidHeader = FALSE,collapsible = TRUE,
                                          verbatimTextOutput(outputId = "rmd_frst_cnf"),
                                          verbatimTextOutput(outputId = "rmd_frst_err")),
                                      
                                      box(width = 3,title = "Random Forest : Mtry optimal results",solidHeader = FALSE,collapsible = TRUE,
                                          verbatimTextOutput(outputId = "rf_mtry_optimal"))
                             ),
                             
                             
                             # Plot
                             fluidRow(column(1),
                                      box(width=5,title = "Decision Tree",solidHeader = FALSE,collapsible = TRUE,
                                          plotOutput('plot17',height = 400)),
                                      
                                      # Random Forest : 1srt plot
                                      box(width=5,title = "Random Forest : Optimal number of trees",
                                          solidHeader = FALSE,collapsible = TRUE, plotOutput('plot19',height = 400))
                             ),
                             
                             #--------------
                             # Random Forest
                             fluidRow(column(1), 
                                      box(width=5,title = "Random Forest : Mtry optimal",solidHeader = FALSE,
                                          collapsible = TRUE, plotOutput('plot21',height = 400)),
                                      
                                      box(width=5,title = "Logistic Reg ",solidHeader = FALSE,
                                          collapsible = TRUE, plotOutput('plot23',height = 600))
                             ),
                             
                             #---------------------
                             # Logistic Regression
                             fluidRow(column(1), 
                                      box(width = 5,title = "Performence Metrics ",solidHeader = FALSE,collapsible = TRUE,
                                          verbatimTextOutput(outputId = "prf_metrics"))
                             ),
                             
                             fluidRow(column(1), 
                                      box(width = 10,title = "Logistic Reg : Summary",solidHeader = FALSE,collapsible = TRUE,
                                          verbatimTextOutput(outputId = "summary_logit")
                                          )
                                      
                             )
                             
                           #--------------
                           )#End fluidPage
                           
                  )#En tabPanel
                  
                #----------------
                )#End navbarPage 
                
            )#End dashboardBody
                
  ) #End dashboardPage
  
) #End shinyUI

#-------------------------------------------
#               SERVER AREA                     
#-------------------------------------------
server <- function(input, output, session) {
  
################ Calculation Steps  ##############################################################################################
  output$contents <-reactive({
    
    # Request
    req(input$file1)
    
    tryCatch(
      {
        # INPUT : data file
        df1 <- read.csv2(input$file1$datapat,header = T, sep = ";",skip=1,stringsAsFactors = FALSE,na.strings = c(""," ","NA","N/A"))
        # attach(df1)
        apply(df1, MARGIN = 2, FUN = function(x){x%>%is.na%>%sum})
        sum(is.na(df1))  #No missing values found
        
        # FORMATTING : variables
        df1$Display <- as.factor(df1$Display) 
        df1$ENSEIGNE <- as.factor(df1$ENSEIGNE) 
        df1$Feature <- as.factor(df1$Feature) 
        
        df1$cor_sales_in_vol <- as.numeric(df1$cor_sales_in_vol)
        df1$cor_sales_in_val <- as.numeric(df1$cor_sales_in_val)
        df1$CA_mag <- as.numeric(df1$CA_mag)
        df1$value <- as.numeric(df1$value)
        df1$VenteConv <- as.numeric(df1$VenteConv)
        
        #----------------------------------------------------------------------
        #              TABLES
        #----------------------------------------------------------------------
        
        #-- Table : ranking CA_mag par ENSEIGNE -
        ENSEIGNE_grp<-df1 %>%
          group_by(ENSEIGNE) %>%
          summarise(CA_mag = sum(CA_mag)) %>%
          mutate(rank = dense_rank(desc(CA_mag))) %>%
          arrange(desc(CA_mag))
        ENSEIGNE_grp<-data.frame(ENSEIGNE_grp) # transformation en data.frame
        
        #-- Table : CA_mag_Total par ENSEIGNE --
        CA_mag_sum<-df1 %>%
          group_by(ENSEIGNE) %>%
          summarise(CA_mag = sum(CA_mag)) %>%
          mutate(pct= round(prop.table(CA_mag),4)) %>%
          arrange(desc(CA_mag))
        CA_mag_sum<-data.frame(CA_mag_sum)
        
        #---Table : CA_mag par No_Disp---
        CA_mag_Displ <- df1 %>%
          subset(Display=="Displ") %>%
          select(c(ENSEIGNE,CA_mag)) %>% 
          group_by(ENSEIGNE) %>%
          summarise(CA_mag=sum(CA_mag)) %>%
          mutate(rank=dense_rank(desc(CA_mag)))
        CA_mag_Displ<-data.frame(CA_mag_Displ)
        
        
        #---Table : CA_mag par Disp---
        CA_mag_No_Displ <- df1 %>%
          subset(Display=="No_Displ") %>%
          select(c(ENSEIGNE,CA_mag)) %>% 
          group_by(ENSEIGNE) %>%
          summarise(CA_mag=sum(CA_mag)) %>%
          mutate(rank=dense_rank(desc(CA_mag)))
        CA_mag_No_Displ<-data.frame(CA_mag_No_Displ)
        
        ENSEIGNE_donut<-df1 %>%
          group_by(ENSEIGNE) %>%
          summarise(CA_mag = sum(CA_mag)) %>%
          mutate(pct= round(prop.table(CA_mag),2)) %>%
          subset(pct>=0.01) %>%
          arrange(desc(CA_mag))
        ENSEIGNE_donut<-data.frame(ENSEIGNE_donut)
        
        #-----------------------------------------------------
        #           LISTS : Qualitatives Variables
        #-----------------------------------------------------
        # Display list
        Display_list<-as.data.frame(sort(unique(df1$Display)))
        names(Display_list)<-"Display_List"
        Display_list<-Display_list %>%
          mutate(Row=1:n())
        colnames(Display_list)<-NULL  #remove headers
        
        # ENSEIGNE list
        ENSEIGNE_list<-as.data.frame(sort(unique(df1$ENSEIGNE)))
        names(ENSEIGNE_list)<-"Enseigne_List"
        ENSEIGNE_list<-ENSEIGNE_list %>%
          mutate(Row=1:n())
        colnames(ENSEIGNE_list)<-NULL  #remove headers
        
        # Feature list
        Feature_list<-as.data.frame(sort(unique(df1$Feature)))
        names(Feature_list)<-"Feature_List"
        Feature_list<-Feature_list %>%
          mutate(Row=1:n())
        colnames(Feature_list)<-NULL  #remove headers
        
        #----------------------------------------------------------------------
        #                     Table ENSEIGNE - Display
        #----------------------------------------------------------------------
        
        # Displ
        ENSEIGNE_Displ_CA<- filter(df1,Display %in% c("Displ")) %>%
          group_by(ENSEIGNE) %>%
          summarise(CA_mag = sum(CA_mag))
        
        # No_Displ
        ENSEIGNE_No_Displ_CA<-filter(df1,Display %in% c("No_Displ")) %>%
          group_by(ENSEIGNE) %>%
          summarise(CA_mag = sum(CA_mag))
        
        # Mergin Data by ENSEIGNE for CA_mag
        ENSEIGNE_Displ_CA<-as.data.table(ENSEIGNE_Displ_CA)
        ENSEIGNE_No_Displ_CA<-as.data.table(ENSEIGNE_No_Displ_CA)
        
        setkey(ENSEIGNE_Displ_CA,ENSEIGNE)
        setkey(ENSEIGNE_No_Displ_CA,ENSEIGNE)
        Merge_DT<- ENSEIGNE_Displ_CA[ENSEIGNE_No_Displ_CA, nomatch=0]
        colnames(Merge_DT)<-c("ENSEIGNE","Displ_CA","No_Displ_CA")
        
        #--------------
        # Merging for N
        
        # Displ
        ENSEIGNE_Displ_N<- filter(df1,Display %in% c("Displ")) %>%
          count(ENSEIGNE)
        
        # No_Displ
        ENSEIGNE_No_Displ_N<-filter(df1,Display %in% c("No_Displ")) %>%
          count(ENSEIGNE)
        
        # Mergin Data by ENSEIGNE (using a inner join)
        ENSEIGNE_Displ_N<-as.data.table(ENSEIGNE_Displ_N)
        ENSEIGNE_No_Displ_N<-as.data.table(ENSEIGNE_No_Displ_N)
        
        setkey(ENSEIGNE_Displ_N,ENSEIGNE)
        setkey(ENSEIGNE_No_Displ_N,ENSEIGNE)
        Merge_DT2<- ENSEIGNE_Displ_N[ENSEIGNE_No_Displ_N, nomatch=0]
        colnames(Merge_DT2)<-c("ENSEIGNE","Displ_N","No_Displ_N")
        
        # Merging pour la table finale
        setkey(Merge_DT,ENSEIGNE)
        setkey(Merge_DT2,ENSEIGNE)
        ENSEIGNE_base<- Merge_DT2[Merge_DT, nomatch=0]
        
        # Recalisation des colonnes
        ENSEIGNE_base <- ENSEIGNE_base %>% 
          relocate(Displ_CA, .after = No_Displ_N)
        
        #----------------------------------------------------------------------
        #              ML ANALYSIS : cALCULATION STEP 
        #----------------------------------------------------------------------
        # dummies creation
        df2=dummy_cols(df1, select_columns =c("Display","ENSEIGNE","Feature"),remove_selected_columns = T)
        
        # Discretization
        mdlp=mdlp(df2)
        
        MDLP_sales_in_val=mdlp[["Disc.data"]][["cor_sales_in_val"]]
        df2=cbind(df2, MDLP_sales_in_val)   #1: cor_sales_in_val Added
        
        MDLP_sales_in_vol=mdlp[["Disc.data"]][["cor_sales_in_vol"]]
        df2=cbind(df2, MDLP_sales_in_vol)   #2: cor_sales_in_vol Added
        
        MDLP_CA_mag=mdlp[["Disc.data"]][["CA_mag"]]
        df2=cbind(df2,MDLP_CA_mag)          #3: CA_mag Added
        
        MDLP_value=mdlp[["Disc.data"]][["value"]]
        df2=cbind(df2,MDLP_value)           #4: value Added
        
        MDLP_venteConv=mdlp[["Disc.data"]][["VenteConv"]]
        df2=cbind(df2,MDLP_venteConv)       #5: VenteConv Added
        
        df2=df2[,-c(1,2,3,4,5)]  # Remove the 5 first columns

        #-----------------------------------------------------
        # Creation d echantillon apprentissage + test
        #-----------------------------------------------------
        # Base d'etude 
        learning_base=df2[,-c(2,23)]
        
        # Creation d echantillons
        set.seed(1)
        dt=sort(sample(nrow(learning_base),nrow(learning_base)*0.7)) # Tirage aleatoire
        train=learning_base[dt,]
        train_test=learning_base[-dt,]
        
        train_target=train[,1]            # echantillon d apprentissage
        train_test_target=train_test[,1]  # echantillon train_test
        
        #--------------------------------------------
        #         Model : Decision Tree
        #--------------------------------------------
        set.seed(1)
        react_Decision_Tree <- rpart(train$Display_Displ ~.,
                                     data=train, method="class",control=rpart.control(minsplit = 10, maxdepth = 30))
        
        ## Decision Tree : Print Confusion Matrix
        pred <- predict(react_Decision_Tree,train_test,type = "class")
        cm1 <- table(pred,train_test$Display_Displ)
        e1 <- round((cm1[2] + cm1[3])/sum(colSums(cm1)),3) # Decision Tree : Print Error Rate
        
        #--------------------------------------------
        #        Model : Random Forest    
        #--------------------------------------------
        # Random Forest : Remommage des variables (des 2 bases)
        colnames(train)[4]  <- "CARREFOUR_MARKET"
        colnames(train)[10] <- "Hyper_U"
        colnames(train)[13] <- "MARCHE_U"
        colnames(train)[19] <- "SIMPLY_MARKET"
        colnames(train)[20] <- "SUPER_U"
        
        colnames(train_test)[4]  <- "CARREFOUR_MARKET"
        colnames(train_test)[10] <- "Hyper_U"
        colnames(train_test)[13] <- "MARCHE_U"
        colnames(train_test)[19] <- "SIMPLY_MARKET"
        colnames(train_test)[20] <- "SUPER_U"
        
        ## Random forest pour mtr=8 ##
        react_rdm_forest <- randomForest(Display_Displ ~ ., data = train, ntree = 1000)
        w_data <- tuneRF(train_test[,-1], train_test[,1], ntreeTry = 1000, plot=F, trace=F) #pensez a laisser trace=T
        
        rdm_frst <- randomForest(Display_Displ ~ ., data = train, mtry=8,ntree = 1000)
        
        ## Random Forest : Print Confusion Matrix
        set.seed(1)
        predicted2 <- predict(rdm_frst, train_test)
        pred2 <-ifelse(predicted2 > 0.5, 1, 0)
        cm2 <- table(pred2, train_test$Display_Displ)
        e2 <- round((cm2[2] + cm2[3])/sum(colSums(cm2)),3)
        
        #---------------------------------------------
        #        Model : Logistic Regression    
        #---------------------------------------------
        
        set.seed(1)
        react_Logistic_reg <- glm(Display_Displ ~ ENSEIGNE_AUCHAN + ENSEIGNE_CARREFOUR + CARREFOUR_MARKET + 
                             ENSEIGNE_CASINO + ENSEIGNE_ECOMARCHE + ENSEIGNE_FRANPRIX + 
                             ENSEIGNE_GEANT + ENSEIGNE_INTERMARCHE + Hyper_U + ENSEIGNE_LECLERC + 
                             ENSEIGNE_MATCH + ENSEIGNE_MONOPRIX + ENSEIGNE_PRISUNIC + 
                             ENSEIGNE_SHOPI + Feature_Feat + MDLP_sales_in_val + MDLP_sales_in_vol + 
                             SUPER_U + MDLP_CA_mag + MDLP_value + MDLP_venteConv, data = train,family = binomial(logit))
        
        ## Logistic Reg : Confusion Matrix
        pred3 <- predict(react_Logistic_reg, newdata = train_test, type = "response")
        cm3<-table(pred3 > 0.5, train_test$Display_Displ)    # Matrice de Confusion
        e3 <- round((cm3[2] + cm3[3])/sum(colSums(cm3)),3)   # Taux d'erreur
        
        ##-----------------------------------------------------------------------
        #         Comparaison des Modeles : Performences , ect ....
        #-----------------------------------------------------------------------
        # Model 1 - Decision Tree : Sensibilte et specificity -------------------
        precision_1 <- round((cm1[1])/sum(cm1[1],cm1[2]),3)
        sensitivity_1 <- round((cm1[1])/sum(cm1[1],cm1[3]),3)
        specificity_1 <- round((cm1[4])/sum(cm1[2],cm1[4]),3)

        tab_sensi_spec1 <- cbind(precision_1, sensitivity_1, specificity_1)
        colnames(tab_sensi_spec1) <- c("precision","sensitivity","specificity")
        tab_sensi_spec1 <-as.data.frame(tab_sensi_spec1)
        tab_sensi_spec1

        # Model 2 - Random Forest : Sensibilte et specificity -------------------
        precision_2 <- round((cm2[1])/sum(cm2[1],cm2[2]),3)
        sensitivity_2 <- round((cm2[1])/sum(cm2[1],cm2[3]),3)
        specificity_2 <- round((cm2[4])/sum(cm2[2],cm2[4]),3)

        tab_sensi_spec2 <- cbind(precision_2, sensitivity_2, specificity_2)
        colnames(tab_sensi_spec2) <- c("precision","sensitivity","specificity")
        tab_sensi_spec2 <-as.data.frame(tab_sensi_spec2)
        tab_sensi_spec2

        # Model 3 - Logistic : Reg Sensibilte et specificity --------------------
        precision_3 <- round((cm3[1])/sum(cm3[1],cm3[2]),3)
        sensitivity_3 <- round((cm3[1])/sum(cm3[1],cm3[3]),3)
        specificity_3 <- round((cm3[4])/sum(cm3[2],cm3[4]),3)

        tab_sensi_spec3 <- cbind(precision_3, sensitivity_3, specificity_3)
        colnames(tab_sensi_spec3) <- c("precision","sensitivity","specificity")
        tab_sensi_spec3 <-as.data.frame(tab_sensi_spec3)

        #-------------------------------------------------------------------
        Model <-c("Decision Tree","Random Forest","Logistic Reg")
        Models <- rbind(tab_sensi_spec1, tab_sensi_spec2, tab_sensi_spec3)
        Perf_mtrics <-cbind(Model,Models)
        Perf_mtrics <- as.data.frame(Perf_mtrics)
        

        
      #################################################################################################################################
      },
      
      error = function(e) { #Gestion des erreurs
        stop(safeError(e)) 
      }
    )   
  
    ##################### Displaying Data Tables #####################################################################################
    #------------------------- Interactive table : table1 and Save (.imgn .png) -------------------------
    output$table1 <- renderReactable({ #sortie table df1 en mode rectable
      reactable(#Update_df1(),
        df1,compact = TRUE,resizable = TRUE,            #Rendre la table redimensionnable avec resizeable
        searchable = TRUE,defaultPageSize = 12, 
        
        #Mise en forme de table de donnees
        columns = list(
          cor_sales_in_vol = colDef(footer = "TOTAL"),
          CA_mag = colDef(footer = JS("function(colInfo) {
                                     var total = 0
                                     colInfo.data.forEach(function(row) {
                                     total += row[colInfo.column.id]})
                                     return total.toFixed(2)+' EUR'}")
                         
                         ,format = colFormat(currency = "EUR"))
          ),
        
        #theme et sortie du tableau et de la fenetre de recherche
        theme = reactableTheme( 
          searchInputStyle = list(width = "100%"),
          headerStyle = list(
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),borderColor = "#555")
        ),
        
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold")) #Mise en forme du footer 
        )
    })
  
    #------------------------- Table : Display and Save (.imgn .png) ------------------------
    # Display_table <- df1 %>%
    output$table3 <- renderReactable({
      Display_tab1 <- df1 %>% 
        count(Display) %>%
        arrange(desc(n()))
      
      Display_tab2 <- df1 %>%
        group_by(Display) %>%
        summarise(CA_mag = sum(CA_mag)) %>%
        mutate(pct_CA= round(prop.table(CA_mag),4)) %>%
        mutate(rank_by_CA = dense_rank(desc(CA_mag))) %>%
        arrange(CA_mag)   #arrange(desc(CA_mag))
      
      Display_tab3 <- df1 %>%
        group_by(Display) %>%
        summarise(cor_sales_in_vol=sum(cor_sales_in_vol)) 
      
      Display_tab4 <- df1 %>%
        group_by(Display) %>%
        summarise(cor_sales_in_val=sum(cor_sales_in_val))
      
      Display_tab5 <- df1 %>%
        group_by(Display) %>%
        summarise(VenteConv=sum(VenteConv)) 
      
      Display_tab6 <- df1 %>%
        group_by(Display) %>%
        summarise(value=sum(value))
      
      # Merging data by Display
      Display_table<-cbind(Display_tab1,Display_tab6[-1],Display_tab3[-1],Display_tab4[-1],Display_tab5[-1],Display_tab2[-1])
      Display_table %>%
        arrange(desc(CA_mag))

      reactable(
        Display_table,
        
        columns = list(
          CA_mag=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          pct_CA = colDef(format = colFormat(percent = TRUE, digits = 1)),
          cor_sales_in_vol=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          cor_sales_in_val=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          VenteConv=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE"))
        )
      )
    })
    
    #------------------------- Table : table5 and Save (.imgn .png) ------------------------
    output$table5 <- renderReactable({
      # Penser a un Merge en utilisant data.table
      ENSEIGNE_tab1 <- df1 %>% 
        count(ENSEIGNE) %>%
        arrange(desc(n()))
      
      ENSEIGNE_tab2 <- df1 %>%
        group_by(ENSEIGNE) %>%
        summarise(CA_mag = sum(CA_mag)) %>%
        mutate(pct_CA= round(prop.table(CA_mag),4)) %>%
        mutate(rank_by_CA = dense_rank(desc(CA_mag)))
      
      ENSEIGNE_tab3 <- df1 %>%
        group_by(ENSEIGNE) %>%
        summarise(cor_sales_in_vol=sum(cor_sales_in_vol)) 
      
      ENSEIGNE_tab4 <- df1 %>%
        group_by(ENSEIGNE) %>%
        summarise(cor_sales_in_val=sum(cor_sales_in_val))
      
      ENSEIGNE_tab5 <- df1 %>%
        group_by(ENSEIGNE) %>%
        summarise(VenteConv=sum(VenteConv)) 
      
      ENSEIGNE_tab6 <- df1 %>%
        group_by(ENSEIGNE) %>%
        summarise(value=sum(value))
      
      # Merging data by ENSEIGNE
      ENSEIGNE_table<-cbind(ENSEIGNE_tab1,ENSEIGNE_tab6[-1],ENSEIGNE_tab3[-1],ENSEIGNE_tab4[-1],ENSEIGNE_tab5[-1],ENSEIGNE_tab2[-1])
      ENSEIGNE_table %>%
        arrange(desc(CA_mag))
      
      reactable(
        ENSEIGNE_table,compact = TRUE,resizable = TRUE,searchable = TRUE,
        
        columns = list(
          CA_mag=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          pct_CA = colDef(format = colFormat(percent = TRUE, digits = 1)),
          cor_sales_in_vol=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          cor_sales_in_val=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          VenteConv=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE"))
        ),
        
        #theme et sortie du tableau et de la fenetre de recherche
        theme = reactableTheme( 
          searchInputStyle = list(width = "100%"),
          
          headerStyle = list(
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),borderColor = "#555")
        )
        
      )
      
    })
    
    #------------------------- Table : table7 and Save (.imgn .png) ------------------------
    # Feature_table <- df1 %>%
    output$table7 <- renderReactable({
      Feature_tab1 <- df1 %>% 
        count(Feature) %>%
        arrange(desc(n()))
      
      Feature_tab2 <- df1 %>%
        group_by(Feature) %>%
        summarise(CA_mag = sum(CA_mag)) %>%
        mutate(pct_CA= round(prop.table(CA_mag),4)) %>%
        mutate(rank_by_CA = dense_rank(desc(CA_mag))) %>%
        arrange(CA_mag)   #arrange(desc(CA_mag))
      
      Feature_tab3 <- df1 %>%
        group_by(Feature) %>%
        summarise(cor_sales_in_vol=sum(cor_sales_in_vol)) 
      
      Feature_tab4 <- df1 %>%
        group_by(Feature) %>%
        summarise(cor_sales_in_val=sum(cor_sales_in_val))
      
      Feature_tab5 <- df1 %>%
        group_by(Feature) %>%
        summarise(VenteConv=sum(VenteConv)) 
      
      Feature_tab6 <- df1 %>%
        group_by(Feature) %>%
        summarise(value=sum(value))
      
      # Merging data by Feature
      Feature_table<-cbind(Feature_tab1,Feature_tab6[-1],Feature_tab3[-1],Feature_tab4[-1],Feature_tab5[-1],Feature_tab2[-1])
      Feature_table %>%
        arrange(desc(CA_mag))
      
      #---------
      reactable(
        Feature_table,
        
        columns = list(
          CA_mag=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          pct_CA = colDef(format = colFormat(percent = TRUE, digits = 1)),
          cor_sales_in_vol=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          cor_sales_in_val=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          VenteConv=colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE"))
        )
        
      )
      
    })
    
    #------------------------------- Table : table9 and Save (.imgn .png) -----------------------------------
    output$table9 <- renderReactable({
      ENSEIGNE_base
      reactable(ENSEIGNE_base,compact = TRUE,resizable = TRUE,searchable = TRUE,
        columns = list(
          Displ_CA = colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE")),
          No_Displ_CA = colDef(format = colFormat(currency = "EUR", separators = TRUE, locales = "de-DE"))
        ),
        
        #theme et sortie du tableau et de la fenetre de recherche
        theme = reactableTheme( 
          searchInputStyle = list(width = "100%"),
          
          headerStyle = list(
            "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)"),borderColor = "#555")
        )
    )
    })
    
    #---------------------------------- Table : table11 and Save (.imgn .png) -------------------------------
    # # Base table for MDLP title="Table by MDLP Method After Discretization"
    react_discretiz <- eventReactive(input$btn_decretiz , {
      df2
    })
    
    output$table11 <- renderReactable({ #df2
      reactable(react_discretiz(),resizable = TRUE,searchable = TRUE,defaultPageSize = 12)
    })

    #---------------------- Learning Tables Results -------------------------
    ### Learning table : # (train, train_test) ###
    react_lrng_train <- eventReactive(input$btn_train_dataset ,{
      train
    })
 
    output$table13 <- renderReactable({
      reactable(react_lrng_train(),resizable = TRUE,searchable = TRUE,defaultPageSize = 12)
    })
    
    # Test table : # train_test
    output$table15 <- renderReactable({ 
      reactable(react_lrng_train()[-dt,],resizable = TRUE,searchable = TRUE,defaultPageSize = 12)
    })
    
  
    
    #################### Displaying ValueBox #################################################################
    # valuebox 1 : Total Turnover-----------------------------------------------------------------------------
    output$valuebox1<- renderValueBox({
      Total_CA_mag<-df1 %>% 
        group_by(ENSEIGNE) %>%
        summarise(CA_mag = sum(CA_mag))
      Total_CA_mag=sum(Total_CA_mag$CA_mag)
      
      #--------
      valueBox(
        paste0(Total_CA_mag," EUR"), "Total Turnover", icon = icon("credit-card"),
        color = "purple"
      )
    })
    
    
    #valuebox 2 : Top 10 best sellers --------------------------------------------------------------
    output$valuebox2<- renderValueBox({
      Total_big_grp<-df1 %>%
        group_by(ENSEIGNE) %>%
        summarise(CA_mag = sum(CA_mag)) %>%
        mutate(rank = dense_rank(desc(CA_mag))) %>%
        mutate(pct= 100*round(prop.table(CA_mag),4)) %>%  #Pourcentage que prend le top10 des depenses
        arrange(desc(CA_mag))

      Total_big_grp<-data.frame(Total_big_grp)
      Total_big_grp_top10<-head(Total_big_grp,10)         # Top 10 best sellers
      Total_big_grp_CA_mag<-sum(Total_big_grp_top10$pct)  

      #--------
      valueBox(
        paste0(Total_big_grp_CA_mag," %"), "Top 10 Best Sellers (T.O)", icon = icon("list"), #icon = icon("credit-card"),
        color = "blue"
      )
    })
    
    #valuebox 3 : Best seller rank--------------------------------------------------------------
    output$valuebox3<- renderValueBox({
      Top_ENSEIGNE<-df1 %>%
        group_by(ENSEIGNE) %>%
        summarise(CA_mag = sum(CA_mag)) %>%
        mutate(pct= 100*round(prop.table(CA_mag),4)) %>%
        arrange(desc(CA_mag))

      Top_ENSEIGNE<-data.frame(Top_ENSEIGNE)
      Top_ENSEIGNE_top10<-head(Top_ENSEIGNE,1)      #Afficher le Top10 des meilleurs vendeurs

      Top1_ENSEIGNE<-Top_ENSEIGNE_top10$pct
      Top1_ENSEIGNE_name<-Top_ENSEIGNE_top10$ENSEIGNE

      valueBox(
        paste0(Top1_ENSEIGNE," %"), paste0("BSR (T.O): ",Top1_ENSEIGNE_name), icon = icon("list"),
        color = "aqua"
      )
    })
    
    #valuebox 4 : ENSEIGNE LIST  --------------------------------------------------------------
    output$valuebox4<- renderValueBox({
      Desig_grp<-df1 %>%
        group_by(ENSEIGNE) %>%
        summarise(CA_mag = sum(CA_mag)) %>%
        arrange(desc(CA_mag))
      
      #--------
      valueBox(
        paste0(nrow(Desig_grp)), "Total Stores",
        color = "blue"
      )
    })
    
    #################### Displaying Pickerinput lists #########################################################
    # PickerInput Display
    filteredData_display <- reactive({
      if (input$display == "All displays") {
        df1
      } 
      else {
        filter(df1, Display == input$display)
      }
    })
    
    output$type_display<- renderUI({
      pickerInput("display", label = "Select a Display:",
                  choices = list("All displays", `Display :` =c("Displ", "No_Displ")),
                  options = list(`live-search` = TRUE)
      )
    })
    
  ###################### Displaying Plots ##########################################################################################
    #--- Plot 1 : Boxplot of the ENSEIGNE types ---
     output$plot1<-renderPlot({
    ggplot(df1, aes(ENSEIGNE, CA_mag, fill = factor(ENSEIGNE))) +
      geom_boxplot()+
      ggtitle("Boxplot of CA_mag by type of ENSEIGNE")
    })
     
     #-- Plot 3 : Horizontal Barplot of types ENSEIGNE ---
     output$plot3<-renderPlot({
       ggplot(data = ENSEIGNE_grp,aes(x=reorder(ENSEIGNE,CA_mag),y=CA_mag),label=CA_mag)+
         geom_bar(stat = "identity",fill = "#8f1a09",color = "white")+coord_flip() +
         ggtitle("Distribution of CA_mag by type of ENSEIGNE")
     })
    
     ##-- Plot 5 : Donut Chart of ENSEIGNE ---
     output$plot5<-renderPlot({ 
       text <- df1 %>%
         pull(ENSEIGNE)
       corpus <- stri_trans_general(text, "latin-ascii")
       corpus <- Corpus(VectorSource(corpus))
       dtm <- TermDocumentMatrix(corpus)
       matrix <- as.matrix(dtm)
       words <- sort(rowSums(matrix),decreasing=TRUE)
       df <- data.frame(word = names(words),freq=words)
       
       set.seed(1234) # for reproducibility
       wordcloud(words = df$word,freq = df$freq,min.freq = 1,max.words = 1000,
                 random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))
     })
     
     ##-- Plot 7 : Donut Chart of ENSEIGNE ---
     output$plot7<-renderPlotly({ 
       colourCount <- length(unique(ENSEIGNE_donut$pct))         # number of levels
       getPalette <- colorRampPalette(brewer.pal(9, "Set1"))   # definition de la palette de couleur
       
       plot_ly(data = ENSEIGNE_donut, labels = ~ENSEIGNE, values = ~pct, sort= FALSE,
               marker= list(colors=colorRampPalette(brewer.pal(11,"RdYlGn"))(colourCount), line = list(color="black", width=1))) %>%
         add_pie(hole = 0.6) %>%
         layout(title="Donut Chart of ENSEIGNE")
      })
     
     #--------------------------- Plot 9 ----------------------------
     output$plot9<-renderPlot({
       filteredData_display() %>%
       arrange(desc(CA_mag))

       ggplot(filteredData_display(),aes(x=reorder(ENSEIGNE,CA_mag),y=CA_mag),label=CA_mag)+
         geom_bar(stat = "identity",fill = "#8f1a09",color = "blue")+coord_flip() +
         ggtitle("Distribution of CA_mag by Display modality")
     })
     
     ################ Displaying Action Button And Plot #############################################################
     #------------------------------------------------
     #        PCA Analysis
     #------------------------------------------------
     # PCA : Reactive Action Button et Plots ---------
     react_PCA <- eventReactive(input$btn_pca , {
       PCA(df2,graph=FALSE)
     })
     
     # PCA : Graph of Variables
     output$plot13<-renderPlot({
       plot.PCA(react_PCA(),choix='var',title="Graph of PCA variables")
     })
     
     # PCA : Graph of Individuals
     output$plot15<-renderPlot({
       plot.PCA(react_PCA(),title="Plot of the PCA individuals")
     })
     
     # # PCA : Console output summary 
     output$summary_PCA<-renderPrint({
       summary(react_PCA())
     })
     
     # PCA : Graph of Eigein_values (PCA)
     output$plt_eboulis<-renderPlot({
       plot(react_PCA()$eig[,1],type = "o",main = "Eigenvalue graph",xlab = "dimensions",ylab = "eigenvalue")
     })
     
     
     #################################################################################################################
     ## Decision Tree : Plot Model
     output$plot17<-renderPlot({
       rpart.plot(react_Decision_Tree)
     })
    
     ## Decision Tree : Confusion Mtrix
     output$d_tree_cnf<-renderPrint({
       cm1
     })
     
     ## Decision Tree : Print Error Rate
     output$d_tree_err<-renderPrint({
       paste0("error rate : ", e1)  # e1 : error rate value
     })
     
     #---------------------------------------------------------------------
     #        Random Forest Model - RF
     #---------------------------------------------------------------------
     # Random Forest : Reactive Action Button Tables and Plots ------------

     ## Random Forest : Plot (Optimal number of trees)##
     output$plot19<-renderPlot({
       set.seed(1)
       plot(react_rdm_forest,main="Optimal number of trees")
     })

     ## Random Forest : Print (Only results)
     output$rf_mtry_optimal<-renderPrint({
       w_data
     })
     
     ## Random Forest : Plot m_try Optimal
     output$plot21<-renderPlot({
       w_list <- c(w_data[plot=T])
       m_try <- w_list[1:(length(w_list)/2)]
       oob_error <- w_list[(length(w_list)/2):length(w_list)]
       oob_error<- oob_error[-1]
       
       tnRF_df <- cbind(m_try,oob_error)
       colnames(tnRF_df) <- c("m_try","oob_error")
       tnRF_df <- as.data.frame(tnRF_df)
       
       # plots
       ggplot(tnRF_df) + geom_line(size=1,aes(m_try, oob_error),color='darkblue') + 
         labs(title="Mtry optimal")
     })
     
     output$rmd_frst_cnf<-renderPrint({
       cm2
     })

     ## Random Forest : Print Error Rate
     output$rmd_frst_err<-renderPrint({
       paste0("error rate : ", e2)  # e2 : error rate
     })

     #--------------------------------------------------------------------------------------------------------------
     #        Logistic Regression Model - LRM
     #--------------------------------------------------------------------------------------------------------------
     # Logistic Regression : Reactive Action Button Tables and Plots -----------------------------------------------------

     ## Logistic Reg : Print Summary of Model
     output$summary_logit<-renderPrint({
       summary(react_Logistic_reg)
     })
     
     ## Logistic Reg : Confusion Mtrix
     output$logit_cnf<-renderPrint({
       cm3
     })
     
     ## Logistic Reg : Print Error Rate
     output$logit_err<-renderPrint({
       paste0("error rate : ", e3)  # e3 : error rate
     })
     
     ## Logistic Reg : Plot Model
     output$plot23<-renderPlot({
       ggcoef_model(react_Logistic_reg, exponentiate = TRUE)
     })

     ## Logistic Reg : Print Metrics Performences table
     output$prf_metrics<-renderPrint({
       Perf_mtrics
     })
     
  ######################################################################################################################
     
  }) #Ending output$contents
} #Ending server

#----------------------
#     Lanching APP
#----------------------
shinyApp(ui, server)

