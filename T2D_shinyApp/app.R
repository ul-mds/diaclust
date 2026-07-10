# # # interactive Web-application to classify T2D patients with the help of our multinom-model # # # 
#--------------------------------------------------------------------------------------------------#

# install/load required packages
package_names<-c("shiny","ggplot2","gridExtra","nnet")    
# check if required packages are installed
for (package_name in package_names) {
  if (!requireNamespace(package_names, quietly = TRUE)) {
    install.packages(package_name)
  }
}

library('shiny')
library('ggplot2')
library('gridExtra')
library('nnet')
# load model
dia_model<-readRDS("DiaClusT_model.rds")

# used dataset (march 2026)
data<-read.csv("dataset_wo_ident.csv")

# declarations
cluster_names <- c("1" = "SIDD", "2" = "SIRD", "3" = "MOD",  "4" = "MARD")
cluster_colors = c("#A6CEE3","#B2DF8A","#FBB4AE", "#CAB2D6")


# # #---------------------------------------------------------------------------
# building the app

ui <- fluidPage(
  div(
    style = "max-width: 1200px; margin: auto;",
  titlePanel(
    div(
      style = "color: steelblue;",
      "DiaClusT Explorer")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 5,
      h4("Patient characteristics"),
      radioButtons("gender", "Sex", choices = c("male" ="male", "female"="female")),
      sliderInput("age", "Age [years]", min = 18, max = 100, value = 50, step = 0.1),
      numericInput("mean_BMI", "BMI [kg/m²]", min = 10, max = 80, value = 25, step = 0.1),   
      numericInput("HbA1c", "HbA1c [%]", min = 0, max = 15, value = 6, step = 0.1),
      selectInput(
        "trig_unit",
        "Triglycerides unit",
        choices = c("mmol/l", "mg/dl"),
        selected = "mmol/l"
      ),
      uiOutput("trig_input"),
      numericInput("TRIG", "Triglycerides", value = 1.5, step = 0.01)
    ),
    
    mainPanel(
      width = 7,
      
      tabsetPanel(
        
        tabPanel(
          "Overview",
          h5("The plots provide a classification of the characteristics in comparison with our cohort of 8,106 patients. 
             The red dashed line displays the patient data that has been entered."),
          br(),
          # add prediction output here
          uiOutput("prediction_output_overview"),
          br(),
          fluidRow(
            column(6,plotOutput("age_boxplot",height="280px")),
            column(6,plotOutput("bmi_boxplot",height="280px"))
          ),
          fluidRow(
            column(6, plotOutput("hba1c_boxplot",height="280px")),
            column(6, plotOutput("trig_boxplot",height="280px"))
          ),
          br(),
          h5(strong("SIDD:"), "Severe Insulin-Deficient Diabetes"),
          h5(strong("SIRD:"), "Severe Insulin-Resistant Diabetes"),
          h5(strong("MOD:"), "Mild Obesity-Related Diabetes"),
          h5(strong("MARD:"), "Mild Age-Related Diabetes"),
          #div(style = "height: 2px;"),
          h5(span(strong("Reference:"), style="color:steelblue;"),("Holdgrün et al.,"),em("Differential long-term clinical outcomes among 
                                                                                          type 2 diabetes subphenotypes in hospitalized patients")),
          div(style = "height: 10px;"),
          h6("DISCLAIMER"),
          h6("I understand that this tool is not approved as a medicinal product for clinical use, 
             and I confirm that I will use this tool for research purposes only.")   
        ),
        tabPanel(
          "Prediction",
          h4("Prediction of T2D subphenotype"),
          br(),
          h5(strong("Main features of the subtypes:")),
          verbatimTextOutput("prediction_output"),
          br(),
          textOutput("prediction_output2"),  
          br(),# using text
          #strong(textOutput("prediction_output_plot")), 
          div(
            style = "text-align:center; font-size:24px; font-weight:bold; color:#003366;",
            textOutput("prediction_output_plot")
          ),
          br(),
          plotOutput("prediction_plot", height = "350px"),
          br(),
          textOutput("prediction_output3")
        ),
        tabPanel(
          "Interpretation",
          h4("Additional information"),
          br(),
          textOutput("interpret_output0"),
          br(),
          strong("Sex"),
          textOutput("interpret_output1"),
          br(),
          strong("Age"),
          textOutput("interpret_output2"),
          br(),
          strong("BMI"),
          textOutput("interpret_output3"),
          br(),
          strong("Triglycerides"),
          textOutput("interpret_output4")
        )
        
      )
    )
    
  ),
  
  tags$head(
    tags$style(
      HTML("
           .shiny-output plot {
             height: 300px !important;
             width: 100% !important;
           }
           ")
    )
  )
)
)

# # # server logic
server <- function(input, output, session) {
  
  patient_trig <- reactive({
    req(input$TRIG, input$trig_unit)
    
    if (input$trig_unit == "mmol/l") {
      input$TRIG
    } else { (input$TRIG / 88.57)
    }
    
  })
  
  # reactive function to filter data based on gender
  filtered_data <- reactive({
    data[data$gender == input$gender, ]
  })
  
  # create dataframe from input in app
  patient_data <- reactive({
    
    data.frame(
      gender = factor(input$gender, levels = c("male","female")), 
      age = input$age,
      mean_BMI = input$mean_BMI,      
      HbA1c = input$HbA1c,
      TRIG = patient_trig()
    )
    
  })
  
# # # Tab PREDICTION
  
  # probabilities
  prediction_prob <- reactive({
    probs <- predict(dia_model, newdata = patient_data(), type = "probs")
    
    # Convert to data.frame if returned as a vector
    if (is.vector(probs)) {
      probs <- t(as.data.frame(probs))  # transpose to 1-row data.frame
    }
    
    probs
  })
  
  # add output text 
  
  output$prediction_output <- renderText({
    paste(
      "",
      "Cluster SIDD: \t highest HbA1c and increased BMI, 
      \t \t highest risk for Coronary artery disease",
      "",
      "Cluster SIRD: \t highest TG/HDL-C ratio (marker of insulin resistance), 
      \t \t increased BMI, 
      \t \t highest prevalance of chronic kidney disease, 
      \t \t highest inflammatory markers at admission, 
      \t \t highest risk for inpatient complications, i.e. mechanical ventilation, kidney replacement therapy, 
      \t \t highest risk for intensive care unit/intermediate care unit admission, 
      \t \t highest risk for in-hospital mortality",
      "",
      "Cluster MOD: \t highest BMI,
      \t \t lowest mortality risk",
      "",
      "Cluster MARD: \t highest age, 
      \t \t lowest BMI, HbA1c and TG/HDL-C ratio, 
      \t \t high prevalence of chronic kidney disease",
      "", 
      "A three-dimensional visualization of the principal component analysis (PCA)",
      "showing the four clusters is available at the following link:",
      "https://ul-mds.github.io/DIA-Cluster-3D-Path/", sep="\n"
      
    )
  })
  
  output$prediction_output2 <- renderText({
    paste(
      "The plot below shows the probabilities for assignment to one of the four subphenotypes of type 2 diabetes based on the patient data you entered.")
  })
  
  # add plot to show probabilities 
  predicted_cluster <- reactive({
    
    probs <- as.numeric(prediction_prob())
    
    cluster_names <- c("SIDD", "SIRD", "MOD", "MARD")
    
    cluster_names[which.max(probs)]
    
  })
  
  output$prediction_output_plot <- renderText({
    paste("Predicted subtype:", predicted_cluster())
  })
  
 # output$prediction_output_plot <- renderText({
    
#    probs <- prediction_prob()
    
#    cluster_names <- c("SIDD", "SIRD","MOD", "MARD")
    
 #   predicted_cluster <- cluster_names[which.max(probs)]
    
#    paste("Predicted subtype:", predicted_cluster)
#  })
  
  output$prediction_plot <- renderPlot({
    
    probs <- prediction_prob()
    
    # Convert to numeric vector
    probs <- as.numeric(probs)
    
    prob_df <- data.frame(
      cluster = factor(c("SIDD", "SIRD", "MOD", "MARD"),
                       levels=c("SIDD", "SIRD", "MOD", "MARD")),
      probability = probs
    )
    prob_df$winner <- prob_df$probability == max(prob_df$probability)
    
    ggplot(prob_df,aes(x = cluster, y = probability, fill = cluster)) +
      #geom_col(width = 0.7) +
      geom_col(aes(linewidth = winner), colour = "black") +
      scale_linewidth_manual(values = c(0.3, 1.8), guide = "none") +
      geom_text(aes(label = sprintf("%.1f %%", probability*100)),
                vjust = -0.4, size = 5) +
      scale_fill_manual(values = cluster_colors) +
      coord_cartesian(ylim = c(0,1)) +
      labs(x = NULL,y = "Probability") +
      theme_minimal() +
      theme(
        legend.position = "none",
        text = element_text(size = 13),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)
      )
  })
  
  output$prediction_output3 <- renderText({
    paste(
      "Please note the information in the 'Interpretation' tab.")
  })
  
  
  
  
# # # Tab OVERVIEW


  output$prediction_output_overview <- renderUI({
    
    tags$strong(
      "The patient belongs most likely to the",
      tags$span(
        predicted_cluster(),
        style = "color: steelblue;"
      ),
      " subtype. Please note the tab 'Prediction' for more information."
    )
    
  })
  
  
  # # # #
  # boxplot for age
  output$age_boxplot <- renderPlot({
    p <- ggplot(filtered_data(), aes(x = as.factor(cluster), y = age)) +
      geom_boxplot(fill=cluster_colors) +
      geom_hline(yintercept = input$age, color = "red", linetype = "dashed") +
      labs(title = "Age distribution", y = "Age [years]") +
      scale_x_discrete(labels = c("1" = "SIDD", "2" = "SIRD", "3" = "MOD", "4" = "MARD"))+
      scale_fill_manual(values = cluster_colors)+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size=14, face="bold", color="darkslategrey")
            )
    
    print(p)
  })
  
  # boxplot for BMI
  output$bmi_boxplot <- renderPlot({
    p <- ggplot(filtered_data(), aes(x = as.factor(cluster), y = mean_BMI)) +
      geom_boxplot(fill=cluster_colors) +
      geom_hline(yintercept = input$mean_BMI, color = "red", linetype = "dashed") +
      labs(title = "BMI distribution", y = "BMI [kg/m²]") +
      scale_x_discrete(labels = c("1" = "SIDD",  "2" = "SIRD", "3" = "MOD", "4" = "MARD"))+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size=14, face="bold", color="darkslategrey")
            )
    
    print(p)
  })
  
  # boxplot for HbA1c
  output$hba1c_boxplot <- renderPlot({
    p <- ggplot(filtered_data(), aes(x = as.factor(cluster), y = HbA1c)) +
      geom_boxplot(fill=cluster_colors) +
      geom_hline(yintercept = input$HbA1c, color = "red", linetype = "dashed") +
      labs(title = "HbA1c distribution", y = "HbA1c [%]") +
      scale_x_discrete(labels = c("1" = "SIDD", "2" = "SIRD","3" = "MOD", "4" = "MARD"))+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size=14, face="bold", color="darkslategrey")
      )
    
    print(p)
  })
  
  # boxplot for TRIG
  output$trig_boxplot <- renderPlot({
    p <- ggplot(filtered_data(), aes(x = as.factor(cluster), y = TRIG)) +
      geom_boxplot(fill=cluster_colors) +
      geom_hline(yintercept = patient_trig(), color = "red", linetype = "dashed") +
      labs(title = "Triglycerides distribution", y = "TG [mmol/l]") +
      scale_x_discrete(labels = c("1" = "SIDD",  "2" = "SIRD","3" = "MOD", "4" = "MARD"))+
      theme_minimal()+
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size=14, face="bold", color="darkslategrey")
            )
    
    print(p)
  })
  
  
# # Tab INTERPRETATION  
  
  output$interpret_output0 <- renderText({
    paste(
      "Please note that this is only an initial assessment and 
      do not rely entirely on the predicted cluster assignment. The model used is 
      based on a dataset of 8,106 patients from University of Leipzig Medical Center.")
  })
  
  output$interpret_output1 <- renderText({
    paste(
      "The model is based on a study cohort comprised of approximately 62% male individuals and 38% female individuals.")
  })
  
  output$interpret_output2 <- renderText({
    paste(
      "Our study focuses on a population admitted to a tertiary care unit in Germany, with 
      70% of the participants in the age range from 58 to 82 (median age 71 years). The underlying model may 
      therefore not be able to differentiate accurately for age groups outside this range.")
  })
  
  output$interpret_output3 <- renderText({
    paste(
      "Our cohort covers a BMI range from 13 to 65 kg/m². Due to the age distribution, only 336 measurements are
      from individuals younger than 50 years of age, in contrast to 7,770 measurements from the group of those over 50 years of age.")
  })
  
  output$interpret_output4 <- renderText({
    paste(
      "The ratio of triglycerides to high density lipoprotein cholesterol (TG/HDL-C ratio) was originally used for clustering. 
      Predictive models based on machine learning demonstrate high accuracy even when restricted to triglycerides.")
  })
  
  #observe({
  # print(patient_data())
  # })
  
}

#-------------------------------------------------------------------------------

# run
shinyApp(ui = ui, server = server)
