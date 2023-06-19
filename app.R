library(shiny)
library(plotly)
library(readxl)

### data

tab1 <- read_excel("data/Table1.xlsx") 

Table_all <- read.csv("data/Table_all.csv", colClasses = c("NULL", NA, NA, NA, NA, NA, NA, NA))
Copenhagen_all <- read.csv("data/Copenhagen_all.csv", colClasses = c("NULL", NA, NA, NA, NA, NA, NA))

Table_CD <- read.csv("data/Table_CD.csv", colClasses = c("NULL", NA, NA, NA, NA, NA, NA))
Copenhagen_CD <- read.csv("data/Copenhagen_CD.csv", colClasses = c("NULL", NA, NA, NA, NA, NA, NA, NA))

Table_NCD <- read.csv("data/Table_NCD.csv", colClasses = c("NULL", NA, NA, NA, NA, NA, NA))
Copenhagen_NCD <- read.csv("data/Copenhagen_NCD.csv", colClasses = c("NULL", NA, NA, NA, NA, NA, NA, NA))


source("data/Chapters.R")


# Define UI ----

ui <- fluidPage(
  
  h2("Atlas covering educational inequalities in disease incidence"),
  h4("Results from our paper Exploring Inequalities in Population Health: A Phenome-Wide Study Examining the Association between Education and 833 Diseases in Denmark."),
  h5("Authors: Anna Vera Jørring Pallesen, Jochen Mierau and Laust Hvas Mortensen"),
  br(),
  p(strong("Affiliations: ")),
  h6("Department of Public Health, University of Copenhagen, Copenhagen, Denmark"),
  h6("Methods and Analysis, Statistics Denmark, Copenhagen, Denmark"),
  h6("Faculty of Economics and Business, University of Groningen, Groningen, The Netherlands"),
  
  navlistPanel(
    "Population characteristics",
    tabPanel("Table 1",
             h3("Table 1. Population characteristics (N = 4,258,789)"),
             tableOutput('table')),
    "The atlas",
    tabPanel("All ICD-10 chapters",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('atlas_plot')),
                 tabPanel("Table",
                          dataTableOutput('atlas_table'))
               )
             )),
    tabPanel("Chapter 1",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap1_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap1_table'))
               )
             )),
    tabPanel("Chapter 2",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap2_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap2_table'))
               )
             )),
    tabPanel("Chapter 3",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap3_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap3_table'))
               )
             )),
    tabPanel("Chapter 4",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap4_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap4_table'))
               )
             )),
    tabPanel("Chapter 5",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap5_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap5_table'))
               )
             )),
    tabPanel("Chapter 6",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap6_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap6_table'))
               )
             )),
    tabPanel("Chapter 7",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap7_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap7_table'))
               )
             )),
    tabPanel("Chapter 8",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap8_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap8_table'))
               )
             )),
    tabPanel("Chapter 9",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap9_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap9_table'))
               )
             )),
    tabPanel("Chapter 10",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap10_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap10_table'))
               )
             )),
    tabPanel("Chapter 11",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap11_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap11_table'))
               )
             )),
    tabPanel("Chapter 12",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap12_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap12_table'))
               )
             )),
    tabPanel("Chapter 13",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap13_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap13_table'))
               )
             )),
    tabPanel("Chapter 14",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap14_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap14_table'))
               )
             )),
    tabPanel("Chapter 18",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap18_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap18_table'))
               )
             )),
    tabPanel("Chapter 19",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput('chap19_plot')),
                 tabPanel("Table",
                          dataTableOutput('chap19_table'))
               )
             )),
    "Global Burden of disease",
    tabPanel("Noncommunicable diseases",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          h4("Age-standardised incidence rate ratios for NCDs in 2012-2021 presented on a log scale"),
                          plotOutput('NCD_plot')),
                 tabPanel("Table",
                          dataTableOutput('NCD_table'))
               )
             )),
    tabPanel("Communicable diseases",
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          h4("Age-standardised incidence rate ratios for CDs in 2012-2021 presented on a log scale"),
                          plotOutput('CD_plot')),
                 tabPanel("Table",
                          dataTableOutput('CD_table'))
               )
             ))
  )
)



# Define server logic ----

server <- function(input, output) {
  
  # Table1
  output$table <- renderTable(tab1, na = "")
  
  # Atlas
  output$atlas_plot <- renderPlot({
    ggplot(Copenhagen_all, aes(x = log(RR), y = as.factor(KAP), color = as.factor(type), size = prop)) +
      geom_jitter(width = 0, height = 0.25) + 
      theme_minimal() +
      theme_classic() + 
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_rect(color="black", fill=NA),
            strip.background = element_rect(fill=NA, color="black")) +
      scale_y_discrete(limits=rev, labels = c('Chap. 19: Injury, poisoning and certain other external causes',
                                              'Chap. 18: Symptoms, signs and abnormal clinical and laboratory findings',
                                              'Chap. 14: Diseases of the genitourinary system', 
                                              'Chap. 13: Diseases of the musculoskeletal system and connective tissue',
                                              'Chap. 12: Diseases of the skin and subcutaneous tissue',
                                              'Chap. 11: Diseases of the digestive system',
                                              'Chap. 10: Diseases of the respiratory system',
                                              'Chap. 9: Diseases of the circulatory system',
                                              'Chap. 8: Diseases of the ear and mastoid process',
                                              'Chap. 7: Diseases of the eye and adnexa',
                                              'Chap. 6: Diseases of the nervous system', 
                                              'Chap. 5: Mental and behavioural disorders',
                                              'Chap. 4: Endocrine, nutritional and metabolic diseases',
                                              'Chap. 3: Diseases of the blood and blood-forming organs',
                                              'Chap. 2: Neoplasms',
                                              'Chap. 1: Infectious and parasitic diseases'))  +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium")) +
      scale_size_continuous(name = "Proportion") +
      geom_vline(xintercept = log(1), col = "Black", linetype="dashed") +
      labs(x = 'log(Incidence rate ratio)', y = 'ICD-10 chapter')+
      ggtitle("Age-standardised incidence rate ratios for 2012-2021 presented on a log scale") +
      xlim(-2,2)
  }, height = 500, width = 1000)
  
  output$atlas_table <- renderDataTable({
    Table_all
  })
  
  
  # Chapter 1
  output$chap1_plot <- renderPlot({
    ggplot(Chap1, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 1: Infectious and parasitic diseases") +
      scale_y_discrete(limits=rev) +
      xlim(-2,2)
  }, height = 1000)
  
  output$chap1_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 1)
  })
  
  
  # Chapter 2
  output$chap2_plot <- renderPlot({
    ggplot(Chap2, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 2: Neoplasms") +
      scale_y_discrete(limits=rev) +
      xlim(-1.3,1.3)
  }, height = 2300)
  
  output$chap2_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 2) 
  })
  
  
  # Chapter 3
  output$chap3_plot <- renderPlot({
    ggplot(Chap3, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 3: Diseases of the blood and blood-forming organs") +
      scale_y_discrete(limits=rev) +
      xlim(-1.5,1.5)
  }, height = 500)
  
  output$chap3_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 3) 
  })
  
  
  # Chapter 4
  output$chap4_plot <- renderPlot({
    ggplot(Chap4, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 4: Endocrine, nutritional and metabolic diseases") +
      scale_y_discrete(limits=rev) +
      xlim(-1.5,1.5)
  }, height = 1200)
  
  output$chap4_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 4) 
  })
  
  
  # Chapter 5
  output$chap5_plot <- renderPlot({
    ggplot(Chap5, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 5: Mental and behavioural disorders") +
      scale_y_discrete(limits=rev) +
      xlim(-1.5,1.5)
  }, height = 500)
  
  output$chap5_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 5) 
  })
  
  
  # Chapter 6
  output$chap6_plot <- renderPlot({
    ggplot(Chap6, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 6: Diseases of the nervous system") +
      scale_y_discrete(limits=rev) +
      xlim(-1,1)
  }, height = 1300)
  
  output$chap6_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 6) 
  })
  
  
  # Chapter 7
  output$chap7_plot <- renderPlot({
    ggplot(Chap7, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 7: Diseases of the eye and adnexa") +
      scale_y_discrete(limits=rev) +
      xlim(-1,1)
  }, height = 900)
  
  output$chap7_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 7) 
  })
  
  
  # Chapter 8
  output$chap8_plot <- renderPlot({
    ggplot(Chap8, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 8: Diseases of the ear and mastoid process") +
      scale_y_discrete(limits=rev) +
      xlim(-1,1)
  }, height = 400)
  
  output$chap8_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 8) 
  })
  
  
  # Chapter 9
  output$chap9_plot <- renderPlot({
    ggplot(Chap9, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 9: Diseases of the circulatory system") +
      scale_y_discrete(limits=rev) +
      xlim(-1.5,1.5)
  }, height = 1400)
  
  output$chap9_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 9) 
  })
  
  
  # Chapter 10
  output$chap10_plot <- renderPlot({
    ggplot(Chap10, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 10: Diseases of the respiratory system") +
      scale_y_discrete(limits=rev) +
      xlim(-1.5,1.5)
  }, height = 900)
  
  output$chap10_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 10) 
  })
  
  
  # Chapter 11
  output$chap11_plot <- renderPlot({
    ggplot(Chap11, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 11: Diseases of the digestive system") +
      scale_y_discrete(limits=rev) +
      xlim(-1,1)
  }, height = 1200)
  
  output$chap11_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 11) 
  })
  
  
  # Chapter 12
  output$chap12_plot <- renderPlot({
    ggplot(Chap12, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 12: Diseases of the skin and subcutaneous tissue") +
      scale_y_discrete(limits=rev) +
      xlim(-1,1)
  }, height = 1000)
  
  output$chap12_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 12) 
  })
  
  
  # Chapter 13
  output$chap13_plot <- renderPlot({
    ggplot(Chap13, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 13: Diseases of the musculoskeletal system and connective tissue") +
      scale_y_discrete(limits=rev) +
      xlim(-1,1)
  }, height = 1400)
  
  output$chap13_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 13) 
  })
  
  
  # Chapter 14
  output$chap14_plot <- renderPlot({
    ggplot(Chap14, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 14: Diseases of the genitourinary system") +
      scale_y_discrete(limits=rev) +
      xlim(-2,2)
  }, height = 1200)
  
  output$chap14_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 14) 
  })
  
  
  # Chapter 18
  output$chap18_plot <- renderPlot({
    ggplot(Chap18, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 18: Symptoms, signs and abnormal clinical and laboratory findings") +
      scale_y_discrete(limits=rev) +
      xlim(-1,1)
  }, height = 1500)
  
  output$chap18_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 18) 
  })
  
  
  # Chapter 19
  output$chap19_plot <- renderPlot({
    ggplot(Chap19, aes(x = log(RR), y = DIAG, color = type)) +
      geom_point(aes(size = prop))+
      geom_path(aes(group = DIAG), alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      theme(panel.border = element_rect(color="black", fill=NA)) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
      scale_size_continuous(name = "Proportion") +
      theme(axis.text.x = element_text(angle=90, hjust=1, size = 10))+
      geom_vline(xintercept=log(1), linetype="dashed", 
                 color = "black")+
      labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
           title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
           subtitle ="Chapter 19: Injury, poisoning and certain other external causes") +
      scale_y_discrete(limits=rev) +
      xlim(-1,1)
  }, height = 800)
  
  output$chap19_table <- renderDataTable({
    Table_all %>% filter(ICD10_Chapter == 19) 
  })
  
  
  # NCD
  output$NCD_plot <- renderPlot({
    p <- ggplot(Copenhagen_NCD, aes(x = log(RR), y = NCD, color = as.factor(type), size = prop)) +
      geom_point(aes(size = prop))+ 
      geom_line(aes(group = NCD, color = balance), linewidth=1, alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
                 labeller = labeller(order = c("1" = "Malignant neoplasms",
                                               "2" = "Other neoplasms", 
                                               "3" = "Diabetes Mellitus",
                                               "4" = "Endocrine disorders",
                                               "5" = "Neuropsychiatric conditions",
                                               "6" = "Sense organ diseases", 
                                               "7" = "Cardiovascular diseases", 
                                               "8" = "Respiratory diseases",
                                               "9" = "Digestive diseases", 
                                               "10" = "Genitourinary diseases",
                                               "11" = "Skin diseases",
                                               "12" = "Musculoskeletal diseases", 
                                               "13" = "Oral conditions")))+
      theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
            strip.placement = "outside",
            strip.background = element_blank()) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium")) +
      scale_size_continuous(name = "Proportion") +
      scale_alpha_continuous(name = "Proportion") +
      geom_vline(xintercept = log(1), col = "black", linetype="dashed") +
      labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
      scale_y_discrete(limits=rev, label = function(NCD){
        NCD %>% sub("....", "", .)
      })+
      xlim(-1.5,1.5)
    
    # Making a line for each GBD_cause_name to indicate length/size of group
    library(grid)
    q <- ggplotGrob(p)
    lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                    gp=gpar(col="black", lwd=3))
    
    for (k in grep("strip-l",q$layout$name)) {
      q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
    }
    
    grid.draw(q)
    
  }, height = 1300, width = 1000)
  
  output$NCD_table <- renderDataTable({
    Table_NCD 
  })
  
  
  # CD
  output$CD_plot <- renderPlot({
    p <- ggplot(Copenhagen_CD, aes(x = log(RR), y = CD, color = as.factor(type), size = prop)) +
      geom_point(aes(size = prop))+ 
      geom_line(aes(group = CD, color = balance), linewidth=1, alpha = 0.4) +
      theme_minimal() +
      theme_classic() + 
      facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
                 labeller = labeller(order = c("1" = "Tuberculosis",
                                               "2" = "Sexually transmitted diseases excluding HIV/AIDS", 
                                               "3" = "HIV/AIDS",
                                               "4" = "Diarrheal diseases",
                                               "5" = "Childhood-cluster diseases",
                                               "6" = "Meningitis", 
                                               "7" = "Hepatitis A", 
                                               "8" = "Hepatitis B",
                                               "9" = "Hepatitis C", 
                                               "10" = "Malaria",
                                               "11" = "Tropical-cluster diseases",
                                               "12" = "Leprosy", 
                                               "13" = "Dengue",
                                               "14" = "Japanese ecephalitis",
                                               "15" = "Trachoma",
                                               "16" = "Intestinal nomatode infections",
                                               "17" = "Other infectious diseases",
                                               "18" = "Respiratory infections")))+
      theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
            strip.placement = "outside",
            strip.background = element_blank()) +
      scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium")) +
      scale_size_continuous(name = "Proportion") +
      scale_alpha_continuous(name = "Proportion") +
      geom_vline(xintercept = log(1), col = "black", linetype="dashed") +
      labs(x = 'log(Incidence rate ratio)', y = 'Communicable diseases') +
      ggtitle("Age-standardised incidence rate ratios for CDs in 2012-2021 presented on a log scale") +
      scale_y_discrete(limits=rev, label = function(CD){
        CD %>% sub("....", "", .)
      })+
      xlim(-1.8,1.8)
    
    
    
    # Making a line for each GBD_cause_name to indicate length/size of group
    library(grid)
    q <- ggplotGrob(p)
    lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                    gp=gpar(col="black", lwd=3))
    
    for (k in grep("strip-l",q$layout$name)) {
      q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
    }
    
    grid.draw(q)
    
  }, height = 500, width = 1000)
  
  output$CD_table <- renderDataTable({
    Table_CD 
  })
}


# Run the app ----

shinyApp(ui = ui, server = server)







