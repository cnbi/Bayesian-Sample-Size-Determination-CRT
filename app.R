
# Libraries
library(shiny)
library(dplyr) # Manage data frames
library(ggplot2) # Plots
library(ggpubr) # Plots


# Load data sets
data_equality <- readRDS("data/final_dataset_Eq.RDS")
data_inequalities <- readRDS("data/final_dataset_Ineq.RDS")
data_plots_findN1_eq <- readRDS("data/BFs_findN1_null.RDS")
data_plots_findN2_eq <- readRDS("data/BFs_findN2_null.RDS")
data_plots_findN1_ineq <- readRDS("data/BFs_findN1_inform.RDS")
data_plots_findN2_ineq <- readRDS("data/BFs_findN2_inform.RDS")

# Name of columns in table
names_col <- c(b = "b", "Cluster size" = "n1.final", "P(BF.01 > thresh)" = "eta.BF01",
               "P(BF.10 > thresh)" = "eta.BF10", "Number of clusters" = "n2.final",
               "P(BF.12 > thresh)" = "eta.BF12", "P(BF.21 > thresh)" = "eta.BF21")

# Palette for plots
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#661100")

# Define UI ###################################################################
ui <- fluidPage(
    
    theme = shinythemes::shinytheme("paper"),
    
    
    # Application title
    titlePanel("Sample Size Determination for Cluster Randomised Trials with Bayes Factor",
               windowTitle = "Bayes Sample Size Determination: CRT"),
    
    tabsetPanel(
        tabPanel("Results", 
                 sidebarLayout(
                     sidebarPanel(
                         # Widgets for inputs
                         radioButtons("find",
                                      "The sample size to be found given the other one is fixed:",
                                      choices = c("Number of clusters" = "n2",
                                                  "Cluster sizes" = "n1")),
                         radioButtons("hypotheses",
                                      "Type of hypotheses to compare:",
                                      choices = c("Null vs. Informative" = "Equality",
                                                  "Informative vs. Informative" = "Informative")),
                         conditionalPanel(
                             condition = "input.find == 'n1'",
                             selectInput("n2",
                                         "Number of clusters:",
                                         choices = c(
                                             "30" = 30,
                                             "60" = 60,
                                             "90" = 90
                                         ))),
                         conditionalPanel(
                             condition = "input.find == 'n2'",
                             selectInput("n1",
                                         "Cluster sizes:",
                                         choices = c("5" = 5,
                                                     "10" = 10,
                                                     "40" = 40))),
                         selectInput("rho",
                                     "Intraclass correlation:",
                                     choices = c("0.025" = 0.025,
                                                 "0.05" = 0.05,
                                                 "0.1" = 0.1)),
                         selectInput("eff_size",
                                     "Effect size:",
                                     choices = c("0.2" = 0.2,
                                                 "0.5" = 0.5,
                                                 "0.8" = 0.8)),
                         selectInput("bf_thresh",
                                     "Bayes factor threshold:",
                                     choices = c("1" = 1,
                                                 "3" = 3,
                                                 "5" = 5)),
                         conditionalPanel(
                             condition = "input.hypotheses == 'Equality'",
                             checkboxGroupInput("b",
                                                "Fraction b in plots:",
                                                c("1" = 1,
                                                  "2" = 2,
                                                  "3" = 3), 
                                                selected = 1
                             )
                         )),
                     mainPanel(
                         # Show a plot 
                         plotOutput("plots"),
                         # Show results in table
                         h3("Final Sample Size"),
                         tableOutput("dataTableResults"),
                         # Example of interpretation
                         uiOutput("example")
                     )
                 )),
        tabPanel("Information",
                 p(HTML("This Shiny app display the result from the research 
                                <i><a href='https://www.overleaf.com/project/65f16c0441556b4e4487bc30' 
                        target='_blank'>Method for Sample Size Determination for Cluster Randomized Trials Using the Bayes Factor</a></i> by Barragan et al. (2024).  
                                The source code is available at <i><a 
                                href='https://github.com/cnbi/Bayesian-Sample-Size-Determination-CRT' 
                        target='_blank'>Bayesian Sample Size Determination-CRT</a></i>")),
                 p("To reference the application, please use the following:"),
                 p(HTML("@misc{barragan_sample_2024,
	title = {Sample size determination for cluster randomised trials with the Bayes factor}, <br>
	shorttitle = {Bayes sample size determination: CRT}, <br>
	url = {https://utrecht-university.shinyapps.io/BayesSamplSizeDet-CRT/}, <br>
	publisher = {Utrecht University}, <br>
	author = {Barragan, Camila and Moerbeek, Mirjam and Hoijtink, Herbert}, <br>
	month = apr, <br>
	year = {2024}, <br>
}
                        ")),
p("Or in APA style:"),
p(HTML("Barragan, C., Moerbeek, M., & Hoijtink, H. (2024). 
                          <i>Sample size determination for cluster randomised trials 
                          with the Bayes factor </i> [Shiny app]. Utrecht University. 
                          https://utrecht-university.shinyapps.io/BayesSamplSizeDet-CRT/")
),
p("For any bug, error, or feedback you may contact Camila Barragán via email at cn.barragan.ibanez@gmail.com or GitHub")
        )
    )
)

# Server #######################################################
server <- function(input, output) {
    
    # Chosen data set
    dataset <- reactive({
        switch(input$hypotheses,
               Equality = data_equality,
               Informative = data_inequalities
        )
    })
    
    # Filter dataset based on ICC, effect size and threshold
    filtered_dataset <- reactive({
        
        filtered <- dataset() %>% filter(rho == as.numeric(input$rho),
                                         eff_size == as.numeric(input$eff_size),
                                         BF_threshold == as.numeric(input$bf_thresh))
        filtered
        
    })
    # Filter n2 or n1
    second_filter <- reactive({
        if (input$find == "n1") {
            filtered <- filtered_dataset() %>% filter(fixed == "n2",
                                                      sample_size == input$n2)
        } else {
            filtered <- filtered_dataset() %>% filter(fixed == "n1",
                                                      sample_size == input$n1)
        }
        filtered
    })
    
    # Give format to table and render
    output$dataTableResults <- renderTable({
        # Select columns and change the column names
        if (input$find == "n1" && input$hypotheses == "Equality") {
            second_filter() %>% select(b, n1.final, eta.BF01, eta.BF10) %>% 
                rename(any_of(names_col))
        } else if (input$find == "n2" && input$hypotheses == "Equality") {
            second_filter() %>% select(b, n2.final, eta.BF01, eta.BF10) %>% 
                rename(any_of(names_col))
        } else if (input$find == "n1" && input$hypotheses == "Informative") {
            second_filter() %>% select(n1.final, eta.BF12) %>% 
                mutate(eta.BF21 = 1 - eta.BF12) %>% 
                rename(any_of(names_col))
        } else if (input$find == "n2" && input$hypotheses == "Informative") {
            second_filter() %>% select(n2.final, eta.BF12) %>%
                mutate(eta.BF21 = 1 - eta.BF12) %>% 
                rename(any_of(names_col))
        }
        
    },
    # Make pretty table
    striped = TRUE, spacing = "l", digits = 3, width = "90%"
    )
    
    
    
    # Make plot
    output$plots <- renderPlot({
        
        # Filter data
        if (input$find == "n1" && input$hypotheses == "Equality") {
            data_plot <- data_plots_findN1_eq %>% 
                filter(rho == input$rho &
                           eff_size == input$eff_size &
                           BF_threshold == input$bf_thresh &
                           N2 == input$n2)
        } else if (input$find == "n2" && input$hypotheses == "Equality") {
            data_plot <- data_plots_findN2_eq %>%
                filter(rho == input$rho &
                           eff_size == input$eff_size &
                           BF_threshold == input$bf_thresh &
                           N1 == input$n1)
        } else if (input$find == "n1" && input$hypotheses == "Informative") {
            data_plot <- data_plots_findN1_ineq %>% 
                filter(rho == input$rho &
                           eff_size == input$eff_size &
                           BF_threshold == input$bf_thresh &
                           N2 == input$n2)
        } else if (input$find == "n2" && input$hypotheses == "Informative") {
            data_plot <- data_plots_findN2_ineq %>% 
                filter(rho == input$rho &
                           eff_size == input$eff_size &
                           BF_threshold == input$bf_thresh &
                           N1 == input$n1)
        }
        
        
        
        # Reactive
        if (input$hypotheses == "Equality") {
            reactive_data_plot <- reactive({
                data_plot_filt <- data_plot %>% filter(b %in% input$b)
                data_plot_filt
            })
        }
        
        # Draw plots
        # When working with equality constrains
        if (input$hypotheses == "Equality") {
            plot_left <- ggplot(reactive_data_plot(), aes(x = log(BF_01), color = as.factor(b), fill = as.factor(b))) + 
                geom_histogram(binwidth = 1, aes(y = after_stat(density)), alpha = 0.5, position = "identity") +
                geom_density(alpha = .2) +
                geom_vline(aes(xintercept = log(BF_threshold), colour = cbbPalette[9]), linetype = "dashed") +
                scale_fill_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8]), name = "Fraction b") +
                scale_color_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8]), name = "Fraction b") +
                ylab("Density") + xlab(bquote("log Bayes factor"["01"])) +
                theme(legend.position = "bottom", axis.title = element_text(size = 16),
                      axis.text = element_text(size = 15),
                      plot.caption = element_text(hjust = 0, size = 12),
                      plot.title = element_text(size = 14, face = "bold")) + 
                labs(title = "Null hypothesis is true")
            
            plot_right <- ggplot(reactive_data_plot(), aes(x = log(BF_10), color = as.factor(b), fill = as.factor(b))) + 
                geom_histogram(binwidth = 1, aes(y = after_stat(density)), alpha = 0.5, position = "identity") +
                geom_density(alpha = .2) +
                geom_vline(aes(xintercept = log(BF_threshold), colour = cbbPalette[9]), linetype = "dashed") +
                scale_fill_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8]), name = "Fraction b") +
                scale_color_manual(values = c("1" = cbbPalette[2], "2" = cbbPalette[3], "3" = cbbPalette[8]), name = "Fraction b") +
                ylab("Density") + xlab(bquote("log Bayes factor"["10"])) + labs(title = "Alternative hypothesis is true") +
                theme(legend.position = "bottom", axis.title = element_text(size = 16),
                      axis.text = element_text(size = 15),
                      plot.title = element_text(size = 14, face = "bold"))
            
        } # When working with inequality constraints
        else if (input$hypotheses == "Informative") {
            plot_BF <- ggplot(data_plot, aes(x = log(BF_12))) + 
                geom_histogram(binwidth = 1, colour = "#69b3a2", fill = "#69b3a2",  aes(y = after_stat(density)), alpha = 0.8, position = "identity") +
                geom_density(alpha = .2, colour = "#69b3a2") + geom_vline(aes(xintercept = log(BF_threshold), colour = cbbPalette[9]), linetype = "dashed") +
                ylab("Density") + xlab(bquote("log Bayes factor"["12"])) + 
                labs(caption = "Eta (\u03B7) = 0.8") +
                theme(legend.position = "none", axis.title = element_text(size = 16),
                      axis.text = element_text(size = 15),
                      plot.caption = element_text(hjust = 0, size = 12))
        }
        # Arrange plots
        if (input$hypotheses == "Equality") {
            grid_plots <- ggarrange(plot_left, plot_right, ncol = 2, common.legend = TRUE,
                                    legend = "bottom")
            annotate_figure(grid_plots, bottom = text_grob("Eta (\u03B7) = 0.8"))
        } else if (input$hypotheses == "Informative") {
            plot_BF
        }
    })
    # Example of interpretation
    output$example <- renderUI({
        
        #Render message according to the input
        if (input$find == "n1" && input$hypotheses == "Equality") {
            paragraphs <- list(
                paste0("When fraction b is set to ",
                       second_filter()$b[1], ", cluster randomised trials with ",
                       second_filter()$n1.final[1], " individuals per cluster and ", second_filter()$sample_size[1], 
                       " clusters per treatment condition result in ",second_filter()$eta.BF01[1] * 100, "% of 
                  Bayes factors larger than ", input$bf_thresh, " when the null hypothesis is true, whereas when the alternative 
                  hypothesis is true, ", second_filter()$eta.BF10[1] * 100, "% of 
                  Bayes factors are larger than ", input$bf_thresh, "."
                ),
                paste0("When fraction b is set to ",
                       second_filter()$b[2], ", cluster randomised trials with ", 
                       second_filter()$n1.final[2], " individuals per cluster and", second_filter()$sample_size[2], 
                       " clusters per treatment condition result in ", second_filter()$eta.BF01[2] * 100, "% of 
                       Bayes factors are larger than ", input$bf_thresh, " when the null hypothesis is true, whereas when the alternative 
                  hypothesis is true, ", second_filter()$eta.BF10[2] * 100, "% of Bayes factors are 
                  larger than ", input$bf_thresh, "."
                ),
                paste0("When fraction b is set to ",
                       second_filter()$b[3], ", cluster randomised trials with ", 
                       second_filter()$n1.final[3], " individuals per cluster and ", second_filter()$sample_size[3], 
                       "clusters per treatment condition result in ", second_filter()$eta.BF01[3] * 100, "% of 
                       Bayes factors larger than ", input$bf_thresh, " when the null hypothesis is true, whereas when the alternative 
                  hypothesis is true, ", second_filter()$eta.BF10[3] * 100, "% of Bayes factors are 
                  larger than ", input$bf_thresh, "."
                )
            )
            #Render message according to the input
        } else if (input$find == "n2" && input$hypotheses == "Equality") {
            paragraphs <- list(
                paste0("When fraction b is set to ",
                       second_filter()$b[1], ", cluster randomised trials with ",
                       second_filter()$n2.final[1], " clusters per treatment condition and ", 
                       second_filter()$sample_size[1], " individuals per cluster yield ", 
                       second_filter()$eta.BF01[1] * 100, "% of Bayes factors
                  larger than ", input$bf_thresh, " when the null hypothesis is true; whereas when the alternative 
                  hypothesis is true, ", second_filter()$eta.BF10[1] * 100, "% of Bayes factors are 
                  larger than ", input$bf_thresh, "."
                ),
                paste0("When fraction b is set to ",
                       second_filter()$b[2], ", cluster randomised trials with ", 
                       second_filter()$n2.final[2], " clusters per treatment 
                   condition and ", second_filter()$sample_size[2], " individuals per cluster yield ",
                   second_filter()$eta.BF01[2] * 100, "% of Bayes factors 
                   larger than ", input$bf_thresh, " when the null hypothesis is true, whereas when the alternative 
                  hypothesis is true, ", second_filter()$eta.BF10[2] * 100, "% of Bayes factors are 
                  larger than ", input$bf_thresh, "."
                ),
                paste0("When fraction b is set to ",
                       second_filter()$b[3], ", cluster randomised trials with ", 
                       second_filter()$n2.final[3], " clusters per treatment 
                   condition and ", second_filter()$sample_size[3], " individuals per cluster result in ",
                   second_filter()$eta.BF01[3] * 100, "% of Bayes factors
                   larger than ", input$bf_thresh, " when the null hypothesis is true , whereas when the alternative 
                  hypothesis is true, ", second_filter()$eta.BF10[3] * 100, "% of Bayes factors are 
                  larger than ", input$bf_thresh, ".")
            )
            #Render message according to the input
        } else if (input$find == "n1" && input$hypotheses == "Informative") {
            paragraphs <- list(
                paste0("When hypothesis 1 is true, cluster randomised trials with ",
                       second_filter()$n1.final, " individuals per cluster and ", 
                       second_filter()$sample_size, " clusters per 
                   treatment condition yield ", second_filter()$eta.BF12 * 100, "% 
                   of Bayes factors larger than ", input$bf_thresh, "."
                )) 
            #Render message according to the input
        } else if (input$find == "n2" && input$hypotheses == "Informative") {
            paragraphs <- list(
                paste0("When the hypothesis 1 is true, cluster randomised trials with ",
                       second_filter()$n2.final, " clusters per treatment condition and ", 
                       second_filter()$sample_size, " individuals per cluster yield ",
                       second_filter()$eta.BF12 * 100, "% of Bayes factors larger than ",
                       input$bf_thresh, "."
                ))
        }
        
        # Convert to HTML
        paragraphs_html <- lapply(paragraphs, function(paragraph) {
            paste0("<p>&#8226; ", paragraph, "</p>")})
        
        div(
            style = "margin-left: 20px;",
            lapply(paragraphs_html, HTML)
        )
        
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
