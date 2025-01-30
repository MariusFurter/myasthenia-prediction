library(shiny)
library(bnlearn)
library(ggplot2)

# Load models
load("./models.RData")


query_models <- function(evidence) {
  lapply(models, function(m) {
    cpquery(
      m,
      event = (mg == "positive"),
      evidence = evidence,
      method = "lw"
    )
  })
}

# returns 100 samples of the probability of MG being positive given the evidence
predict_mg <- function(age = NA,
                       sex = NA,
                       rns.an.fn.max = NA,
                       sfemg = NA,
                       simpson.test = NA,
                       endrophonium = NA,
                       ice.test = NA,
                       anti.achr = NA,
                       besinger.score = NA,
                       diplopia = NA,
                       ptosis = NA) {
  evidence <- list(
    age = factor(age, levels = c("(0,50]", "(50,70]", "(70,120]")),
    sex = factor(sex, levels = c("male", "female")),
    rns.an.fn.max = factor(rns.an.fn.max, levels = c("negative", "positive")),
    sfemg = factor(sfemg, levels = c("negative", "positive")),
    simpson.test = factor(simpson.test, levels = c("negative", "positive")),
    endrophonium = factor(endrophonium, levels = c("negative", "positive")),
    ice.test = factor(ice.test, levels = c("negative", "positive")),
    anti.achr = factor(anti.achr, levels = c("negative", "positive")),
    besinger.score = factor(
      besinger.score,
      levels = c("(-1,1]", "(1,4]", "(4,8]", "(8,24]")
    ),
    diplopia = factor(diplopia, levels = c("negative", "positive")),
    ptosis = factor(ptosis, levels = c("negative", "positive"))
  )
  if (all(is.na(evidence))) {
    NA
  } else {
    evidence <- Filter(function(x) {
      !is.na(x)
    }, evidence)
    unlist(query_models(evidence = evidence))
  }
}


# Define UI for application
ui <- fluidPage(
  
  titlePanel("Ocular Myasthenia Gravis Prediction"),
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(column(
        12,
        selectInput(
          inputId = "anti.achr",
          label = "Acetylcholine Receptor Antibodies",
          choices = c("positive", "negative", NA),
          selected = NA
        )
      ), ),
      fluidRow(
        column(
          6,
          selectInput(
            inputId = "endrophonium",
            label = "Edrophonium Test",
            choices = c("positive", "negative", NA),
            selected = NA
          )
        ),
        column(
          6,
          selectInput(
            inputId = "sfemg",
            label = "Single-Fiber EMG",
            choices = c("positive", "negative", NA),
            selected = NA
          )
        ), 
      ),
      fluidRow(
        column(
          6,
          selectInput(
            inputId = "simpson.test",
            label = "Sustained Upgaze Test",
            choices = c("positive", "negative", NA),
            selected = NA
          )
        ),
      column(
        6,
        selectInput(
          inputId = "rns.an.fn.max",
          label = "Repetitive Nerve Stimulation",
          choices = c("positive", "negative", NA),
          selected = NA
        )
      )),
      fluidRow(
        column(
          6,
          selectInput(
            inputId = "besinger.score",
            label = "Besinger Score",
            choices = c("(-1,1]", "(1,4]", "(4,8]", "(8,24]", NA),
            selected = NA
          )
        ),
        column(
        6,
        selectInput(
          inputId = "ice.test",
          label = "Ice Test",
          choices = c("positive", "negative", NA),
          selected = NA
        )
      )),
      fluidRow(column(
        6,
        selectInput(
          inputId = "sex",
          label = "Sex",
          choices = c("male", "female", NA),
          selected = NA
        )
      ), column(
        6,
        selectInput(
          inputId = "age",
          label = "Age",
          choices = c("(0,50]", "(50,70]", "(70,120]", NA),
          selected = NA
        )
      )),
      fluidRow(column(
        6,
        selectInput(
          inputId = "ptosis",
          label = "Ptosis",
          choices = c("positive", "negative", NA),
          selected = NA
        )
      ), column(
        6,
        selectInput(
          inputId = "diplopia",
          label = "Diplopia",
          choices = c("positive", "negative", NA),
          selected = NA
        )
      )),
      fluidRow(column(
        6,
        actionButton(
          inputId = "reset",
          label = "Reset Inputs",
          class = "btn btn-danger"
        )
      )),
    ),
    mainPanel(tabsetPanel(
      id = "inTabset",
      tabPanel(
        title = "Prediction",
        value = "prediction",
        br(),
        textOutput(outputId = "intro"),
        htmlOutput(outputId = "ci"),
        plotOutput(outputId = "plot"),
        textOutput(outputId = "legend")
      ),
      tabPanel(
        title = "Model Description",
        value = "description",
        br(),
        markdown("This app predicts the probability of *ocular myasthenia gravis* using a [discrete Bayesian network](https://en.wikipedia.org/wiki/Bayesian_network) whose conditional probabilities were fit to retrospective study data from 89 adults. You may enter any partial combination of variables in the left panel to make a prediction.

                  The code used for fitting the model and building this app is available on [GitHub](https://github.com/MariusFurter/myasthenia-prediction). For additional details see

                  **'Multivariable Prediction Model for Suspected Ocular Myasthenia Gravis: Development and Validation'** by *Armin Handzic, MD; Marius P. Furter; Brigitte C. Messmer; Magdalena A. Wirth, MD; Yulia Valko, MD; Fabienne C. Fierz, MD; Edward A. Margolin, MD; Konrad P. Weber, MD,* in ***Journal of Neurology-Ophthalmology.***

                  The directed acyclic graph depicted below summarizes the variables and conditional independencies assumed by the model. The graph was chosen according to medical plausibility and statistical independence tests. It is not meant to imply causal connections between the variables.

                  *Abbreviations:* Ocular myasthenia gravis (OMG), acetylcholine receptor antibodies (anti-AChR), repetitive nerve stimulation (RNS), single-fiber electromyography (SFEMG). The sustained upgaze test is also known as Simpson test.
                  
                 "),
        imageOutput("DAG"),
      ),
    ), )
  )
)


# Define server logic
server <- function(input, output, session) {
  # reset: find way to also reset model and prediction data
  observeEvent(input$reset, {
    labels <- c(
      "sex",
      "ptosis",
      "diplopia",
      "simpson.test",
      "ice.test",
      "rns.an.fn.max",
      "sfemg",
      "anti.achr",
      "endrophonium",
      "besinger.score",
      "age"
    )
    for (item in labels) {
      updateSelectInput(session, item, selected = "NA")
    }
  })

  predictions <- reactive(
    predict_mg(
      age = input$age,
      sex = input$sex,
      rns.an.fn.max = input$rns.an.fn.max,
      sfemg = input$sfemg,
      simpson.test = input$simpson.test,
      endrophonium = input$endrophonium,
      ice.test = input$ice.test,
      anti.achr = input$anti.achr,
      besinger.score = input$besinger.score,
      diplopia = input$diplopia,
      ptosis = input$ptosis
    )
  )

  output$ci <- renderUI(
    if (!all(is.na(predictions()))) {
    m <- median(predictions())
    q <- quantile(predictions(), c(0.025, 0.975), na.rm = T)
    
    line1 <- paste0("Median probability of ocular myasthenia gravis:  ", 
           "<b>",
           100*signif(m, 2), 
           "%,</b>")
    
    line2 <- paste0(
           "with 95% credible interval around median:  ",
           "<b>",
           "[", 100*signif(q[1], 2), "%, ",
           100*signif(q[2], 2), "%].</b>")
    
    HTML( paste(line1, line2, sep="<br/>") )
  })

  output$plot <- renderPlot(
    {
      if (!all(is.na(predictions()))) {
        pred <- as.vector(predictions())
        m <- signif(median(predictions()), 2)

        q_025 <- quantile(pred, 0.025, na.rm = T)
        q_25 <- quantile(pred, 0.25, na.rm = T)
        q_75 <- quantile(pred, 0.75, na.rm = T)
        q_975 <- quantile(pred, 0.975, na.rm = T)
        qdf <- data.frame(m = m, q_025 = q_025, q_25 = q_25, q_75 = q_75, q_975 = q_975)

        ggplot(data = data.frame(pred)) +
          geom_density(aes(x = pred)) +
          geom_vline(aes(xintercept = m), linetype = "dashed", data = qdf) +
          geom_segment(aes(x = q_025, xend = q_975, y = 0, yend = 0), linewidth = 1, color = "lightblue3", data = qdf) +
          geom_segment(aes(x = q_25, xend = q_75, y = 0, yend = 0), linewidth = 2.5, color = "dodgerblue4", data = qdf) +
          xlim(0, 1) +
          xlab("Probability of OMG") +
          ylab("Probability Density") +
          theme_light() +
          theme(
            panel.grid = element_blank(), axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
      }
    },
    res = 120
  )

  output$intro <- output$legend <- renderText(
    if (all(is.na(predictions()))) {
      "Please enter at least one predictor value in the left panel."
    })
  
  output$legend <- renderText({
    if (!all(is.na(predictions()))) {
      "Posterior distribution over the probability of ocular myasthenia gravis (OMG) with median (dashed line), 50% credible interval (dark blue) and 95% credible interval (light blue). 
      A higher value of the density indicates that the corresponding probability of OMG is more likely."
    }
  })

  output$DAG <- renderImage(
    {
      width <- session$clientData$output_DAG_width
      height <- session$clientData$output_DAG_height
      list(
        src = "./OMG_DAG.png",
        contentType = "image/png",
        width = width,
        alt = "DAG structure of the model"
      )
    },
    deleteFile = FALSE
  )
}

# Run the application
shinyApp(ui = ui, server = server)
