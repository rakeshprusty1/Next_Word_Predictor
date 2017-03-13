# This is the user-interface definition of a Shiny web application.

library(shiny)

shinyUI(fluidPage(

    titlePanel("Next Word Predictor"),
    
    sidebarPanel(
        textInput("entry",
                  h4("Input word/sentence"),
                  "",placeholder = "Enter Text:"),
        numericInput("maxResults",
                     h4("Numbers of words to predict"), 
                     value = 3),
        #submitButton("SUBMIT"),
        br(),
        "This app is created by ", 
        a("Rakesh Prusty", href = "mailto:rakeshprusty@gmail.com")
    ),
    
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Result",
                             h3("Instruction"),
                             p("This app predicts the next word of the sentence/phrase that you have entered in input text bar in the side panel.By default, 3 words are predicted with their ranking. You can change that number in 'Number of words to predict' to increase/decrease the number of word prediction. Incase of no input, the most frequestly used words will be displayed"),
                             p("Navigate through the other tabs for further reading and contact me in case of any questions or suggestions by clicking following link"),
                             p(a("Contact Me!", href = "mailto:rakeshprusty@gmail.com")),
                             h3("Your input:"),
                             verbatimTextOutput("entry",placeholder = TRUE),
                             h3("Next Word Prediction:"),
                             span(h3(tableOutput('predict')),style = "color:green")),
                             
                    tabPanel("Methods",
                             br(),
                             h5("Below are the steps and methods followed to create this word predictor:"),
                             h4("Data Read and Summary"),
                             p("Three files were provided for this project: News,Twitter and Blogs. The given files were read and here is the summary:"),
                             img(src = "filesummary1.png"),
                             br(),
                             h4("Data Cleaning and Pre-Processing"),
                             p("Since the size of the files is huge, a sample size of 20,000 lines were collected. The following operations were done on the text."),
                             p("1. Removal of non alpha/numeric characters: numbers, punctuations and sepcial characters."),
                             p("2. Conversion to lower case"),
                             p("3. Removal of profanity"),
                             p("3. Stripping white space"),
                             h4("Data Model"),
                             p("5-Gram data model was used for this prediction model. 'ngram' package was used to create the N-grams. Below are the frequencies of the top N-grams."),
                             img(src = "Rplot.png"),
                             p("Data pruning was performed by removing low frequency (freq < 1) N-grams to improve prediction accuracy and speed."),
                             h4('Prediction Model'),
                             p("Stupid Backoff algorithm was implemented on the 5-gram model to determine the score of the next probable word."),
                             img(src = "stupid_backoff1.png"),
                             p("Lambda value of 0.4 has been used as discount in the backoff model. Please checkout the 'Stanford NLP Videos' in Reference tab for more information.")),
                             
                    tabPanel("Accuracy & Limitation",
                             br(),
                             p("In order to evaluate the precision,performance  and the memory usage of the prediction model, the benchmarking script provided by Coursera discussion forum (check reference tab) was run with 100 test samples. Below is the result."),
                             img(src = "Accuracy1.png"),
                             br( ),
                             p("At the time of the test run, the result was better than average scores posted in the forum."),
                             p("A subset of the original files are used for this prediction model as it's a time and resource consuming process for N-gram creation on local PC."),
                             p("A small sample of original data does compromise with accuracy. But at the same time it minimizes memory and computation time. Also in order to host the application on web, the small data size helped.")),
                   
                    tabPanel("Future Enhancements",
                             br(),
                             p("1. The immediate enhancement would be to fix a bug - If the sentence,'have you ever seen' is entered as input, the prediction algorithm is failing now. The reason in this case is some inconsistencies with the N-gram model."),
                             p("2. The next enhancement would be attempted to optimize the algorithm to reduce computaional time."),
                             p("3. Different scoring models, like Kneser-Ney Interpolation will be implemented and compared with existing scoring model to improve precision."),
                             p("4. Enhancement will be made to accept user's input and update the existing N-grams. By this way, the model will improve as it will be used further."),
                             p("5. Current implementation is on shiny server, which has memory constraints. In future, larger N-grams will be stored in high capacity server. ")),
                    
                    tabPanel("Reference",
                             br(),
                             p(a("ngram: Fast n-Gram 'Tokenization'",href = "https://cran.r-project.org/web/packages/ngram/index.html")),
                             p(a("Basic Text Mining in R",href = "https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html")),
                             p(a("Coursera Data Science Specialization",href = "https://www.coursera.org/learn/data-science-project/home/welcome")),
                             p(a("Stanford NLP Videos",href = "https://www.youtube.com/watch?v=s3kKlUBa3b0")),
                             p(a("Prediction Model Benchmarking",href = "https://github.com/hfoffani/dsci-benchmark"))
                    )
                    
        ))
))
