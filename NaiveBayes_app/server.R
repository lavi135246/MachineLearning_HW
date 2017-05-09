library(shiny)

function(input, output) {
	input_csv <- reactive({

		inFile <- input$file1
		if (is.null(inFile))
			return(NULL)
    
		read.csv(inFile$datapath, header=input$header, sep=input$sep, 
				 quote=input$quote)
	})
  
	output$contents <- renderTable({
		head(input_csv(), 10)
	})
	
	model_List <- reactive({
	  nin = ncol(input_csv())
	  
	  target_n = input_csv()[,nin]
	  infile_n = input_csv()[, -nin]
	  colname = names(infile_n)
	  
	  #normal distribution
	  
	  model_list = lapply(unique(target_n), function(x){
	    sd_ = sapply(infile_n[which(target_n==x),], function(x){sd(x, na.rm=TRUE)})
	    mean_ = sapply(infile_n[which(target_n==x),], function(x){mean(x, na.rm=TRUE)})
	    list(
	      "class" = x,
	      "mean" = mean_,
	      "sd" = sd_
	    )
	  })
	})
	
	query <- eventReactive(input$confirm,{
	  input$myQuery
	  unlist(strsplit(input$myQuery, split = ","))
	})
	
	Naive_Bayes_Score <- reactive({
	  nin = ncol(input_csv())
	  
	  target_n = input_csv()[,nin]
	  infile_n = input_csv()[, -nin]
	  colname = names(infile_n)
	  
	  naive_score = sapply(model_List(), function(class_n){
	    prod(sapply(1:length(infile_n), function(feature){
	          pnorm(as.numeric(query()[feature]), mean=class_n[[2]][feature], sd=class_n[[3]][feature])
	          }),
	      na.rm=TRUE
	    )
	  })
	})
	
	output$NaiveBayesAns <- renderPrint({
	  nin = ncol(input_csv())
	  target_n = input_csv()[,nin]
	  unique(target_n)[which.max(Naive_Bayes_Score())]
	})
	
  
}