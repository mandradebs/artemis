#Bot factory/class
#Install and load required packages
#---------------------------------------------
list.of.packages <- c("R6","quantmod","ggplot2","svDialogs","caret","doParallel","rmarkdown",
                      "xts","TSdist","h2o","e1071","randomForest","kernlab")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages>0))
  install.packages(new.packages,
                   repos = "http://cran.rstudio.com/")
temp <- tryCatch(lapply(
  list.of.packages,
  FUN = function(X) {
    do.call("require", list(X))
  }
),error=function(e){FALSE})
rm(temp,list.of.packages,new.packages)
#---------------------------------------------
twoClassSum <- function (data, lev = NULL, model = NULL){ #custom function to get accuracy, specificity and sensitivity for each resample in training with caret
  lvls <- levels(data$obs)
  if (length(lvls) > 2) 
    stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
  if (!all(levels(data[, "pred"]) == lvls)) 
    stop("levels of observed and predicted data do not match")
  rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0,1), data[, lvls[1]])
  out <- c(rocAUC,mean(data[, "pred"] == data[, "obs"]),sensitivity(data[, "pred"], data[, "obs"],"Positive"), specificity(data[, "pred"], data[, "obs"], "Negative"))
  names(out) <- c("ROC", "Accuracy" ,"Sensitivity", "Specificity")
  out
}

Bot <- R6Class(classname = "Bot",
               public = list(
                ##########FEATURES##########
                name = NULL, #name of the bot
                birthday = NULL,
                studyMaterial = list(), #It must be a list with quote names and historical quote prices
                #It is a matrix with a pattern names column, a class (TRUE or FALSE) column and values (y values in a scatter plot) the TRUE or FALSE pattern 
                knowledge = list(),
                models = list(),#trained models for prediction
                exploration = list(),#results are saved here when explore method is called
                
                ##########METHODS##########
                #initialize a new bot
                initialize = function(name = "bot"){
                  self$name = name
                  self$birthday = Sys.Date()
                },
                
                getName = function(){return(self$name)},#return name
                
                getBirthday = function(){return(self$birthday)},#return birthday
                
                renderArticle = function(file="sample.Rmd"){render(file)},#render rmarkdown file
                
                #generate the study material a char vector cointaing ticker names must be provided
                genStudyMaterial = function(tickers = NULL,...){
                  if(is.null(tickers))
                    message("You must provide a char vector tickers containing tickers names") else {
                      getSymbols(Symbols = as.character(tickers),env = environment())
                      
                      self$studyMaterial$prices <- lapply(tickers,FUN = function(ticker){
                        tryCatch({
                          dta <- eval(parse(text = ticker))
                          colnames(dta) <- lapply(colnames(dta),
                                                  FUN = function(n) strsplit(x = n,split = ".",fixed = TRUE)[[1]][2])
                          dta
                        },error = function(e){NULL})
                        
                      })
                      if(sum(is.null(self$studyMaterial$prices)))
                        tickers <- tickers[which(!is.null(self$studyMaterial$prices))]
                      self$studyMaterial$quotes <- tickers
                      
                    }
                },
                
                #getData
                getData = function(ticker = NULL){
                  if(is.null(ticker)){
                    message("Ticker parameter must be set")
                  } else{
                    if(ticker %in% self$studyMaterial$quotes)
                      self$studyMaterial$prices[[which(ticker == self$studyMaterial$quotes)[1]]] else
                        message(sprintf("Ticker %s was not found in the study material",ticker))
                  }
                },
                
                #learn
                learn = function(pattern=NULL,name = "CH",minN=50,maxN=-1,maxSharing=.40,df=100,distanceMethod="euclidean",...){
                  message("Please wait... I am comparing the pattern you send me with my study material.")
                  #send pattern to a 1 by 1 box with 100 points
                  patternX <- (0:(nrow(pattern)-1))/(nrow(pattern)-1)
                  patternY <- (pattern[,1] - min(pattern[,1]))/(max(pattern[,1]) - min(pattern[,1]))
                  sp <- smooth.spline(x = patternX,y = patternY,df = min(df,length(unique(patternX))))
                  sp <- predict(sp,x=seq(0.01,1,0.01))
                  sp$y <- (sp$y - min(sp$y))/(max(sp$y) - min(sp$y))
                  
                  cl <- makeCluster(detectCores()-1)#create a cluster
                  studyMaterial <- self$studyMaterial
                  clusterExport(cl,c("sp","minN","maxN","maxSharing","studyMaterial","distanceMethod","df"),envir = environment())#send variables to each node
                  p <- list(...); if(length(p)) clusterExport(cl,"...",envir = environment())#send additional arguments if any
                  results <- parLapply(cl,X = 1:length(self$studyMaterial$quotes),fun = function(k){
                    library(xts)
                    library(TSdist)
                    serie <- studyMaterial$prices[[k]][,"Close"]
                    
                    if(maxN == -1) maxN <- nrow(serie)#list(minN,maxSharing,maxN)
                    candidates <- list()
                    j <- 1
                    repeat{
                      start <- 1
                      end <- minN
                      repeat{
                        candidates[[j]] <- data.frame(start = start,end = end)
                        j <- j + 1
                        start <- start + floor(maxSharing*minN)
                        end <- end + floor(maxSharing*minN)
                        if(end > nrow(serie)) break
                      }
                      if(maxN <= minN) break
                      minN <- min(minN + floor(maxSharing*minN), maxN)
                      if(nrow(serie) < minN) break
                    }
                    candidates <- Reduce(f = function(x,y){rbind(x,y)},candidates)
                    
                    #calculate distances and 1 by 1 boxes for each candidate
                    procCandidates <- lapply(1:nrow(candidates), FUN = function(j){
                      tryCatch({
                        candidate <- serie[candidates$start[j]:candidates$end[j],1]
                        candidateX <- (0:(nrow(candidate)-1))/(nrow(candidate)-1)
                        candidateY <- (candidate[,1] - min(candidate[,1]))/(max(candidate[,1]) - min(candidate[,1]))
                        spc <- smooth.spline(x = candidateX,y = candidateY,df = min(df,length(unique(candidateX))))
                        spc <- predict(spc,x=seq(0.01,1,0.01)); spc$y <- (spc$y - min(spc$y))/(max(spc$y) - min(spc$y))
                        list(dta=spc,distance = TSDistances(x = as.vector(sp$y), y = as.vector(spc$y), distance = distanceMethod,...))
                      },error = function(e) {list(dta=NULL,distance=NA)})
                    })
                    distances <- sapply(procCandidates, FUN = function(k) k$distance)
                    dta <- lapply(procCandidates, FUN = function(k) k$dta)
                    list(candidates=candidates,order=order(distances,decreasing = F), dta=dta)
                  })
                  stopCluster(cl)
                  #Start asking for positive/negative patterns to add to knowledge
                  knowledge <- list()
                  pn <- 0; nn <- 0;
                  for(j in 1:length(self$studyMaterial$quotes)){
                    ans <- "not sure"
                    k <- 1#to iterave over candidates
                    while(ans == "not sure"){
                      pos <- results[[j]]$candidates[results[[j]]$order[k],]
                      dta <- tryCatch({self$studyMaterial$prices[[j]][pos$start:pos$end,]},error = function(e) NULL)
                      if(!is.null(dta)){
                        x11()
                        print(self$plot(dta = dta, df = 10))
                        ans <- dlg_list(c("yes","no","not sure"),
                                        title=sprintf("(%s/%s: %s+/%s- )Please tell me if the current plot contains a true %s pattern? To stop teaching me select cancel...",
                                                                                                             length(knowledge),length(self$studyMaterial$quotes),pn,nn,name))$res
                        dev.off()
                        if(!length(ans)) ans <- "cancel"
                        if(ans == "yes"){
                          knowledge[[j]] <- data.frame(name=name,class=1,t(as.vector(results[[j]]$dta[[ results[[j]]$order[k] ]]$y)), stringsAsFactors = F)
                          pn <- pn + 1
                        }
                        if(ans == "no"){
                          knowledge[[j]] <- data.frame(name=name,class=0,t(as.vector(results[[j]]$dta[[ results[[j]]$order[k] ]]$y)), stringsAsFactors = F)
                          nn <- nn + 1;
                        }
                      }
                      k <- k + 1
                      if(k > length(results[[j]]$order) & ans != "cancel") ans <- "next j"
                    }
                    if(ans == "cancel") break
                  }
                  self$knowledge <- c(self$knowledge,knowledge)
                },
                
                #train
                train = function(patternName=NULL,method = "knn",trainProp = 0.60,nsocks = 1,package="caret",...){
                  if(is.null(patternName)) message("You must tell me a pattern name to begin my training") else {
                    #remove null knowledge if any
                    if(length(self$knowledge) == 0) {message("You should call my learn function before training me."); return(NULL)}
                    if(sum(sapply(self$knowledge,is.null))) self$knowledge <- self$knowledge[-which(sapply(self$knowledge,is.null))]
                    #subset only knowledge for the patternName
                    dta <- lapply(self$knowledge, FUN = function(k){
                      if(k$name == patternName) k else NULL
                    })
                    #check if there is at least some knowledge about the patternName
                    if(sum(sapply(dta, is.null)) == length(self$knowledge)){
                      message(sprintf("There are no records about the pattern %s in my knowledge. You should call my learn function before training me.",patternName))
                    } else {
                      message("Please wait, I am training. It could take a while...")
                      if(sum(sapply(dta, FUN = function(k) is.null(k))))#remove NULL cases if any
                        dta <- dta[-which(sapply(dta, is.null))]
                      dta <- Reduce(f = function(x,y) rbind(x,y),dta)
                      dta <- dta[,-1]; dta$class <- ifelse(dta$class == 1,"Positive","Negative"); dta$class <- as.factor(dta$class)
                      trainIndex <- sample(1:nrow(dta),size = round(trainProp * nrow(dta)),replace = F)
                      
                      #check if is there any trained model with the same name inside models feature
                      ans <- "ans"; replace <- -1
                      if(length(self$models) > 0)
                        if(sum(sapply(self$models,FUN = function(j) j$method == method & j$patternName == patternName))){
                          while (!(ans %in% c("yes","no"))) {
                            ans <- readline(
                              prompt = sprintf("A model for %s pattern using %s method already exists in my models. Do you want to replace it? (yes/no): ",patternName,method))
                          }
                          replace <- if(ans == 'yes') TRUE else FALSE
                        }
                      if(replace == -1 | replace == TRUE){#replace -1 there is no before train. replace TRUE will replace the saved train
                        #available methods names(getModelInfo())
                        if(nsocks > 1){#for parallel processing
                          cl <- makePSOCKcluster(nsocks); registerDoParallel(cl)
                        }
                        if(package == "h2o" | method == "deepNet"){
                          invisible(capture.output(h2o.init()))
                          message("\nReading data...")
                          train <- as.h2o(dta[trainIndex,]); test <- as.h2o(dta[-trainIndex,])
                          message("Training...")
                          m <- h2o.deeplearning(x = setdiff(colnames(dta),"class"), y = "class", training_frame = train, model_id = patternName,
                                                seed = 1,reproducible = TRUE,...)
                          temp <- h2o.saveModel(m,path = sprintf("%s/h2omodels/%s",getwd(),patternName),force = TRUE)
                          cm <- confusionMatrix(table(as.data.frame(predict(m,test))[,1],dta$class[-trainIndex])[2:1,2:1])
                          cmA <- confusionMatrix(table(as.data.frame(predict(m,train))[,1],dta$class[trainIndex])[2:1,2:1])#apparent errors
                        } else{
                          if(length(list(...))>0)
                            m <- train(x = dta[trainIndex,-1],y = dta$class[trainIndex],method = method,...) else
                              m <- train(x = dta[trainIndex,-1],y = dta$class[trainIndex],method = method)
                          cm <- confusionMatrix(table(predict(m,dta[-trainIndex,-1]),dta$class[-trainIndex])[2:1,2:1])
                          cmA <- confusionMatrix(table(predict(m,dta[-trainIndex,-1]),dta$class[-trainIndex])[2:1,2:1])
                        }
                        if(replace == -1) self$models[[length(self$models)+1]] <- list(method = method,patternName = patternName,model = m,cm=cm,cmA=cmA) else
                          self$models[[which(method == sapply(self$models,FUN = function(j) j$method) & patternName == sapply(self$models,FUN = function(j) j$patternName))]] <- 
                            list(method = method,patternName = patternName,model = m,cm = cm,cmA = cmA)
                        if(nsocks > 1) stopCluster(cl)
                        message("Done.")
                      }
                    }
                  }
                },
                
                #training summary returns statistics from the confusion matrix (recall, precision, specificity,...)
                trainSummary = function(trainProp = 0.60){
                  if(!length(self$models)) message("You must call at least one training process before requesting a summary.") else {
                    tab <- lapply(self$models, FUN = function(m){
                      if(m$method != "deepNet"){
                        best <- which.max(m$model$results$Accuracy)
                        P2 <- m$model$results[best,c("Accuracy","Sensitivity","Specificity")]
                      } else {
                        P2 <- data.frame(Accuracy=m$cmA$overall[1],Sensitivity=m$cmA$byClass[1],Specificity=m$cmA$byClass[1])
                      }
                      r <- data.frame(P0 = m$patternName,P1=m$method,P2=P2,
                                      S0=m$cm$overall[1],S1 = m$cm$byClass[1],S2 = m$cm$byClass[2],stringsAsFactors = F)
                      colnames(r) <- c("Pattern name","Method",paste("Train",c("Accuracy","Sensitivity","Specificity"),sep = " "),"Test Accuracy","Test Sensitivity","Test Specificity")
                      r
                    })
                    tab <- Reduce(f = function(x,y) rbind(x,y),x = tab)
                    rownames(tab) <- NULL
                    tab[order(tab[,1],tab[,3],decreasing = TRUE),]
                  }
                },
                
                #explore a list of tickers for trained patterns
                explore = function(tickers = NULL, dta = NULL, nlast = 240, minN = 100, maxSharing = 0.40, ncandidates = 1, df=20,cores = detectCores()){
                  if(is.null(dta) & is.null(tickers)) {message("I require at least one ticker or a xts data to explore."); return(NULL)}
                  if(is.null(dta)){#download data from yahoo finance
                    getSymbols(Symbols = as.character(tickers),env = environment())
                    dta <- lapply(tickers,FUN = function(ticker){
                      tryCatch({
                        dta <- eval(parse(text = ticker))
                        colnames(dta) <- lapply(colnames(dta),
                                                FUN = function(n) strsplit(x = n,split = ".",fixed = TRUE)[[1]][2])
                        dta
                      },error = function(e){NULL})
                    })
                    if(sum(is.null(dta))){
                      tickers <- tickers[which(!is.null(dta))]
                      dta <- dta[which(!is.null(dta))]
                    }
                  }
                  if(is.null(tickers)) tickers <- "Unknown"
                  cl <- makeCluster(cores)#create a cluster
                  models <- self$models
                  clusterExport(cl,varlist = "models",envir = environment())#send models to each node
                  clusterEvalQ(cl,{source("Bot.R",local = TRUE);h2o.init();#make Bot libraries available in the cluster and start h2o
                    if(length(list.dirs(sprintf("%s/h2omodels",getwd()),recursive = F)))#load h2o models if any
                      for(d in list.dirs(sprintf("%s/h2omodels",getwd()),recursive = F,full.names = F)){
                        models[[which("deepNet" == sapply(models,FUN = function(j) j$method) & 
                                        d == sapply(models,FUN = function(j) j$patternName))]]$model <-
                          h2o.loadModel(gsub(pattern = "/",replacement = "\\\\",sprintf("%s/h2omodels/%s/%s",getwd(),d,d)))
                      }
                    })
                  tabT <- list()
                  for(k in 1:length(dta)){
                    print(sprintf("Working on ticker %s (%s/%s)",tickers[k],k,length(dta)))
                    #determine candidates
                    d <- dta[[k]]
                    minN <- min(minN,nrow(d))
                    nlast <- min(nlast,nrow(d))
                    ends <- seq(minN,nlast,by = 10)
                    nfeatures <- length(models[[1]]$model$finalModel$xName)#number of predictors used when fitting the model. Must be changed to each model in a later version
                    #calculate probability of positive pattern for each candidate
                    clusterExport(cl,c("minN","nlast","nfeatures","d","df"),envir = environment())#send variables to each node
                    probs <- parLapply(cl,X = ends,fun = function(j){
                      subDta <- tail(d[,"Close"],j)#subset dta to keep only records for the current candidate, only Close price
                      #send candidate to 1 by 1 box and extract nfeatures
                      X <- (0:(nrow(subDta)-1))/(nrow(subDta)-1)
                      Y <- (subDta[,1] - min(subDta[,1]))/(max(subDta[,1]) - min(subDta[,1]))
                      spc <- smooth.spline(x = X,y = Y,df = min(df,length(unique(X))))
                      spc <- predict(spc,x=seq(0,1,length.out = nfeatures))
                      probs <- lapply(models, FUN = function(m){#calculate probability of positive pattern for each model and pattern
                        if(m$method == "deepNet")
                          invisible(capture.output(prob <- as.data.frame(predict(m$model,as.h2o(data.frame(t(spc$y)))))[,"Positive"])) else
                        prob <- predict(m$model,data.frame(t(spc$y)),type="prob")[,"Positive"]
                        data.frame(Pattern = m$patternName,Method = m$method,Prob = prob,ends = j)
                      })
                      Reduce(f = function(x,y) rbind(x,y),x = probs)
                    })
                    probs <- Reduce(f = function(x,y) rbind(x,y),probs)
                    #keep only the required n candidates with highest probabilities and sharing not more than maxSharing points
                    tab <- list(); r <- 1
                    for(method in unique(probs$Method))
                      for(pattern in unique(probs$Pattern)){
                        results <- probs[which(probs$Method == method & probs$Pattern == pattern),]
                        results <- results[order(results$Prob,decreasing = TRUE),]
                        founds <- c(); found <-1; j <- 1; repeat{
                          founds[j] <- found; j <- j + 1
                          repeat{
                            found <- found + 1
                            if(found > nrow(results)) break
                            if(results$ends[max(founds)]/results$ends[max(found)] <= maxSharing | 
                               results$ends[max(found)]/results$ends[max(founds)] <= maxSharing) break
                          }
                          if(found > nrow(results) | j>ncandidates) break
                        }
                        tab[[r]] <- data.frame(Method = method, Pattern = pattern, start = index(d)[1+nrow(d)-results[founds,"ends"]],Probability = results[founds,"Prob"])
                        r <- r + 1
                      }
                    tabT[[k]] <- Reduce(f = function(x,y) rbind(x,y),tab)
                  }
                  stopCluster(cl)
                  self$exploration$candidates <- data.frame(Tickers = unlist(lapply(1:length(tickers),FUN = function(j){
                    rep(tickers[j],nrow(tabT[[j]]))
                  })),Reduce(f = function(x,y) rbind(x,y),tabT),stringsAsFactors = F)
                  self$exploration$candidates <- self$exploration$candidates[order(self$exploration$candidates$Pattern,self$exploration$candidates$Probability,decreasing = TRUE),]
                  self$exploration$tickers <- tickers
                  self$exploration$dta <- dta
                  self$exploration$candidates
                },
                
                #returns a plot with a smoothing spline ajusted to the provided data
                plot = function(ticker = NULL,dta=NULL,df = 10,...){
                  if(is.null(dta)) dta <- self$getData(ticker)
                  y <- smooth.spline(dta$Close,df = df)$y
                  ggplot(data = dta,aes(x = Index,y=Close)) +  geom_line() +
                    geom_point(aes(x=Index,y=y),colour="blue",size=1) +
                    geom_line(aes(x=Index,y=y),colour="blue") + theme_minimal()
                }
               ))