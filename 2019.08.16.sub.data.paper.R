  ### data
  data = read.csv("C:/Users/User/Desktop/jb/data/20190715.csv",header = FALSE,skip = 1)
  dim(data) # row : 2149254, col : 269
  
  ### colnames
  colnames = read.csv("C:/Users/User/Desktop/jb/data/colnames.csv")
  colnames = colnames(colnames)
  qcinfo.names = c()
  
  for(i in 1:245){
    qcinfo.names = c(qcinfo.names,paste0("qcinfo",i))
  }
  
  colnames = c(colnames[-24],qcinfo.names,"delete")
  colnames(data) = colnames
  data[1,]
  
  ### Change NA to 0
  na.num.vec = c()
  zero.num.vec = c()
  
  for(i in 1:245){
    na.num.vec = c(na.num.vec,sum(is.na(data[,qcinfo.names[i]])))
  }
  
  for(i in 1:245){
    data[,qcinfo.names[i]][is.na(data[,qcinfo.names[i]])] = 0
    zero.num.vec = c(zero.num.vec,sum(data[,qcinfo.names[i]]==0))
  }
  
  cbind(na.num.vec,zero.num.vec) #check
  sum(na.num.vec!=zero.num.vec)
  
  
  
  # ### data split
  # row.length = dim(data)[1]
  # id = row.length*cumsum(c(1/3,1/3,1/3))
  # data_1 = data[1:id[1],]
  # data_2 = data[(id[1]+1):id[2],]
  # data_3 = data[(id[2]+1):id[3],]
  # 
  # 
  # ### data save
  # write.csv(data_1,"C:/Users/User/Desktop/jb/data/splitdata_1.csv",row.names = FALSE)
  # write.csv(data_2,"C:/Users/User/Desktop/jb/data/splitdata_2.csv",row.names = FALSE)
  # write.csv(data_3,"C:/Users/User/Desktop/jb/data/splitdata_3.csv",row.names = FALSE)
  
  
  #######################################################################################################################
  #######################################################################################################################
  #######################################################################################################################
  
  
  ### sub data
  library(caret)
  library(dplyr)
  library(glmnet)
  library(lightgbm)
  library(gbm)
  library(xgboost)
  library(randomForest)
  library(arules)
  library(stringr)
  set.seed(2019)
  # data.id = caret::createDataPartition(data$rightFlag,p = 0.01)
  # s.data = data[data.id$Resample1,]
  
  
  ### Remove unnecessary data & variables
  nonzero.qcinfo.names = qcinfo.names[colSums(data[,qcinfo.names]) != 0]
  
  data = data %>%
    filter(curriculumId!="tw102") %>%
    filter(score!=0) %>% 
    filter(ex_seq!=25479) %>% 
    filter(qType!=3 & qType!=4) %>% 
    filter(qHeight!=0) %>% 
    filter(expHeight!=0) %>% 
    filter(qType==1 | qType==2) %>% 
    select(-seqNo,-ex_seq,-ex_type,-u_seq,-orgId,
           -questionNo,-ev_date,-q_seq,-conceptCount,
           -m_rightRatio,-m_difficulty,-m_ex_avg,-m_ex_stdev,-delete,
           nonzero.qcinfo.names) 
  
  
  
  
  ### data trainform
  data = data %>% 
    #syear
    mutate(syear=replace(syear,syear==10,"H1")) %>% 
    mutate(syear=replace(syear,syear==11,"H2")) %>% 
    mutate(syear=replace(syear,syear==12,"H3")) %>% 
    mutate(syear=as.factor(syear)) %>% 
    #dvision
    mutate(division=replace(division,division==0,"C")) %>% 
    mutate(division=replace(division,division==1,"A")) %>% 
    mutate(division=replace(division,division==2,"S")) %>% 
    mutate(division=as.factor(division)) %>%
    #curriculumId
    mutate(curriculumId= factor(curriculumId, levels = c("ma2007a","ma2007b","ma2009","ma2014","ma2015"))) %>% 
    #score
    mutate(score=ifelse(score>6,round(score,digits = -1)*0.1,score)) %>% 
    mutate(score=as.factor(score)) %>% 
    #qType
    mutate(qType=replace(qType,qType==1,"R")) %>% 
    mutate(qType=replace(qType,qType==2,"W")) %>% 
    mutate(qType=as.factor(qType)) %>%
    #imageAudit
    mutate(imageAudit=replace(imageAudit,imageAudit==0,"N")) %>% 
    mutate(imageAudit=replace(imageAudit,imageAudit==1|imageAudit==9,"P")) %>% 
    mutate(imageAudit=as.factor(imageAudit)) %>% 
    as.data.frame()
  
  
  
  
  ### associaition rule
  
  arules.qcinfo = data.frame(apply(data[,nonzero.qcinfo.names],2,as.factor))
  
  rules <- apriori(arules.qcinfo,control = list(verbose=F),
                   parameter = list(minlen=1,supp=0.001,conf=0.1),
                   appearance = list(both=paste(nonzero.qcinfo.names,"=1",sep='')))
  
  #duplicated
  #detach("package:arules", unload=TRUE)
  gi <- generatingItemsets(rules)
  di <- which(duplicated(gi))
  
  #result
  result.rules = inspect(head(sort(rules[-di], by="support"),100))
  
  #extract variable name from arules result
  
  a1 = str_sub(result.rules[,1],2,-2)
  a1 = gsub("=1", "", a1)
  a2 = str_sub(result.rules[,3],2,-2)
  a2= gsub("=1", "", a2)
  a3 = paste(a1,",",a2,sep='')
  a4 = strsplit(a3, split = ",") #final value
  
  
  
  #######################################################################################################################
  ####################################################### Analysis ######################################################
  #######################################################################################################################
  final.list = list()
  fit.glm.list = list()
  start_time <- Sys.time()
  for(k in 1:10){
    
    
    #Variable combination from aprior analysis result
    names.apri = a4[[k]]
    if(sum(a4[[k]]=="")!=0){names.apri = a4[[k]][a4[[k]]!=""]}
    
    #Extract rows with a value of 1 in all combined variables
    apri.id = data %>% 
      select(names.apri) %>% 
      apply(1,sum) == length(names.apri)
    
    ss.data = data %>% 
      select(-qcinfo.names) %>% 
      filter(apri.id)
    
    #Table of categorical variables
    tcu = table(ss.data$curriculumId); tsy = table(ss.data$syear)
    tdi = table(ss.data$division); tsc = table(ss.data$score)
    tqt = table(ss.data$qType) ; tim = table(ss.data$imageAudit)
    
    #Remove data that is a category with a number less than 30
    cu.id = rowSums(outer(ss.data$curriculumId,names(tcu)[tcu<30],"=="))==0
    sy.id = rowSums(outer(ss.data$syear,names(tsy)[tsy<30],"=="))==0
    di.id = rowSums(outer(ss.data$division,names(tdi)[tdi<30],"=="))==0
    sc.id = rowSums(outer(ss.data$score,names(tsc)[tsc<30],"=="))==0
    qt.id = rowSums(outer(ss.data$qType,names(tqt)[tqt<30],"=="))==0
    ti.id = rowSums(outer(ss.data$imageAudit,names(tim)[tim<30],"=="))==0
    
    #select data that is all TRUE
    com.id = rowSums(cbind(cu.id,sy.id,di.id,sc.id,qt.id,ti.id)) == 6
    sss.data = ss.data %>% 
      filter(com.id)
    
    #Remove variables when the data has only one category
    va.names = c("curriculumId","syear","division","score","qType","imageAudit")
    remove.va.id = c(sum(tcu>30)==1,sum(tsy>30)==1,sum(tdi>30)==1,sum(tsc>30)==1,sum(tqt>30)==1,sum(tim>30)==1) 
    remove.names = va.names[remove.va.id]
    sss.data = sss.data %>% 
      select(-remove.names)
    #################################################################################################################################
    
    
    #create data group
    set.seed(2019)
    sss.data = sss.data[sample(1:dim(sss.data)[1])[1:(dim(sss.data)[1]*0.1)],]
    sss.data.group = apply(sss.data[,va.names[!remove.va.id]],1,paste,collapse="")
    
    mean.y.mat = NULL ; logit.result = NULL ; logit.time = NULL
    rf.tune.result = NULL ; rf.tune.save = NULL ; rf.time = NULL
    lgb.tune.result = NULL ; lgb.tune.save = NULL ; lgb.time = NULL
    gbm.tune.result = NULL ; gbm.tune.save = NULL ; gbm.time = NULL
    xgb.tune.result = NULL ; xgb.tune.save = NULL ; xgb.time = NULL
    
    for(iter in 1:30){
      cat("#List : ",k,"#Iter :",iter,"\n")
      #train test split
      set.seed(iter)
      sss.tr.id = caret::createDataPartition(sss.data.group, p = 0.7)$Resample1
      train.sss.data = sss.data[sss.tr.id,]
      test.sss.data = sss.data[-sss.tr.id,]
      mean.y.mat = rbind(mean.y.mat,mean(test.sss.data$rightFlag))
      ######################################### create folds for CV #################################################
      ###stratified 3 fold
      set.seed(2019)
      folds <- 3
      cvIndex <- createFolds(train.sss.data$rightFlag, folds, returnTrain = F)
      
      ############################################logstic fit & prediction###########################################
      scale.train.sss.data = train.sss.data %>% 
        mutate(difficulty = (difficulty-mean(difficulty))/sd(difficulty)) %>% 
        mutate(qHeight = (qHeight-mean(qHeight))/sd(qHeight)) %>% 
        mutate(expHeight = (expHeight-mean(expHeight))/sd(expHeight))
      
      scale.test.sss.data = test.sss.data %>% 
        mutate(difficulty = (difficulty-mean(train.sss.data$difficulty))/sd(train.sss.data$difficulty)) %>% 
        mutate(qHeight = (qHeight-mean(train.sss.data$qHeight))/sd(train.sss.data$qHeight)) %>% 
        mutate(expHeight = (expHeight-mean(train.sss.data$expHeight))/sd(train.sss.data$expHeight))
      
      logit.start.time = Sys.time()
      fit.glm = glm(rightFlag~.,data=train.sss.data,family="binomial")
      logit.end.time = Sys.time()
      scale.fit.glm = glm(rightFlag~.,data=scale.train.sss.data,family="binomial")
      tab=table(test.sss.data$rightFlag,(predict(fit.glm,test.sss.data)>0)+0)
      
      #acc, sen, spc
      if(length(tab)==4){
        acc=sum(diag(tab))/sum(tab)
        sen=tab[2,2]/sum(tab[2,])
        spc = tab[1,1]/sum(tab[1,])
      } else{
        acc = tab[2,1]/sum(tab)
        sen = 1
        spc = 0
      }
      logit.time = rbind(logit.time,logit.end.time-logit.start.time)
      logit.result = rbind(logit.result,c(acc,sen,spc))
      
      
      ############################################ridge fit & prediction###########################################
      # foldid = rep(NA,length(train.sss.data$rightFlag))
      # for(m in 1:folds){
      #   foldid[cvIndex[[m]]] = m
      # }
      # 
      # x_train <- model.matrix(rightFlag ~ .-1, train.sss.data)
      # cv.ridge = cv.glmnet(x=x_train ,y = as.factor(train.sss.data$rightFlag),family = "binomial",alpha=0,foldid = foldid )
      # x_test <- model.matrix(rightFlag ~ .-1, test.sss.data)
      # tab=table(test.sss.data$rightFlag,(predict(cv.ridge,x_test)>0)+0)
      # 
      # #acc, sen, spc
      # if(length(tab)==4){
      #   acc=sum(diag(tab))/sum(tab)
      #   sen=tab[2,2]/sum(tab[2,])
      #   spc = tab[1,1]/sum(tab[1,])
      # } else{
      #   acc = tab[2,1]/sum(tab)
      #   sen = 1
      #   spc = 0
      # }
      # ridge.result = rbind(ridge.result,c(acc,sen,spc))
      
      ############################################rf.tune fit & prediction#################################################

      ###rf tune
      set.seed(2019)
      rf.grid.search = expand.grid(ntree = (1:25)*20, mtry = c(4))
      perf.rf.mat <- matrix(0,nrow(rf.grid.search),1)
      colnames(perf.rf.mat) = c("score")

      #grid search
      for(i in 1:nrow(rf.grid.search)){
        cat('rf grid',i,"\n")

        #cross validation
        rf.cv.err = NULL

        for(ind in 1:folds){
          set.seed(2019)
          rf = randomForest(as.factor(rightFlag)~.,data=train.sss.data[-cvIndex[[ind]],],
                            mtry = rf.grid.search[i,"mtry"],
                            ntree=rf.grid.search[i,"ntree"])

          rfcr = mean(train.sss.data[cvIndex[[ind]],"rightFlag"] != (predict(rf,train.sss.data[cvIndex[[ind]],],"prob")[,2]>0.5) + 0   )
          rf.cv.err = rbind(rf.cv.err,rfcr)

        }

        perf.rf.mat[i,] = colMeans(rf.cv.err)

      }

      #find best tuning parameters
      final.perf.rf.mat = cbind(rf.grid.search,perf.rf.mat)
      rf.opt.par = final.perf.rf.mat[which.min(final.perf.rf.mat[,"score"]),]
      rf.tune.save = rbind(rf.tune.save,rf.opt.par)

      ###fit rf
      set.seed(2019)
      rf.start.time = Sys.time()
      rf.fit = randomForest(as.factor(rightFlag)~.,data=train.sss.data,
                            mtry = rf.opt.par$mtry,
                            ntree=rf.opt.par$ntree)
      rf.end.time = Sys.time()
      tab=table(test.sss.data$rightFlag,(predict(rf.fit,test.sss.data,"prob")[,2]>0.5)+0)
      #acc, sen, spc
      if(length(tab)==4){
        acc=sum(diag(tab))/sum(tab)
        sen=tab[2,2]/sum(tab[2,])
        spc = tab[1,1]/sum(tab[1,])
      } else{
        acc = tab[2,1]/sum(tab)
        sen = 1
        spc = 0
      }
      rf.tune.result = rbind(rf.tune.result,c(acc,sen,spc))
      rf.time = rbind(rf.time,rf.end.time-rf.start.time)
      #############################################gbm.tune fit & prediction#################################################

      ###gbm tune

      gbm.grid.search = expand.grid(interaction.depth=c(6),
                                    shrinkage=c(0.01,0.1,0.3),
                                    bag.fraction = c(0.7,1))

      perf.gbm.mat <- matrix(0,nrow(gbm.grid.search),2)
      colnames(perf.gbm.mat) = c("iter","score")

      #grid search
      for(i in 1:nrow(gbm.grid.search)){
        cat('gbm grid',i,"\n")

        #cross validation
        gbm.cv.err = NULL

        for(ind in 1:folds){
          set.seed(2019)
          gbm.model <- gbm(rightFlag~.,data=train.sss.data[-cvIndex[[ind]],]
                           , distribution = "bernoulli"
                           , n.trees = 2000
                           , interaction.depth = gbm.grid.search[i,"interaction.depth"]
                           , shrinkage = gbm.grid.search[i,"shrinkage"]
                           , bag.fraction = gbm.grid.search[i,"bag.fraction"])

          gcr = colMeans(train.sss.data[cvIndex[[ind]],"rightFlag"] != (predict(gbm.model,train.sss.data[cvIndex[[ind]],], 1:2000)>0)+0)
          gbm.cv.err = rbind(gbm.cv.err,gcr)

        }

        gbm.opt.iter = which.min(colMeans(gbm.cv.err))
        perf.gbm.mat[i,] = c(gbm.opt.iter,min(colMeans(gbm.cv.err)))

      }

      #find best tuning parameters
      final.perf.gbm.mat = cbind(gbm.grid.search,perf.gbm.mat)
      gbm.opt.par = final.perf.gbm.mat[which.min(final.perf.gbm.mat[,"score"]),]
      gbm.tune.save = rbind(gbm.tune.save,gbm.opt.par)

      ###fit gbm
      gbm.start.time = Sys.time()
      gbm.fit = gbm(rightFlag~.,data=train.sss.data
                    , distribution = "bernoulli"
                    , n.trees = gbm.opt.par$iter
                    , interaction.depth = gbm.opt.par$interaction.depth
                    , shrinkage = gbm.opt.par$shrinkage
                    , bag.fraction = gbm.opt.par$bag.fraction)
      gbm.end.time = Sys.time()
      tab=table(test.sss.data$rightFlag,(predict(gbm.fit,test.sss.data,gbm.opt.par$iter)>0)+0)
      #acc, sen, spc
      if(length(tab)==4){
        acc=sum(diag(tab))/sum(tab)
        sen=tab[2,2]/sum(tab[2,])
        spc = tab[1,1]/sum(tab[1,])
      } else{
        acc = tab[2,1]/sum(tab)
        sen = 1
        spc = 0
      }
      gbm.tune.result = rbind(gbm.tune.result,c(acc,sen,spc))
      gbm.time = rbind(gbm.time,gbm.end.time-gbm.start.time)
      #############################################xgboost.tune fit & prediction#################################################
      ### xgb data
      sp.tr.data = sparse.model.matrix(rightFlag ~ .-1, data = train.sss.data)
      sp.te.data = sparse.model.matrix(rightFlag ~ .-1, data = test.sss.data)
      tr.label = train.sss.data$rightFlag
      te.label = test.sss.data$rightFlag
      d.train = xgb.DMatrix(data=sp.tr.data,label=tr.label)
      d.test = xgb.DMatrix(data=sp.te.data,label=te.label)


      ### xgb tune
      xgb.grid.search <- expand.grid(max_depth = c(6), #default = 6
                                     eta = c(0.01,0.1,0.3), #default = 0.3
                                     colsample_bytree = c(0.7,1), #default = 1
                                     min_child_weight = c(0.1,1), #default = 1
                                     subsample = c(0.7,1)) #default = 1

      perf.xgb.mat <- matrix(0,nrow(xgb.grid.search),2)
      colnames(perf.xgb.mat) = c("iter","score")

      #grid search
      for(i in 1:nrow(xgb.grid.search)){
        cat('xgboost grid',i,"\n")
        params.xgb<-list(objective = "binary:logistic",
                         booster = "gbtree",
                         eta = xgb.grid.search[i,"eta"], #default = 0.3
                         max_depth = xgb.grid.search[i,"max_depth"], #default=6
                         min_child_weight = xgb.grid.search[i,"min_child_weight"], #default=1
                         subsample = xgb.grid.search[i,"subsample"],
                         colsample_bytree = xgb.grid.search[i,"colsample_bytree"])

        set.seed(2019)
        xgbcv <- xgb.cv(params = params.xgb, data = d.train,nrounds = 1000,folds = cvIndex,
                        print_every_n = 50,early_stopping_rounds = 50, maximize = F, verbose = FALSE)

        perf.xgb.mat[i,]=c(xgbcv$best_iteration,min(xgbcv$evaluation_log$test_error_mean))
      }


      #find best tuning parameters
      final.perf.xgb.mat = cbind(xgb.grid.search,perf.xgb.mat)
      xgb.opt.par = final.perf.xgb.mat[which.min(final.perf.xgb.mat[,"score"]),]
      xgb.tune.save = rbind(xgb.tune.save,xgb.opt.par)


      ###fit xgb
      params.opt.xgb<-list(objective = "binary:logistic",
                           booster = "gbtree",
                           eta = xgb.opt.par$eta, #default = 0.3
                           max_depth = xgb.opt.par$max_depth, #default=6
                           min_child_weight = xgb.opt.par$min_child_weight, #default=1
                           subsample = xgb.opt.par$subsample,
                           colsample_bytree = xgb.opt.par$colsample_bytree)

      set.seed(2019)
      xgb.start.time = Sys.time()
      xgb_mod <- xgb.train(data = d.train, params=params.opt.xgb, nrounds = xgb.opt.par$iter)
      xgb.end.time = Sys.time()
      tab=table(test.sss.data$rightFlag,(predict(xgb_mod,sp.te.data)>0.5) + 0)

      #acc, sen, spc
      if(length(tab)==4){
        acc=sum(diag(tab))/sum(tab)
        sen=tab[2,2]/sum(tab[2,])
        spc = tab[1,1]/sum(tab[1,])
      } else{
        acc = tab[2,1]/sum(tab)
        sen = 1
        spc = 0
      }
      xgb.tune.result = rbind(xgb.tune.result,c(acc,sen,spc))
      xgb.time = rbind(xgb.time,xgb.end.time-xgb.start.time)
      ############################################lightgbm.base fit & prediction########################################

      lgb.train = lgb.Dataset(data = sp.tr.data,label=tr.label)
      lgb.test = lgb.Dataset(data=sp.te.data,label=te.label)


      ###lgb tune
      lgb.grid.search <- expand.grid(num_leaves = c(2^6), #xgboost max_depth, light gbm is leaf wise
                                     learning_rate = c(0.01,0.1,0.3), #xgboost eta
                                     min_sum_hessian_in_leaf = c(0.1,1), #default = 0.001 #xgboost min_child_weight
                                     feature_fraction = c(0.7,1), # xgboost subsample
                                     bagging_fraction = c(0.7,1)) # colsample subsample

      perf.lgb.mat <- matrix(0,nrow(lgb.grid.search),2)
      colnames(perf.lgb.mat) = c("iter","score")

      #grid search
      for(i in 1:nrow(lgb.grid.search)){
        cat('lightgbm grid',i,"\n")
        params.lgb = list(
          boost_from_average = FALSE
          , objective = "binary"
          , metric = "binary_error"
          , num_leaves = lgb.grid.search[i, "num_leaves"]
          , learning_rate = lgb.grid.search[i, "learning_rate"]
          , min_sum_hessian_in_leaf = lgb.grid.search[i, "min_sum_hessian_in_leaf"]
          , feature_fraction = lgb.grid.search[i, "feature_fraction"]
          , bagging_fraction = lgb.grid.search[i, "bagging_fraction"]
        )

        set.seed(2019)
        lgb.model <- lgb.cv(
          params = params.lgb
          , data = lgb.train
          , folds = cvIndex
          , num_threads = 12
          , nrounds = 1000
          , early_stopping_rounds = 50
          , eval_freq = 20
          , verbose = -1
          , boost_from_average=FALSE
        )
        perf.lgb.mat[i,]=c(lgb.model$best_iter,lgb.model$best_score)
      }

      #find best tuning parameters
      final.perf.lgb.mat = cbind(lgb.grid.search,perf.lgb.mat)
      lgb.opt.par = final.perf.lgb.mat[which.min(final.perf.lgb.mat[,"score"]),]
      lgb.tune.save = rbind(lgb.tune.save,lgb.opt.par)

      #fit lgb
      params.opt.lgb = list(
        boost_from_average = FALSE
        , objective = "binary"
        , metric = "binary_error"
        , num_leaves = lgb.opt.par$num_leaves
        , learning_rate = lgb.opt.par$learning_rate
        , min_sum_hessian_in_leaf = lgb.opt.par$min_sum_hessian_in_leaf
        , feature_fraction = lgb.opt.par$feature_fraction
        , bagging_fraction = lgb.opt.par$bagging_fraction)

      set.seed(2019)
      lgb.start.time = Sys.time()
      lgb.fit = lgb.train(params = params.opt.lgb, data = lgb.train,
                          num_threads=12, nrounds = lgb.opt.par$iter,eval_freq = 20)
      lgb.end.time = Sys.time()

      tab=table(test.sss.data$rightFlag,(predict(lgb.fit,sp.te.data)>0.5)+0)

      #acc, sen, spc
      if(length(tab)==4){
        acc=sum(diag(tab))/sum(tab)
        sen=tab[2,2]/sum(tab[2,])
        spc = tab[1,1]/sum(tab[1,])
      } else{
        acc = tab[2,1]/sum(tab)
        sen = 1
        spc = 0
      }
      lgb.tune.result = rbind(lgb.tune.result,c(acc,sen,spc))
      lgb.time = rbind(lgb.time,lgb.end.time-lgb.start.time)
      if(iter==1){fit.glm.list = append(fit.glm.list,list(summary(scale.fit.glm)))}
    }
    colnames(logit.result) = c("logit.acc","logit.sen","logit.spc") ; colnames(logit.time) = c("logit.time")
    colnames(rf.tune.result) = c("rf.acc","rf.sen","rf.spc") ; colnames(rf.time) = c("rf.time")
    colnames(gbm.tune.result) = c("gbm.acc","gbm.sen","gbm.spc") ; colnames(gbm.time) = c("gbm.time")
    colnames(xgb.tune.result) = c("xgb.acc","xgb.sen","xgb.spc") ; colnames(xgb.time) = c("xgb.time")
    colnames(lgb.tune.result) = c("lgb.acc","lgb.sen","lgb.spc") ; colnames(lgb.time) = c("lgb.time")
    colnames(mean.y.mat) = "mean.y"
    final.list[[k]] = cbind(mean.y.mat,logit.result,logit.time,
                            rf.tune.result,rf.tune.save,rf.time,
                            gbm.tune.result,gbm.tune.save,gbm.time,
                            xgb.tune.result,xgb.tune.save,xgb.time,
                            lgb.tune.result,lgb.tune.save,lgb.time)
  }
  
  end_time <- Sys.time()
  total.time = end_time - start_time
  
  
  final.list
  fit.glm.list

  
  #result box plot
  library(ggplot2)
  library(gridExtra)
  #Accuracy boxplot
  for(i in 1:10){
    df = final.list[[i]][,c("mean.y","logit.acc","rf.acc","gbm.acc","xgb.acc","lgb.acc")]
    colnames(df) = c("Mean","LR","RF","GBM","XGB","LGB")
    s.df = stack(df) ; colnames(s.df) = c("Accuracy","Methods")
    result.acc.boxplot = ggplot(data=s.df, aes(x=Methods,y= Accuracy))+
      stat_boxplot(geom ='errorbar') + 
      geom_boxplot(fill='slategrey',color='darkslategrey',width=0.7)
    #Sensitivity boxplot
    df = final.list[[i]][,c("logit.sen","rf.sen","gbm.sen","xgb.sen","lgb.sen")]
    colnames(df) = c("LR","RF","GBM","XGB","LGB")
    s.df = stack(df) ; colnames(s.df) = c("Sensitivity","Methods")
    result.sen.boxplot = ggplot(data=s.df, aes(x=Methods,y= Sensitivity))+
      stat_boxplot(geom ='errorbar') + 
      geom_boxplot(fill='slategrey',color='darkslategrey',width=0.7)
    #Specificity boxplot
    df = final.list[[i]][,c("logit.spc","rf.spc","gbm.spc","xgb.spc","lgb.spc")]
    colnames(df) = c("LR","RF","GBM","XGB","LGB")
    s.df = stack(df) ; colnames(s.df) = c("Specificity","Methods")
    result.spc.boxplot = ggplot(data=s.df, aes(x=Methods,y= Specificity))+
      stat_boxplot(geom ='errorbar') + 
      geom_boxplot(fill='slategrey',color='darkslategrey',width=0.7)
    #Train time/iter boxplot
    df  = final.list[[i]][,c("rf.time","gbm.time","xgb.time","lgb.time")]/final.list[[i]][,c(9,19,30,41)]
    # if(sum(df$rf.time<0.005)!=0){df$rf.time[df$rf.time<0.005] = df$rf.time[df$rf.time<0.005]*60 }
    # if(sum(df$gbm.time<0.005)!=0){df$gbm.time[df$gbm.time<0.005] = df$gbm.time[df$gbm.time<0.005]*60 }
    # if(sum(df$xgb.time<0.005)!=0){df$xgb.time[df$xgb.time<0.005] = df$xgb.time[df$xgb.time<0.005]*60 }
    colnames(df) = c("RF","GBM","XGB","LGB")
    write.table(df, file = "C:/Users/User/Desktop/jb/table/result.time.sub.table.csv", append = TRUE,sep = ",",
                col.names = TRUE,row.names = FALSE)
    s.df = stack(df) ; colnames(s.df) = c("Time","Methods")
    result.time.boxplot = ggplot(data=s.df, aes(x=Methods,y= Time))+
      stat_boxplot(geom ='errorbar') + 
      geom_boxplot(fill='slategrey',color='darkslategrey',width=0.7)
    result.boxplot = grid.arrange(result.acc.boxplot,result.sen.boxplot,result.spc.boxplot,result.time.boxplot,nrow=2,ncol=2)
    dir.boxplot = paste0("C:/Users/User/Desktop/jb/image/result.sub.boxplot",i,".png")
    ggsave(file=dir.boxplot, plot=result.boxplot , width=5, height=5)
  }
  
  
  
  #mean of acc&sen&spc
  result.mean.mat = NULL
  for(i in 1:10){
    result.mean.mat = rbind(result.mean.mat,colMeans(final.list[[i]][,c("mean.y","logit.acc","logit.sen","logit.spc","rf.acc","rf.sen","rf.spc",
                                                                        "gbm.acc","gbm.sen","gbm.spc","xgb.acc","xgb.sen","xgb.spc","lgb.acc","lgb.sen","lgb.spc")]))
  }
  
  write.table(result.mean.mat, file = "C:/Users/User/Desktop/jb/table/result.mean.sub.table.csv", append = TRUE,sep = ",",
              col.names = TRUE,row.names = FALSE)
  
  #result table
  for(i in 1:10){
    final.result.table = round(final.list[[i]],4)
    if(i ==1){final.result.table[,"gbm.time"] = round(final.result.table[,"gbm.time"]*60,4)}
    write.table(final.result.table , file = "C:/Users/User/Desktop/jb/table/final.result.sub.table.csv", append = TRUE,sep = ",",
                col.names = TRUE,row.names = FALSE)
  }

  
  #coef of glm
  for(i in 1:10){
    glm.coef = t(round(fit.glm.list[[1]]$coefficients[,c(1,4)],4))
    write.table(glm.coef, file = "C:/Users/User/Desktop/jb/table/glm.coef.sub.csv", append = TRUE,sep = ",",
                col.names = TRUE,row.names = TRUE)
  }

  
  
  save.image("C:/Users/User/Desktop/jb/paper/paper.2019.08.16.sub.data.image.RData")
