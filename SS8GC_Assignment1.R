setwd("/Users/sameer/Desktop/UVA/Fall/Data Mining/Assignment/1/Data_all")

db = read_csv("train.csv")

db$Pclass = as.factor(db$Pclass)
db$Sex = as.factor(db$Sex)
db$Embarked = as.factor(db$Embarked)
db$Age[is.na(db$Age)]=median(db$Age[!is.na(db$Age)])

smp = sample(891,200)

db_test = db[smp,]
db_train = db[-smp,]

drop_col = c("Ticket","Cabin","Name","PassengerId","Fare")
db_rel = db_train[!(colnames(db_train) %in% drop_col) ]

log_mod = glm(Survived~.,data = db_rel,family = "binomial")
summary(log_mod)

db_rel_test = db_test[!(colnames(db_test) %in% drop_col) ]

pred = rep(0,200)
prob = as.vector(predict(log_mod, newdata = db_rel_test,type = "response"))
pred[prob>0.5] = 1
table(db_test$Survived,pred)

db_rel_all = db[!(colnames(db) %in% drop_col) ]

log_mod_f = glm(Survived~.,data = db_rel_all,family = "binomial")
summary(log_mod_f)

pred = rep(0,200)
prob = as.vector(predict(log_mod, newdata = db_rel_test,type = "response"))
pred[prob>0.5] = 1
table(db_test$Survived,pred)

##------------------------------------------------------------------------------------##
db_t = read_csv("test.csv")

db_t$Pclass = as.factor(db_t$Pclass)
db_t$Sex = as.factor(db_t$Sex)
db_t$Embarked = as.factor(db_t$Embarked)
db_t$Age[is.na(db_t$Age)]=median(db_t$Age[!is.na(db_t$Age)])

db_t_rel = db_t[!(colnames(db_t) %in% drop_col) ]

pred = rep(0,418)
prob = as.vector(predict(log_mod, newdata = db_t_rel,type = "response"))
pred[prob>0.5] = 1

rel_data = as.data.frame(cbind(pred,db_t$PassengerId))

write.table(rel_data, file = "SS8GC-hw3-p1-mypredictions.csv", row.names=F, col.names=c("Survived","PassengerId"), sep=",")
          