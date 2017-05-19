games = data.frame("Gender" = c("Male","Female"),"Game A" = c(200,250),"Game B" = c(150,300), "Game C" = c(50,50))
str(games)
games

library('dplyr')

#games = bind_rows(games , select (summarise (games,"Gender" = NA,"Game.A" = sum(Game.A),"Game.B" = sum(Game.B),"Game.C" = sum(Game.C)),Gender,Game.A,Game.B,Game.C))

games[nrow(games)+1,] = select (summarise (games,"Gender" = NA,"A" = sum(Game.A),"B" = sum(Game.B),"C" = sum(Game.C)),Gender,A,B,C)

games = inner_join(games,summarise(group_by(games,Gender),"Total" = sum(Game.A,Game.B,Game.C)),by="Gender")
games
p_games = games
mutate(p_games,"P.Game.A" = Total*sum(Game.A))
400*450
summarise(group_by(games,Gender,Game.A),sum(Game.A))



tb<- matrix(c(200,250,150,300,50,50),nrow = 2,ncol = 3,byrow = F)
tb
chisq.test(tb)
qchisq(.95,2)
1-pchisq(16.204,2)

expectedfrequency = c(0.08*207,.47*207,.34*207,.11*207)
observedfrequency = c(21,109,62,15)
chisq.test(observedfrequency,p = expectedfrequency/sum(expectedfrequency))
qchisq(.95,3)

#t.test(observedfrequency,expectedfrequency,alternative = "two.sided")

data <- data.frame(scores = c(643,655,702,469,427,525,484,456,402),method = factor(rep(c("M1","M2","M3"),c(3,3,3))))
model = aov(scores~method,data)
summary(model)
model.tables
(86049.56/2)/(10254.00/6)

qf(.95,2,6)
View(data)
model

coffee = data.frame(scores = c(3,5,6,2,1,2,6,7,9,7,11,6,9,10,15,12,11,10),m = factor(rep(c("M1","M2","M3"),c(6,6,6))))
coffee_model = aov(scores~m,coffee)
coffee_model
summary(coffee_model)
qf(.95,2,15)
model.tables(coffee_model,"effects")
model.tables(coffee_model,"means")
sd(c(3.167,7.667,11.167))


grapes = data.frame(scores = c(30000,34000,36000,38000,40000,30000,35000,37000,38000,40000,40000,41000,43000,44000,45000),model = factor(rep(c("1996","1997","1998"),c(5,5,5))))
grapes_model = aov(scores~model,grapes)
grapes_model
summary(grapes_model)
qf(.95,2,12)
qf(.99,2,12)

ob = c(12,38,5,8,7,19,3,1)
exp = c(.8*19,.8*57,.8*8,.8*9,.2*19,.2*57,.2*8,.2*9)
chisq.test(ob,p = exp/sum(exp))
qchisq(.95,7)

cards_ob = c(402,358,273,467)
cards_exp = rep(c(375),4)
chisq.test(cards_ob,p=cards_exp/sum(cards_exp))
qchisq(.95,3)

mtcars[,c("cyl","vs","am","gear","carb")] = 
  data.frame(apply(mtcars[,c("cyl","vs","am","gear","carb")],2,function(x)
    as.factor(as.character(x))
  ))
str(mtcars)

mtcars_model = aov(mpg~cyl,mtcars)
summary(mtcars_model)
qf(.95,2,30)


machine1 = c(22.3,21.9,21.8,22.4,22.3,22.5,21.6,22.2,21.8,21.6)
machine2 = c(22.0,21.7,22.1,21.9,21.8,22.0,21.9,22.1,22.2,21.9,22.0,22.1)
var.test(machine1,machine2,alternative = "two.sided")
1/qf(0.975,9,11)
qf(0.975,9,11)
0.0051/0.0284


sound_test = data.frame(scores = c(7,4,6,8,6,6,2,9,5,5,3,4,4,7,2,2,2,4,7,1,2,1,5,5),model = factor(rep(c("cs","rs","ns"),c(8,8,8))))
sound_model = aov(scores~model,sound_test)
summary(sound_model)
qf(.95,2,21)


dell_comp = matrix(c(12,14,21,18,17,18),nrow = 3 ,ncol = 2,byrow = T)
dell_comp
chisq.test(dell_comp)
qchisq(.95,2)


drug = matrix(c(26,95,18,41,40,20,24,13,32),nrow = 3,ncol = 3,byrow = T)
drug
chisq.test(drug)  

ob_freq = c(50,45,5)
exp_freq_p = c(.3,.6,.1)
chisq.test(ob_freq,p=exp_freq_p)
qchisq(.95,2)
