# Each vector contains a column
groceries = data.frame(
  c(1.17,1.77,1.49,0.65,1.58,3.13,2.09,0.62,5.89,4.46),
  c(1.78,1.98,1.69,0.99,1.70,3.15,1.88,0.65,5.99,4.84),
  c(1.29,1.99,1.79,0.69,1.89,2.99,2.09,0.65,5.99,4.99),
  c(1.29,1.99,1.59,1.09,1.89,3.09,2.49,0.69,6.99,5.15))
rownames(groceries) = c("lettuce","potatoes","milk","eggs","bread","cereal",
                        "ground.beef","tomato.soup","laundry.detergent","aspirin")
colnames(groceries) = c("storeA","storeB","storeC","storeD")

str(groceries)
gr2 <- stack(groceries)
gr2$subject <- rep(rownames(groceries), 4)

colnames(gr2)=c("price","store","subject")
gr2
with(gr2,tapply(price,store,sum))

aov.out = aov(price ~ store + Error(subject/store), data=gr2)
summary(aov.out)

with(gr2, pairwise.t.test(x=price, g=store, p.adjust.method="none", paired=T))
with(gr2, pairwise.t.test(x=price, g=store, p.adjust.method="bonf", paired=T))

cons <- cbind(c(-1, 1/3, 1/3, 1/3), c(0, -1/2, -1/2, 1), c(0, 1, -1, 0))
t(cons) %*% cons
contrasts(gr2$store) <- cons
aov.out <- aov(price ~ store + Error(subject/ store), data = gr2)
summary(aov.out, split = list(store = list("A vs BCD" = 1, 
                          "BC vs D" = 2, "B vs C" = 3)))

aov.tbys= aov(price ~ store + subject,data=gr2)
summary(aov.tbys)
TukeyHSD(aov.tbys,which="store")

friedman.test(price~store|subject,data=gr2)
friedman.test(as.matrix(groceries))

example(friedman.test)

gromat<- as.matrix(groceries[,1:4])
gromat
D<- gromat[,4]-gromat[,1]
D<- matrix(D)
D

result<- lm(D ~ 1)
summary(result)$coef
t.test(D, mu=0)
contr.sum(4)
D=gromat %*% contr.sum(4)
result= lm(D~1)
anova(result)
anova(result, test="Wilks")

EMG = read.table(header=T, text="
LB1 LB2 LB3  LB4
143 368 345  772
142 155 161  178
109 167 356  956
123 135 137  187
276 216 232  307
235 386 398  425
208 175 207  293
267 358 698  771
183 193 631  403
245 268 572 1383
324 507 556  504
148 378 342  796
130 142 150  173
119 171 333 1062
102  94  93   69
279 204 229  299
244 365 392  406
196 168 199  287
279 358 822  671
167 183 731  203
345 238 572 1652
524 507 520  504
")
EMG = as.matrix(EMG)

colMeans(EMG)
apply(EMG, 2, FUN = sd)
cons <- cbind(c(-1, 1, 0, 0), c(-1, 0, 1, 0), c(-1, 0, 0, 1))
cons
Demg <- EMG %*% cons
results <- lm(Demg ~ 1)
anova(results)
summary(results)