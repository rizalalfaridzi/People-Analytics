
#Running regression between Teamwork as dependent variable and Employee
#involmentand 
model1 <- lm(eteam ~ einvol + etra, data = kenexa)
summary(model1)
coef(model1)
residuals(model1)
lm.beta(model1)
help(lm.beta)


model2 <- lm(ecomm ~ einvol + etra, data = kenexa)
summary(model2)
coef(model2)
# model2$coefficients
residuals(model2)
lm.beta(model2)


model3 <- lm(cloy ~ ecomm + eteam, data = kenexa)
summary(model3)
coef(model3)
# model3$coefficients
residuals(model3)
lm.beta(model3)


model4 <- lm(cbrpb ~ ecomm + eteam, data = kenexa)
summary(model4)
coef(model4)
# model4$coefficients
residuals(model4)
lm.beta(model4)


model5 <- lm(prod ~ ecomm + eteam, data = kenexa)
summary(model5)
coef(model5)
# model6$coefficients
residuals(model5)
lm.beta(model5)



model6 <- lm(teltr ~ ecomm + eteam, data = kenexa)
summary(model6)
coef(model6)
# model6$coefficients
residuals(model6)
lm.beta(model6)


model7 <- lm(prod ~ cloy + cbrpb, data = kenexa)
summary(model7)
coef(model7)
# model6$coefficients
residuals(model7)
lm.beta(model7)



model8 <- lm(teltr ~ cloy + cbrpb, data = kenexa)
summary(model8)
coef(model8)
# model6$coefficients
residuals(model8)
lm.beta(model8)
