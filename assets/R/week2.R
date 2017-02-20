#week2table
houseprice <- c(100000, 150000, 200000, 162000)
housesize <- c(1000, 1240, 1500, 1200)
houserooms <- c(2,2,4,3)

houses <- data.frame(housesize,houserooms, houseprice)
houses
lm(houseprice~.,data=houses)