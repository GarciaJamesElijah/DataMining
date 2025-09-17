#Math Menu
x <- 10L
y <- 5L

choice <- "Subtract"
input<- switch(choice,
               "Add"=x + y,
               "Subtract"=x - y,
               "Multiply"=x * y,
               "Divide"=x / y,
               "invalid choice")
print(input)

