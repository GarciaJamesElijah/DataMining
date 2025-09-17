#Traffic Lights
traff <- "Otherwise"
input <- switch(traff,
                "Red"="Stop",
                "Yellow"="Ready",
                "Green"="Go",
                "Otherwise"="Invalid signal")
print(input)

