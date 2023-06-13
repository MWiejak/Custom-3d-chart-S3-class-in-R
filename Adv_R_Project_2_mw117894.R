# Mateusz Wiejak mw117894
library(rgl)
library(stringr)

# S3 class constructor

chart <- function(f, xrange, yrange, n = 100, col = "black", alpha = 1, ...){
  f<- substitute(f)
  additional <- substitute(list(...))
  structure(list(f = f, xrange = xrange, yrange = yrange, n = n, col = col, alpha = alpha, additional = additional), class = "3dChart")
}

# Printing function

print.3dChart <- function(chart, ...){
  `%G%` <- paste0
  cat("3d chart of f(arg1,arg2) = " %G% deparse(chart$f))
}

# Plotting function

plot.3dChart <- function(chart, ... , plotOptions = list(), justSurface = F){  
  
  x <- seq(from = chart$xrange[1], to = chart$xrange[2], length.out = chart$n)
  y <- seq(from = chart$yrange[1], to = chart$yrange[2], length.out = chart$n)
  data <- expand.grid(x,y)
  data["result"] <- eval(chart$f, envir = list(arg1 = data[1], arg2 = data[2]))
  res <- as.data.frame(t(data$result))
  
  decorate_command <-"decorate3d(data$Var1, data$Var2, data$result"
 
  for (option in plotOptions){
    eval(option,envir = environment())
  }
  
  if(!justSurface){
    a <- tryCatch({
      
      plot3d()
      
    }, error = function(e) {
      
    })
    
    rm(a)
    
    additionals <- deparse(chart$additional)
    additionals <- str_replace(additionals, "list\\(", ",")
    
    if(length(additionals) > 1){
      additionals_2 <- ""
      for (str in additionals){
        additionals_2 <- paste0(additionals_2,str)
      }
      additionals <- additionals_2
    }
    
    additionals <- str_sub(additionals,1,nchar(additionals)-1)
    decorate_command <- paste(decorate_command,additionals, sep = "")
    
    if(!str_detect(decorate_command, "xlab")){
      decorate_command <- paste0(decorate_command, ", xlab = ''")
    }
    
    if(!str_detect(decorate_command, "ylab")){
      decorate_command <- paste0(decorate_command, ", ylab = ''")
    }
    
    if(!str_detect(decorate_command, "zlab")){
      decorate_command <- paste0(decorate_command, ", zlab = ''")
    }
    decorate_command <- paste0(decorate_command,")")
  }

  surface3d(x = x, y = y, z = res, col = chart$col, alpha = chart$alpha)
  
  
  if(!justSurface){
    eval(parse(text = decorate_command))
  }

  for(el in list(...)){
    if (class(el) == "3dChart"){
      plot(el,plotOptions = plotOptions, justSurface = T)
    }
  }
} 

# Examples of usage

f1 <- chart(
  f = 1 + 5 * exp(-arg1 ^ 2 - arg2 ^ 2) ,
  xrange = c(-5, 5),
  yrange = c(-5, 5),
  n = 100,
  col = "blue", 
  alpha = 0.2,
  title3d(main = 'F1 plot title', line = 10, level = 1))

f1

plot(f1)

viewPointMatrix <- matrix(
  c(0.91853338, -0.29720816,  0.26069823,  0.00000000,  0.39085239,  0.58357424,
    -0.71181136,  0.00000000,  0.05941933,  0.75571710,  0.65219706,  0.00000000,
    0.00000000,  0.00000000,  0.00000000,  1.00000000), 4, 4)

plot(f1,
     plotOptions = list(
       par3d(windowRect = 50 + c(100, 100, 1000, 1000)), 
       rgl.bg(color = rgb(.95, .95, .95, .2)), 
       rgl.viewpoint(userMatrix = viewPointMatrix)
     )
)

f2 <- chart(
  sin(arg2 ^ 2) + cos(arg1 * arg2),
  c(-5, 5),
  c(-5, 5),
  n = 100,
  alpha= 0.6, col = "red",
  title3d(main = "F2 plot title", level  = 10, line = 1),
  aspect = c(1, 1, .2),
  xlab = "x values", ylab = "y values")

f2

plot(f2,
     plotOptions = list(
       par3d(windowRect = 50 + c(0, 0, 1000, 1000)),
       rgl.bg(color = rgb(.97, .97, .97, .2)),
       rgl.viewpoint(userMatrix = viewPointMatrix)
     )
)


f1 <- chart(
  f = 1 + 5 * exp(-arg1 ^ 2 - arg2 ^ 2) ,
  xrange = c(-5, 5),
  yrange = c(-5, 5),
  n = 100,
  col = "blue", alpha = 0.6, xlab = "x axis",
  title3d(main = "New F1 plot title", level = 10, line = 1)
)

plot(f1)

plot(f2, f1, 
     plotOptions = list(
       par3d(windowRect = 50 + c(0, 0, 1000, 1000)),
       rgl.bg(color = "white"),
       rgl.viewpoint(userMatrix = viewPointMatrix)
     )
)

f3 <- chart(
  f = arg1 ^ 3 - arg2 ^ 2,
  xrange = c(-2, 2),
  yrange = c(-2, 3),
  n = 100,
  col = "yellow", alpha = 0.9, xlab = "New x axis", ylab = "New y axis",
  aspect = T,
  title3d(main = "F3 plot title", level = 10, line = 1)
)
plot(f3)

plot(f3,f2,f1)
plot(f2,f3,f1)
plot(f1,f2,f3)