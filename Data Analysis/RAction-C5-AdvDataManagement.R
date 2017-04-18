options(digits=2)
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
               "David Jones", "Janice Markhammer", "Cheryl Cushing",
               "Reuven Ytzrhak", "Greg Knox", "Joel England",
               "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

#Don't convert students names into factors (can't strsplit later on)
roster<- data.frame(Student,Math,Science,English, stringsAsFactors=FALSE)

#standardize those variable
z<- scale(roster[,2:4])
#apply row mean
score<- apply(z,1,mean)
roster<- cbind(roster,score)

#set the rank range to y
y<- quantile(score,c(.8,.6,.4,.2))
#create variable grade in roster
roster$grade[score >= y[1]] <- "A" 
roster$grade[score < y[1] & score >= y[2]] <- "B" 
roster$grade[score < y[2] & score >= y[3]] <- "C" 
roster$grade[score < y[3] & score >= y[4]] <- "D" 
roster$grade[score < y[4]] <- "F" 

name<- strsplit((roster$Student)," ")
# "[" is a function that extract part of an object
lastname<- sapply(name,"[",2)
firstname<- sapply(name,'[',1)

#drop the Student variable (index=1)
roster<- cbind(firstname,lastname,roster[,-1])

#sort the dataset
roster<- roster[order(lastname,firstname),]
roster