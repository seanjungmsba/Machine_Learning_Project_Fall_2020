
# Read in the data

FileName <- "SelfieImageData.csv"
Labs <- scan(file=FileName,what="xx",nlines=1,sep="|")
DataAsChars <- matrix(scan(file=FileName,what="xx",sep="|",skip=1),byrow=T,ncol=length(Labs))
colnames(DataAsChars) <- Labs
dim(DataAsChars)
# size in memory in MBs
as.double(object.size(DataAsChars)/1024/1024)

ImgData <- matrix(as.integer(DataAsChars[,-1]),nrow=nrow(DataAsChars))
colnames(ImgData) <- Labs[-1]
rownames(ImgData) <- DataAsChars[,1]
# size in memory in MBs
as.double(object.size(ImgData)/1024/1024)

# Take a look
ImgData[1:8,1:8]

# Free up some memory just in case
remove(DataAsChars)

# Show each Image
for(whImg in 1:nrow(ImgData)) {
  Img <- matrix(ImgData[whImg,],byrow=T,ncol=sqrt(ncol(ImgData)))
  Img <- apply(Img,2,rev)
  par(pty="s",mfrow=c(1,1))
  image(z=t(Img),col = grey.colors(255),useRaster=T)
  Sys.sleep(1)
}

##########################################################

# Find Average Face
avg <- apply(ImgData, 2, FUN = mean)
new <- matrix(avg, byrow=T, ncol=451)
dim(new) # 451 by 451

new <- apply(new,2,rev)
dim(new) # 451 by 451
par(pty="s",mfrow=c(1,1))
image(z=t(new),col = grey.colors(255),useRaster=T)
title("Q2-AvgFace")

##########################################################

############ Centered Matrix
# 1. subtract the mean from the actual value
# Purpose:
ImgC <- apply(ImgData, 2, function(y) {y-mean(y)})
dim(ImgC) # 28 by 203,401
range(apply(ImgC, 2, mean)) # -1.218200e-14  1.218211e-14

# 2. Multiply by the transpose
# Purpose: to make a matrix of 28 by 28
dim(t(ImgC)) # 203,401 by 28
ImgS <- ImgC%*%t(ImgC)
dim(ImgS) # 28 by 28

# 3. Calculate Eigenvalues and Eigenvectors for small matrix
SDecomp <- eigen(ImgS) # purpose: to get the var-cov matrix of 28-28 matrix
names(SDecomp) # "values" and "vectors"
dim(SDecomp$vectors) #28 by 28

# 4. Calculate Eigenvectors and Eigenvectors for original big matrix
# equations are based off of the powerpoints
EIGENVALUE <- SDecomp$values/(28-1) 

XcTv <- t(ImgC) %*% SDecomp$vectors # numerator
dim(XcTv) # 203,401 by 28
vector_length <- function(x) {sqrt(sum(x^2))}
magnitudes <- apply(XcTv, 2, vector_length) # denominator
EIGENVECTOR <- sweep(XcTv, 2, magnitudes, FUN="/") # division across columns
apply(EIGENVECTOR, 2, vector_length) # confirmation in length of 1
dim(EIGENVECTOR) # 203,401 by 28

# remove(XcTv)

# 5. Plot a Scree Plot
par(mfrow=c(1,2))
plot(EIGENVALUE)
title("Q4-ScreePlot")

# biggest eigenvalue
max(EIGENVALUE) # 65,608,278

# 6. Find which value explains 85 percent of the variance
sum(cumsum(SDecomp$values)/sum(SDecomp$values)<0.85)+1 # 13

# 7. Assign 20 PC
PC20 <- ImgData%*%EIGENVECTOR[,1:20]
dim(PC20) # 28 by 20

# checking the dimension to make sure if matrix multiplication works
dim(EIGENVECTOR[,1:20])
dim(t(PC20))

## Reconstructing images using PC20
Rec20 <- PC20%*%t(EIGENVECTOR[,1:20])
dim(Rec20) # 28 by 203,401

# 8. Find PC20 Face
for(whImg in 1:nrow(Rec20)) {
  ImgPC <- matrix(Rec20[whImg,],byrow=T,ncol=sqrt(ncol(Rec20)))
  ImgPC<- apply(ImgPC,2,rev)
  par(pty="s",mfrow=c(1,1))
  image(z=t(ImgPC),col = grey.colors(255),useRaster=T)
  Sys.sleep(1)
}

##### Q10
face <- EIGENVECTOR[,8]
face <- (face-min(face))/(max(face)-min(face))
face <- face*255
eigenface <- matrix(face, byrow=T, ncol=451)
eigenface <- t(apply(eigenface,2,rev))
?rev
par(pty="s", mfrow=c(1,1))
image(z=eigenface,col = grey.colors(255),useRaster=T)
title("Q10-Eigenface8")

##### Q11
for (x in 1:20) {
  face <- EIGENVECTOR[,x]
  face <- (face-min(face))/(max(face)-min(face))
  face <- face*255
  eigenface <- matrix(face, byrow=T, ncol=451)
  eigenface <- t(apply(eigenface,2,rev))
  ?rev
  par(pty="s", mfrow=c(1,1))
  image(z=eigenface,col = grey.colors(255),useRaster=T)
  tmp <- scan()
}