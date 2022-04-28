# Import libraries
library(factoextra)
library(FactoMineR)
library(gplots)
# Read the data
data.ca <- read_excel("Sem3/CPSC/Project/size_CA.xlsx")
head(data.ca)
data.ca$product_size <- data.ca$...1
data.ca <- as.data.frame(data.ca)
# Edit First Column as Rownames of Data
rownames(data.ca) <- data.ca$product_size
data.ca
#Removing staff members column
data.ca$product_size <- NULL
data.ca$...1<- NULL
head(data.ca)
str(data.ca)

# Convert the data into contingency table
dt = as.table(as.matrix(data.ca))
# Create a balloonplot
balloonplot(t(dt), 
            main = 'Sub-Sector of Creative Industries', 
            xlab = '',
            ylab = '',
            label = FALSE, 
            show.margins = FALSE)



chisq = chisq.test(data.ca)
chisq


# Correspondence analysis
res.ca = CA(data.ca, graph = TRUE)
print(res.ca)
# Eigenvalue
eig.val = get_eigenvalue(res.ca)
eig.val

# Create Scree plot
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
# Create symmetric plot of correspondence analysis
fviz_ca_biplot(res.ca, repel = TRUE) # Make correspondence plot


##standard ca using cacoord
res.ca <- ca(data.ca)
standard_ca <- cacoord(res.ca, type = "standard")
summary(standard_ca)
standard_ca

rowcoord <- standard_ca$rows
colcoord <- standard_ca$columns
rowcoord[,c(1,2)]
colcoord[,c(1,2)]





# Read the data
data.ca <- read_excel("Sem3/CPSC/Project/CA.xlsx")
head(data.ca)

data.ca <- as.data.frame(data.ca)
# Edit First Column as Rownames of Data
rownames(data.ca) <- data.ca$Product_colour
data.ca
#Removing staff members column
data.ca$Product_colour <- NULL
head(data.ca)
str(data.ca)

# Convert the data into contingency table
dt = as.table(as.matrix(data.ca))
# Create a balloonplot
balloonplot(t(dt), 
            main = 'Sub-Sector of Creative Industries', 
            xlab = '',
            ylab = '',
            label = FALSE, 
            show.margins = FALSE)



chisq = chisq.test(data.ca)
chisq


# Correspondence analysis
res.ca = CA(data.ca, graph = TRUE)
print(res.ca)
# Eigenvalue
eig.val = get_eigenvalue(res.ca)
eig.val

# Create Scree plot
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 50))
# Create symmetric plot of correspondence analysis
fviz_ca_biplot(res.ca, repel = TRUE) # Make correspondence plot


##standard ca using cacoord
res.ca <- ca(data.ca)
standard_ca <- cacoord(res.ca, type = "standard")
summary(standard_ca)
standard_ca

rowcoord <- standard_ca$rows
colcoord <- standard_ca$columns
rowcoord[,c(1,2)]
colcoord[,c(1,2)]






