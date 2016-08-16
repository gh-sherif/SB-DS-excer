# Load list
dat = read.csv("/Users/Sherif/Documents/Springboard - Data science/refine_original.csv", header = TRUE)
str(dat)  # data frame

getwd()

library(dplyr)
glimpse(dat9)
View(dat9)

# Clean up brand name
dat2 <- data.frame(lapply(dat, as.character), stringsAsFactors=FALSE) # to character

dat3 <- data.frame(lapply(dat2["company"], toupper), select(dat2, -company)) # to upper

install.packages("car")
library(car)

dat4 <- recode(dat3$company,"c('PHILLIPS', 'PHLLIPS', 'PHILLPS', 'FILLIPS', 'PHLIPS')='PHILIPS'; c('AKZ0', 'AK ZO')='AKZO'; c('UNILVER')='UNILEVER'") # replace

dat5 <- data.frame(company = dat4, select(dat3, -company)) # concatenate

# Separate product code and number
list <- strsplit(dat5$Product.code...number, "-")

library(plyr)

prd <- ldply(list)

colnames(prd) <- c("product_name", "product_code")

dat6 <- data.frame(select(dat5, -Product.code...number), prd) # concatenate


# add product category
dat7 <- dat6 %>% 
  mutate(product_category = ifelse(product_name == 'p', 'Smart phone',
                    ifelse(product_name == 'v', 'TV',
                           ifelse(product_name == 'x', 'Laptop',
                                  ifelse(product_name == 'q', 'Tablet', NA))))) 

# add full address
install.packages("tidyr")
library(tidyr)
dat8 <- unite(dat7, "full_address", address, city, country, sep = ", ", remove = FALSE)


# add binary fields
distinct(dat8["product_category"])

dat9 <- dat8 %>% 
  mutate(
            company_philips = ifelse(company == 'PHILIPS', TRUE, FALSE),
            company_akzo = ifelse(company == 'AKZO', TRUE, FALSE),
            company_van_houten = ifelse(company == 'VAN HOUTEN', TRUE, FALSE),
            company_unilever = ifelse(company == 'UNILEVER', TRUE, FALSE),
            product_smartphone = ifelse(product_category == 'Smart phone', TRUE, FALSE),
            product_laptop = ifelse(product_category == 'Laptop', TRUE, FALSE),
            product_tv = ifelse(product_category == 'TV', TRUE, FALSE),
            product_tablet = ifelse(product_category == 'Tablet', TRUE, FALSE)
         ) 

# Write CSV
getwd()
setwd("/Users/Sherif/Documents/Springboard - Data science")
write.csv(dat9, file = "refine_clean.csv")


############################################

# test

dat2[dat2 %in% c("PHILLIPS", "PHLLIPS", "PHILLPS", "FILLIPS", "PHLIPS")] <- "PHILIPS"

toupper(dat2["company"])



str(dat)

distinct(dat2["company"])

dat4 <- c(company = replace(dat3$company, dat3$company %in% c("PHILLIPS", "PHLLIPS", "PHILLPS", "FILLIPS", "PHLIPS"), "PHILIPS"))




x <- data.frame(a = c(0,1,2,NA), b = c(0,NA,1,2), c = c(NA, 0, 1, 2)) 

x$a <- replace(x$a, is.na(x$a), 0)

x$b <- replace(x$b, x$b==2, 333)

