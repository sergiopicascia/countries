# Setup
library(tidyverse)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(factoextra)
library(cluster)
library(dendextend)
set.seed(22)

# Import data
df <- read_delim('/Users/sergiopicascia/Desktop/country_profile.csv', ';', na = c('-99', '.../...', '...'))
str(df)
summary(df)
sapply(df, function(x) sum(is.na(x)))

# Removing 'Region' and columns/rows with too many NA
countries <- subset(df, select = (colSums(is.na(df)) < 40))
countries <- countries %>% drop_na()

# Separating columns with two values
countries <- separate(countries, col = `Labour force participation (female/male pop. %)`, 
                      sep = '/', into = c('Labour force participation (female pop. %)', 
                                          'Labour force participation (male pop. %)'))
countries <- separate(countries, col = `Life expectancy at birth (females/males, years)`, 
                      sep = '/', into = c('Life expectancy at birth (females, years)', 
                                          'Life expectancy at birth (males, years)'))
countries <- separate(countries, col = `Population age distribution (0-14 / 60+ years, %)`, 
                      sep = '/', into = c('Population age distribution (0-14, %)',
                                          'Population age distribution (60+ years, %)'))
countries <- separate(countries, col = `International migrant stock (000/% of total pop.)`, 
                      sep = '/', into = c('International migrant stock (x1000)',
                                          'International migrant stock (% of total pop.)'))
countries <- separate(countries, col = `CO2 emission estimates (million tons/tons per capita)`, 
                      sep = '/', into = c('CO2 emission estimates (million tons)',
                                          'CO2 emission estimates (tons per capita)'))
countries <- separate(countries, col = `Pop. using improved drinking water (urban/rural, %)`, 
                      sep = '/', into = c('Pop. using improved drinking water (urban, %)',
                                          'Pop. using improved drinking water (rural, %)'))
countries <- separate(countries, col = `Pop. using improved sanitation facilities (urban/rural, %)`, 
                      sep = '/', into = c('Pop. using improved sanitation facilities (urban, %)',
                                          'Pop. using improved sanitation facilities (rural, %)'))

# Transform each variable to numeric
countries[countries == c('~0', '~0.0')] <- '0'
countries[, -1:-2] <- mutate_all(countries[, -1:-2], function(x) as.numeric(x))
countries[is.na(countries)] <- 0

# Converting countries to row names, region to categorical
countries <- column_to_rownames(countries, 'country')
countries$Region <- as.factor(countries$Region)
countries$Region <- recode(countries$Region, 'CentralAmerica' = 'North America',
                           'NorthernAmerica' = 'North America', 'Caribbean' = 'North America',
                           'SouthAmerica' = 'South America', 'EasternAfrica' = 'Africa',
                           'MiddleAfrica' = 'Africa', 'NorthernAfrica' = 'Africa', 
                           'SouthernAfrica' = 'Africa', 'WesternAfrica' = 'Africa', 
                           'CentralAsia' = 'Asia', 'EasternAsia' = 'Asia', 'South-easternAsia' = 'Asia',
                           'SouthernAsia' = 'Asia', 'WesternAsia' = 'Asia', 'EasternEurope' = 'Europe',
                           'NorthernEurope' = 'Europe', 'SouthernEurope' = 'Europe',
                           'WesternEurope' = 'Europe', 'Melanesia' = 'Oceania', 'Polynesia' = 'Oceania')

summary(countries)

### Principal Component Analysis
countries_pca <- prcomp(countries[, -1], center = T, scale. = T)
summary(countries_pca)
str(countries_pca)

# Visualization
countries_pca$sdev
pca_var <- countries_pca$sdev^2
pve <- pca_var / sum(pca_var)

plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

ggbiplot(countries_pca, obs.scale = 1, var.scale = 1, groups = countries$Region)

ggbiplot(countries_pca, labels = row.names(countries), obs.scale = 1, labels.size = 2,
         var.scale = 1, groups = countries$Region)

ggbiplot(countries_pca, obs.scale = 1, var.scale = 1, groups = countries$Region, choices = c(3, 4))

ggbiplot(countries_pca, labels = row.names(countries), obs.scale = 1, labels.size = 2,
         var.scale = 1, groups = countries$Region, choices = c(3, 4))

# Loadings
loadings_1 <- countries_pca$rotation[, 1]
loadings_1[order(loadings_1)]
loadings_2 <- countries_pca$rotation[, 2]
loadings_2[order(loadings_2)]
loadings_3 <- countries_pca$rotation[, 3]
loadings_3[order(loadings_3)]
loadings_4 <- countries_pca$rotation[, 4]
loadings_4[order(loadings_4)]

# Scores
cov(countries_pca$x)
scores_1 <- countries_pca$x[, 1]
scores_1[order(scores_1)]
scores_2 <- countries_pca$x[, 2]
scores_2[order(scores_2)]
scores_3 <- countries_pca$x[, 3]
scores_3[order(scores_3)]
scores_4 <- countries_pca$x[, 4]
scores_4[order(scores_4)]

### K-means
countries_scaled <- scale(countries[, -1])

# N of clusters 
fviz_nbclust(countries_scaled, FUNcluster = kmeans, method = 'wss', k.max = 20)
fviz_nbclust(countries_scaled, FUNcluster = kmeans, method = 'silhouette', k.max = 20)
gap_stat <- clusGap(countries_scaled, FUN = kmeans, nstart = 25, K.max = 20, B = 50)
fviz_gap_stat(gap_stat)

# k = 2
countries_km_2 <- kmeans(countries_scaled, 2, nstart = 25)
countries_km_2
fviz_cluster(countries_km_2, data = countries_scaled, stand = T, repel = T, pointsize = 1,
             labelsize = 8, ggtheme = ggplot2::theme_minimal())

# k = 6
countries_km_6 <- kmeans(countries_scaled, 6, nstart = 25)
countries_km_6
fviz_cluster(countries_km_6, data = countries_scaled, stand = T, repel = T, pointsize = 1,
             labelsize = 8, ggtheme = ggplot2::theme_minimal())

aggregate(countries[, -1], by = list(cluster = countries_km_6$cluster), mean)

### Hierarchical clustering
fviz_nbclust(countries_scaled, FUNcluster = hcut, method = 'wss', k.max = 20)
fviz_nbclust(countries_scaled, FUNcluster = hcut, method = 'silhouette', k.max = 20)
gap_stat2 <- clusGap(countries_scaled, FUN = hcut, nstart = 25, K.max = 20, B = 50)
fviz_gap_stat(gap_stat2)

# Distancies
dist_euc <- get_dist(countries_scaled, method = 'euclidean')
dist_man <- get_dist(countries_scaled, method = 'manhattan')
fviz_dist(dist_euc, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size = 3)
fviz_dist(dist_man, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size = 3)

# Linkage methods
hclust_avg <- hclust(dist_euc, method = 'average')
plot(hclust_avg, cex = 0.5, hang = -1)
hclust_sing <- hclust(dist_euc, method = 'single')
plot(hclust_sing, cex = 0.5, hang = -1)
hclust_cent <- hclust(dist_euc, method = 'centroid')
plot(hclust_cent, cex = 0.5, hang = -1)
hclust_comp <- hclust(dist_euc, method = 'complete')
plot(hclust_comp, cex = 0.5, hang = -1)
hclust_comp2 <- hclust(dist_man, method = 'complete')
plot(hclust_comp2, cex = 0.5, hang = -1)
hclust_ward <- hclust(dist_euc, method = 'ward.D2')
plot(hclust_ward, cex = 0.5, hang = -1)
hclust_ward2 <- hclust(dist_man, method = 'ward.D2')
plot(hclust_ward2, cex = 0.5, hang = -1)

# Agglomerative coefficient
coef.hclust(hclust_comp)
coef.hclust(hclust_comp2)
coef.hclust(hclust_ward)
coef.hclust(hclust_ward2)

plot(hclust_ward2, cex = 0.5, hang = -1)
rect.hclust(hclust_ward2, k = 6, border = 2:6)

# Dendogram
dend_ward2 <- as.dendrogram(hclust_ward2)
par(cex = 0.2, mar = c(20,4,4,2) + 0.1)
plot(color_branches(dend_ward2, k = 6))

# Divisive HC
div_hclust <- diana(countries_scaled)
div_hclust$dc
pltree(div_hclust, cex = 0.5, hang = -1)
rect.hclust(div_hclust, k = 5, border = 2:6)
rect.hclust(div_hclust, k = 13, border = 2:6)