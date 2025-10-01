##################################
### Cadenza ######################
### CAD1 - Listener Panel #######
### Clean Analysis ###############
### Scott Bannister, 14/11/2024 ##
##################################

#############################
### PRE-AMBLE AND DATA PREP
#############################

# Reset and clear memory
rm(list=ls(all=TRUE))

# Set working directory
setwd('Your Directory Here')

# Use custom "libraries.R" script to load required R packages
source('libraries.R')

# Import .xlsx data file
df <- readxl::read_excel('CAD1_data_and_acoustics.xlsx')

# Rescale variables and attribute dimensions to 0-1
df <- df %>%
  mutate(clarity = rescale(clarity, to = c(0, 1)), 
         harshness = rescale(harshness, to = c(0, 1)),
         distortion = rescale(distortion, to = c(0, 1)),
         frequency_balance = sin(pi*frequency_balance/100), # Requires different approach (bipolar scale)
         audio_quality_overall = rescale(audio_quality_overall, to = c(0, 1)),
         preference = rescale(preference, to = c(0, 1)))

# Change participant ID to string, for easier groupings and modelling
df$participant_id <- as.character(df$participant_id) 

# Demarcate baseline and control ML systems from submitted entrant systems
df <- df %>%
  mutate(type = ifelse(Team %in% c('E001', 'E021'), 'Control', 'System'))

# Collapse HL severity into 3 levels (assimilate small "no impairment" and "severe" groups)
df <- df %>%
  mutate(across('Severity', str_replace, 'No impairment', 'Mild')) %>%
  mutate(across('Severity', str_replace, 'Severe', 'Moderately severe'))

####################
### Descriptives
####################

# Generate basic histogram for Basic Audio Quality (BAQ) distribution; note the zero inflation
hist(df$audio_quality_overall, breaks = 100, main = 'BAQ Distribution', xlab = "BAQ", ylab = "Total Responses")

# Split BAQ data into bins, for boxplot visualisation vs. HAAQI
df$baq <- cut(df$audio_quality_overall, breaks = 10, labels = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, '1.0'))

# Generate the boxplot
ggplot(df, aes(x = baq, y = remix_score, fill = baq)) +
  geom_boxplot(color = 'grey40', outlier.alpha = 0.3) + # Specify boxplot
  scale_fill_viridis_d(option = "B") +  # Set colour scheme for plot
  xlab('BAQ Bins') +  # x-axis label
  ylab('HAAQI') + # y-axis label
  facet_wrap(.~Team) +  # split plot into 8 (for the 8 systems)
  theme_pubclean() +  # Set theme scheme
  theme(axis.title = element_text(face = 'bold', size = 14), axis.text = element_text(size = 8, face = 'bold'), 
        strip.text = element_text(face = 'bold', size = 10), legend.position = 'none')  # Modify the presentation

### Calculate spearman rank correlations between BAQ and HAAQI, for each system (just change filter condition)

# Loop to generate correlations
spearman <- NULL
for (k in 1:length(unique(df$Team))){
  label <- unique(df$Team)
  corr <- df %>%
    filter(Team == label[k])
  ans <- cor.test(corr$audio_quality_overall, corr$remix_score, method = 'spearman')
  output <- cbind(label[k], ans$estimate, round(ans$p.value, digits = 4))
  spearman <- as.data.frame(rbind(spearman, output))
}

# print correlations
spearman <- spearman %>%
  rename(System = V1,  Correlation = V2, P_Value = V3)
print(spearman, row.names = FALSE)

### Basic descriptives

# Attribute Means by system
df %>%
  group_by(Team) %>%
  summarise(BAQ = round(mean(audio_quality_overall, na.rm=T), digits = 2), 
            Clarity = round(mean(clarity, na.rm = T), digits = 2), 
            Harshness = round(mean(harshness, na.rm = T), digits = 2), 
            Distortion = round(mean(distortion, na.rm = T), digits = 2), 
            Freq_Bal = round(mean(frequency_balance, na.rm = T), digits = 2), 
            Liking = round(mean(preference, na.rm = T), digits = 2))

# Attribute Means by HL severity
df %>%
  group_by(Severity) %>%
  summarise(BAQ = round(mean(audio_quality_overall, na.rm=T), digits = 2), 
            Clarity = round(mean(clarity, na.rm = T), digits = 2), 
            Harshness = round(mean(harshness, na.rm = T), digits = 2), 
            Distortion = round(mean(distortion, na.rm = T), digits = 2), 
            Freq_Bal = round(mean(frequency_balance, na.rm = T), digits = 2), 
            Liking = round(mean(preference, na.rm = T), digits = 2))

# Intercorrelations between perceptual attributes
viz <- df %>%
  dplyr::select(clarity:audio_quality_overall)
viz <- cor(viz, use = "complete.obs")
ggcorrplot(viz, type = 'upper', hc.order = TRUE, lab = TRUE)

##############################################
### BAQ scores across system and HL severity
##############################################

### General linear mixed model approach (allows us to use all of the data)

# Fit GLMM with ordered beta regression method (Kubinec, 2023) - Use "glmmTMB" package
# This is to deal with a zero-inflated bounded continuous distribution (i.e., BAQ data)
glmm <- glmmTMB(audio_quality_overall ~ Team * Severity + (1|participant_id) + (1|Song),
                data = df, family = ordbeta(link = 'logit'))

### Fit standard LMM alternative for model comparisons - use "lmerTest" package
lmm <- lmer(audio_quality_overall ~ Team * Severity + (1|participant_id) + (1|Song), 
            data = df, control = lmerControl(optimizer = 'bobyqa'))

### Diagnostics

# Get overall indices of model - using "modelsummary" package
modelsummary(list(glmm, lmm)) 

# Compare performance metrics between models - using "performance" package
compare_performance(glmm, lmm, rank = TRUE, verbose = FALSE) # LMM shows better "performance" (via weights), but concerns over bias

check_model(glmm)   # Visually check assumptions of model

check_model(lmm)    # We can check assumptions of LMM approach too

glmm_res <- simulateResiduals(fittedModel = glmm, n = 1000, plot = F)
plot(glmm_res, quantreg = TRUE) # Check residuals, dispersion, outliers

lmm_res <- simulateResiduals(fittedModel = lmm, n = 1000, plot = F)
plot(lmm_res, quantreg = TRUE)  # Compare with LMM modelling approach

# Summary
summary(glmm) # Check model characteristics, parameters, effects structures
Anova(glmm)   # Capture broad effects of ML system, HL severity, and interactions

### Post-hoc comparisons - using "emmeans" package

# System effects
emmeans(glmm, ~ Team, type = 'response')
pairs(emmeans(glmm, ~ Team, type = 'response')) 
confint(pairs(emmeans(glmm, ~ Team, type = 'response')))

# HL Severity effects
emmeans(glmm, ~ Severity, type = 'response')
pairs(emmeans(glmm, ~ Severity, type = 'response')) 
confint(pairs(emmeans(glmm, ~ Severity, type = 'response')))

# System * HL Severity interaction effects
emmeans(glmm, ~ Severity|Team, type = 'response')
pairs(emmeans(glmm, ~ Severity|Team, type = 'response')) 
confint(pairs(emmeans(glmm, ~ Severity|Team, type = 'response')))

### Visualise results

# Visualise BAQ scores by system
df %>%
  group_by(Team) %>%
  summarise_at(vars(audio_quality_overall), funs(mean(., na.rm=TRUE)))

ggplot(df, aes(x = Team, y = audio_quality_overall, fill = type)) +
  geom_boxplot(color = 'black') +
  stat_summary(fun.y=mean, geom="point", shape=20, size=8, color="black", fill="black") +
  scale_fill_manual(values = c('white', 'grey80')) +
  xlab('System') +
  ylab('BAQ') +
  theme_bw() +
  theme(axis.title = element_text(face = 'bold', size = 16), title = element_text(face = 'bold', size = 18), 
        legend.position = 'none', axis.text = element_text(size = 11), 
        axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = 'bold', size = 12))

# Visualise BAQ scores by HL severity
df %>%
  group_by(Severity) %>%
  summarise_at(vars(audio_quality_overall), funs(mean(., na.rm=TRUE)))

ggplot(df, aes(x = Severity, y = audio_quality_overall)) +
  geom_boxplot(color = 'black') +
  stat_summary(fun.y=mean, geom="point", shape=20, size=8, color="black", fill="black") +
  xlab('HL Severity') +
  ylab('BAQ') +
  theme_bw() +
  theme(axis.title = element_text(face = 'bold', size = 16), title = element_text(face = 'bold', size = 18), 
        legend.position = 'none', axis.text = element_text(size = 11), 
        axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = 'bold', size = 12))

# Interaction effects (we see that systems E016 - E022 start to outperform others at more severe HL)
ggplot(df, aes(x = Team, y = audio_quality_overall, fill = Team)) +
  geom_boxplot(color = 'grey20', outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "B") +
  facet_wrap(~Severity, scales = 'free_x') +
  scale_y_continuous(breaks = seq(0.00, 1.00, 0.25), limits = c(0, 1.10)) +
  ggtitle('') +
  xlab('ML System') +
  ylab('Audio Quality Rating') +
  theme_pubclean() +
  theme(axis.title = element_text(face = 'bold', size = 16), title = element_text(face = 'bold', size = 18), 
        legend.position = 'none', axis.text = element_text(size = 11), 
        axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = 'bold', size = 12))

##############################################
### Perceptual Attributes and Signal Features
##############################################

### Prepare data for PCA dimensionality reduction

# Extract the audio signal features, and normalise
df2 <- na.omit(df) # Remove rows with missing values to enable PCA
dfNorm <- df2 %>%
  dplyr::select(!Distortion) %>%  # Distortion feature not used; remove
  dplyr::select(19:32)  # Select remaining signal features
dfNorm <- as.data.frame(scale(dfNorm, center = TRUE, scale = TRUE)) # Normalise

### PCA Preparation

# Check assumptions for PCA
data_matrix = cor(dfNorm, use = 'complete.obs') # Correlation matrix
round(data_matrix, 2) # Remove any variables with a correlation over .90 - "Rolloff" and "Spread" have multiple...
ggcorrplot(data_matrix, type = 'upper', hc.order = TRUE, lab = TRUE)  # Visualise correlations
det(data_matrix)  # Falls below .00001
psych::cortest.bartlett(dfNorm) # Significant, suggesting intercorrelations (good)
psych::KMO(dfNorm)  # Need to evaluate and remove scores lower than 0.60

# Data cleaning after assumptions check
dfNorm <- dfNorm %>%
  dplyr::select(!c(SpecKurt, SpecRollOff, SpecSpread, Rms, SpecSlope)) # Remove variables

# Now re-check assumptions
data_matrix = cor(dfNorm, use = 'complete.obs') # Correlation matrix
round(data_matrix, 2) # No correlations above .90
ggcorrplot(data_matrix, type = 'upper', hc.order = TRUE, lab = TRUE)  # Visualise
det(data_matrix)  # .00006; good
psych::cortest.bartlett(dfNorm) # Significant, suggesting intercorrelations (good)
psych::KMO(dfNorm)  # 'crest factor' is < 0.60 - consider removal

# Further data cleaning
dfNorm <- dfNorm %>%
  dplyr::select(!c(CrestFactor))  # Remove variable

# Re-check assumptions
data_matrix = cor(dfNorm, use = 'complete.obs') # Correlation matrix
det(data_matrix)  # .0003
psych::cortest.bartlett(dfNorm) # Significant, suggesting intercorrelations (good)
psych::KMO(dfNorm)  # All items > 0.60 MSA

### Run PCA - using "FactoMineR" package

# Run PCA
pcdat <- PCA(dfNorm)  

# Get eigenvalues
pcdat$eig   

# Scree plot (suggesting 2 components to retain)
fviz_eig(pcdat, addlabels = TRUE, ylim = c( 0, 60)) 

# Run parallel analysis (suggesting two components)
paran(dfNorm)

# Check communalities (easiest to replicate using "principal" function)
pcmax <- psych::principal(dfNorm, nfactors = 2, rotate = 'none')
pcmax$communality # All above 0.5 (good)
rm(pcmax)

### Visualise PCA

# Visualise variance and contribution
var <- get_pca_var(pcdat) # Get key data from PCA model
corrplot::corrplot(var$cos2, is.corr = FALSE) # Plot cos2
corrplot::corrplot(var$contrib, is.corr = FALSE)  # Plot contribution of variables to PCs

# Plot dimensions and cos2 values for signal features
fviz_pca_var(pcdat, axes = c(1,2), col.var = "cos2", 
             gradient.cols = c('#FDE725FF', '#1F968BFF', '#440154FF'), repel = TRUE)

# Visualise contribution of signal features to each principal component
a <- fviz_contrib(pcdat, choice = "var", axes = 1)
b <- fviz_contrib(pcdat, choice = "var", axes = 2)
gridExtra::grid.arrange(a, b, ncol=2, top='Contribution of the variables to the first two PCs')

### Explore perceptual attributes and BAQ in the PCA, as supplementary variables

# Extract perceptual attributes from data
attribs <- df2[, 13:17]

# Calculate values for perceptual attributes
quanti.coord <- cor(attribs, pcdat$x)
quanti.cos2 <- quanti.coord^2

# Plot PCA, and add perceptual attributes
p <- fviz_pca_var(pcdat, axes = c(1,2), col.var = 'black', repel = TRUE, labelsize = 5) +
  theme()
fviz_add(p, quanti.cos2, color ="blue", geom="arrow", repel = TRUE, labelsize = 5)

### Perform the final PCA with varimax rotation for improved interpretability
pcmax <- psych::principal(dfNorm, nfactors = 2, rotate = 'varimax')
pcmax$scores
pcmax$fit
pcmax$loadings

# Add PC scores to dataset for further analysis and modelling
df2 <- df2 %>%
  add_column(pcmax$scores[,1]) %>%
  add_column(pcmax$scores[,2])
names(df2)[names(df2) == 'pcmax$scores[, 1]'] <- 'RC1'
names(df2)[names(df2) == 'pcmax$scores[, 2]'] <- 'RC2'
df2$RC1 <- scale(df2$RC1, center = TRUE, scale = TRUE)
df2$RC2 <- scale(df2$RC2, center = TRUE, scale = TRUE)

### Characterise ML systems based on PC components

# Get data ready for ggplot
viz <- df2 %>%
  dplyr::select(c(Team, Severity, RC1, RC2)) %>%
  group_by(Team, Severity) %>%
  pivot_longer(!c(Team, Severity), names_to = 'RCs', values_to = 'scores')

# Plot
ggplot(viz, aes(x = RCs, y = scores, fill = Severity)) +
  geom_boxplot(color = 'black', outlier.alpha = 0.1) +
  scale_fill_manual(values = c('#58508d', '#bc5090', '#ffa600')) +
  xlab('Rotated Principal Components') +
  ylab('RC Scores') +
  facet_wrap(.~Team, nrow = 2, ncol = 4) +
  theme_pubclean() +
  theme(axis.title = element_text(face = 'bold', size = 16), 
        title = element_blank(), strip.text = element_text(face = 'bold', size = 12))

### Zoom in on PC2 scores for the systems; connections to BAQ?
zoom <- df2

# Rescale only to aid in the visualisation
zoom$BAQ <- rescale(zoom$audio_quality_overall, to = c(0, 1))  
zoom$RC2 <- rescale(zoom$RC2, to = c(0, 1))

# Prepare variable for ggplot
viz <- zoom %>%
  dplyr::select(c(Team, Severity, BAQ, RC2)) %>%
  group_by(Team, Severity) %>%
  pivot_longer(!c(Team, Severity), names_to = 'vars', values_to = 'scores')

# Plot
ggplot(viz, aes(x = vars, y = scores, fill = Severity)) +
  geom_boxplot(color = 'black', outlier.alpha = 0.1) +
  scale_fill_manual(values = c('#58508d', '#bc5090', '#ffa600')) +
  xlab('BAQ and RC2') +
  ylab('Rescaled Scores') +
  facet_wrap(.~Team, nrow = 2, ncol = 4) +
  theme_pubclean() +
  theme(axis.title = element_text(face = 'bold', size = 16), 
        title = element_blank(), strip.text = element_text(face = 'bold', size = 12))

###################################################################
# Run GLMM with PCs as predictors of basic audio quality?
###################################################################

# First model = include just the first PC
model01 <- glmmTMB(audio_quality_overall ~ RC1 + (1|participant_id) + (1|Song), data = df2, family = ordbeta(link = 'logit'))

# Second model = include both PCs
model02 <- glmmTMB(audio_quality_overall ~ RC1 + RC2 + (1|participant_id) + (1|Song), data = df2, family = ordbeta(link = 'logit'))

# Alternative model = LMM approach with both PCs
lmm <- lmer(audio_quality_overall ~ RC1 + RC2 + (1|participant_id) + (1|Song), data = df2, control = lmerControl(optimizer = 'bobyqa'))

# Diagnostics
compare_performance(model01, model02, lmm, rank = TRUE, verbose = FALSE)
modelsummary(list(model01, model02, lmm))
check_model(model02)   # Visually check assumptions and performance
check_model(lmm)       # Do the same for the LMM approach

glmm_res <- simulateResiduals(fittedModel = model02, n = 1000, plot = F)
plot(glmm_res, quantreg = TRUE) # Check residuals, dispersion, outliers
lmm_res <- simulateResiduals(fittedModel = lmm03, n = 1000, plot = F)
plot(lmm_res, quantreg = TRUE)  # Check for LMM approach

summary(model02)
