
# ------------------------------------------------------------------------------------------------------
#### SET UP
# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(rattle)
library(ranger)
library(Hmisc)
library(kableExtra)

# set data dir, load theme and functions
source("codes/gabor_textbook/theme_bg.R")
source("codes/gabor_textbook/da_helper_functions.R")

#location folders
data_in  <- "data/clean/"
output <- "output(s)/"

options(digits = 3)



########################################
# PART I.
########################################

# !!! make sure you have run airbnb-rio-prepare.R before


#############
# Load data #
#############

# Used area
area <- "copacabana"
data <-
  read_csv(paste0(data_in, "airbnb_", area, "_workfile_adj.csv")) %>%
  mutate_if(is.character, factor)


######################
# Quick look at data #
######################
glimpse(data)
skim(data)

N=nrow(data)
N
# N=5584

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

#####################################
# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type`, `room_type`?
data %>%
  group_by(property_type, room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(data$ln_price)


# Distribution of price by type

# Histograms
# price
price_density <- ggplot(data=data, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 50) +
  #geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
#  coord_cartesian(xlim = c(0, 400)) +
  labs(x = "Price (Brazilian reals)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
    scale_x_continuous(expand = c(0.00,0.00),limits=c(0,6000), breaks = seq(0,6000, 500)) +
  theme_bg()+
  ggtitle('Distribution of apartment prices in Copacabana (level scale)')
price_density

# lnprice
ln_price_density<- ggplot(data=data, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.2) +
  #  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.18,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(3.5, 8.7)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(3.3,8.9, 0.6)) +
  labs(x = "ln(price, Brazilian reals)",y = "Percent")+
  theme_bg()+
  ggtitle('Distribution of apartment prices in Copacabana (log scale)')
ln_price_density



## Boxplot of price by room type
price_vs_room_box <- ggplot(data = data, aes(x = room_type, y = price)) +
  stat_boxplot(aes(group = room_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = room_type),
               color = c(color[2],color[1]), fill = c(color[2],color[1]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,2000), breaks = seq(0,6000,500)) +
  labs(x = "Room type",y = "Price (Brazilian reals)")+
  theme_bg()
price_vs_room_box
ggsave(paste0(output, "price_vs_room_boxplot.png"), width=mywidth_small, height=myheight_small, units = "cm", dpi = 1200)

# Boxplot
g5 <- ggplot(data, aes(x = factor(accommodates), y = price,
                        fill = factor(property_type), color=factor(property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
    stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
    scale_color_manual(name="",
                     values=c(color[2],color[1],color[3],color[4])) +
  scale_fill_manual(name="",
                     values=c(color[2],color[1],color[3],color[4])) +
  labs(x = "Accomodates (Persons)",y = "Price (Brazilian reals)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 3000), breaks = seq(0,3000, 500))+
  theme_bg() +
  theme(legend.position = c(0.1,0.8)        )
g5
ggsave(paste0(output, "price_accommodates_room_boxplot.png"), width=mywidth_verylarge, height=myheight_verylarge, units = "cm", dpi = 1200)


########################################
# PART II.
########################################


#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("accommodates", "ln_beds", "property_type", "room_type", "f_bathroom")

# Factorized variables
reviews <- c("f_number_of_reviews","review_scores_rating", "flag_review_scores_rating")
host_characteristics <- c("host_response_rate","flag_host_response_rate","host_acceptance_rate","flag_host_acceptance_rate","host_is_superhost","host_has_profile_pic","host_identity_verified", "f_host_listings_count")
nights_add <- "f_minimum_nights"

amenities <- paste(colnames(data[21:87]), sep = ",")

# create dummy vars
dummies <- names(data)[seq(21,87)]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
data <- data %>% select(-dummies)
# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)

#################################################
# Look for interactions
################################################

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "accommodates", "room_type", "No. of people it accommodates", "Room type") # no interaction needed
p2 <- price_diff_by_variables2(data, "property_type", "room_type", "Property type", "Room type") # no interaction needed
#Look up listings count interactions
p3 <- price_diff_by_variables2(data, "f_host_listings_count", "host_is_superhost", "Host's property listings", "Host is Superhost") # interaction needed - superhost starts to count as of 10+ listings (count as in higher price)
p4 <- price_diff_by_variables2(data, "f_host_listings_count", "host_identity_verified", "Host's property listings", "Host Identity Verified") # interaction needed - cannot ask a high price when no listings and not verified
#Look up number of reviews interactions
p5 <- price_diff_by_variables2(data, "f_number_of_reviews", "host_is_superhost", "Number of reviews", "Host is Superhost") # no interaction
#Look up minimum nights interactions
p6 <- price_diff_by_variables2(data, "f_minimum_nights", "room_type", "Minimum nights stay", "Room type") # no interaction
#Look at property type interactions with diff amenities
p7 <- price_diff_by_variables2(data, "property_type", "d_pool_agg", "Property type", "Listing has a pool") # interaction between pool and having an entire apartment (price goes up)
p8 <- price_diff_by_variables2(data, "property_type", "d_patioorbalcony", "Property type", "Listing has patio or balcony") # no interaction

g_interactions <- plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2)
g_interactions
#save_fig("ch14_airbnb_interactions",output,"verylarge")
save_fig("airbnb-rio-interactions",output,"verylarge")

g_interactions2 <- plot_grid(p5, p6, p7, p8, nrow = 2, ncol = 2)
g_interactions2
save_fig("airbnb-rio-interactions_part2",output,"verylarge")


# dummies suggested by graphs
X1  <- c("f_host_listings_count*host_is_superhost",  "f_host_listings_count*host_identity_verified")
X2  <- "f_host_listings_count*d_pool_agg"

# Additional interactions of factors and dummies
X3  <- c(paste0("(room_type + accommodates) * (",
                paste(amenities, collapse=" + "),")"))

# Create models in levels models: 1-8
modellev1 <- " ~ accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev, collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev, host_characteristics, X1),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev, amenities, X3),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev, reviews, nights_add), collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev, reviews, nights_add, host_characteristics, X1),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev, reviews, host_characteristics, nights_add, X1, X2, amenities, X3),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)


##############################
#   cross validation OLS    #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20180124)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:8)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")

  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))

  # Initialize values
  rmse_train <- c()
  rmse_test <- c()

  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared

  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)

    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)

  }

  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)

t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                 "Test RMSE")

knitr::kable(t1)

# -In sample rmse: average on training data; avg test : average on test data
t14_2 <- t1 %>%
    select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
xtable(t14_2, type = "html")

library(huxtable)
quick_html(t14_2, file = paste0(output,"regression_models_comparison.html"))

# library(tableHTML)
# write_tableHTML(tableHTML(t1), file = paste0(output,"regression_models_comparison.html"))

# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_y_continuous(name = "RMSE", limits = c(400, 500), breaks = seq(400,500, 10)) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()
model_result_plot_levels
save_fig("model_RMSE_by_coefficients", output, "small")



#################################
#           LASSO               #
#################################

# take model 8
vars_model_8 <- c("price", basic_lev, reviews, host_characteristics, nights_add, X1, X2, amenities, X3)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_8, "price"), collapse = " + ")))

set.seed(1234)
lasso_model <- caret::train(formula,
                      data = data_work,
                      method = "glmnet",
                      preProcess = c("center", "scale"),
                      trControl = train_control,
                      tuneGrid = tune_grid,
                    na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])
# best choice would be to pick the LASSO output as it has the best RMSE out of all

#########################################################################################
#
# RANDOM FORESTS -------------------------------------------------------
#
#########################################################################################

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# set tuning
tune_grid <- expand.grid(
  .mtry = c(5, 7, 9),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)

# Define the two sets of predictors for Random Forest without interactions
predictors_1 <- c(basic_lev, reviews, nights_add, host_characteristics) # equivalent of model 7 without interactions
predictors_2 <- c(basic_lev, reviews, host_characteristics, nights_add, amenities) # equivalent of model 8 without interactions


# simpler model for model A (1)
set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_1

# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2

# auto tuning first
set.seed(1234)
system.time({
  rf_model_2auto <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    importance = "impurity"
  )
})
rf_model_2auto

# rf_model_2auto <-rf_model_2


# evaluate random forests -------------------------------------------------

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2,
    model_2b = rf_model_2auto
    
  )
)
summary(results)

# Save outputs -------------------------------------------------------

# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

kable(x = rf_tuning_modelB, format = "html", digits = 2, caption = "CV RMSE") %>%
  add_header_above(c(" ", "vars" = 3)) %>%
  cat(.,file= paste0(output,"rf_tuning_modelB.html"))


# Tuning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_2auto$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size,
  rf_model_2auto$finalModel$min.node.size
  
),
nrow=3, ncol=2,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c("Min vars","Min nodes"))
)
kable(x = result_1, format = "html", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_tuning_choices.html"))

# Tuning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`),
                     mean(results$values$`model_2b~RMSE`)
),
nrow=3, ncol=1,
dimnames = list(c("Model A", "Model B","Model B auto"),
                c(results$metrics[2]))
)


kable(x = result_2, format = "html", digits = 3) %>%
  cat(.,file= paste0(output,"rf_models_rmse.html"))


########################################
# PART III - Diagnostics of OLS
########################################

model7_level <- model_results_cv[["modellev7"]][["model_work_data"]]
model8_level <- model_results_cv[["modellev8"]][["model_work_data"]]


# look at holdout RMSE
model7_level_work_rmse <- mse_lev(predict(model7_level, newdata = data_work), data_work[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse <- mse_lev(predict(model7_level, newdata = data_holdout), data_holdout[,"price"] %>% pull)**(1/2)
model7_level_holdout_rmse

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])


# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price using non-extreme values -> fit of model for "normal" prices
level_vs_pred_normal <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 1000, yend =1000), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 1000), ylim = c(0, 1000)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 1000), breaks=seq(0, 1000, by=100)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 1000), breaks=seq(0, 1000, by=100)) +
  labs(y = "Price (Brazilian reals)", x = "Predicted price  (Brazilian reals)") +
  theme_bg() 
level_vs_pred_normal
save_fig("predicted_versus_actual_price_limit-1000", output, "small")

# Plot predicted vs price also using extreme values -> fit of model for all prices
level_vs_pred_all <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 5000, yend =5000), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(0, 5000)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks=seq(0, 5000, by=500)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks=seq(0, 5000, by=500)) +
  labs(y = "Price (Brazilian reals)", x = "Predicted price  (Brazilian reals)") +
  theme_bg() 
level_vs_pred_all
save_fig("predicted_versus_actual_price_noLimit", output, "small")

# Plot mean predicted price according to number of accommodates; visualizing 95% CI

CI_95_accommodates <- ggplot(predictionlev_holdout_summary, aes(x=factor(accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  #geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (Brazilian reals)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
CI_95_accommodates
save_fig("95_ConfInterval_accommodates", output, "small")

# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model7_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
            conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "html", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= paste0(output, "modellev7_holdout_summary.html"))

# Plot mean predicted price according to number of accommodates; visualizing 80% PI

PI_80_accommodates <- ggplot(predictionlev_holdout_summary, aes(x=factor(accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (Brazilian reals)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
PI_80_accommodates
save_fig("80_PredInterval_accommodates", output, "small")

#########################################################################################
#
# PART IV - Diagnostics of RF -------------------------------------------------------
#
#########################################################################################

# create the predicted price from random forest model 2 into a new holdout dataframe
# use for fitted versus actual outcome plots

data_holdout_w_rf_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))

# # FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
# ###################################################

# Plot predicted vs price using non-extreme values -> fit of model for "normal" prices
rf_level_vs_pred_normal <- ggplot(data = data_holdout_w_rf_prediction) +
  geom_point(aes(y=price, x=predicted_price), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 1000, yend =1000), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 1000), ylim = c(0, 1000)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 1000), breaks=seq(0, 1000, by=100)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 1000), breaks=seq(0, 1000, by=100)) +
  labs(y = "Price (Brazilian reals)", x = "Predicted price  (Brazilian reals)") +
  theme_bg() 
rf_level_vs_pred_normal
save_fig("random-forest_predicted_versus_actual_price_limit-1000", output, "small")

# Plot predicted vs price also using extreme values -> fit of model for all prices
rf_level_vs_pred_all <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 5000, yend =5000), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 5000), ylim = c(0, 5000)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks=seq(0, 5000, by=500)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 5000), breaks=seq(0, 5000, by=500)) +
  labs(y = "Price (Brazilian reals)", x = "Predicted price  (Brazilian reals)") +
  theme_bg() 
rf_level_vs_pred_all
save_fig("random-forest_predicted_versus_actual_price_noLimit", output, "small")


#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


# variable importance plot
# 1) full varimp plot, full
# 2) varimp plot grouped
# 3) varimp plot , top 10
# 4) varimp plot  w copy, top 10


rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  # mutate(varname = gsub("property_type", "Property Type:", varname) ) %>%
  # mutate(varname = gsub("room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


##############################
# 1) full varimp plot, above a cutoff
##############################

# to have a quick look
plot(varImp(rf_model_2))

cutoff = 600
rf_model_2_var_imp_plot <- ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
                                  aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1.5) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_2_var_imp_plot
save_fig("rf-varimp-base-plot",output, "verylarge")

##############################
# 2) full varimp plot, top 10 only
##############################


# have a version with top 10 vars only
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_plot_b
save_fig("rf-varimp-top10-plot",output, "small")


##############################
# 3) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model_2$finalModel$xNames
f_bathroom_varnames <- grep("f_bathroom",varnames, value = TRUE)
f_numer_of_reviews_varnames <- grep("f_number_of_reviews",varnames, value = TRUE)
f_minimum_nights_varnames <- grep("f_minimum_nights",varnames, value = TRUE)
f_property_type_varnames <- grep("property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("room_type",varnames, value = TRUE)
f_host_listings_count_varnames <- grep("f_host_listings_count",varnames, value = TRUE)


groups <- list(f_bathroom=f_bathroom_varnames,
               f_numer_of_reviews = f_numer_of_reviews_varnames,
               f_minimum_nights = f_minimum_nights_varnames,
               f_property_type = f_property_type_varnames,
               f_room_type = f_room_type_varnames,
               f_host_listings_count = f_host_listings_count_varnames,
               review_scores_rating = "review_scores_rating",
               accommodates = "accommodates",
               ln_beds = "ln_beds",
               host_acceptance_rate = 'host_acceptance_rate',
               host_response_rate = 'host_response_rate',
               host_is_superhost = 'host_is_superhost',
               host_identity_verified = 'host_identity_verified',
               pool_dummy = 'd_pool_agg')

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_2_var_imp_grouped_plot <-
  ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_grouped_plot
save_fig("rf-varimp-group-plot",output, "large")

###########################################################################
# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set.
###########################################################################

######### create nice summary table of heterogeneity
a <- data_holdout_w_rf_prediction %>%
  mutate(is_low_size = ifelse(accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )


b <- data_holdout_w_rf_prediction %>%
  group_by(f_number_of_reviews) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_rf_prediction %>%
  group_by(room_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_rf_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Apartment type", "", "", "")
line2 <- c("Room type", "", "", "")
line3 <- c("Number of reviews", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

options(knitr.kable.NA = '')
kable(x = result_3, format = "html", booktabs=TRUE, linesep = "",digits = c(0,2,1,2), col.names = c("","RMSE","Mean price","RMSE/price")) %>%
  cat(.,file= paste0(output, "performance_across_subsamples.html"))
options(knitr.kable.NA = NULL)

##########################################