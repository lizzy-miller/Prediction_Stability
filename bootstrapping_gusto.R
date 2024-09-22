library(ProSGPV)
library(glmnet)
library(corrplot)
library(predtools)
library(dplyr)
library(tidyr)
library(purrr)


# Next, try with interactions
# Then prune them with lasso

# Try RF

# Try variability with n = 500, n = 500 to see the variability changes

# Try a smaller sample size first to get code running, then try with full dataset bootstrap




#require(snow)
#spec = 4
#cl = makeCluster(spec, type = "SOCK")


#head(gusto)

# Setting up data

data <- gusto %>% 
    select(day30, sex, age, htn, hyp, ste, sysbp, pmi, hrt) %>%
    mutate(
        sex = ifelse(sex == "female", 1, 0),
        pmi = ifelse(pmi == "yes", 1, 0),
        id = row_number()
    ) %>%
    select(id, everything())

nrow(data)
ncol(data)

# Big Model

mod1 = glm(day30 ~ sex + age + htn + hyp + ste + sysbp + pmi + hrt, 
family = binomial, data = data)

df = data.frame(id = data$id, yhat = predict(mod1, type = 'response'))

# Setting up parameters for bootstrap

n = 1000 # Sample size
M = 1000 # Bootstrap size
set.seed(100)

size_events <- (n * 0.07)
size_non_events <- (n - size_events)

data_e <- data %>%
    filter(day30 == 1)

data_ne <- data %>% 
    filter(day30 == 0)

for (i in 1:M) {
    # Sample event and non-event data with replacement
    sampled_data_e <- data_e %>%
        sample_n(size_events, replace = TRUE)

    sampled_data_ne <- data_ne %>%
        sample_n(size_non_events, replace = TRUE)

    # Combine event and non-event samples
    ds <- bind_rows(sampled_data_e, sampled_data_ne)

    df_temp <- data.frame(id = ds$id, y = ds$day30)

    df_temp <- df_temp %>%
        rename_with(~ paste0("y_", i), .cols = y)

    # Fit the model on the bootstrap sample
    mod_bs <- glm(day30 ~ sex + age + htn + hyp + ste + sysbp + pmi + hrt, 
                  family = binomial, data = ds)

    # Get predicted probabilities for the bootstrap sample
    df_bs <- data.frame(id = ds$id, yhat_i = predict(mod_bs, type = "response"))

    df_bs <- df_bs %>%
        group_by(id) %>%
        mutate(occurrence = row_number()) %>%  
        ungroup()

    # Reshape to wide format
    df_bs_wide <- df_bs %>%
        pivot_wider(names_from = occurrence, values_from = yhat_i, names_prefix = paste0("yhat_", i, "_"))

    # Combine the predictions with the original df
    df <- df %>%
        left_join(df_bs_wide, by = 'id')

}

## Finding 2.5 and 97.5 percent


bootstrap_predictions <- df[,-c(1, 2)] #Exclude ID's and Original predictions

bootstrap_predictions_matrix <- as.matrix(bootstrap_predictions)

quantiles <- apply(bootstrap_predictions_matrix, 1, function(x) {
    # Calculate 2.5% and 97.5% quantiles, ignoring NA values
    quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
})

#quantiles <- apply(bootstrap_predictions_matrix, 1, quantile, c(0.025, 0.975), na.rm = TRUE)

quantiles <- t(quantiles)

quantiles <- as.data.frame(quantiles)
colnames(quantiles) <- c("LowerQuantile", "UpperQuantile")

quantiles <- quantiles %>%
    mutate(Estimated_Risk = df[[2]]) %>%
    select(Estimated_Risk, LowerQuantile, UpperQuantile) %>%
    arrange(Estimated_Risk)

par(pty = "s")
plot(df[[2]], df[[3]],
    type = 'n',
    xlim = c(0,1),
    ylim = c(0,1),
    xlab = "estimated risk from developed model",
    ylab = "estimated risk from bootstrap models",
    main = "Estimated Risk Logistic Regression")
abline(0, 1, lwd = 1)
for (i in 3:ncol(df)){
    points(df[[2]], df[[i]],
    pch = 16,
    col = "black",
    cex = 0.1)
}
lines(lowess(x = quantiles$Estimated_Risk, y = quantiles$LowerQuantile, f = 1/15), lty = 2, lwd = 1.5)
lines(lowess(x = quantiles$Estimated_Risk, y = quantiles$UpperQuantile, f = 1/15), lty = 2, lwd = 1.5)

big_model_mean <- mean(df[[2]])
means_bs <- apply(bootstrap_predictions, 2, mean, na.rm = TRUE)
head(means_bs)

par(pty = "s")
hist(means_bs, 
    breaks = 100,
    main = "Bootstrap Mean Distribution")
abline(v = big_model_mean,
    col = "red")

#stopCluster(cl)
#reset("snow")