# =============================================================================
# 01_data_and_eda.R
# Interpretable ML on the German Credit Dataset
# =============================================================================

library(mlr3)
library(mlr3data)
library(data.table)
library(ggplot2)

# ---------------------------------------------------------------------------
# 1.1 Daten laden
# ---------------------------------------------------------------------------

task <- tsk("german_credit")
task

cat("Anzahl Beobachtungen:", task$nrow, "\n")
cat("Anzahl Features:    ", task$ncol, "\n")
cat("Zielvariable:       ", task$target_names, "\n")
cat("Klassen:            ", paste(task$class_names, collapse = ", "), "\n\n")

df <- task$data()

table(df$credit_risk)
prop.table(table(df$credit_risk))

# Feature-Typen ausgeben (wichtig für Debugging!)
cat("\nFeature-Typen:\n")
print(task$feature_types)

# ---------------------------------------------------------------------------
# 1.2 Exploratory Data Analysis
# ---------------------------------------------------------------------------

# --- 1. Klassenverteilung (1 Plot) ---
ggplot(df, aes(x = credit_risk, fill = credit_risk)) +
  geom_bar() +
  scale_fill_manual(values = c("good" = "#2ca02c", "bad" = "#d62728")) +
  labs(title = "Class Distribution",
       subtitle = "~70% good, ~30% bad",
       x = "Credit Risk", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# --- 2. Numerische Features: Dichte (sollte ~7 Plots geben) ---
num_features <- task$feature_types[type %in% c("numeric", "integer"), id]
cat("\nNumerische Features gefunden:", length(num_features), "\n")
cat(paste(num_features, collapse = ", "), "\n")

for (feat in num_features) {
  p <- ggplot(df, aes(x = get(feat), fill = credit_risk)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("good" = "#2ca02c", "bad" = "#d62728")) +
    labs(title = paste("Distribution:", feat), x = feat, y = "Density") +
    theme_minimal()
  print(p)
}

# --- 3. Boxplots (4 Plots) ---
key_num <- c("duration", "amount", "age", "installment_rate")

for (feat in key_num) {
  p <- ggplot(df, aes(x = credit_risk, y = get(feat), fill = credit_risk)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("good" = "#2ca02c", "bad" = "#d62728")) +
    labs(title = paste(feat, "by Credit Risk"), x = "Credit Risk", y = feat) +
    theme_minimal() +
    theme(legend.position = "none")
  print(p)
}

# --- 4. Korrelationsmatrix (1 Plot) ---
num_df <- df[, ..num_features]
cor_matrix <- cor(num_df)

cor_dt <- as.data.table(as.table(cor_matrix))
setnames(cor_dt, c("Var1", "Var2", "Correlation"))

ggplot(cor_dt, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "#d62728", mid = "white", high = "#1f77b4",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Matrix (Numeric Features)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- 5. Kategoriale Features (4 Plots) ---
cat_features <- task$feature_types[type == "factor", id]
cat("\nKategoriale Features gefunden:", length(cat_features), "\n")
cat(paste(head(cat_features, 4), collapse = ", "), "\n")

for (feat in head(cat_features, 4)) {
  p <- ggplot(df, aes(x = get(feat), fill = credit_risk)) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = c("good" = "#2ca02c", "bad" = "#d62728")) +
    labs(title = paste("Proportion:", feat), x = feat, y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}