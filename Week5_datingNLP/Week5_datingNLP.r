# === שלב 1: טעינת חבילות ===
required <- c("tidyverse", "quanteda", "Matrix", "xgboost", "caret", "rpart", "rpart.plot", "Rtsne")
for (p in required) if (!require(p, character.only = TRUE)) {
  install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

# === שלב 2: טעינת הנתונים ===
profiles <- read.csv("Week5_datingNLP/okcupid_profiles.csv.gz", stringsAsFactors = FALSE)

# === שלב 3: ניקוי ו-tokenization על essay0 בלבד ===
toks <- profiles$essay0 |>
  na.omit() |>
  corpus() |>
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE) |>
  tokens_select(stopwords("en"), "remove") |>
  tokens_wordstem()

# === שלב 4: DFM וסינון מילים נדירות ===
dfm_mat <- dfm(toks)
min_doc_freq <- 0.10 * ndoc(dfm_mat)
dfm_trimmed <- dfm_trim(dfm_mat, min_docfreq = min_doc_freq, docfreq_type = "count")

# === חישוב TF-IDF ידני עם שמות עמודות ===
tfidf_custom <- function(mat) {
  tf <- mat / rowSums(mat)
  idf <- log(nrow(mat) / colSums(mat > 0))
  sweep(tf, 2, idf, `*`)
}
X_tfidf <- tfidf_custom(as.matrix(dfm_trimmed))
colnames(X_tfidf) <- colnames(dfm_trimmed)          # 🟢 שמירת שמות מילים
rownames(X_tfidf) <- docnames(dfm_trimmed)          # 🟢 שמירת שמות מסמכים

# === שלב 5: תיוג מגדר והתאמה ===
labels <- profiles$sex[!is.na(profiles$essay0)] |> as.factor()
names(labels) <- docnames(dfm_mat)
labels <- labels[docnames(dfm_trimmed)] |> droplevels()
labels <- labels[rownames(X_tfidf)]  # 🟢 התאמת הסדר

# === שלב 6: אימון מודל עם 10-fold * 3 ===
df_dense <- as.data.frame(X_tfidf)
names(df_dense) <- make.names(names(df_dense))  # להפוך שמות חוקיים

folds <- createMultiFolds(labels, k = 10, times = 3)
fit_control <- trainControl(method = "cv", index = folds)

tree_model <- train(x = df_dense, y = labels, method = "rpart", trControl = fit_control)
rpart.plot(tree_model$finalModel)
preds <- predict(tree_model, newdata = df_dense)
confusionMatrix(preds, labels)

# === שלב 7: הסרת מילים מגדריות לפי TF-IDF ===

df_male   <- X_tfidf[labels == "m", , drop = FALSE]
df_female <- X_tfidf[labels == "f", , drop = FALSE]

tfidf_male   <- colMeans(df_male)
tfidf_female <- colMeans(df_female)

top_male_words   <- names(sort(tfidf_male,   decreasing = TRUE))[1:10]
top_female_words <- names(sort(tfidf_female, decreasing = TRUE))[1:10]

gender_words <- union(top_male_words, top_female_words)
print("🔠 מילים מגדריות שנבחרו להסרה:")
print(gender_words)

# הסרה ויצירת מטריצת נייטרליות
dfm_neutral       <- dfm_remove(dfm_trimmed, pattern = gender_words)
dfm_tfidf_neutral <- dfm_tfidf(dfm_neutral)
X_sparse_neutral  <- as(dfm_tfidf_neutral, "dgCMatrix")
df_dense_neutral  <- as.data.frame(as.matrix(X_sparse_neutral))
names(df_dense_neutral) <- make.names(names(df_dense_neutral))

# אימון מחדש
tree_model_neutral <- train(x = df_dense_neutral, y = labels, method = "rpart", trControl = fit_control)
preds_neutral <- predict(tree_model_neutral, newdata = df_dense_neutral)
confusionMatrix(preds_neutral, labels)

# === שלב 8: קלאסטרינג ===
mat_clust <- df_dense_neutral
set.seed(42)
k2  <- kmeans(mat_clust, centers = 2 , nstart = 10)
k3  <- kmeans(mat_clust, centers = 3 , nstart = 10)
k4  <- kmeans(mat_clust, centers = 4 , nstart = 10)
k10 <- kmeans(mat_clust, centers = 10, nstart = 10)

valid_idx <- which(!is.na(profiles$essay0) & profiles$sex %in% c("m", "f"))
profiles$cluster_k2  <- profiles$cluster_k3 <- profiles$cluster_k4 <- profiles$cluster_k10 <- NA
profiles$cluster_k2 [valid_idx] <- k2$cluster
profiles$cluster_k3 [valid_idx] <- k3$cluster
profiles$cluster_k4 [valid_idx] <- k4$cluster
profiles$cluster_k10[valid_idx] <- k10$cluster

# === שלב 9: גרף PCA לכל הקלאסטרים ושמירה ל־PDF ===
pca <- prcomp(mat_clust, rank. = 2, center = TRUE, scale. = TRUE)

pdf("Week5_datingNLP.pdf")
for (k in c(2,3,4,10)) {
  clust <- get(paste0("k",k))$cluster
  pca_df <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], cluster = factor(clust))
  print(ggplot(pca_df, aes(PC1, PC2, color = cluster)) +
          geom_point(alpha = 0.6) +
          theme_minimal() +
          labs(title = paste("PCA –", k, "Clusters (neutral words)")))
}
dev.off()

# === שלב 10: t-SNE ===
set.seed(123)
mat_tsne <- as.matrix(mat_clust)
dupes <- duplicated(mat_tsne)
mat_tsne_unique <- mat_tsne[!dupes, ]

tsne_out <- Rtsne(mat_tsne_unique,
                  dims = 2,
                  perplexity = 30,
                  verbose = TRUE,
                  check_duplicates = FALSE)

tsne_df <- data.frame(D1 = tsne_out$Y[,1],
                      D2 = tsne_out$Y[,2],
                      cluster = factor(k4$cluster[!dupes]))

ggplot(tsne_df, aes(D1, D2, color = cluster)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "t-SNE – 4 Clusters (neutral words, dupes removed)")

# === שלב 11: שמירה ל־.rdata ===
train.model <- tree_model_neutral
train.dfm <- dfm_neutral
cluster.pca <- pca
save(file = "Week5_datingNLP.rdata", train.model, train.dfm, cluster.pca)
