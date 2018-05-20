# Uncomment this and run it the first time you use the code
#devtools::install_github("kosukeimai/qss-package", build_vignettes = TRUE)

library("readtext")
# use readtext package to import all documents as a dataframe
corpus_texts <- readtext(system.file("extdata/federalist/", package = "qss"))

# create docvar with number of paper
corpus_texts$paper_number <- paste("No.", seq_len(nrow(corpus_texts)), sep = " ")

# transform to a quanteda corpus object
corpus_raw <- corpus(corpus_texts, text_field = "text", docid_field = "paper_number")

# create docvar with authorship (used in Section  5.1.4)
docvars(corpus_raw, "paper_numeric") <- seq_len(ndoc(corpus_raw))

# create docvar with authorship (used in Section  5.1.4)
docvars(corpus_raw, "author") <- factor(NA, levels = c("madison", "hamilton"))
docvars(corpus_raw, "author")[c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)] <- "hamilton"
docvars(corpus_raw, "author")[c(10, 14, 37:48, 58)] <- "madison"


texts(corpus_raw)[10] %>% 
  stringi::stri_sub(1, 240) %>% 
  cat()


dfm_prep <- dfm(corpus_raw, remove_numbers = TRUE, tolower = TRUE,
                remove_punct = TRUE, verbose = TRUE)

hamilton <- c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)
madison <- c(10, 14, 37:48, 58)

# term frequency per 1000 words
tfm <- dfm_weight(dfm_prep, "prop") * 1000

# select words of interest
words <- c("although", "always", "commonly", "consequently",
           "considerable", "enough", "there", "upon", "while", "whilst")
tfm <- dfm_select(tfm, words, valuetype = "fixed")

# average among Hamilton/Madison essays
tfm_ave <- dfm_group(dfm_subset(tfm, !is.na(author)), "author") /
  as.numeric(table(docvars(tfm, "author")))

# bind docvars from corpus and tfm to a data frame
author_data <- data.frame(docvars(corpus_raw), tfm)

# create numeric variable that takes value 1 for Hamilton's essays,
# -1 for Madison's essays and NA for the essays with unknown authorship
author_data$author_numeric <- ifelse(author_data$author == "hamilton", 1, 
                                     ifelse(author_data$author == "madison", -1, NA))

# use only known authors for training set
author_data_known <- na.omit(author_data)

hm_fit <- lm(author_numeric ~ upon + there + consequently + whilst,
             data = author_data_known)
hm_fit

hm_fitted <- fitted(hm_fit) # fitted values
sd(hm_fitted)


disputed <- c(49, 50:57, 62, 63) # 11 essays with disputed authorship
tf.disputed <- as.data.frame(tfm[disputed, ])

## prediction of disputed authorship
pred <- predict(hm_fit, newdata = tf.disputed)
pred # predicted values

author_data$prediction <- predict(hm_fit, newdata = author_data)

author_data$prediction
author_data$author_plot <- ifelse(is.na(author_data$author), "unknown", as.character(author_data$author))

library(ggplot2)
ggplot(data = author_data, aes(x = paper_numeric, 
                               y = prediction, 
                               shape = author_plot, 
                               colour = author_plot)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Federalist Papers", y = "Predicted values") +
  theme_minimal() + 
  theme(legend.title=element_blank())

