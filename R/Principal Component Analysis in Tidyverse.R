#row names#
# Detect row names
View(mtcars)
has_rownames(mtcars)
View(iris)
has_rownames(iris)

# Convert between row names and column
mtcars_tbl <- rownames_to_column(mtcars, var = "car") %>% #row names in mtcars are transferred to column, and named "car"
  as_tibble()

mtcars_tbl
View(mtcars_tbl)

# Adding rowid as a column
View(iris)
iris_rowid<-rowid_to_column(iris)
View(iris_rowid)

#if you want to visualize its row names in your visual representation,
#tibbles are not friendly
#you may want to study more about tibbles with row names:
browseURL("https://tbradley1013.github.io/2018/02/01/pca-in-a-tidy-verse-framework/")

#PCA with mtcars tibble df
library(tidyverse)
library(broom)
library(knitr)
library(ggfortify)

#let's see how we have to handle tibble that has no row names
#tibble method
mtcars
mtcars_tbl_WithRowName_1 <- mtcars %>% 
  rownames_to_column(var = "car") %>% 
  as_tibble()

View(mtcars_tbl_WithRowName_1)

mtcar_pca <- mtcars_tbl_WithRowName_1 %>% 
  nest(data = everything()) %>%
  mutate(pca = map(data, ~ prcomp(.x %>% select(-car), 
                                  center = TRUE, scale = TRUE)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

mtcar_pca

var_exp <- mtcar_pca %>% 
  unnest(pca_aug) %>% 
  summarize_at(.vars = vars(contains("PC", ignore.case = FALSE)), .funs = list(var)) %>% 
  gather(key = pc, value = variance) %>% 
  mutate(var_exp = variance/sum(variance),
         cum_var_exp = cumsum(var_exp),
         pc = str_replace(pc, ".fitted", ""))

var_exp

var_exp %>% 
  rename(
    `Variance Explained` = var_exp,
    `Cumulative Variance Explained` = cum_var_exp
  ) %>% 
  gather(key = key, value = value, `Variance Explained`:`Cumulative Variance Explained`) %>% 
  ggplot(aes(pc, value, group = key)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  lims(y = c(0, 1)) +
  labs(y = "Variance",
       title = "Variance explained by each principal component")

#Always show all labels, even when they have too many overlaps, set it globally:
options(ggrepel.max.overlaps = Inf)

mtcar_pca %>%
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, label = TRUE,
                 label.label = "car",
                 label.repel = TRUE) +
        theme_bw() +
        labs(x = "Principal Component 1",
             y = "Principal Component 2",
             title = "First two principal components of PCA on mtcar dataset")
    )
  ) %>%
  pull(pca_graph)
