# Environnement ----------------------------------------------------------

rm(list = ls())
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("MASS")) install.packages("MASS")
library(tidyverse)
library(dplyr)
library(MASS)
library(forcats)
api_pwd <- "trotskitueleski$1917"


# Fonctions ----------------------------------------------------------------
fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  ignoreNA <<- !ignoreNA
  checkvalue <- FALSE
  for (x in c("moyenne", "variance", "ecart-type", "sd", "ecart type")) {
    checkvalue <- (checkvalue | b == x)
  }
  if (checkvalue == FALSE) stop("statistique non supportée")
  
  if (b == "moyenne") {
    x <- mean(a, na.rm = ignoreNA, ...)
  } else if (b == "ecart-type" | b == "sd" | b == "ecart type") {
    x <- sd(b, na.rm = ignoreNA, ...)
  } else if (a == "variance") {
    x <- var(a, na.rm = ignoreNA, ...)
  }
  return(x)
}
decennie_a_partir_annee <- function(ANNEE) {
  return(ANNEE - ANNEE %%
           10)
}
recode_na = function(data,varname,value){
  data %>% 
    dplyr::mutate(!!rlang::sym(varname) := na_if(!!rlang::sym(varname),value))
}

# Import des données ----------------------------------------------------------
# j'importe les données avec read_csv2 parce que c'est un csv avec des ;
# et que read_csv attend comme separateur des ,
df <- readr::read_csv2(
  "individu_reg.csv",
  col_names = TRUE,
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
                "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
                "trans", "ur")
)

# Traitements des données -----------------------------------------------

df = recode_na(data=df,varname="na38",value="ZZ")
df = recode_na(data=df,varname="trans",value="Z")
df = recode_na(data=df,varname="tp",value="Z")
df[endsWith(df$naf08, "ZZZ"), "naf08"] <- NA

str(df)
df = df %>% mutate(dplyr::across(c(trans,ur),factor))
df$sexe <-
  fct_recode(as.character(df$sexe), "Homme" = "1", "Femme" = "2")
df$aged = as.numeric(df$aged)

# Statistiques decriptives ------------------------------------------
## combien de professions ===========================================
print("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs3[!is.na(cs1)])))))
print("Nombre de professions :''")
print(summarise(df, length(unique(unlist(cs3[!is.na(cs2)])))))
oprint("Nombre de professions :")
print(summarise(df, length(unique(unlist(cs3[!is.na(cs3)])))))

print.data.frame <- summarise(group_by(df, aged), n())
print(print.data.frame)

df %>%
  select(aged) %>%
  ggplot(.) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")

ggplot(df[df$aged > 50, c(3, 4)], aes(
  x = as.numeric(aged), # x = as.numeric(aged) - as.numeric(aged) %% 5,
  y = ..density.., fill = factor(decennie_a_partir_annee(as.numeric(aemm)))
), alpha = 0.2) +
  geom_histogram() # position = "dodge") + scale_fill_viridis_d()

## part d'homme dans chaque cohort ===========================
ggplot(df %>% group_by(as.numeric(aged, sexe)) %>% summarise(SH_sexe = n()) %>% 
         group_by(aged) %>% summarise(SH_sexe = SH_sexe / sum(SH_sexe))) %>% 
  filter(sexe == 1) + 
  geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity") + 
  geom_point(aes(x = as.numeric(aged), y = SH_sexe), stat = "identity", 
             color = "red") + 
  coord_cartesian(c(0, 100))
# correction (qu'il faudra retirer)
# ggplot(
#   df %>% group_by(aged, sexe) %>% summarise(SH_sexe = n()) 
#   %>% group_by(aged) %>% mutate(SH_sexe = SH_sexe/sum(SH_sexe)) %>% filter(sexe==1)
# ) + geom_bar(aes(x = as.numeric(aged), y = SH_sexe), stat="identity") + 
# geom_point(aes(x = as.numeric(aged), y = SH_sexe), 
# stat="identity", color = "red") + coord_cartesian(c(0,100))


## stats surf par statut ==============================================
df3 <- tibble(df |> group_by(couple, surf) %>% summarise(x = n()) %>% 
                group_by(couple) |> mutate(y = 100 * x / sum(x)))
ggplot(df3) %>%
  geom_bar(aes(x = surf, y = y, color = couple), stat = "identity", position = "dodge")

## stats trans par statut =============================================
df3 <- tibble(df |> group_by(couple, trans) %>% summarise(x = n()) %>% 
                group_by(couple) |> mutate(y = 100 * x / sum(x)))
p <- ggplot(df3) +
  geom_bar(aes(x = trans, y = y, color = couple), stat = "identity", position = "dodge")

dir.create("/home/onyxia/formation-bonnes-pratiques-R/output")
setwd("ome/onyxia/formation-bonnes-pratiques-R/output")

ggsave(p, "p.png")


## stat agregee ==============================
ignoreNA <- TRUE

fonction_de_stat_agregee(rnorm(10))
fonction_de_stat_agregee(rnorm(10), "cart type")
fonction_de_stat_agregee(rnorm(10), "ecart type")
fonction_de_stat_agregee(rnorm(10), "variance")


fonction_de_stat_agregee(df %>% filter(sexe == "Homme") %>% 
                           mutate(aged = as.numeric(aged)) %>% 
                           pull(aged), na.rm = TRUE)
fonction_de_stat_agregee(df %>% filter(sexe == "Femme") %>% 
                           mutate(aged = as.numeric(aged)) %>% 
                         pull(aged), na.rm = TRUE)
fonction_de_stat_agregee(df %>% filter(sexe == "Homme" & couple == "2") %>% 
                           mutate(aged = as.numeric(aged)) %>% 
                         pull(aged), na.rm = TRUE)
fonction_de_stat_agregee(df %>% filter(sexe == "Femme" & couple == "2") %>% 
                           mutate(aged = as.numeric(aged)) 
                         %>% pull(aged), na.rm = TRUE)


# Modelisation ------------------------------------------------------------
df3 <- df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, 1] <- factor(df3$surf, ordered = T)
df3[, "cs1"] <- factor(df3$cs1)
polr(surf ~ cs1 + factor(ur), df3 %>% 
       filter(couple == "2" && as.numeric(aged > 40 && aged < 60)))
