# Environnement ----------------------------------------------------------

rm(list = ls())

library(dplyr)
library(MASS)
library(forcats)
library(yaml)
library(arrow)
api_pwd <- yaml::read_yaml("secrets.yaml")

# Fonctions ----------------------------------------------------------------
source("R/functions.R", encoding = "UTF-8")

# Import des données ----------------------------------------------------------
# j'importe les données avec read_csv2 parce que c'est un csv avec des ;
# et que read_csv attend comme separateur des ,
df <- arrow::read_parquet(
  file="individu_reg.parquet",
  col_select = c("region", "aemm", "aged", "anai", "catl", "cs1", "cs2", "cs3",
                 "couple", "na38", "naf08", "pnai12", "sexe", "surf", "tp",
                 "trans", "ur")
)

# Traitements des données -----------------------------------------------

df = recode_as_na(df=df,var_name="na38",value="ZZ")
df = recode_as_na(df=df,var_name="trans",value="Z")
df = recode_as_na(df=df,var_name="tp",value="Z")
df[endsWith(df$naf08, "ZZZ"), "naf08"] <- NA

str(df)
df = df %>% mutate(dplyr::across(c(trans,ur),factor))
df$sexe <-
  fct_recode(as.character(df$sexe), "Homme" = "1", "Femme" = "2")
df$aged = as.numeric(df$aged)

# Statistiques decriptives ------------------------------------------
## combien de professions ===========================================
calculate_nb_professions = function(data,varname){
  print("Nombre de professions :")
  print(summarise(data, length(unique(unlist({{varname}}[!is.na({{varname}})])))))
}
calculate_nb_professions(data=df,varname=cs1)
calculate_nb_professions(data=df,varname=cs2)
calculate_nb_professions(data=df,varname=cs3)

stats_age <- df2 |> 
  group_by(decennie = decennie_a_partir_annee(age)) |>
  summarise(n())

table_age <- gt(stats_age) |>
  tab_header(
    title = "Distribution des âges dans notre population"
  ) |>
  fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) |>
  cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )


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
part_total <- function(data,var_groupe="age",var_interet="sexe"){
  data |>
    group_by(!!!syms(c(var_groupe,var_interet))) |>
    summarise(share=n()) |>
    group_by(!!sym(var_groupe)) |>
    mutate(share=share/sum(share))
}
ggplot(df %>% part_total(var_groupe="aged",var_interet = "sexe")%>%
         dplyr::filter(sexe=="Homme")) + 
  geom_bar(aes(x = aged, y = share), stat = "identity") + 
  geom_point(aes(x = aged, y = share), stat = "identity", 
             color = "red") + 
  coord_cartesian(c(0, 100))

## stats surf par statut ==============================================
df3 <- tibble(part_total(data=df,var_groupe="surf",var_interet = "couple"))

ggplot(df3) +
  geom_bar(aes(x = surf, y = share, color = factor(couple)), stat = "identity", position = "dodge")

## stats trans par statut =============================================
df3 <- tibble(part_total(data=df,var_groupe="trans",var_interet = "couple"))
p <- ggplot(df3) +
  geom_bar(aes(x = trans, y = share, color = factor(couple)), stat = "identity", position = "dodge")
p

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
