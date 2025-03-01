library(readr)

#Ecriture du jeu de données dans l'objet data_exercice
data_exercice <- read_delim("data/elus-conseillers-municipaux-cm (1).csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Création de data frame
df_Nantes <- subset(data_exercice,`Libellé de la commune` == 'Nantes')
df_Faverelles <- subset(data_exercice,`Libellé de la commune` == 'Faverelles')

df_Loire_Atlantique <- subset(data_exercice, `Libellé du département` == 'Loire-Atlantique')
df_Gers <- subset(data_exercice, `Libellé du département` == 'Gers')

#Création d'une fonction pour le comptage du nombre unique d'élus
compter_nombre_d_elus <- function(data) {
    unique_elus <- unique(data[, c("Nom de l'élu", "Prénom de l'élu", "Date de naissance")])
    return(nrow(unique_elus))
}

#Application de la fonction aux df

# Comptage du nombre unique d'élus pour Nantes
nombre_elus_Nantes <- compter_nombre_d_elus(df_Nantes)
print(paste("Nombre unique d'élus à Nantes :", nombre_elus_Nantes))

# Comptage du nombre unique d'élus pour Faverelles
nombre_elus_Faverelles <- compter_nombre_d_elus(df_Faverelles)
print(paste("Nombre unique d'élus à Faverelles :", nombre_elus_Faverelles))

# Comptage du nombre unique d'élus pour Loire-Atlantique
nombre_elus_Loire_Atlantique <- compter_nombre_d_elus(df_Loire_Atlantique)
print(paste("Nombre unique d'élus en Loire-Atlantique :", nombre_elus_Loire_Atlantique))

# Comptage du nombre unique d'élus pour le Gers
nombre_elus_Gers <- compter_nombre_d_elus(df_Gers)
print(paste("Nombre unique d'élus dans le Gers :", nombre_elus_Gers))


#Création d'une fonction permettant le comptage du nombre d'adjoints
compter_nombre_d_adjoints <- function(data) {
  nombre_adjoints <- sum(grepl("adjoint au Maire", data$`Libellé de la fonction`, ignore.case = TRUE), na.rm = TRUE)
  return(nombre_adjoints)
}


# Application de la fonction sur les data.frames définies précédemment

# Compter le nombre d'adjoints pour Nantes
nombre_adjoints_Nantes <- compter_nombre_d_adjoints(df_Nantes)
print(paste("Nombre d'adjoints à Nantes :", nombre_adjoints_Nantes))

# Compter le nombre d'adjoints pour Faverelles
nombre_adjoints_Faverelles <- compter_nombre_d_adjoints(df_Faverelles)
print(paste("Nombre d'adjoints à Faverelles :", nombre_adjoints_Faverelles))

# Compter le nombre d'adjoints pour Loire-Atlantique
nombre_adjoints_Loire_Atlantique <- compter_nombre_d_adjoints(df_Loire_Atlantique)
print(paste("Nombre d'adjoints en Loire-Atlantique :", nombre_adjoints_Loire_Atlantique))

# Compter le nombre d'adjoints pour le Gers
nombre_adjoints_Gers <- compter_nombre_d_adjoints(df_Gers)
print(paste("Nombre d'adjoints dans le Gers :", nombre_adjoints_Gers))


# Fonction pour trouver l'élu le plus âgé
trouver_l_elu_le_plus_age <- function(data) {
  # Convertir la colonne Date de naissance en format Date
  data$`Date de naissance` <- as.Date(data$`Date de naissance`, format = "%d/%m/%Y")
  
  # Trouver l'élu le plus âgé
  elu_le_plus_age <- data[which.min(data$`Date de naissance`), ]
  
  # Calculer l'âge de l'élu le plus âgé
  age <- as.numeric(difftime(Sys.Date(), elu_le_plus_age$`Date de naissance`, units = "weeks")) %/% 52
  
  # Retourner un objet contenant le nom, le prénom et l'âge de l'élu le plus âgé
  return(list(nom = elu_le_plus_age$`Nom de l'élu`, prenom = elu_le_plus_age$`Prénom de l'élu`, age = age))
}

# Application de la fonction sur les data.frames définies précédemment

# Trouver l'élu le plus âgé pour Nantes
elu_plus_age_Nantes <- trouver_l_elu_le_plus_age(df_Nantes)
print(paste("L'élu le plus âgé à Nantes :", elu_plus_age_Nantes$nom, elu_plus_age_Nantes$prenom, "âgé de", elu_plus_age_Nantes$age, "ans"))

# Trouver l'élu le plus âgé pour Faverelles
elu_plus_age_Faverelles <- trouver_l_elu_le_plus_age(df_Faverelles)
print(paste("L'élu le plus âgé à Faverelles :", elu_plus_age_Faverelles$nom, elu_plus_age_Faverelles$prenom, "âgé de", elu_plus_age_Faverelles$age, "ans"))

# Trouver l'élu le plus âgé pour Loire-Atlantique
elu_plus_age_Loire_Atlantique <- trouver_l_elu_le_plus_age(df_Loire_Atlantique)
print(paste("L'élu le plus âgé en Loire-Atlantique :", elu_plus_age_Loire_Atlantique$nom, elu_plus_age_Loire_Atlantique$prenom, "âgé de", elu_plus_age_Loire_Atlantique$age, "ans"))

# Trouver l'élu le plus âgé pour le Gers
elu_plus_age_Gers <- trouver_l_elu_le_plus_age(df_Gers)
print(paste("L'élu le plus âgé dans le Gers :", elu_plus_age_Gers$nom, elu_plus_age_Gers$prenom, "âgé de", elu_plus_age_Gers$age, "ans"))


#Création d'une fonction pour calculer l'âge à partir de la date de naissance
calculer_age <- function(date_naissance) {
  today <- Sys.Date()
  age <- as.numeric(difftime(today, as.Date(date_naissance, format="%d/%m/%Y"), units="weeks")) %/% 52
  return(age)
}

# Fonction pour calculer la distribution des âges
calcul_distribution_age <- function(data) {
  # Vérifiez que la colonne "Date de naissance" est au bon format
  data$`Date de naissance` <- as.Date(data$`Date de naissance`, format="%d/%m/%Y")
  
  # Calculer l'âge de chaque élu
  data$age <- sapply(data$`Date de naissance`, calculer_age)
  
  # Calculer les quantiles
  quantiles <- quantile(data$age, probs = c(0, 0.25, 0.5, 0.75, 1))
  
  # Retourner les quantiles
  return(quantiles)
}

# Appliquer la fonction sur les data.frames définies précédemment

# Calculer la distribution des âges pour Nantes
distribution_age_Nantes <- calcul_distribution_age(df_Nantes)
print("Distribution des âges à Nantes :")
print(distribution_age_Nantes)

# Calculer la distribution des âges pour Faverelles
distribution_age_Faverelles <- calcul_distribution_age(df_Faverelles)
print("Distribution des âges à Faverelles :")
print(distribution_age_Faverelles)

# Calculer la distribution des âges pour Loire-Atlantique
distribution_age_Loire_Atlantique <- calcul_distribution_age(df_Loire_Atlantique)
print("Distribution des âges en Loire-Atlantique :")
print(distribution_age_Loire_Atlantique)

# Calculer la distribution des âges pour le Gers
distribution_age_Gers <- calcul_distribution_age(df_Gers)
print("Distribution des âges dans le Gers :")
print(distribution_age_Gers)


#Création de la fonction  plot_code_professions()

# charger la bibliothèque ggplot2
library(ggplot2)

# Fonction pour créer un graphique en barres horizontal des codes professionnels
plot_code_professions <- function(data, title) {
  # Compter le nombre d'élus par code professionnel
  code_count <- table(data$`Code de la catégorie socio-professionnelle`)
  
  # Convertir en data.frame pour ggplot2
  df_code_count <- as.data.frame(code_count)
  colnames(df_code_count) <- c("Code professionnel", "Nombre d'élus")
  
  # Création d'un bar chart horizontal
  ggplot(df_code_count, aes(x = reorder(`Code professionnel`, `Nombre d'élus`), y = `Nombre d'élus`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = title, x = "Code professionnel", y = "Nombre d'élus") +
    theme_minimal() 
}

# Application de la fonction sur les data.frames 

# Plot pour Nantes
plot_code_professions(df_Nantes, "Répartition des codes professionnels des élus à Nantes")

# Plot pour Faverelles
plot_code_professions(df_Faverelles, "Répartition des codes professionnels des élus à Faverelles")

# Plot pour Loire-Atlantique
plot_code_professions(df_Loire_Atlantique, "Répartition des codes professionnels des élus en Loire-Atlantique")

# Plot pour le Gers
plot_code_professions(df_Gers, "Répartition des codes professionnels des élus dans le Gers")


#Définition de la fonction générique summary.commune()
summary.commune <- function(x) {
  # Vérifiez que l'objet est de type commune
  if (!inherits(x, "commune")) {
    stop("L'objet doit être de type 'commune'")
  }
  
  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé de la commune", "Nom de l'élu", "Prénom de l'élu", "Date de naissance") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé de la commune', 'Nom de l'élu', 'Prénom de l'élu', et 'Date de naissance'")
  }
  
  # Nom de la commune
  nom_commune <- unique(x$`Libellé de la commune`)
  
  # Nombre d'élu.e.s de la commune
  nombre_elus <- nrow(x)
  
  #Calcul des âges 
  x$age <- round(as.numeric(difftime(Sys.Date(), as.Date(x$`Date de naissance`, format="%d/%m/%Y"), units = "days")) / 365)
  
  # Distribution des âges des élu.e.s
  distribution_ages <- summary(x$age)
  
  # Nom et âge de l'élu.e le ou la plus âgé.e
  elu_plus_age <- x[which.max(x$age), ]
  nom_elu_plus_age <- paste(elu_plus_age$`Nom de l'élu`, elu_plus_age$`Prénom de l'élu`)
  age_elu_plus_age <- elu_plus_age$age
  
  # Impression des informations dans la console
  cat("Nom de la commune:", nom_commune, "\n")
  cat("Nombre d'élu.e.s:", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s:\n")
  print(distribution_ages)
  cat("Élu.e le ou la plus âgé.e: ", nom_elu_plus_age, ", Âge: ", age_elu_plus_age, "\n")
}

# Affectez la classe 'commune' aux data.frames df_Nantes tout en conservant la classe data.frame
class(df_Nantes) <- c("commune", "data.frame")


# Testez la fonction summary.commune sur l data.frame df_Nantes
summary.commune(df_Nantes)


#Définition de la fonction générique summary.departement
summary.departement <- function(x) {
  # Vérifiez que l'objet est de type departement
  if (!inherits(x, "departement")) {
    stop("L'objet doit être de type 'departement'")
  }
  
  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé du département", "Libellé de la commune", "Nom de l'élu", "Prénom de l'élu", "Date de naissance") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé du département', 'Libellé de la commune', 'Nom de l'élu', 'Prénom de l'élu', et 'Date de naissance'")
  }
  
  # Nom du département
  nom_departement <- unique(x$`Libellé du département`)
  
  # Nombre de communes dans le département
  nombre_communes <- length(unique(x$`Libellé de la commune`))
  
  # Nombre d'élu.e.s dans le département
  nombre_elus <- nrow(x)
  
  # Calcul des âges des élu.e.s et arrondir à l'entier le plus proche
  x$age <- round(as.numeric(difftime(Sys.Date(), as.Date(x$`Date de naissance`, format="%d/%m/%Y"), units = "weeks")) / 52.25)
  
  # Distribution des âges des élu.e.s du département
  distribution_ages <- summary(x$age)
  
  # Nom et âge de l'élu.e le ou la plus âgé.e et sa commune
  elu_plus_age <- x[which.max(x$age), ]
  nom_elu_plus_age <- paste(elu_plus_age$`Nom de l'élu`, elu_plus_age$`Prénom de l'élu`)
  age_elu_plus_age <- elu_plus_age$age
  commune_elu_plus_age <- elu_plus_age$`Libellé de la commune`
  
  # Nom et âge de l'élu.e le ou la plus jeune et sa commune
  elu_plus_jeune <- x[which.min(x$age), ]
  nom_elu_plus_jeune <- paste(elu_plus_jeune$`Nom de l'élu`, elu_plus_jeune$`Prénom de l'élu`)
  age_elu_plus_jeune <- elu_plus_jeune$age
  commune_elu_plus_jeune <- elu_plus_jeune$`Libellé de la commune`
  
  # Nom de la commune à la moyenne d’âge la plus faible
  moyenne_age_par_commune <- aggregate(x$age, by=list(x$`Libellé de la commune`), mean)
  commune_moyenne_age_plus_faible <- moyenne_age_par_commune[which.min(moyenne_age_par_commune$x), 1]
  ages_commune_plus_faible <- x[x$`Libellé de la commune` == commune_moyenne_age_plus_faible, ]$age
  distribution_ages_commune_plus_faible <- summary(ages_commune_plus_faible)
  
  # Nom de la commune à la moyenne d’âge la plus élevée
  commune_moyenne_age_plus_elevee <- moyenne_age_par_commune[which.max(moyenne_age_par_commune$x), 1]
  ages_commune_plus_elevee <- x[x$`Libellé de la commune` == commune_moyenne_age_plus_elevee, ]$age
  distribution_ages_commune_plus_elevee <- summary(ages_commune_plus_elevee)
  
  # Impression des informations dans la console
  cat("Nom du département:", nom_departement, "\n")
  cat("Nombre de communes dans le département:", nombre_communes, "\n")
  cat("Nombre d'élu.e.s dans le département:", nombre_elus, "\n")
  cat("Distribution des âges des élu.e.s du département:\n")
  print(distribution_ages)
  cat("Élu.e le ou la plus âgé.e:", nom_elu_plus_age, ", Âge:", age_elu_plus_age, ", Commune:", commune_elu_plus_age, "\n")
  cat("Élu.e le ou la plus jeune:", nom_elu_plus_jeune, ", Âge:", age_elu_plus_jeune, ", Commune:", commune_elu_plus_jeune, "\n")
  cat("Commune à la moyenne d’âge la plus faible:", commune_moyenne_age_plus_faible, "\n")
  cat("Distribution des âges des élu.e.s pour cette commune:\n")
  print(distribution_ages_commune_plus_faible)
  cat("Commune à la moyenne d’âge la plus élevée:", commune_moyenne_age_plus_elevee, "\n")
  cat("Distribution des âges des élu.e.s pour cette commune:\n")
  print(distribution_ages_commune_plus_elevee)
}

# Ajoutez la classe 'departement' tout en conservant la classe 'data.frame'
class(df_Loire_Atlantique) <- c("departement", "data.frame")
class(df_Gers) <- c("departement", "data.frame")

# Teste de la fonction summary.departement sur df_Loire_Atlantique et df_Gers
summary.departement(df_Loire_Atlantique)
summary.departement(df_Gers)


#Création de la fonction plot.commune
plot.commune <- function(x) {
  # Vérifiez que l'objet est de type commune
  if (!inherits(x, "commune")) {
    stop("L'objet doit être de type 'commune'")
  }
  
  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé de la commune", "Libellé du département", "Code de la catégorie socio-professionnelle") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé de la commune', 'Libellé du département', et 'Code de la catégorie socio-professionnelle'")
  }
  
  # Nom de la commune et du département
  nom_commune <- unique(x$`Libellé de la commune`)
  nom_departement <- unique(x$`Libellé du département`)
  
  # Nombre d'élu.e.s de la commune
  nombre_elus <- nrow(x)
  
  # Titre du graphique
  titre_graphique <- paste(nom_commune, "-", nom_departement)
  
  # Compter le nombre d'élus par code socio-professionnel
  code_count <- table(x$`Code de la catégorie socio-professionnelle`)
  
  # Convertir en data.frame pour ggplot2
  df_code_count <- as.data.frame(code_count)
  colnames(df_code_count) <- c("Code socio-professionnel", "Nombre d'élus")
  
  # Création d'un bar chart horizontal avec ggplot2
  ggplot(df_code_count, aes(x = reorder(`Code socio-professionnel`, `Nombre d'élus`), y = `Nombre d'élus`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = titre_graphique, 
      x = paste("Libellés des codes professionnels pour les élus (", nombre_elus, " élus)", sep = ""), 
      y = "Nombre d'élus"
    ) +
    theme_minimal()
}

# Ajoutez la classe 'commune' tout en conservant la classe 'data.frame'
class(df_Nantes) <- c("commune", "data.frame")
class(df_Faverelles) <- c("commune", "data.frame")

# Testez la fonction plot.commune sur les data.frames df_Nantes et df_Faverelles
plot.commune(df_Nantes)
plot.commune(df_Faverelles)


# Déclarez la fonction plot.departement
plot.departement <- function(x) {
  # Vérifiez que l'objet est de type departement
  if (!inherits(x, "departement")) {
    stop("L'objet doit être de type 'departement'")
  }
  
  # Vérifiez que les colonnes nécessaires existent
  if (!all(c("Libellé du département", "Libellé de la commune", "Code de la catégorie socio-professionnelle") %in% colnames(x))) {
    stop("Le data.frame doit contenir les colonnes 'Libellé du département', 'Libellé de la commune', et 'Code de la catégorie socio-professionnelle'")
  }
  
  # Nom du département et nombre de communes
  nom_departement <- unique(x$`Libellé du département`)
  nombre_communes <- length(unique(x$`Libellé de la commune`))
  
  # Compter le nombre d'élus pour chaque code socio-professionnel
  code_count <- table(x$`Code de la catégorie socio-professionnelle`)
  top10_code_count <- sort(code_count, decreasing = TRUE)[1:10]
  
  # Convertir en data.frame pour ggplot2
  df_top10_code_count <- as.data.frame(top10_code_count)
  colnames(df_top10_code_count) <- c("Code socio-professionnel", "Nombre d'élus")
  
  # Titre du graphique
  titre_graphique <- paste(nom_departement, "-", nombre_communes, "communes")
  
  # Création d'un bar chart horizontal avec ggplot2
  ggplot(df_top10_code_count, aes(x = reorder(`Code socio-professionnel`, `Nombre d'élus`), y = `Nombre d'élus`)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
      title = titre_graphique, 
      x = paste("Libellés des 10 codes professionnels les plus représentés pour le département ", nom_departement), 
      y = "Nombre d'élus"
    ) +
    theme_minimal()
}

# Ajoutez la classe 'departement' tout en conservant la classe 'data.frame'
class(df_Loire_Atlantique) <- c("departement", "data.frame")
class(df_Gers) <- c("departement", "data.frame")

# Testez la fonction plot.departement sur les data.frames df_Loire_Atlantique et df_Gers
plot.departement(df_Loire_Atlantique)
plot.departement(df_Gers)

