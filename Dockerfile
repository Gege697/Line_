# Utiliser l'image officielle R de base
FROM r-base:latest

# Installer les packages nécessaires
RUN R -e "install.packages(c('tidyverse','ggplot2'), repos='https://cloud.r-project.org')"

# Copier ton script R dans le container
COPY NgueteuLine_23P314.r /app/NgueteuLine_23P314.r

# Définir le dossier de travail
WORKDIR /app

# Commande pour exécuter ton script R au démarrage du service
CMD ["Rscript", "NgueteuLine_23P314.r"]
