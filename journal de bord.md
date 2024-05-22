**pour trouver les informations nécéssaires quant à la mise en page au format .md, suivre ce lien :**(https://docs.github.com/fr/get-started/writing-on-github/getting-started-with-writing-and-formatting-on-github/basic-writing-and-formatting-syntax#links)
# Journée du 15 avril
## Matinée
Découverte des locaux du laboratoire, présentation des différentes personnes présentes ce jour. Installation au sein des bureaux.
## Après-midi
lecture des sources bibliographiques fournies par Valentin Mathieu par mail. Découverte des tutoriels sur R studio.
# Journée du 16 avril
## Matinée
travail de bibliographie sur le thème général du sujet. découverte de GitHub.
## Après-midi
poursuite des recherches bibliographiques, réalisation des tutoriels de GitHub, premier coup d'oeil sur les bases de données 
FAOSTAT et UN Comtrade
# Journée du 17 avril
## Matinée
gestion de la convention de stage, finalisation des tutoriels. 
## Après-midi
préparation de la première réunion de stage : recherches bibliographiques, 
rédaction des questions, recherche de problématiques et de questions de recherche, finalisation de la bibliographie "grossière"
# Journée du 18 avril
## Matinée
mise au propre du document préparé pour la réunion, première réunion de stage pour déterminer
les sujets à étudier et vérifier que les éléments de base du stage ont été acquis et compris
## Après-midi
réalisation du journal de bord, début de la bibliographie portant sur le sujet choisi lors de la réunion du matin. Suivi d'un tutoriel 
pour la rédaction des documents en markdown (type .md sur GitHub).
# Journée du 19 avril
## Matinée
recherche d'articles bibliographiques en rapport avec la thématique de recherche abordée, à savoir la production des produits bois européens, les imports et exports des pays européens avec le reste du monde.
## Après-midi
recherche d'articles et lecture d'articles, réalisation d'un word et regroupement des informations importantes obtenues dans les différents articles trouvés. PS: trop d'articles utilisés pour la bibliographie aujourd'hui sont de dates trop anciennes (avant les années 1980!). Par la suite, essayer de trouver des articles plus récents.
Compréhension de la base de données GitHub avec recherche et compréhension des codes du Harmonized System. Cela a permis entre autre de visualiser les grandes tendances des produits bois (attention , le travail mené ici n'avait pas pour but de découvrir en détail les imports/exports et producions de chaque pays, mais d'observer le fonctionnement des valeurs et les tendances générales).
# Journée du 22 avril
## Matinée
recherche de nouvelles revues et de nouveaux articles scientifiques en lien avec le sujet élaboré la semaine dernière, dans l'attente des consignes données pour l'utilisation des bases de données FAOSTAT et UN Comtrade. 
Utilisation de divers moteurs de recherches afin de diversifier la provenance des sources utilisées exemples de sites utilisés tels que scholar, web of science, sciencedirect, HAL. 
Problème rencontré: il existe de nombreuses études sur le commerce du bois en France et en Europe. Cependnt, la majeure partie de ces études semblent dater dune période assez vieille. Pour dire, aucun des documents ne parlant simplement du commerce et des flux de bois ne datait d'après 1986. Autrement dit, ces articles sont vieux, et certains datent même des années 70. Par conséquent, leur prise en compte éventuelle dans la bibliographie du sujet ne sera que partielle.
## Après-midi
exploitation des sources bibliographiques accumulées depuis le début du stage. Tri en profondeur des informations utiles au sujet et des informations dont on peut se dispenser. A noter qu'il manque peut-être de sources quant aux documents législatifs de l'union européenne quant aux plans de production et de transition énergétique (en rapport avec la bioéconomie et l'intensification de l'utilisation du bois énergie + impact sur les autres fuilières de produits bois et le marché qui en dépend.
# Journée du 23 avril
## Matinée
Reunion portant sur la compréhension des bases de données FAOSTAT et UN comtrade. (Note pour moi-même, fichier Word dispo avec d'eventuelles consignes oubliées anotées!) Analyse de la manière de récupérer les données et de comment les lire. En attente du programme python pour récupérer les données UN Comtrade, cependant on peut commencer la récupération des données qui nous intéressent sur FAOSTAT.
# Après-midi
Découverte de l'ensemble du classement harmonized system sur le site de l'organisation mondiale des douanes, et tri des données de produits bois et dérivés du bois. La seconde étape sera de conserver entre 5 et 10 produits des plus pertinents pour le sujet choisi, c'est-à-dire dans le domaine de l'énergie et de la construction. 
P.S: il faut encore chercher de la bibliographie sur la planification bioéconomique de l'union Européenne (horizon 2030 et 2050 notamment, et comparaison du chemin parcouru avec l'horizon 2020 pour se figurer l'évolution de la tendance européenne). 
# Journée du 24 avril
## Matinée
Réalisation d'une liste complète des produits bois et produits dérivés du bois avec leurs codes HS (harmonized system) respectifs depuis le site WEB de l'organisation mondiale des douanes. 
## Après-midi 
Finalisation de la liste entreprise durant la matinée et sélection des produits les plus pertinents vis-à-vis du sujet d'étude choisi (reste encore à confirmer le choix des produits via la disponibilité des données sur les différentes bases de données UN Comtrade et FAOSTAT). 
Poursuite de l'extraction des informations pertinentes au sein des oeuvres bibliographiques regroupées plus tôt pour constituer les connaissances sur le sujet. 
Mise à jour du journal de bord en fin de journée
# Journée du 25 avril
## Matinée
Vérification de certaies données sur UN Comtrade et FAOSTAT et vérification des données miroir. Problème relevé : la quasi-totalité des données étudiées semblent montrer des écarts significatifs entre les valeurs imports enregistrées et les valeurs d'exports enregistrées par les deux pays effectuant le commerce. 
Récupération du script automatisé pour le téléchargement des données de la base UN Comtrade. Installation de Anaconda 3 pour réaliser cette tâche. Synchronisation des fichiers du script avec le repository GitHub. 
## Après-midi
Correction et compréhension du script automatisé, récupération des données de la base UN Comtrade (le script fonctione correctement). Début de réflexion sur le tri des données à effectuer depuis le jeu de données brut téléchargé avec les fonctions de R studio, et réflexion sur les groupes de données à constituer. Réflexion également sur les méthodes statistiques à mobiliser et quels produits utiliser pour obtenir une reproductibilité correcte et des résultats exploitables.
Il reste désormais à commencer l'exploitation des données de la base FAOSTAT, mais le package R studio permettant l'automatisation du téléchargement n'existe plus. Il est possible en attendant de récupérer les donées une par une avec le click button de la page WEB, en attendant de trouver une méthode automatisée efficace.
# Journée du 26 avril
## Matinée
Rechecrhe de nouveaux articles en lien avec le sujet pour combler le manque d'informations
## Après-midi
Lecture d'articles scientifiques recherchés précedemment et extraction des informations importantes et pertinentes
# Journée du 29 avril
## Matinée
Bibliographie et rédaction des informations pertinentes
## Après-midi
Bibliographie et rédaction des informations pertinentes
P.S : de nombreux documents présentent visiblement les connaissances nécessaires au sujet, mais nécessitent un long temps de lecture et de tri en raison du grand nombre d'informations connexes au thème, mais qui ne concernent pas le bois et les produits bois, ainsi que leurs différents marchés (par exemple le biocommerce ne concerne pas seulement la forêt alors qu'il est un élément essentiel pour la compréhension du sujet du stage).
# Journée du 30 avril
## Matinée
Bibliographie, lecture des documents européens sur la structure de la forêt et des actions à mener
## Après-midi
Bibliographie, lecture des documents européens sur la structure de la forêt et des actions à mener
# Journée du 2 mai 
## Matinée
Bibliographie, lecture des documents européens sur la structure de la forêt et des actions à mener
## Après-midi
Bibliographie, lecture des documents européens sur la structure de la forêt et des actions à mener
# Journée du 3 mai
## Matinée
Bibliographie, lecture des documents européens sur la structure de la forêt et des actions à mener
## Après-midi
Bibliographie, lecture des documents européens sur la structure de la forêt et des actions à mener. Mise à jour du journal de bord (un certain retard avait été pris). Les informations concernant les objectifs de l'UE commencent à se préciser. Le script R va pouvoir être travaillé, il manque seulement quelques recherches permettant de justifier l'utilisation de tel ou tel produit forestier pour les études statistiques qui seront menées (ces produits seront certainement le pellet, le bois de charpente, l'OSB, et un dernier produit encore à définir (pourquoi pas le bois rond)).

Cependant, j'ai quelques réserves sur la quantité d'informations récoltées en lien avec le sujet. J'ai l'impression de ne pas avoir suffisamment de détails pour justifier les études statistiques et rédiger un rapport suffisamment complet. De plus, j'ai pris un peu de retard que je dois rattraper rapidement.
# Journée du 6 mai
## Matinée 
bibliographie
## Après-midi
bibliographie
# Journée du 7 mai
## Matinée 
Bibliographie
## Après-midi
Détermination de 5-6 produits bois pertinents pour l'étude et bibliographie en conséquence pour expliquer leur choix et leur pertinence dans l'étude statistique
Mise à jour du Script R pour préparer de manière organisée la rédaction du code en fonction des différents produits
Mise à jour du journal de bord
# Journée du 10 mai
## Matinée
Bibliographie
## Après-midi
Bibliographie
# Journée du 21 mai
## Matinée
Réunion pour faire un point sur l'avancement du stage et des différents problèmes rencontrés jusqu'à présent + plus précision sur certains points flous et éléments de réponses supplémentaires obtenus
Téléchargement des jeu de données
## Après-midi
Téléchargement des jeux de données (les données UN Comtrade sont finies d'être téléchargées)
début de la manipulation des jeux de données
# Journée du 22 mai
## Matinée
amélioration du script sur R studio
## Après-midi
Amélioration du script sur R Studio
