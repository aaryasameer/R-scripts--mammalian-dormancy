# Torpor strategies and activity pattern
Figuring out if there is a connection between an animal's activity pattern (nocturnal/diurnal/crepuscular,etc.) and their ability to demonstrate dormancy
##files in this dataset
packages used- readxl, ggplot2, dplyr
data- excel file containing list of mammals which exhibit dormancy traits (torpor, hibernation, etc.)
result- contains bar plots, probability distribution graphs and statistical tests- chi sq. test, logistic regression using glm
##key findings
nocturnal species were over represented in the mammalian data, even when compared with non-dormant species
resampled the dataset (bootstrap= 1000, with replacement) to judge the stability of dormancy rates wrt activity pattern 
odds of dormancy for baseline group cathemeral/crepuscular = 0.014 (low odds)
diurnals are 5.52x more likely to show dormancy as compared to baseline
nocturnals are 15.83x more likely to show dormancy as compared to baseline
