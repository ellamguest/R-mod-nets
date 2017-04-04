library(ggplot2)

#------------------
# CREATE DATA FRAME
#------------------
df.team_data <- expand.grid(teams = c("Team A", "Team B", "Team C", "Team D")
                            ,metrics = c("Metric 1", "Metric 2", "Metric 3", "Metric 4", "Metric 5")
)

# add variable: performance
set.seed(41)
df.team_data$performance <- rnorm(nrow(df.team_data))

#inspect
head(df.team_data)

ggplot(data = df.team_data, aes(x = metrics, y = teams)) +
  geom_tile(aes(fill = performance)) 

df = read.csv('/Users/emg/Programming/GitHub/the_donald_project/raw_data/all_mods_merged.csv')

df = df[ , which(names(df) %in% c("name","date"))]
df$present = 1

ggplot(data = df, aes(x = date, y = name)) +
  geom_tile(aes(fill = present)) 


library(lattice) 

weeks = read.csv('/Users/emg/Programming/GitHub/the_donald_project/tidy_data/mod_weeks.csv')
weeks = as.matrix(weeks)
levelplot(weeks[1:ncol(weeks),ncol(weeks):1])
