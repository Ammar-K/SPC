# Load packages
library(tidyverse) # for creating and transforming data
library(qcc) # for control charts and process capability
library(SixSigma) # to use "ss.study.ca" function

# generate data
# We assume the following example: Contractors will install light poles.
# the spacing between poles are variable of interest/ Critical To Quality (CTQ),
# too much/little space between the poles will effect the lighting.
# there are 4 teams.

# Step 1: generate the data, this is not part of the six sigma,
# as the data usually coming from the actual data from the process.
poles <-
  tibble(distance = c(rnorm(100, 30.0, 0.01),
                      rnorm(100, 30.0, 0.1),
                      rnorm(100, 30.2, 0.01),
                      rnorm(100, 29.8, 0.1)),
         group = rep(1:4, each = 100))

# create mean and sd
poles_stat <-
  poles |>
  group_by(group) |>
  summarise(mean = mean(distance),
            sd = sd(distance)) |>
  ungroup()

### visual check
ggplot(poles, aes(x= distance, fill= as.factor(group))) +
  geom_histogram(binwidth = 0.01) +
  facet_grid(facets = "group") +
  # geom_vline(xintercept = 30) +
  geom_vline(data = poles_stat, aes(xintercept = mean), linetype = 2)+
  geom_vline(xintercept = c(29.5, 30.5), color = "red")+
  scale_x_continuous(limits = c(29.4, 30.6))


### visual check
ggplot(poles, aes(x= as.factor(group) , y= distance)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2, height = 0) +
  geom_path(data = poles_stat, aes(x= group, y= mean), color = "steelblue") +
  geom_point(data = poles_stat, aes(x= group, y= mean), color = "firebrick4") +
  geom_hline(yintercept = c(29.5, 30.5), color = "firebrick3") +
  labs(x = "distance", y = "group") +
  theme_bw()

"orchid"


library(ggpubr)
ggline(poles, x = "group", y = "distance",
       add = c("mean_se", "jitter"),
       ylab = "Weight", xlab = "Treatment")



### Anova and Tukey HSD test
anova_model <- aov(distance ~ as.factor(group), data = poles)
anova_model |> summary()
anova_model |> TukeyHSD()

###



poles_group <- qccGroups(data = poles, distance, group)

q1 <- qcc(poles_group, type = "xbar", sizes = 100)
plot(q1)

pq <- processCapability(q1, spec.limits = c(29.5, 30.5), target = 30)
plot(pq)


### six sigma package
SixSigma::ss.study.ca(xST = poles$distance[101:200],
                      xLT = poles$distance,
                      LSL = 29.5,
                      USL = 30.5,
                      Target = 30)



poles |>
  group_by(group) |>
  slice_sample(n = 3)



