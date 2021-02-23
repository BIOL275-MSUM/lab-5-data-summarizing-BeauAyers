
# Load Libraries ----------------------------------------------------------

library(tidyverse)


# Print Data --------------------------------------------------------------

iris
iris <- as_tibble(iris)
iris

# Question 1 --------------------------------------------------------------

iris_r <- rename(iris, sepal_length = Sepal.Length,
       sepal_width = Sepal.Width,
       petal_length = Petal.Length,
       petal_width = Petal.Width,
       species = Species
       )
iris_r

# Question 2 --------------------------------------------------------------

iris_cm <- mutate(iris_r, sepal_length = sepal_length * 10,
                  sepal_width = sepal_width * 10,
                  petal_length = petal_length * 10,
                  petal_width = petal_width * 10)
iris_cm

# Question 3 --------------------------------------------------------------

iris_area <- mutate(iris_cm, sepal_area = sepal_length * sepal_width,
                    petal_area = petal_length * petal_width) %>% 
  select(petal_area, sepal_area, species)

iris_area

# Question 4 --------------------------------------------------------------

summarize(
  iris_cm,
  sampl_size = n(),
  max = max(sepal_length),
  min = min(sepal_length),
  range = max - min,
  med = median(sepal_length),
  q1 = quantile(sepal_length, probs = 0.25),
  q3 = quantile(sepal_length, probs = 0.75),
  iqr = IQR(sepal_length)
)

# Question 5 --------------------------------------------------------------

iris_sum <-
  iris_cm %>%
  group_by(species) %>%
  summarize(
    sampl_size = n(),
    mean_w = mean(petal_width),
    str_dev = sd(petal_width),
    var = var(petal_width),
    sem = mean(petal_width) / sqrt(n()),
    ci_upper = mean_w + 2 * sem,
    ci_lower = mean_w - 2 * sem,
  )

iris_sum

# Question 6 --------------------------------------------------------------

ggplot(data = iris_cm) +
  geom_jitter(mapping = aes(x = species, y = petal_width))

# Question 7 --------------------------------------------------------------

ggplot(data = iris_cm) +
  geom_jitter(mapping = aes(x = species, y = petal_width)) +
  geom_crossbar(
    data = iris_sum,
    mapping = aes(x = species, y = mean_w, ymax = ci_upper, ymin = ci_lower),
    color = "darkorchid4"
  )

# Question 8 --------------------------------------------------------------

ggplot(data = iris_cm) +
  geom_point(mapping = aes(x = petal_length,
                          y = petal_width,
                          color = species)) 
