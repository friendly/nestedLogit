#' ---
#' title: test augment methods
#' ---
#'

library(nestedLogit)
library(dplyr)
library(broom)
library(ggplot2)
data(Womenlf, package = "carData")

wlf.nested <- nestedLogit(partic ~ hincome + children,
                          logits(work=dichotomy("not.work", c("parttime", "fulltime")),
                                 full=dichotomy("parttime", "fulltime")),
                          data=Womenlf)

# what does broom return?
broom::augment(wlf.nested$models[[1]]) |> head()
broom::augment(wlf.nested$models[[1]], se_fit = TRUE) |> head()

broom::augment(wlf.nested$models[[1]], type.predict = "response") |> head()
broom::augment(wlf.nested$models[[1]], type.predict = "response", se_fit = TRUE) |> head()

predict(wlf.nested) |> head()


#' try getting augment directly on each submodel
wlf.augmented <- lapply(wlf.nested$models, broom::augment)
names(wlf.augmented)

# try our function
wlf.aug <- augment(wlf.nested)

names(wlf.aug)
# why don't we get the other variables computed by broom::augment.glm?
names(wlf.augmented[[1]])

wlf.aug.probs <- augment(wlf.nested, type.predict = "probs")

# try using new data
new <- expand.grid(hincome = seq(0, 40, by = 5),
                   children = c("absent", "present"))

wlf.augmented.new <- lapply(wlf.nested$models, broom::augment, se_fit=TRUE, newdata=new)
names(wlf.augmented.new[[1]])

wlf.aug.new <- augment(wlf.nested, newdata = new)


#' Make the plot
wlf.aug <- wlf.aug |>
  mutate(response = ordered(response, levels=c("work", "full")))

gg <- ggplot(wlf.aug,
             aes(x=hincome, y=.fitted, color=children)) +
  geom_line(linewidth = 3) +
  geom_point(size = 1.5, shape = 16, color = "black") +
  scale_color_discrete() +
  labs(x="Husband's Income", y= "Log Odds") +
  facet_wrap(~ response, labeller = label_both) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.35, .85))

#' add error bars
gg + geom_errorbar(aes(ymin=.fitted-.se.fit, ymax=.fitted+.se.fit),
                   colour="black", width=.1)

#' better: use a ribbon
gg + geom_ribbon(aes(ymin=.fitted-.se.fit,
                     ymax=.fitted+.se.fit,
                     fill = children), alpha = 0.4)

#' label the lines directly
library(geomtextpath)
gg + geom_textline(aes(label = children), vjust=-0.5, size=6) +
  theme(legend.position = "none")


