#' ---
#' title: try broom::augment
#' ---
#'

# quick try at an augment function
augment.nested <- function(x,
                           data = NULL,
                           newdata = NULL,
                           type.predict = c("link", "response"),
                           type.residuals = c("deviance", "pearson"),
                           se_fit = TRUE,    # it is FALSE in augment.glm()
                           ...)
{
  if (is.null(data))
    data <- model.frame(x$models[[1L]])
  if (missing(newdata))
    newdata <- x$models[[1L]]$data

  type.predict <- match.arg(type.predict)
  if (type.predict != "link") stop("type.predict = ", type.predict, " is not implemented.")

  nms <- names(x$models)
  result <- lapply(x$models, broom::augment,
                   data=data,
                   newdata=newdata,
                   type.predict = type.predict,
                   type.residuals = type.residuals,
                   se_fit = se_fit,
                   ...
  )

  cls <- class(result[[1L]])
  for (i in seq_along(x$models)) {
    result[[i]] <- cbind(response = nms[i], result[[i]])
  }
  result <- dplyr::bind_rows(result) |>
    select(-..y)
  class(result) <- cls
  result

}



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



wlf.augmented <- lapply(wlf.nested$models, broom::augment)
names(wlf.augmented)

# try our function
wlf.aug <- augment(wlf.nested)

names(wlf.aug)
# why don't we get the other variables computed by broom::augment.glm?
names(wlf.augmented[[1]])

#' Make the plot
wlf.aug <- wlf.aug |>
  mutate(response = factor(response, levels=c("work", "full")))

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
