library(ggplot2)
# devtools::install_github("tdhock/directlabels")
library(directlabels)
library(dplyr)
library(tidyr)

# p <- merge.data %>%
#   filter(wk_before_release %in% 0:-8) %>%
#   select(EngTitle, ChiTitle, wk_before_release, DI, Aware) %>%
#   tidyr::gather(key = variable, value = value, DI, Aware, na.rm = T) %>%
#   filter(grepl("少女時代|蟻人|變形金剛|環太平洋", ChiTitle)) %>%
#   ggplot() +
#   aes(x = wk_before_release, y = value, colour=EngTitle) +
#   facet_grid( ~ variable, scales = "free_y") +
#   geom_line(aes(group=EngTitle), size=1.5, alpha=0.8) +
#   scale_x_continuous("Week-before-release", breaks=(-8:0)) +
#   scale_y_continuous(labels = scales::percent) +
#   expand_limits(y=0)
#
# directlabels::direct.label(p, "first.qp")
