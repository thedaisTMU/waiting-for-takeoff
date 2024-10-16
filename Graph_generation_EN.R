library(tidyverse)
library(data.table)
library(ggplot2)
library(DaisTheme)
library(readr)
library(scales)

graph.data <-  fread("Graphs_spreadsheet.csv")

figure_1_data <- fread("Figure_1.csv")

fig_1 <- ggplot(figure_1_data, aes(x = financial_year, y = mean_tfp, group =C320010)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = C320010))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 1",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 1",Figure_title],
       x = graph.data[graph.data$Figure_number=="Figure 1",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 1",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 1", Legend_name])+
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = ""))+
  scale_color_manual(values = setNames(c("#eb0072", "#004c9b"),
                                       c(graph.data[graph.data$Figure_number == "Figure 1", "Legend_label_1"],
                                         graph.data[graph.data$Figure_number == "Figure 1", "Legend_label_2"])))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

figure_2_data <- fread("Figure_2.csv")

fig_2 <- ggplot(figure_2_data, aes(x = financial_year, y = mean_tfp, group =C320010)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = C320010))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 2",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 2",Figure_title],
       x = graph.data[graph.data$Figure_number=="Figure 2",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 2",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 2", Legend_name])+
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = ""))+
  scale_color_manual(values = setNames(c("#eb0072", "#004c9b"),
                                       c(graph.data[graph.data$Figure_number == "Figure 2", "Legend_label_1"],
                                         graph.data[graph.data$Figure_number == "Figure 2", "Legend_label_2"])))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

figure_3_data <- fread("Figure_3.csv")

fig_3 <- ggplot(figure_3_data, aes(x = financial_year, y = mean_tfp, group = EMPSIZE)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = EMPSIZE))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 3",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 3",Figure_title],
       x = graph.data[graph.data$Figure_number=="Figure 3",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 3",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 3", Legend_name])+
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = ""))+
  scale_color_manual(values = setNames(c("#eb0072", "#004c9b","#000000"),
                                       c(graph.data[graph.data$Figure_number == "Figure 3", "Legend_label_1"],
                                         graph.data[graph.data$Figure_number == "Figure 3", "Legend_label_2"],
                                         graph.data[graph.data$Figure_number == "Figure 3", "Legend_label_3"])))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

figure_4_data <- fread("Figure_4.csv")

fig_4 <- ggplot(figure_4_data, aes(x = factor(Year), fill=Treatment)) +
  dais.base.theme()+
  geom_boxplot(
    aes(ymin = Minimum,
        lower = Minimum,
        middle = `Difference in Productivity Growth`,
        upper = Maximum,
        ymax = Maximum),
    stat = "identity")+
  scale_fill_manual(values = setNames(c("#eb0072", "#004c9b"),
                                      c(graph.data[graph.data$Figure_number == "Figure 4", "Legend_label_1"],
                                        graph.data[graph.data$Figure_number == "Figure 4", "Legend_label_2"])),
                    limits = c(as.character(graph.data[graph.data$Figure_number == "Figure 4", "Legend_label_1"]),
                               as.character(graph.data[graph.data$Figure_number == "Figure 4", "Legend_label_2"])))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 4",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 4",Figure_title],
       x = graph.data[graph.data$Figure_number=="Figure 4",X_Axis],
       y = graph.data[graph.data$Figure_number=="Figure 4",Y_Axis],
       color = graph.data[graph.data$Figure_number=="Figure 4", Legend_name])+
  geom_hline(yintercept = 0)+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14))

export.dais.plot("Graph_exports_EN/Figure_1.pdf",fig_1,p.height = 6, p.width = 9)
export.dais.plot("Graph_exports_EN/Figure_2.pdf",fig_2,p.height = 6, p.width = 9)
export.dais.plot("Graph_exports_EN/Figure_3.pdf",fig_3,p.height = 6, p.width = 9)
export.dais.plot("Graph_exports_EN/Figure_4.pdf",fig_4,p.height = 6, p.width = 9)