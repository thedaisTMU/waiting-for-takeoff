library(tidyverse)
library(data.table)
library(ggplot2)
library(DaisTheme)
library(readr)
library(scales)

graph.data <-  fread("Graphs_spreadsheet.csv", encoding = "Latin-1")
graph.translation <- fread("Graphs_translation.csv")

figure_1_data <- fread("Figure_1.csv")
translation_f1 <- graph.translation[str_detect(Figure, "Figure 1,")]

figure_1_data[, C320010 := fifelse(
  str_detect(C320010, translation_f1$English[2]), translation_f1$French[2],
  fifelse(str_detect(C320010, translation_f1$English[3]), translation_f1$French[3], C320010)
)]

fig_1 <- ggplot(figure_1_data, aes(x = financial_year, y = mean_tfp, group =C320010)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = C320010))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 1",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 1",Figure_title_FR],
       x = graph.data[graph.data$Figure_number=="Figure 1",X_Axis_FR],
       y = graph.data[graph.data$Figure_number=="Figure 1",Y_Axis_FR],
       color = graph.data[graph.data$Figure_number=="Figure 1", Legend_name_FR])+
  scale_y_continuous(labels = label_currency(prefix = "", suffix = " $", decimal.mark = ",")) +
  scale_color_manual(values = setNames(c("#eb0072", "#004c9b"),
                                       c(graph.data[graph.data$Figure_number == "Figure 1", "Legend_label_1_FR"],
                                         graph.data[graph.data$Figure_number == "Figure 1", "Legend_label_2_FR"])))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

figure_2_data <- fread("Figure_2.csv")
translation_f2 <- graph.translation[str_detect(Figure, "Figure 2")]

figure_2_data[, C320010 := fifelse(
  str_detect(C320010, translation_f2$English[2]), translation_f2$French[2],
  fifelse(str_detect(C320010, translation_f2$English[3]), translation_f2$French[3], C320010)
)]

fig_2 <- ggplot(figure_2_data, aes(x = financial_year, y = mean_tfp, group =C320010)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = C320010))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 2",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 2",Figure_title_FR],
       x = graph.data[graph.data$Figure_number=="Figure 2",X_Axis_FR],
       y = graph.data[graph.data$Figure_number=="Figure 2",Y_Axis_FR],
       color = graph.data[graph.data$Figure_number=="Figure 2", Legend_name_FR])+
  scale_y_continuous(labels = label_currency(prefix = "", decimal.mark = ",")) +
  scale_color_manual(values = setNames(c("#eb0072", "#004c9b"),
                                       c(graph.data[graph.data$Figure_number == "Figure 2", "Legend_label_1_FR"],
                                         graph.data[graph.data$Figure_number == "Figure 2", "Legend_label_2_FR"])))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

figure_3_data <- fread("Figure_3.csv")

fig_3 <- ggplot(figure_3_data, aes(x = as.numeric(Year), 
                                   fill = Treatment, 
                                   y = `Difference in Productivity Growth`,
                                   color = Treatment)) +
  dais.base.theme()+
  geom_errorbar(width=0.2, size=0.8,
                aes(ymin = Minimum,
                    ymax = Maximum))+
  geom_point(size=5)+
  scale_color_manual(values = c("Before"="#eb0072", "After"="#004c9b"))+
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "black", size = 1) + # Vertical line between 2019 and 2020
  annotate("text", x = 2018, y = 1.4, label = graph.data[graph.data$Figure_number=="Figure 3",Legend_label_1_FR], hjust = 0.5, color = "black", size = 5, family="Replica-Regular") +
  annotate("text", x = 2021, y = 1.4, label = graph.data[graph.data$Figure_number=="Figure 3",Legend_label_2_FR], hjust = 0.5, color = "black", size = 5, family="Replica-Regular") +
  scale_y_continuous(limits = c(-1.5,1.5))+
  labs(title = graph.data[graph.data$Figure_number=="Figure 3",Figure_number],
       subtitle = graph.data[graph.data$Figure_number=="Figure 3",Figure_title_FR],
       x = graph.data[graph.data$Figure_number=="Figure 3",X_Axis_FR],
       y = graph.data[graph.data$Figure_number=="Figure 3",Y_Axis_FR])+
  geom_hline(yintercept = 0)+
  theme(
    legend.position="none",
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 14))

export.dais.plot("Graph_exports_FR/Figure_1.pdf",fig_1,p.height = 6, p.width = 9)
export.dais.plot("Graph_exports_FR/Figure_2.pdf",fig_2,p.height = 6, p.width = 9)
export.dais.plot("Graph_exports_FR/Figure_3.pdf",fig_3,p.height = 6, p.width = 9)
