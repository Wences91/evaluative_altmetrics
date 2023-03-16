library(ggplot2)
library(scales)
library(ggh4x)

# 1. Density
df_b <- read.delim('data/Twitter/tweeters_bots.tsv',
                 check.names = FALSE)

dens <- density(df_b$score_over)
df <- data.frame(x=dens$x, y=dens$y)
df$quant <- factor(findInterval(df$x,4))

ggplot(df, aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant), alpha=0.9) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_vline(xintercept = 4, size = 0.5, linetype = 'dashed') +
  scale_fill_manual(values=c('#00acdf', '#e6195f'), name="fill", guide='none') +
  labs(x='Botscore', y='Density (%)') +
  theme_classic() +
  theme(axis.title = element_text(size=14, face='bold', family = 'Arial'),
        axis.text = element_text(size=12, color='black', family = 'Arial'))


# 2. Scatter plot

## 2.1. Twitter

df <- read.delim('data/Twitter/publications_metrics_mean.tsv',
                 check.names = FALSE)


df_m <- data.frame()
for(i in c('Avg_fav', 'Avg_RT', 'Avg_fw', 'RT')){
  df_aux <- df[,c('Details Page URL', 'Type', 'Mentions', i)]
  names(df_aux)[length(names(df_aux))] <- 'Value'
  df_aux$Metric <- i
  
  df_m <- rbind.data.frame(df_m, df_aux, stringsAsFactors = FALSE)
  
  rm(df_aux)
}

top_paper_1 <- 'https://www.altmetric.com/details/112716251'
top_paper_2 <- 'https://www.altmetric.com/details/80184497'

df_m$Color <- 'All'

df_m$Color[which(df_m$`Details Page URL`==top_paper_1)] <- top_paper_1
df_m$Color[which(df_m$`Details Page URL`==top_paper_2)] <- top_paper_2

df_m <- df_m[which(df_m$Metric %in% c('Avg_fw', 'Avg_fav', 'Avg_RT') & df_m$Type!='Bot'),]

df_m$Metric <- factor(df_m$Metric,
                      levels = c('Avg_fav', 'Avg_RT', 'Avg_fw'),
                      labels = c('Avg. favourite', 'Avg. retweet', 'Av. followers'),
                      ordered = TRUE)

ggplot() +
  geom_point(data=df_m[which(!(df_m$`Details Page URL` %in% c(top_paper_1, top_paper_2))),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=2, alpha=0.6) +
  geom_point(data=df_m[which(df_m$`Details Page URL`==top_paper_1),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=3, alpha=0.9) +
  geom_point(data=df_m[which(df_m$`Details Page URL`==top_paper_2),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=3, alpha=0.9) +
  #geom_abline(linetype='longdash', color='grey80') +
  scale_y_continuous(labels = ~ format(.x, scientific = FALSE))+
  scale_color_manual(labels = c('All', '10.1038/s41586-021-03740-8', '10.1111/gcb.15123'), values = c('#78B7C5', '#e82313', '#E1AF00')) +
  theme_light() +
  labs(x='Mentions (tweets)', y='Audience', color='') +
  theme(legend.position = 'bottom',
        panel.spacing.y = unit(0.75, 'cm'),
        axis.title = element_text(size=12, face='bold'),
        axis.text.y=element_text(hjust=0),
        strip.background = element_rect(fill='black'),
        strip.text.x = element_text(size=12, face='bold'),
        strip.text.y = element_text(size=9),
        panel.background = element_rect(fill='grey95')) +
  ggh4x::facet_grid2(Type~Metric, scales = 'free', independent = 'all', switch='y', strip = strip_themed(background_y = elem_list_rect(fill = 'grey30', 'grey30')))


## 2.2. News

df <- read.delim('data/News/publications_metrics_mean_special.tsv',
                 check.names = FALSE)

df$main_category[which(df$main_category=='news_and_media')] <- 'News and media'
df$main_category[which(df$main_category=='specialized_media')] <- 'Specialized media'

df_m <- data.frame()
for(i in c('Avg_visits', 'Avg_time', 'Avg_pages', 'Avg_rank', 'Avg_bounce')){
  df_aux <- df[,c('Details Page URL', 'main_category', 'Mentions', i)]
  names(df_aux)[length(names(df_aux))] <- 'Value'
  df_aux$Metric <- i
  
  df_m <- rbind.data.frame(df_m, df_aux, stringsAsFactors = FALSE)
  
  rm(df_aux)
}

top_paper_1 <- 'https://www.altmetric.com/details/86953658'
top_paper_2 <- 'https://www.altmetric.com/details/91140259'

df_m$Color <- 'All'

df_m$Color[which(df_m$`Details Page URL`==top_paper_1)] <- top_paper_1
df_m$Color[which(df_m$`Details Page URL`==top_paper_2)] <- top_paper_2
  
df_m <- df_m[which(df_m$Metric %in% c('Avg_bounce', 'Avg_visits', 'Avg_time')),]

df_m$Metric <- factor(df_m$Metric,
                      levels = c('Avg_visits', 'Avg_time', 'Avg_bounce'),
                      labels = c('Avg. visits', 'Avg. time', 'Avg. bounce'),
                      ordered = TRUE)

ggplot() +
  geom_point(data=df_m[which(!(df_m$`Details Page URL` %in% c(top_paper_1, top_paper_2))),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=2, alpha=0.6) +
  geom_point(data=df_m[which(df_m$`Details Page URL`==top_paper_1),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=3, alpha=0.9) +
  geom_point(data=df_m[which(df_m$`Details Page URL`==top_paper_2),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=3, alpha=0.9) +
  #geom_abline(linetype='longdash', color='grey80') +
  scale_y_continuous(labels = ~ format(.x, scientific = FALSE))+
  scale_color_manual(labels = c('All', '10.1016/j.biocon.2020.108596', '10.1016/j.envres.2020.110223'), values = c('#78B7C5', '#e82313', '#E1AF00')) +
  theme_light() +
  labs(x='Mentions', y='Audience', color='') +
  theme(legend.position = 'bottom',
        panel.spacing.y = unit(0.75, 'cm'),
        axis.title = element_text(size=12, face='bold'),
        axis.text.y=element_text(hjust=0),
        strip.background = element_rect(fill='black'),
        strip.text.x = element_text(size=11, face='bold'),
        strip.text.y = element_text(size=10),
        panel.background = element_rect(fill='grey95')) +
  ggh4x::facet_grid2(main_category~Metric, scales = 'free', independent = 'all', switch='y', strip = strip_themed(background_y = elem_list_rect(fill = 'grey30', 'grey30')))


## 2.3. Wikipedia

df <- read.delim('data/Wikipedia/publications_metrics_mean.tsv',
                 check.names = FALSE)

df_m <- data.frame()
for(i in c('Avg_views', 'Avg_edits', 'Avg_words', 'Avg_trans')){
  df_aux <- df[,c('Details Page URL', 'major_topic', 'Mentions', i)]
  names(df_aux)[length(names(df_aux))] <- 'Value'
  df_aux$Metric <- i
  
  df_m <- rbind.data.frame(df_m, df_aux, stringsAsFactors = FALSE)
  
  rm(df_aux)
}

top_paper_1 <- 'https://www.altmetric.com/details/554744'
top_paper_2 <- 'https://www.altmetric.com/details/59053776'

df_m$Color <- 'All'

df_m$Color[which(df_m$`Details Page URL`==top_paper_1)] <- top_paper_1
df_m$Color[which(df_m$`Details Page URL`==top_paper_2)] <- top_paper_2

df_m <- df_m[which(df_m$Metric %in% c('Avg_words', 'Avg_trans', 'Avg_views')),]

df_m$Metric <- factor(df_m$Metric,
                      levels = c('Avg_views', 'Avg_words', 'Avg_trans'),
                      labels = c('Avg. views', 'Avg. words', 'Avg. transl.'),
                      ordered = TRUE)

df_m <- df_m[which(!(df_m$major_topic %in% c('Geography', 'Culture', 'Other'))),]

ggplot() +
  geom_point(data=df_m[which(!(df_m$`Details Page URL` %in% c(top_paper_1, top_paper_2))),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=2, alpha=0.6) +
  geom_point(data=df_m[which(df_m$`Details Page URL`==top_paper_1),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=3, alpha=0.9) +
  geom_point(data=df_m[which(df_m$`Details Page URL`==top_paper_2),], aes(x=Mentions, y=Value, color=Color), stroke=0, size=3, alpha=0.9) +
  #geom_abline(linetype='longdash', color='grey80') +
  scale_y_continuous(labels = ~ format(.x, scientific = FALSE))+
  scale_color_manual(labels = c('All', '10.1038/nclimate1329', '10.1038/s41559-019-0887-1'), values = c('#78B7C5', '#e82313', '#E1AF00')) +
  theme_light() +
  labs(x='Mentions', y='Audience', color='') +
  theme(legend.position = 'bottom',
        panel.spacing.y = unit(0.75, 'cm'),
        axis.title = element_text(size=12, face='bold'),
        axis.text.y=element_text(hjust=0),
        strip.background = element_rect(fill='black'),
        strip.text.x = element_text(size=12, face='bold'),
        strip.text.y = element_text(size=10),
        panel.background = element_rect(fill='grey95')) +
  ggh4x::facet_grid2(major_topic~Metric, scales = 'free', independent = 'all', switch = 'y', strip = strip_themed(background_y = elem_list_rect(fill = 'grey30', 'grey30')))


# 3. Profiles

align_plots1 <- function (...) {
  pl <- list(...)
  stopifnot(do.call(all, lapply(pl, inherits, "gg")))
  gl <- lapply(pl, ggplotGrob)
  bind2 <- function(x, y) gtable:::rbind_gtable(x, y, "first")
  combined <- Reduce(bind2, gl[-1], gl[[1]])
  wl <- lapply(gl, "[[", "widths")
  combined$widths <- do.call(grid::unit.pmax, wl)
  grid::grid.newpage()
  grid::grid.draw(combined)
}


## 3.1. Twitter

# Researcher
researcher <- ''

df <- read.delim('data/Authors/tweets_metrics.tsv',
                 check.names = FALSE)

df_ty <- read.delim('data/Authors/tweets_types.tsv',
                 check.names = FALSE)

ggplot(df_ty[df_ty$new_author_id==researcher,], aes(x='', y=Mentions, fill=Type)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = 'bottom')


ggplot(df_ty[df_ty$new_author_id==researcher,], aes(x=Type, y=Mentions)) +
  geom_col(fill='#15c6e6') +
  labs(x='') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.text.x = element_text(color = 'black', angle=90, size = 12, hjust = 1),
        axis.title.y = element_text(size=14, vjust = 0.5, angle = 90),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())

df$Retweets <- df$Retweets/df$Mentions
df$Local <- df$Local/df$Mentions

metric <- 'Mentions'
metric_label <- 'Mentions'

df_max <- df[which(df$Papers>4),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p1 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())

metric <- 'Retweets'
metric_label <- 'Retweets'

df_max <- df[which(df$Papers>4),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p2 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())


metric <- 'Local'
metric_label <- 'Local'

df_max <- df[which(df$Papers>4),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p3 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())


metric <- 'Avg_fw'
metric_label <- 'Avg. foll.'

df_max <- df[which(df$Papers>4),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p4 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())



metric <- 'Avg_fav'
metric_label <- 'Avg. fav.'

df_max <- df[which(df$Papers>4),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p5 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())



metric <- 'Avg_RT'
metric_label <- 'Avg. RT'

df_max <- df[which(df$Papers>4),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p6 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())

align_plots1(p1, p2, p3, p4, p5, p6)


## 3.2. News

# Researcher
researcher <- ''

df <- read.delim('data/Authors/news_metrics.tsv',
                 check.names = FALSE)

df_ty <- read.delim('data/Authors/news_types.tsv',
                    check.names = FALSE)

df_ty[which(df_ty$main_category=='news_and_media'), 'main_category'] <- 'News\nand media'
df_ty[which(df_ty$main_category=='specialized_media'), 'main_category'] <- 'Specialized\nmedia'

ggplot(df_ty[df_ty$new_author_id==researcher,], aes(x=main_category, y=Mentions)) +
  geom_col(fill='#15c6e6') +
  labs(x='') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.text.x = element_text(color = 'black', angle=90, size = 14, vjust=0.5, hjust = 1),
        axis.title.y = element_text(size=14, vjust = 0.5, angle = 90),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())

df$Local <- df$Local/df$Mentions

metric <- 'Mentions'
metric_label <- 'Mentions'

df_max <- df[which(df$Papers>2),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p1 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())


metric <- 'Local'
metric_label <- 'Local'

df_max <- df[which(df$Papers>2),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p2 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())


metric <- 'Visits'
metric_label <- 'Avg. visits'

df_max <- df[which(df$Papers>2),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p3 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())



metric <- 'Time_Visits'
metric_label <- 'Avg. time'

df_max <- df[which(df$Papers>2),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p4 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())



metric <- 'Bounce'
metric_label <- 'Avg. bounce'

df_max <- df[which(df$Papers>2),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p5 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())


align_plots1(p1, p2, p3, p4, p5)


## 3.3. Wikipedia

# Researcher
researcher <- ''

df <- read.delim('data/Authors/wikipedia_metrics.tsv',
                 check.names = FALSE)

df_ty <- read.delim('data/Authors/wikipedia_types.tsv',
                    check.names = FALSE)

df_ty[which(df_ty$major_topic=='History and Society'), 'major_topic'] <- 'History and\nSociety'


ggplot(df_ty[df_ty$new_author_id==researcher,], aes(x=major_topic, y=Mentions)) +
  geom_col(fill='#15c6e6') +
  labs(x='') +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        axis.text.x = element_text(color = 'black', angle=90, size = 14, vjust=0.5, hjust = 1),
        axis.title.y = element_text(size=14, vjust = 0.5, angle = 90),
        axis.text.y = element_text(size=12, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())

df$Local <- df$Local/df$Mentions

metric <- 'Mentions'
metric_label <- 'Mentions'

df_max <- df[which(df$Papers>1),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p1 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())


metric <- 'Local'
metric_label <- 'Local'

df_max <- df[which(df$Papers>1),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p2 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())


metric <- 'Avg_views'
metric_label <- 'Avg. views'

df_max <- df[which(df$Papers>1),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p3 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())


metric <- 'Avg_words'
metric_label <- 'Avg. words'

df_max <- df[which(df$Papers>1),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p4 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())

metric <- 'Avg_trans'
metric_label <- 'Avg. transl.'

df_max <- df[which(df$Papers>1),]
df_max <- df_max[order(df_max[,metric], decreasing = TRUE)[1],]

p5 <- ggplot() +
  geom_bar(data=df_max, aes(x='', y=.data[[metric]]), fill='grey90', stat="identity", width=1.2) +
  geom_bar(data=df[df$new_author_id==researcher,], aes(x='', y=.data[[metric]]), fill='#1d9bf0', stat="identity", width=1.2) +
  labs(x=metric_label, y='') +
  coord_flip() +
  #theme_minimal() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.margin = unit(c(0, 0, 0, 0), "null"),
        #axis.title.x = element_text(size=18, vjust = 4, angle = 90),
        axis.title.y = element_text(size=12, vjust = 0.5, angle = 0),
        axis.text.y = element_text(size=15, color='black'),
        axis.ticks = element_blank(),
        axis.line = element_blank())

align_plots1(p1, p2, p3, p4, p5)
