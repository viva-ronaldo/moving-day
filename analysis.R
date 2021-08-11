library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

fix_names <- function(name_vec) {
    name_vec <- str_remove(name_vec, '\\*')
    sapply(name_vec,
           function(n) if (grepl(',', n)) paste(strsplit(n,', ')[[1]][2], strsplit(n,', ')[[1]][1]) else n
           ) %>% as.character() %>%
        str_to_title()
}

majors <- read.csv('/home/david/projects/moving_day_2/golf_majors_round_data_1990_2021.csv')
majors <- subset(majors, R2 >= 40) #drop one R2=20 case
majors$made_cut <- ifelse(majors$made_cut == 'True', TRUE, FALSE)
majors <- subset(majors, !(made_cut & is.na(R4)))  #drop a few missing R4s
majors <- majors %>% mutate(pos_change_R1R2 = pos_at_R2-pos_at_R1,
                            pos_change_R2R3 = pos_at_R3-pos_at_R2,
                            pos_change_R3R4 = pos_at_R4-pos_at_R3) %>% 
    mutate(player = fix_names(player),
           pos_at_R3 = ifelse(!made_cut, NA, pos_at_R3),
           pos_at_R4 = ifelse(!made_cut, NA, pos_at_R4))

#majors %>% count(comp, year, made_cut) %>% filter(n < 30)
#Previously found that some Open records were incomplete

#Positions changed between each pair of rounds
majors %>% filter(made_cut) %>% group_by(comp) %>% 
    summarise(mean_abs_pos_change_R1R2 = mean(abs(pos_change_R1R2)),
              mean_abs_pos_change_R2R3 = mean(abs(pos_change_R2R3)),
              mean_abs_pos_change_R3R4 = mean(abs(pos_change_R3R4)))
#More positions moved in R2 than R3
majors %>% filter(made_cut) %>% 
    pivot_longer(cols=c('pos_change_R1R2','pos_change_R2R3','pos_change_R3R4'), 
                 names_prefix = 'pos_change_', names_to = 'round_pair') %>% 
    ggplot() + geom_boxplot(aes(round_pair, abs(value), fill=round_pair)) +
    facet_wrap(~comp) +
    guides(fill = 'none') +
    ylim(0,40) +
    labs(x='Round pair', y='Absolute position change')
#or violin
majors %>% filter(made_cut) %>% 
    pivot_longer(cols=c('pos_change_R1R2','pos_change_R2R3','pos_change_R3R4'), 
                 names_prefix = 'pos_change_', names_to = 'round_pair') %>% 
    ggplot() + geom_violin(aes(round_pair, value, fill=round_pair)) +
    facet_wrap(~comp) +
    guides(fill = 'none') +
    labs(x='Round pair', y='Position change')
#Distributions of moves
majors %>% filter(made_cut) %>% 
    pivot_longer(cols=c('pos_change_R1R2','pos_change_R2R3','pos_change_R3R4'), 
                 names_prefix = 'pos_change_', names_to = 'round_pair') %>% 
    ggplot() + geom_histogram(aes(value)) + facet_wrap(~round_pair) +
    geom_vline(xintercept=0, linetype=2) +
    xlim(-50,50) +
    labs(x='Position change', y='Count', 
         title='Position changes become smaller as rounds go on') +
    theme_minimal() 
#

#All moves as points, just from 2021 comps
majors %>% filter(made_cut) %>% 
    pivot_longer(cols=c('pos_change_R1R2','pos_change_R2R3','pos_change_R3R4'), 
                 names_prefix = 'pos_change_', names_to = 'round_pair') %>% 
    filter(year==2021) %>%
    mutate(mark_pt = ifelse(pos_at_R4 == 1, TRUE, FALSE)) %>%
    ggplot() + geom_jitter(aes(round_pair, value, colour=comp), width=0.15, height=0, size=1) +
    scale_y_continuous(labels = function(x) ifelse(x==0,'0',sprintf('%+g',x))) +
    labs(x='Round pair', y='Position change', colour=NULL,
         title='') +
    theme_light() + theme(legend.position=c(0.8,0.15)) + guides(colour = guide_legend(nrow=2))
#Post plot?

#Grouped moves
majors %>% filter(made_cut) %>% 
    pivot_longer(cols=c('pos_change_R1R2','pos_change_R2R3','pos_change_R3R4'),
                 names_prefix = 'pos_change_', names_to = 'round_pair') %>% 
    mutate(move_group = cut(value, breaks=c(-160,-25,-10,-3,2,9,24,160),
                            labels=c('Gain >=25','Gain 10-24','Gain 3-9',
                                     'Move < 3',
                                     'Lose 3-9','Lose 10-24','Lose >=25'))) %>%
    count(round_pair, move_group) %>% 
    mutate(round_pair = case_when(
        round_pair == 'R1R2' ~ 'R1 -> R2',
        round_pair == 'R2R3' ~ 'R2 -> R3',
        round_pair == 'R3R4' ~ 'R3 -> R4'
    )) %>%
    ggplot() + geom_col(aes(round_pair, n, fill=move_group), colour='black', position='stack', width=0.5) +
    scale_fill_brewer(palette='RdGy') +
    labs(x=NULL, y='Total number of moves', fill='Places moved',
         title='Big position changes become less likely as rounds go on',
         subtitle='Majors, 1990-2021; showing only players making cut') +
    theme_minimal()
#middle group gets bigger as rounds go on; clearly more big moves in R2 than R3, and possibly R4 than R3.
ggsave('moving_day_2_grouped_moves_stacked_bars_plot_1.png', width=6, height=5)
#**Post plot 1

#IQR of round, total scores
majors %>% filter(made_cut) %>% group_by(comp,year) %>% 
    summarise(r1_iqr=IQR(R1), r2_iqr=IQR(R2), r3_iqr=IQR(R3), r4_iqr=IQR(R4),
              r1_10pc = quantile(R1,0.1), r2_10pc = quantile(R2,0.1),
              r3_10pc = quantile(R3,0.1), r4_10pc = quantile(R4,0.1)) %>%
    summarise(r1_iqr=mean(r1_iqr), r2_iqr=mean(r2_iqr), r3_iqr=mean(r3_iqr), r4_iqr=mean(r4_iqr),
              r1_10pc=mean(r1_10pc), r2_10pc=mean(r2_10pc), r3_10pc=mean(r3_10pc), r4_10pc=mean(r4_10pc))
#Round score IQR is higher R3,4 (3.8, 3.9) than R1,2 (3.1, 3.0) - may be a made_cut bias; R3 not higher than R4
#Extreme low scores (10th percentile) are no lower in r3 than r4
majors %>% filter(made_cut) %>% group_by(comp, year) %>% 
    summarise(r1_iqr=IQR(total_at_R1), r2_iqr=IQR(total_at_R2), 
              r3_iqr=IQR(total_at_R3), r4_iqr=IQR(total_at_R4)) %>%
    summarise(mean(r1_iqr), mean(r2_iqr), mean(r3_iqr), mean(r4_iqr))
#Total scores IQR clearly increases as rounds go on (3.1, 3.7, 5.3, 7.1)
#Moving day ease depends on the r2 value, but transitioning to the r3 value as round goes on
#Including players missing cut IQRs are 4.2, 6.5, 5.3, 7.0.

#Round scores
majors %>% pivot_longer(cols=c('R1','R2','R3','R4'), names_to = 'round') %>% 
    filter(made_cut) %>% 
    ggplot() + geom_boxplot(aes(round, value, fill=round), width=0.5) +
    facet_wrap(~comp)
#R3 scores are not lower or more spread out than others
#Round total scores
majors %>% 
    mutate(topar_at_R1 = total_at_R1 - 71,
           topar_at_R2 = total_at_R2 - 2*71,
           topar_at_R3 = total_at_R3 - 3*71,
           topar_at_R4 = total_at_R4 - 4*71) %>%
    pivot_longer(cols=c('topar_at_R1','topar_at_R2','topar_at_R3','topar_at_R4'), 
                 names_prefix = 'topar_at_', names_to = 'round') %>% 
    filter(made_cut) %>% 
    ggplot() + geom_violin(aes(round, value, fill=round)) +#, width=0.5) +
    facet_wrap(~comp) +
    #ylim(30,-20) +
    scale_y_continuous(limits=c(30,-20), trans = 'reverse', labels = function(x) ifelse(x==0, '0', sprintf('%+g',x)))
#Total scores become more spread out as round increases
#Post plot 2?

#Mean strokes needed to move 5 places, at start of each round
#Take shortcut by getting strokes per position, for the middle 80% of players to reduce noise of bottom few
majors %>% filter(made_cut) %>% 
    group_by(comp, year) %>% slice_min(total_at_R2, n=30) %>% 
    summarise(n_players = n(), 
              scores_range_top30 = max(total_at_R2)-min(total_at_R2),
              strokes_per_pos_at_R2_top30 = scores_range_top30 / n_players, .groups='drop') %>%
    summarise(mean(strokes_per_pos_at_R2_top30))
#0.23 strokes per pos in top 30 after R2
majors %>% filter(made_cut) %>% 
    group_by(comp, year) %>% slice_min(total_at_R3, n=30) %>% 
    summarise(n_players = n(), 
              scores_range_top30 = max(total_at_R3)-min(total_at_R3),
              strokes_per_pos_at_R3_top30 = scores_range_top30 / n_players, .groups='drop') %>%
    summarise(mean(strokes_per_pos_at_R3_top30))
#0.31 strokes per pos in top 30 after R3
#(0.15, 0.23, 0.31, 0.38 for r1-4)

#Kendall's tau values of positions between rounds
majors %>% filter(made_cut) %>% 
    group_by(comp, year) %>%
    summarise(r1r2corr = cor(pos_at_R1, pos_at_R2, method='kendall'),
              r2r3corr = cor(pos_at_R2, pos_at_R3, method='kendall'),
              r3r4corr = cor(pos_at_R3, pos_at_R4, method='kendall'), .groups='drop_last') %>%
    summarise(r1_r2 = mean(r1r2corr),
              r2_r3 = mean(r2r3corr),
              r3_r4 = mean(r3r4corr), .groups='drop') %>%
    pivot_longer(cols=c('r1_r2','r2_r3','r3_r4')) %>%
    ggplot(aes(name,value,colour=comp)) + geom_path(aes(group=comp), size=1) + geom_point(size=3) +
    labs(y='Kendall correlation between rankings', x=NULL, colour=NULL,
         title='Rankings become more stable as rounds go on') +
    theme_light() + theme(legend.position=c(0.15,0.85))
#The rankings are more static as time goes on in all cases;
#  so there is generally more movement in R3 than R4, but not as much as R2
majors %>% filter(made_cut) %>% 
    group_by(year, comp) %>%
    summarise(r1_r2 = cor(pos_at_R1, pos_at_R2, method='kendall'),
              r2_r3 = cor(pos_at_R2, pos_at_R3, method='kendall'),
              r3_r4 = cor(pos_at_R3, pos_at_R4, method='kendall'), .groups='drop_last') %>%
    pivot_longer(cols=c('r1_r2','r2_r3','r3_r4')) %>%
    mutate(comp = case_when(
        comp == 'masters' ~ 'The Masters',
        comp == 'theopen' ~ 'The Open',
        comp == 'usopen' ~ 'US Open',
        comp == 'uspga' ~ 'US PGA'
    )) %>%
    ggplot(aes(year,value,colour=name)) + geom_path(aes(group=name), size=1) + #geom_point(size=3) +
    facet_wrap(~comp) +
    geom_label(data = data.frame(comp='US PGA', x=1994.5, y=c(0.38,0.58,0.7), 
                                name=c('r1_r2','r2_r3','r3_r4'), label=c('R1-R2','R2-R3','R3-R4')), 
              aes(x,y,fill=name, label=label), size=4, colour='black', alpha=0.5) +
    scale_colour_brewer(palette = 'Accent') +
    scale_fill_brewer(palette = 'Accent') +
    labs(y='Kendall correlation between rankings at end round', x=NULL, colour=NULL,
         title='Round Three wasn\'t a moving day in the 90s, either',
         subtitle='Leaderboard correlations between rounds, excluding players missing cut') +
    theme_light() + guides(colour = 'none', fill='none') + 
    theme(strip.background = element_rect(fill='lightskyblue'),
          strip.text = element_text(colour='black'))
ggsave('moving_day_2_kendall_corrs_facet_time_series_post_plot_2.png', width=7, height=5)
#**Post plot 3

#Biggest R3 moves ending in the top 10, and their final position
majors %>% filter(pos_at_R3 <= 10, made_cut) %>% arrange(+pos_change_R2R3) %>% 
    select(comp, year, player, pos_at_R2, pos_at_R3, pos_change_R2R3, pos_at_R4) %>% head(10)
#D Johnson rose from 60th to 9th, ended 8th
#K Aphibarnrat rose from 58th to 7th, ended 15th
#Weekley rose from 55th to 6th, finished 9th
#Of the top 10 risers in R3, 5 dropped back in R4, 4 rose (1 won), 1 stayed same
#Of the top 15 risers in R3, 9 dropped back in R4, 5 rose (1 won), 1 stayed same
#B Molder rose 53rd to 8th then dropped back to 43rd

#Some dumbbell plot of individual moves
majors %>% filter(pos_at_R3 <= 10, made_cut) %>% arrange(+pos_change_R2R3) %>% 
    select(comp, year, player, pos_at_R2, pos_at_R3, pos_change_R2R3, pos_at_R4) %>% head(50) %>%
    mutate(slipped_back_R4 = ifelse(pos_at_R4 > pos_at_R3, TRUE, FALSE),
           mark_pt = ifelse(pos_at_R4 == 1, TRUE, FALSE)) %>% 
    pivot_longer(cols=c('pos_at_R2','pos_at_R3','pos_at_R4'), names_to = 'round') %>%
    mutate(mark_pt = ifelse(mark_pt & round=='pos_at_R4', TRUE, FALSE)) %>%
    ggplot() + geom_path(aes(round, value, group=interaction(player,year,comp), colour=slipped_back_R4), alpha=0.7) +
    #ggrepel::geom_text_repel(aes(round, ifelse(round=='pos_at_R2',value,NA), label=player), size=3) +
    geom_point(aes(round, ifelse(mark_pt,value,NA)), colour='black', size=4) +
    geom_point(aes(round, value, colour=slipped_back_R4), size=1.5) +
    scale_colour_manual(values = c('TRUE'='dodgerblue', 'FALSE'='orange')) +
    scale_y_continuous(limits=c(60,1), labels=c(60,40,20,1), trans = 'reverse') +
    labs(x=NULL, y='Position', title='Fifty largest R3 moves into top ten',
         subtitle='Only 35% hold or improve position in R4') +
    guides(colour='none') +
    theme_light()
#32/50 fall back in R4

#How often does the winner come via a big move in R3?
majors %>% filter(pos_at_R4==1) %>% 
    group_by(comp) %>% summarise(frac_leading_R2 = mean(pos_at_R2==1),
                                 frac_leading_R3 = mean(pos_at_R3==1),
                                 frac_top10_R2 = mean(pos_at_R2 <= 10),
                                 frac_top10_R3 = mean(pos_at_R3 <= 10))
#30-55% of winners were leading after R2, and this only increases to 50-60% by end R3
#  (biggest increase for masters: 33->58%.)
#90% winners were in top10 after R2, increasing to 98% after R3 - 
#  so about 8% of time the winner moved from >10 into top 10 on day 3.
majors %>% filter(pos_at_R4==1) %>% 
    group_by(comp) %>% summarise(`frac_Round One` = 1-mean(pos_at_R1<=10),
                                 `frac_Round Two` = 1-mean(pos_at_R2<=10),
                                 `frac_Round Three` = 1-mean(pos_at_R3<=10)) %>% 
    pivot_longer(cols=c('frac_Round One','frac_Round Two','frac_Round Three'), names_prefix='frac_') %>%
    mutate(name = factor(name, levels=c('Round One','Round Two','Round Three')),
           ) %>%
    ggplot() + geom_path(aes(name, value, group=comp, colour=comp), size=0.8) + geom_point(aes(name, value, colour=comp), size=3) + 
    ylim(0,0.5) + expand_limits(x=0.3) +
    scale_colour_brewer(palette='Set2') + scale_fill_brewer(palette='Set2') +
    guides(colour='none', fill='none') +
    geom_label(data = data.frame(name=c(0.76,0.66,0.65,0.72), value=c(0.46,0.38,0.32,0.24),
                                 comp=c('uspga','theopen','usopen','masters'),
                                 label=c('US PGA','The Open','US Open','The Masters')),
               aes(name, value, label=label, fill=comp), colour='black', alpha=0.5) +
    labs(title='Winning is already unlikely if not in the top ten after Round Two',
         subtitle='Majors, 1990-2021',
         y='Fraction of winners NOT in top ten at end round', x=NULL, colour=NULL) +
    theme_light() + theme(legend.position = c(0.8,0.8), axis.text = element_text(size=10))
ggsave('moving_day_2_win_chance_in_top_ten_by_round_plot_post_3.png', width=6, height=5)
#**Plot post 4

#Serial R3 movers
majors %>% filter(pos_change_R2R3 < -15, made_cut) %>% count(player,sort=TRUE) %>% head(5)
#Stricker gained at least 15 places 13 times (from 57 apps) but usually only moving to ~20th
#Snedeker gained at least 15 places 10 times (from 47 apps)
#Poulter gained at least 15 places 9 times (from 61 apps)
#Woods gained at least 15 places 9 times (from 73 apps)
#Toms gained at least 15 places 8 times (from 50 apps)
majors %>% filter(pos_change_R2R3 < -10, pos_at_R3 <= 10, made_cut) %>% count(player,sort=TRUE) %>% head(5)
#Meaningful moves (10 places into top 10): Woods 9 (0 wins, 3 seconds), 
#  Harrington 6 (1 win), Fowler 6 (1 second), McIlroy 5, Grace 5 (1 third)
majors %>% filter(pos_change_R2R3 < -10, pos_at_R3 <= 10, made_cut) %>% count(pos_at_R4) %>% mutate(frac = n/sum(n))
#these moves lead to a win 3% of time (7 times, always from 4-8th at R3)

#Full view of one competition, dumbbell
#2012 Open, mean abs pos changes for anyone in top 20 R2-4 are 18 R1-R2, 10 R2-R3, 12 R3-4
# majors %>% filter(comp=='theopen', year==2012, made_cut,
#                   pos_at_R2 <= 20 | pos_at_R3 <= 20 | pos_at_R4 <= 20) %>%
#     mutate(topar_at_R1 = total_at_R1 - 71,
#            topar_at_R2 = total_at_R2 - 2*71,
#            topar_at_R3 = total_at_R3 - 3*71,
#            topar_at_R4 = total_at_R4 - 4*71) %>%
#     mutate(final_pos = pos_at_R4,
#            mark_line = player %in% c('Ernie Els','Adam Scott','Brandt Snedeker')) %>%
#     pivot_longer(cols=c('pos_at_R2','pos_at_R3','pos_at_R4'), names_to = 'round') %>%
#     ggplot() + geom_path(aes(round, value, group=player, colour=final_pos), 
#                          position=position_jitter(width=0, height=0.3)) +
#     #geom_point(aes(round, value, colour=final_pos))
#     #ggrepel::geom_text_repel(aes(round, ifelse(round=='pos_at_R2',value,NA), label=player), size=3) +
#     #geom_point(aes(round, ifelse(mark_pt,value,NA)), colour='black', size=4) +
#     #geom_point(aes(round, value, colour=slipped_back_R4), size=1.5) +
#     scale_color_gradient(low='green', high='blue') +
#     scale_y_continuous(trans='reverse', limits=c(30,1), labels=c(1,10,20,30)) +
#     #ylim(5,-15) +
#     labs(x=NULL, y='Score', title='') +
#     guides(colour='none') +
#     theme_light()
majors %>% filter(comp=='theopen', year==2012, made_cut) %>%
    ggplot() + geom_histogram(aes(pos_change_R1R2))


