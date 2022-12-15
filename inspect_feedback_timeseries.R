library(tidyverse)



feedback = read_csv('a_DMN_Feedback_2_Scale5_all_frames_feedback.csv', col_names = c('frame', 'roi_max', 'cen', 'dmn'))


feedback_long = feedback %>%
    pivot_longer(c('cen', 'dmn'))


ggplot(feedback_long, aes(x = frame, y = value, color = name)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 25) 


print(2)