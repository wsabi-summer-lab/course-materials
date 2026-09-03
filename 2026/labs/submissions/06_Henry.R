#############
### SETUP ###
#############

# install.packages(c("dplyr", "ggplot2", "readr"))
library(dplyr)
library(ggplot2)
library(readr)

# set seed
set.seed(7)

# create output folder for plots
if (!dir.exists("plots")) dir.create("plots")

##############
### PART 1 ###
##############

# load data
diving_data = read_csv("06_diving.csv.gz", show_col_types = FALSE)

# Task 1:
# - Recreate the permutation-test setup from Lecture 6 for each judge
# - Build a judge-specific test statistic that compares the judge's scoring discrepancy
#   for same-country divers versus other divers
# - A natural starting point is:
#     observed_stat = mean(discrepancy for matched dives) - mean(discrepancy for unmatched dives)

# A "dive" is one diver's single attempt: it is scored simultaneously by all 7 judges.
# We identify each dive by Event + Round + Diver + DiveNo, then measure how far an
# individual judge's score sits from the consensus (mean) score on that same dive.
# This "discrepancy" removes the dive's intrinsic quality/difficulty, leaving only the
# judge-specific generosity/harshness on that dive.
diving_data = diving_data %>%
  group_by(Event, Round, Diver, DiveNo) %>%
  mutate(dive_mean = mean(JScore),
         discrepancy = JScore - dive_mean) %>%
  ungroup() %>%
  # match = 1 when the judge shares the diver's nationality, 0 otherwise
  mutate(match = as.integer(Country == JCountry))

# Per-judge observed statistic = mean discrepancy on same-country dives
# minus mean discrepancy on other-country dives. A positive value means the judge
# scores their own countrymen more generously than other divers.
# Only judges who scored at least one same-country diver (and at least one other)
# can be tested; the rest have an undefined matched mean and are dropped.
judges = diving_data %>%
  group_by(Judge) %>%
  summarise(n_total   = n(),
            n_matched = sum(match),
            obs_stat  = mean(discrepancy[match == 1]) -
                        mean(discrepancy[match == 0]),
            .groups = "drop") %>%
  filter(n_matched > 0 & n_matched < n_total)

# Task 2:
# - For each judge, generate a permutation null distribution by shuffling the match labels
#   while holding fixed the judge's discrepancy values
# - Use enough permutations that your p-values are reasonably stable
# - Compute the unadjusted permutation p-value for each judge

# Under the null of no nationality bias, the "same-country" label is exchangeable across
# the dives a judge scored: the discrepancy values stay put while we reshuffle which dives
# are flagged as matched. We use a two-sided test (bias could run either direction).
B = 5000

perm_pvalue = function(judge_name) {
  d = diving_data %>% filter(Judge == judge_name)
  disc = d$discrepancy
  m    = d$match
  n_match = sum(m)
  obs = mean(disc[m == 1]) - mean(disc[m == 0])
  n = length(disc)
  total = sum(disc)
  # vectorised permutation: each draw picks which n_match discrepancies are "matched",
  # so the permuted statistic is mean(matched) - mean(unmatched).
  perm_stats = replicate(B, {
    idx = sample.int(n, n_match)
    s_match = sum(disc[idx])
    mean_match = s_match / n_match
    mean_other = (total - s_match) / (n - n_match)
    mean_match - mean_other
  })
  # +1 correction so the p-value is never exactly 0
  (1 + sum(abs(perm_stats) >= abs(obs))) / (B + 1)
}

judges = judges %>%
  rowwise() %>%
  mutate(p_unadj = perm_pvalue(Judge)) %>%
  ungroup()

# Task 3:
# - Adjust the judge-level p-values for multiple testing
# - A good default is p.adjust(..., method = "BH")
# - Report both:
#   * the unadjusted p-values
#   * the adjusted p-values

judges = judges %>%
  mutate(p_adj = p.adjust(p_unadj, method = "BH")) %>%
  arrange(p_unadj)

cat("\n===== PART 1: Per-judge permutation test of nationality bias =====\n")
print(as.data.frame(judges %>%
  transmute(Judge, n_total, n_matched,
            obs_stat = round(obs_stat, 4),
            p_unadj  = round(p_unadj, 4),
            p_adj    = round(p_adj, 4))))

# Task 4:
# - Identify which judges show evidence of nationality bias before adjustment
# - Identify which judges still show evidence after adjustment
# - Make at least one plot that helps explain the strongest case(s)

sig_unadj = judges %>% filter(p_unadj < 0.05)
sig_adj   = judges %>% filter(p_adj   < 0.05)

cat("\nJudges significant BEFORE adjustment (p_unadj < 0.05):\n")
print(sig_unadj$Judge)
cat("\nJudges significant AFTER BH adjustment (p_adj < 0.05):\n")
print(sig_adj$Judge)

# --- Interpretation (numbers verified against the printed output above) ---
# 17 judges scored at least one same-country diver and could be tested.
# Before adjustment, 13 of them cross the 0.05 threshold. Every one of those 13 has a
# POSITIVE observed statistic: they scored same-country divers more generously than
# other divers, exactly the own-nationality bias the test is designed to detect.
# After the Benjamini-Hochberg correction (controlling the false-discovery rate over
# the 17 tests) the SAME 13 judges remain significant -- the effect is strong and
# widespread enough that the correction does not knock any of them out.
# The strongest, most reliable cases are the four judges tied at the smallest p-value
# (p_unadj = 0.0002): ALT Walter (+0.39), MENA Jesus (+0.30), BARNETT Madeleine (+0.29),
# and ZAITSEV Oleg (+0.28), each with a large same-country sample. Two judges show even
# bigger raw statistics (GEISSBUHLER +0.68, BURK +0.46) but on very few matched dives
# (3 and 10), so those point estimates are noisier. Only WANG Facheng, BOUSSARD,
# SEAMAN, and CALDERON show no significant own-country effect.

# Plot 1: permutation null distribution for the strongest judge, with the observed
# statistic marked. This visualises how extreme the observed bias is under the null.
strongest = judges$Judge[1]
d_str = diving_data %>% filter(Judge == strongest)
disc_str = d_str$discrepancy; m_str = d_str$match
n_str = length(disc_str); nm_str = sum(m_str); tot_str = sum(disc_str)
obs_str = judges$obs_stat[1]
null_dist = replicate(B, {
  idx = sample.int(n_str, nm_str)
  s = sum(disc_str[idx])
  s / nm_str - (tot_str - s) / (n_str - nm_str)
})

p1 = ggplot(data.frame(stat = null_dist), aes(x = stat)) +
  geom_histogram(bins = 50, fill = "grey75", colour = "white") +
  geom_vline(xintercept = obs_str, colour = "firebrick", linewidth = 1.1) +
  geom_vline(xintercept = -obs_str, colour = "firebrick",
             linetype = "dashed", linewidth = 0.8) +
  labs(title = paste0("Permutation null distribution: ", strongest),
       subtitle = paste0("Observed statistic = ", round(obs_str, 4),
                         " (red); two-sided p = ", round(judges$p_unadj[1], 4)),
       x = "mean(matched discrepancy) - mean(unmatched discrepancy)",
       y = "count") +
  theme_minimal()
ggsave("plots/p1_task4_null_distribution_strongest.png", p1,
       width = 8, height = 5, dpi = 120)

# Plot 2: observed statistic for every tested judge, ordered, coloured by whether
# the bias survives BH adjustment. Puts the strongest case in context.
plot_judges = judges %>%
  mutate(status = case_when(p_adj < 0.05 ~ "sig after BH",
                            p_unadj < 0.05 ~ "sig before BH only",
                            TRUE ~ "not sig"),
         Judge = factor(Judge, levels = Judge[order(obs_stat)]))

p2 = ggplot(plot_judges, aes(x = obs_stat, y = Judge, colour = status)) +
  geom_vline(xintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_point(size = 3) +
  scale_colour_manual(values = c("sig after BH" = "firebrick",
                                 "sig before BH only" = "orange",
                                 "not sig" = "grey50")) +
  labs(title = "Observed own-country scoring bias by judge",
       subtitle = "Positive = scores same-country divers above consensus",
       x = "observed statistic (matched - unmatched discrepancy)",
       y = NULL, colour = NULL) +
  theme_minimal()
ggsave("plots/p1_task4_observed_stat_by_judge.png", p2,
       width = 8, height = 6, dpi = 120)

##############
### PART 2 ###
##############

# load data
tto_data = read_csv("06_tto.csv.gz", show_col_types = FALSE)

# Variable map for the lecture notation:
# - y_i      = EVENT_WOBA_19
# - t_i      = ORDER_CT
# - BQ_i     = WOBA_FINAL_BAT_19
# - PQ_i     = WOBA_FINAL_PIT_19
# - hand_i   = HAND_MATCH
# - home_i   = BAT_HOME_IND

# Task 1:
# - Fit Model 1 from the lab handout using lm()
# - Model 1 uses indicators for 2TTO and 3TTO, plus the batter/pitcher/home/hand controls
# Model 1: y = b1 + b2*1{t>=2} + b3*1{t>=3} + bBQ*BQ + bPQ*PQ + bhand*hand + bhome*home
model1 = lm(EVENT_WOBA_19 ~ I(ORDER_CT >= 2) + I(ORDER_CT >= 3) +
              WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
              HAND_MATCH + BAT_HOME_IND,
            data = tto_data)

# Task 2:
# - Fit Model 2 from the lab handout using lm()
# - Model 2 adds a linear term in ORDER_CT on top of the Model 1 controls
# Model 2: y = b0 + b1*t + b2*1{t>=2} + b3*1{t>=3} + bBQ*BQ + bPQ*PQ + bhand*hand + bhome*home
#
# NOTE on the linear term. A literal linear ORDER_CT term is PERFECTLY collinear with
# the two indicators here, because ORDER_CT only takes values {1,2,3} and
# (ORDER_CT - 1) == 1{t>=2} + 1{t>=3} exactly; R would drop one coefficient as singular.
# The handout's Model 2 instead reports all three coefficients (b1=0.0016, b2=-0.0015,
# b3=-0.0083) and plots predictions "against batter sequence number". Using BATTER_SEQ_NUM
# as the linear term reproduces those handout numbers to the decimal, so that is the
# intended continuous term: cumulative batters faced (a fatigue / exposure proxy), with
# the indicators capturing any extra discrete jump at each turn of the order.
model2 = lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + I(ORDER_CT >= 2) + I(ORDER_CT >= 3) +
              WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
              HAND_MATCH + BAT_HOME_IND,
            data = tto_data)

# Task 3:
# - Run summary(...) on both models
# - Extract the coefficient estimates, standard errors, test statistics, and p-values
# - Interpret the coefficients tied to pitcher decline across times through the order
cat("\n===== PART 2: Model 1 summary =====\n")
print(summary(model1))
cat("\n===== PART 2: Model 2 summary =====\n")
print(summary(model2))

# Interpretation of the time-through-the-order (pitcher decline) coefficients:
#
# Model 1 estimates the jump in expected wOBA each time a batter sees the starter again.
# The 2TTO indicator b2 = 0.0133 (p = 7.9e-08) is positive and highly significant:
# the second time through the order, batters hit meaningfully better than the first time.
# The 3TTO indicator b3 = 0.0054 is the EXTRA bump going to the third time through, on top
# of b2; it is positive but only marginal (p = 0.067, just above 0.05). So the total
# decline from 1st to 3rd time through is about b2 + b3 = 0.019 wOBA. Higher wOBA allowed
# = pitcher decline. The controls behave sensibly: batter quality (BQ, +0.97) and pitcher
# allowed-quality (PQ, +0.96) both raise expected wOBA, a handedness match lowers it
# (b = -0.016), and the home batter gets a small boost (b = +0.010).
#
# Model 2 adds BATTER_SEQ_NUM, the cumulative number of batters the starter has faced
# (a smooth fatigue / exposure measure). Its slope b1 = 0.0016 per batter is positive and
# significant -- expected wOBA climbs steadily as the pitcher works deeper into the game.
# Once this smooth climb is in the model, the two TTO step indicators flip to small
# NEGATIVE values (b2 = -0.0015, b3 = -0.0083) and are no longer significant. In other
# words, there is no extra discrete penalty at the moment the order turns over beyond the
# steady decline already captured by batters-faced: the apparent "times through the order"
# penalty in Model 1 is largely a smooth function of total exposure, not a step change.

# Task 4:
# - State whether the estimated decline from one time through the order to the next
#   is statistically significant
# - Explain how the answer changes, if at all, between Model 1 and Model 2

# Joint test of the two time-through-the-order indicators in each model: do the discrete
# TTO steps add anything beyond the rest of the model? (anova compares to a model that
# drops both indicators.)
m1_noTTO = lm(EVENT_WOBA_19 ~ WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
                HAND_MATCH + BAT_HOME_IND, data = tto_data)
m2_noTTO = lm(EVENT_WOBA_19 ~ BATTER_SEQ_NUM + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
                HAND_MATCH + BAT_HOME_IND, data = tto_data)
cat("\nJoint test of the 2TTO + 3TTO indicators in Model 1:\n")
print(anova(m1_noTTO, model1))
cat("\nJoint test of the 2TTO + 3TTO indicators in Model 2 (with batters-faced):\n")
print(anova(m2_noTTO, model2))

# Answer:
# Yes -- in Model 1 the pitcher decline from one time through the order to the next is
# statistically significant. The 2TTO indicator (b2 = 0.0133) has p = 7.9e-08, far below
# 0.05, so we reject the null that it is zero; batters clearly hit better the second time
# through. The additional 3TTO step (b3 = 0.0054) is only marginal on its own (p = 0.067),
# but the two indicators are jointly highly significant (joint F-test p < 1e-7).
#
# The answer DOES change in Model 2. Once we control for BATTER_SEQ_NUM (total batters
# faced), the discrete step coefficients become small, negative, and individually
# non-significant (b2 p = 0.76, b3 p = 0.098), and the joint test of the two indicators
# is no longer significant at 0.05. The significant decline now lives in the smooth
# batters-faced slope (b1 = 0.0016, highly significant) instead. Interpretation: the
# pitcher's performance really does decline as the game goes on, but the evidence does
# NOT support a special penalty triggered each time the lineup turns over -- the decline
# is better described as a steady drift with cumulative exposure/fatigue than as discrete
# jumps at the 2nd and 3rd times through the order.

cat("\nScript completed.\n")
