# This scripts is to figure out how to match XG's definition
# of a 'decision' -- for checker plays and cube decisions.

devtools::load_all(path = "../bglab")
library(tidyverse)
data(bgmoves)

# Try to compute decisions for a couple of example matches
df <- bgmoves |> 
  filter(str_detect(file, "match9102134|match9284748|match8067917")) |> 
  select(-date, -player1, -player2, -pos_id, -match_id, -xgid) |> 
  mutate(forced_checker = str_count(move_eq, "\n") == 0 & play == "Rolls",
         max_checker_error = str_extract(move_eq, "\\([-\\d.]+\\)$") |> str_remove_all("\\(|\\)") |> as.numeric(),
         max_checker_error = coalesce(max_checker_error, 0),
         checker_decision = !forced_checker & max_checker_error <= -0.001 & play == "Rolls",
         max_cube_error = str_extract(cube_eq, "\\([-\\d.]+\\)\\n") |> str_remove_all("\\(|\\)|\\n") |> as.numeric(),
         max_cube_error = coalesce(max_cube_error, 0),
         cube_decision = (!is.na(cube_eq) & max_cube_error != 0 & max_cube_error >= -0.2) | 
           (play %in% c("Doubles", "Accepts", "Rejects")))


# Sanity check logic for cube decisions
df |> count(cube_decision)
df |> count(is.na(max_cube_error))

# Examples
ex <- df |> 
  group_by(cube_decision, proper_ca) |> 
  slice_sample(n = 1) |> 
  ungroup()

# Walk-through examples:
for (i in (1:nrow(ex))) {
  temp <- ex |> slice(i)
  
  cat("Move: ", temp$move_no, "\n",
      "Match to: ", temp$length, "\n",
      temp$board, "\n",
      temp$cube_eq, "\n",
      "Cube decision:", temp$cube_decision, "\n",
      "Turn: ", temp$turn, "\n",
      "Play: ", temp$play, "\n",
      "Cube action error: ", temp$cube_err, "\n",
      sep = "")

  readline(prompt="Press [enter] to continue")
}

# Sanity check logic for checker plays

# Forced plays cannot be decisions
df |> count(forced_checker, checker_decision)

# max_checker_error is never NA
df |> count(is.na(max_checker_error))

# Logic checks out: A play can *not* be a checker decision for 3 reasons:
# - It's a cube decision
# - No meaningful error is possible
# - It's a forced play
df |> count(checker_decision, forced_checker, play == "Rolls", max_checker_error <= -0.001)

# Compare no. of decisions with XG

# Count of decisions, game by game, specific match
df |> 
  mutate(match = str_remove(file, "_.{3}")) |> 
  filter(match == "match8067917.txt") |> 
  group_by(file, turn) |> 
  summarise(checker_decisions = sum(checker_decision),
            cube_decisions = sum(cube_decision)) |> 
  ungroup() |> 
  mutate(total = checker_decisions + cube_decisions) |> 
  view("games")

# Count of decisions, match by match (our examples matches)
df |> 
  mutate(match = str_remove(file, "_.{3}")) |> 
  group_by(match, turn) |> 
  summarise(checker_decisions = sum(checker_decision),
            cube_decisions = sum(cube_decision)) |> 
  ungroup() |> 
  mutate(total = checker_decisions + cube_decisions) |> 
  view("matches")

# Walk through one selected game with diffs to compare with XG

# In a specific game, pick positions that are NOT marked as 
# checker play decisions (but maybe should). So looking for
# decisions that we did not flag when they should have been.
ex <- df |> 
  filter(file == "match8067917_007.txt",
         turn == "marcel2710",
         !cube_decision,
         !checker_decision,
         !forced_checker)

for (i in (1:nrow(ex))) {
  temp <- ex |> slice(i)
  
  cat("Move: ", temp$move_no, "\n",
      "Match to: ", temp$length, "\n",
      temp$board, "\n",
      temp$cube_eq, "\n",
      temp$move_eq, "\n",
      "Cube decision:", temp$cube_decision, "\n",
      "Checker decision:", temp$checker_decision, "\n",
      "Turn: ", temp$turn, "\n",
      "Play: ", temp$play, "\n",
      "Cube action error: ", temp$cube_err, "\n",
      sep = "")
  
  readline(prompt="Press [enter] to continue")
}
