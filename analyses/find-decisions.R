devtools::load_all(path = "../bglab")
library(tidyverse)
data(bgmoves)

df <- bgmoves |> 
  filter(str_detect(file, "match9102134|match9284748|match8067917")) |> 
  select(-date, -player1, -player2, -pos_id, -match_id, -xgid) |> 
  mutate(forced_checker = str_count(move_eq, "\n") == 0 & play == "Rolls",
         max_checker_error = str_extract(move_eq, "\\([-\\d.]+\\)$") |> str_remove_all("\\(|\\)") |> as.numeric(),
         max_checker_error = coalesce(max_checker_error, 0),
         checker_decision = !forced_checker & max_checker_error <= -0.001 & play == "Rolls",
         max_cube_error = str_extract(cube_eq, "\\([-\\d.]+\\)\\n") |> str_remove_all("\\(|\\)|\\n") |> as.numeric(),
         max_cube_error = coalesce(max_cube_error, 0),
         cube_decision = (!is.na(cube_eq) & max_cube_error != 0 & max_cube_error >= -0.2) | (play %in% c("Doublea", "Accepts")))


# CHECK DECISION LOGIC FOR CUBE PLAYS
df |> count(cube_decision)
df |> count(is.na(max_cube_error))

# Examples
ex <- df |> 
  group_by(cube_decision, proper_ca) |> 
  slice_sample(n = 1) |> 
  ungroup()

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

# CHECK DECISION LOGIC FOR CHECKER PLAYS

# Forced plays cannot be decisions
df |> count(forced_checker, checker_decision)

# max_checker_error is never NA
df |> count(is.na(max_checker_error))

# Logic checks out: A play can *not* be a checker decision for 3 reasons:
# - It's a cube decision
# - No meaningful error is possible
# - It's a forced play
df |> count(checker_decision, forced_checker, play == "Rolls", max_checker_error <= -0.001)


# COMPARE NO. OF DECISIONS WITH XG
df |> 
  mutate(match = str_remove(file, "_.{3}")) |> 
  group_by(match, turn) |> 
  summarise(checker_decisions = sum(checker_decision),
            cube_decisions = sum(cube_decision)) |> 
  ungroup() |> 
  mutate(total = checker_decisions + cube_decisions)
