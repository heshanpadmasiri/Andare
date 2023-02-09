# Andare

## Quest config format
```toml
title = "Quest title"

starting_quest = "subquest_0"
ending_quests = ["subquest_7", "subquest_8"]
# Add var values and plot strings will use "{}" notation to indicate where they needs to be filled with

[[subquests]] # starting_point
name = "subquest_0"
plot = ["str", "str"]
end_type = "player_choice"
next = [["prompt", "subquest_1"], ["promot", "subquest_6"], ["prompt", "subquest_2"]]

[[subquests]]
name = "subquest_1"
plot = [".."]
end_type = "continue"
next = "subquest_5"

[[subquests]]
name = "subquest_6"
plot = [".."]
end_type = "random_event"
next = [["0.7", "subquest_3"], ["0.2", "subquest_4"]]

...

[[subquests]]
name = "subquest_7"
plot = [".."]
end_type = "terminal"
```
