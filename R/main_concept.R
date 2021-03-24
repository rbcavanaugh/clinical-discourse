##### main concept ####
library(tidyverse)

# broken window main concepts:
main_concepts <- read_csv("www/main_concepts.csv")%>%
  group_by(task) %>% mutate(id = row_number())

main_concepts$concept_length = 4-rowSums(is.na(main_concepts))

scoring_mca = tibble(
  Result = c("AC", "AI", "IC", "II", "Absent"),
  score = c(3, 2, 2, 1, 0)
)

tb = tibble(
  num = c(8, 10, 10, 10, 34),
  prefix = c('bw', 'cr', 'u', 's', 'c'),
  name = c("broken_window", "cat_rescue", "refused_umbrella", "sandwich", "cinderella")
)

get_concepts <- function(concept){
  tb = tb %>%
    filter(name == concept)
  
  l = tagList()
  for(i in 1:tb[[1,1]]){
    l[[i]] =  HTML(paste0(get(paste0(tb[[1,2]], i)), "<hr>"))
  }
  return(l)
  
}

get_sentence_data <- function(rds_list){
  a = rds_list
  newlist = list()
  for(i in 1:length(a)){
    
    newlist[[i]] = expand_grid(
      concept = i,
      sentences = unlist(a[[i]])
      
    )
  }
  return(bind_rows(newlist))
}


# broken window

bw1 <- 
  div(style = "line-height: 160%;",
      tags$ol(start = 1,
        tags$li(tags$em(tags$sup("1"), tags$strong("The boy"),HTML('&nbsp;'), tags$sup("2"), tags$strong("was"),HTML('&nbsp;'), tags$sup("3"), tags$strong("outside."))),
        tags$ol(style = "padding-bottom:5px;",
          tags$li("He since referent is unambiguous; some give the boy a name"),
          tags$li("Is, decided to go"),
          tags$li("In his front yard, on the lawn, out of the house, etc.")
          ),
        tags$p("Note: Sometimes, this concept was combined with number 2 in a statement like
         The boy was playing soccer outside or The boy was kicking the ball in the yard.
         These statements would receive full credit for both concept 1 and 2.", style = "color:black; font-size: .9em;line-height: 120%;")
        )
      )


bw2 <- 
  div(style = "line-height: 160%;",
      tags$ol(start = 2,
        tags$li(tags$em(tags$sup("1"), tags$strong("A/The boy"),HTML('&nbsp;'), tags$sup("2"), tags$strong("was"),HTML('&nbsp;'), tags$sup("3"), tags$strong("playing soccer"))),
        tags$ol(style = "padding-bottom:5px;",
          tags$li("See 1.1."),
          tags$li("Played, is kicking, kicks, is practicing, etc."),
          tags$li("With the soccer ball, with the ball, with the football* (*if not from U.S.).")),
       
        tags$p("Note: “He has a ball” or “He has a soccer ball” did not count towards this
         concept because it does not imply any kind of action with the soccer ball,
         and boy-action-ball was the concept that met criterion.", style = "color:black; font-size: .9em;line-height: 120%;")
        )
      )
  
bw3 <- 
  div(style = "line-height: 160%;",
      tags$ol(start = 3,
        tags$li(tags$em(tags$sup("1"), tags$strong("The ball"),HTML('&nbsp;'), tags$sup("2"), tags$strong("breaks"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the"), "man's/neighbor's", tags$strong("window."))),
        tags$ol(style = "padding-bottom:5px;",
          tags$li("Soccer ball, football"),
          tags$li("Goes through, went through, crashes through/into, flew through, sails through/into, shattered, is kicked through"),
          tags$li("glass"))
      )
  )


bw4 <- 
  div(style = "line-height: 160%;",
      tags$ol(start = 4,
        tags$li(tags$em(tags$sup("1"), tags$strong("The man"),HTML('&nbsp;'), tags$sup("2"), tags$strong("is sitting"), "in a chair and/or inside the house.")),
        tags$ol(style = "padding-bottom:5px;",
          tags$li("His dad, his father, the father, the neighbor, the guy; some give the man a name"),
          tags$li("Lounging, resting, relaxing, inside")),
       
        tags$p('Note: Most common were "The man is sitting", "The man is inside", "The man is sitting inside"', style = "color:black; font-size: .9em;line-height: 120%;"),
        tags$p('Note: "The man is watching TV" or something similar did not count for this concept; that was a separate relevant concept that did not meet criterion. However, if an individual said, "The man is sitting watching TV" then they would receive credit for this concept since they included "sitting."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


bw5 <-
  div(style = "line-height: 160%;",
      tags$ol(start = 5,
              tags$li(tags$em(tags$sup("1"), tags$strong("The man"),HTML('&nbsp;'), tags$sup("2"), tags$strong("was startled"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 4.1"),
                tags$li("Surprised, amazed, afraid, astonished, freaked out, stunned, shocked, angry, upset, not happy, mad")),
             
              tags$p('Note: Most common were "The man is sitting", "The man is inside", "The man is sitting inside"', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Occasionally, this concept was combined with number 3, in a statement such as "The ball crashed through the window and startled the man."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )



bw6 <-
  div(style = "line-height: 160%;",
      tags$ol(start = 6,
              tags$li(tags$em(tags$sup("1"), tags$strong("The ball"),HTML('&nbsp;'), tags$sup("2"), tags$strong("broke"),HTML('&nbsp;'), tags$sup("3"), tags$strong("a lamp"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 3.1"),
                tags$li("Knocks down/over, smashes into, breaks, hit"),
                tags$li("No alternative for lamp was produced"))
      )
  )


bw7 <-
  div(style = "line-height: 160%;",
      tags$ol(start = 7,
              tags$li(tags$em(tags$sup("1"), tags$strong("The man"),HTML('&nbsp;'), tags$sup("2"), tags$strong("picked up"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the ball"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 4.1"),
                tags$li("Grabs, gets, holds/is holding, catches, captures, has"),
                tags$li("See 3.1")),
             
              tags$p("Note: Occasionally, “The man stands up with the ball” and “The man jumps up with the ball” was used to express this concept, expressing that the man had performed some action to hold on to the ball", style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

  
  
bw8 <-
  div(style = "line-height: 160%;",
      tags$ol(start = 8,
              tags$li(tags$em(tags$sup("1"), tags$strong("The man"),HTML('&nbsp;'), tags$sup("2"), tags$strong("looked"),HTML('&nbsp;'), tags$sup("3"), tags$strong("out of the window"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 4.1"),
                tags$li("Looks, is looking"),
                tags$li("Outside, out, out of the glass")),
             
              tags$p("Note: The man goes to the window, The man went to the window, or The man goes outside, etc., did not count towards this concept. These were separate relevant concepts that did not meet criterion.", style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )



# cat rescue
cr1 <-
  div(style = "line-height: 160%;",
      tags$ol(start = 1,
              tags$li(tags$em(tags$sup("1"), tags$strong("The little girl"),HTML('&nbsp;'), tags$sup("2"), tags$strong("was riding"),HTML('&nbsp;'), tags$sup("3"), tags$strong("her bicycle"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("She (if appropriate referent), the girl, the child, any girl’s name"),
                tags$li("Rode, rides, was on, is playing on, stopped riding, got off, was beside, has"),
                tags$li("Bike, tricycle, trike, it (if appropriate referent)"))
      )
  )


cr2<-
  div(style = "line-height: 160%;",
      tags$ol(start = 2,
              tags$li(tags$em(tags$sup("1"), tags$strong("The cat"),HTML('&nbsp;'), tags$sup("2"), tags$strong("was in"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the tree"), "because the dog chased it.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Kitty, kitten, it (if appropriate referent), any cat name"),
                tags$li("Was up, was stuck in, got stuck in, climbed up, ran up, goes up, gets in, was caught in, ends up in, was on, was chased up, was scared up"),
                tags$li("The tree limb, limb")),
             
              tags$p('Note: Sometimes expressed as "The dog (2) chased (1) the cat (3) up the tree." or "The girl (2) saw (1) the cat (3) in the tree."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )




cr3<-
  div(style = "line-height: 160%;",
      tags$ol(start = 3,
              tags$li(tags$em(tags$sup("1"), tags$strong("The dog"),HTML('&nbsp;'), tags$sup("2"), tags$strong("was barking"),HTML('&nbsp;'), tags$sup("3"), "up the tree")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("It (if appropriate referent), puppy, pup, any dog name"),
                tags$li("Barks, is barking, barked, is yelping")),
             
              tags$p('Note: "The dog chased the cat" should not apply to this statement as it was a separate relevant concept that did not meet threshold but was occasionally combined with additional elements that could apply to MC2', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )



cr4<-
  div(style = "line-height: 160%;",
      tags$ol(start = 4,
              tags$li(tags$em(tags$sup("1"), tags$strong("The man"),HTML('&nbsp;'), tags$sup("2"), tags$strong("climbed up"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the tree"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("The neighbor, the father, dad, daddy, someone older, big brother, he (if appropriate referent), any man’s name"),
                tags$li("Was climbing, climbed, climbs, ran up, goes up into, got up on, crawls in/on"),
                tags$li("The branch, the limb, the ladder, it (if appropriate referent), there"))
      )
  )



cr5<-
  div(style = "line-height: 160%;",
      tags$ol(start = 5,
              tags$li(tags$em(tags$sup("1"), tags$strong("The man"),HTML('&nbsp;'), tags$sup("2"), tags$strong("tries to rescue"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the cat"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 4.1"),
                tags$li("Wants to help, wants to rescue, tries to get, attempts to get, tries to reach, goes to get, tries to retrieve, went up after, comes to rescue"),
                tags$li("See 2.1")),
             
              tags$p('Note: Frequently combined with MC 4 as in "The man climbed up the tree to get the cat."; a person who says this should receive full credit for MCs 4 and 5.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Occasionally combined with MC 7 as in "He’s stuck in the tree trying to get the cat."; a person who says this should receive full credit for MCs 5 and 7.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


cr6<-
  div(style = "line-height: 160%;",
      tags$ol(start = 6,
              tags$li(tags$em(tags$sup("1"), tags$strong("The ladder"),HTML('&nbsp;'), tags$sup("2"), tags$strong("fell down"),HTML('&nbsp;'))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("It (if appropriate referent)"),
                tags$li("Is down, falls, fell, has fallen, has fallen down, got away from him, is on the ground, has slipped away, has dropped away, fell off, has been knocked down")),
             
              tags$p('Note: Sometimes expressed with an agent that caused the ladder to fall, such as the wind or dog (e.g., "the dog knocked the ladder down").', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )



cr7<-
  div(style = "line-height: 160%;",
      tags$ol(start = 7,
              tags$li(tags$em(tags$sup("1"), tags$strong("The father"),HTML('&nbsp;'), tags$sup("2"), tags$strong("is stuck"),HTML('&nbsp;'), tags$sup("3"), tags$strong("in the tree"), "with the cat.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 4.1, the man and the cat, they (if appropriate referents)"),
                tags$li("Is up, is, is stranded, is caught, ended up, is marooned, is sitting"),
                tags$li("On the branch, on the limb, up there")),
             
              tags$p('Note: Sometimes expressed as: "(1) The man (2) couldn’t (3) get down."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

cr8<-
  div(style = "line-height: 160%;",
      tags$ol(start = 8,
              tags$li(tags$em(tags$sup("1"), tags$strong("Someone"),HTML('&nbsp;'), tags$sup("2"), tags$strong("called"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the fire department"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("The mother, the neighbor, the lady next door, the girl, the father, a passerby, an onlooker, he/she/they"),
                tags$li("Notifies, alerts, got"),
                tags$li("The firemen, 911")),
             
              tags$p('Note: Sometimes expressed as a passive such as: " (3) The fire department (2) has been called."', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: For this concept, a pronoun without a preceding referent is scored as AC since this action is not depicted in the picture stimuli.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


cr9<-
  div(style = "line-height: 160%;",
      tags$ol(start = 9,
              tags$li(tags$em(tags$sup("1"), tags$strong("The fire department"),HTML('&nbsp;'), tags$sup("2"), tags$strong("comes"),HTML('&nbsp;'), tags$sup("3"), tags$strong("with a ladder"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("The firefighters, the firemen, the fire truck, they (if appropriate referent or if includes ladder or other context so that the referent is not ambiguous)"),
                tags$li("Is on the way, is/are coming, came, have arrived, rushes out, brings")),
             
              tags$p('Note: Sometimes combined with MC 8 as in "The mother called the fire department to come with their ladder." A person who says this should receive full credit for MCs 8 and 9.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: While the first two essential elements met 66% threshold, the final element “with a ladder” was only produced by 33% of the sample.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )



cr10<-
  div(style = "line-height: 160%;",
      tags$ol(start = 10,
              tags$li(tags$em(tags$sup("1"), tags$strong("The fire department"),HTML('&nbsp;'), tags$sup("2"), tags$strong("rescues"),HTML('&nbsp;'), tags$sup("3"), tags$strong("them"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 9.1 (but not fire truck)"),
                tags$li("Saves, is going to get, helps, gets, will take"),
                tags$li("The man, the cat, the man and the cat")),
             
              tags$p('Note: Often combined with MC 9 as in "The fire department comes with a ladder to rescue them." A person who says this should receive full credit for MCs 9 and 10.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Sometimes combined with MC 8 and MC 9 as in "The mother called the fire department to come and rescue the father with a ladder." A person who says this should receive full credit for MCs 8, 9, and 10.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


# umbrella
u1<-
  div(style = "line-height: 160%;",
      tags$ol(start = 1,
              tags$li("The mother says", tags$em(tags$sup("1"), tags$strong("It's going to"),HTML('&nbsp;'), tags$sup("2"), tags$strong("rain"), HTML('&nbsp;'), "today.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("It’s supposed to, it might, it’s predicted, it looks like, there’s a chance"),
                tags$li("1.2.	Sprinkle, drizzle, storm")),
             
              tags$p('Note: Occasionally produced as " (2) Rain (1) is in the forecast."', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Statements that implied bad weather was on the way e.g. "the weather was looking gray and cloudy outside" do not count towards this MC as it was another relevant concept that did not meet threshold.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('The statement "It is raining." does not apply to this MC; see MC 5.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

u2<-
  div(style = "line-height: 160%;",
      tags$ol(start = 2,
              tags$li("The mother says", tags$em(tags$sup("1"), tags$strong("you"),HTML('&nbsp;'), tags$sup("2"), tags$strong("need to take"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the umbrella"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("He (if appropriate referent), the boy, (male name)"),
                tags$li("Carry, take, have, need, should have, might need, might want")),
             
              tags$p('Note: Sometimes produced as a command with the subject implied, e.g., "take this umbrella" these statements were considered AC since English allows the subject to be dropped in a command.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Sometimes produced as "(1) his mother (2) offers him (3) an umbrella." or similar.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Occasionally produced as a question "Don’t (1) you (2) want to take (3) this umbrella?"', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Sometimes produced " (2) here is (1) your (3) umbrella."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

u3<-
  div(style = "line-height: 160%;",
      tags$ol(start = 3,
              tags$li(tags$em(tags$sup("1"), tags$strong("The boy"),HTML('&nbsp;'), tags$sup("2"), tags$strong("(does something to refuse)"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the umbrella"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("He (if appropriate referent), the boy, (male name), I (if reported speech)"),
                tags$li("Doesn’t want, refuses, won’t/is not going to take, declines, says no, says he’ll be ok without"),
                tags$li("It (if appropriate referent)")),
             
              tags$p('Note: Occasionally this concept was stated as "He won’t do it." in reference to the mother trying to make him take the umbrella, so the action he "won’t do" is "take the umbrella" and this should receive an AC as long as the referent is produced.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

u4<-
  div(style = "line-height: 160%;",
      tags$ol(start = 4,
              tags$li(tags$em(tags$sup("1"), tags$strong("The boy"),HTML('&nbsp;'), tags$sup("2"), tags$strong("walks"),HTML('&nbsp;'), tags$sup("3"), tags$strong("to school"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 3.1, a child"),
                tags$li("Saves, is going to get, helps, gets, will take"),
                tags$li("Goes, leaves, heads, takes off, starts, sets"),
                tags$li("Outside, out of the house, out, to/for/towards [location], down the road, off, out of the door, further, forth, down, in the rain")),
             
              tags$p('Note: Sometimes the order of elements was switched, e.g. "(3) Off to school (1) he (2) goes” ', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

u5<-
  div(style = "line-height: 160%;",
      tags$ol(start = 5,
              tags$li(tags$em(tags$sup("1"), tags$strong("It"),HTML('&nbsp;'), tags$sup("2"), tags$strong("is raining"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("The rain, the deluge"),
                tags$li("Starts to pour, starts coming down, is falling, is sprinkling, gets harder, gets heavier, is raining, begins to rain, starts to rain, starts falling, comes, is coming down, starts raining, started sprinkling, started, rained")),
             
              tags$p('Note: Sometimes produced as a colloquialism, "The sky opens up" or "We have a downpour."', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Occasionally produced as "Here (2) comes (1) the rain."', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Do not count utterances about rain "increasing"" in severity (e.g., "It starts to rain harder.").', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

u6<-
  div(style = "line-height: 160%;",
      tags$ol(start = 6,
              tags$li(tags$em(tags$sup("1"), tags$strong("The boy"),HTML('&nbsp;'), tags$sup("2"), tags$strong("gets"),HTML('&nbsp;'), tags$sup("3"), tags$strong("soaking wet"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 3.1"),
                tags$li("Is, looks, stands there"),
                tags$li("Soaked, drenched, dripping, very wet")),
             
              tags$p('Note: Sometimes speakers would use first person (e.g. "(1) I (2) am (3) all wet")', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

u7<-
  div(style = "line-height: 160%;",
      tags$ol(start = 7,
              tags$li(tags$em(tags$sup("1"), tags$strong("The fboy"),HTML('&nbsp;'), tags$sup("2"), tags$strong("runs"),HTML('&nbsp;'), tags$sup("3"), tags$strong("back"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 3.1"),
                tags$li("Goes, heads, returns, turns around, races, rushes, comes, gets, arrives, shows"),
                tags$li("Home, inside")),
             
              tags$p('Note: Occasionally combined with MC 6 as in, "The boy runs back soaking wet." A person who says this should receive full credit for MCs 6 and 7.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

u8<-
  div(style = "line-height: 160%;",
      tags$ol(start = 8,
              tags$li(tags$em(tags$sup("1"), tags$strong("The mother"),HTML('&nbsp;'), tags$sup("2"), tags$strong("is"),HTML('&nbsp;'), tags$sup("3"), tags$strong("(negative emotional state"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("The woman, she, the lady, mom "),
                tags$li("looks, feels"),
                tags$li("unhappy, mad, angry, upset, annoyed, frustrated, concerned, cross, disappointed")),
             
              tags$p('Note: Sometimes reported as "his mother doesn’t look happy."', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Statements about physical stance/nonverbal expression do not count, e.g., "She’s scowling."', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Occasionally combined with MC 6 and MC 7 as in "When the boy came back home, mom was mad because he was all wet." A person who says this should receive full credit for MCs 6, 7, and 8.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


u9<-
  div(style = "line-height: 160%;",
      tags$ol(start = 9,
              tags$li(tags$em(tags$sup("1"), tags$strong("The boy"),HTML('&nbsp;'), tags$sup("2"), tags$strong("gets"),HTML('&nbsp;'), tags$sup("3"), tags$strong("an umbrella"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("see 3.1"),
                tags$li("takes, receives, has, asks for, carries, retrieves, picks up, holds"),
                tags$li("it (if appropriate referent)")),
             
              tags$p('Note: Sometimes produced as "The mother (2) gives (1) the boy (3) an umbrella.” Or “she (2) gave (3) it to (1) him.” (if appropriate referents). ', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

u10<-
  div(style = "line-height: 160%;",
      tags$ol(start = 10,
              tags$li(tags$em(tags$sup("1"), tags$strong("The boy"),HTML('&nbsp;'), tags$sup("2"), tags$strong("goes back"),HTML('&nbsp;'), tags$sup("3"), tags$strong("to school"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("see 3.1"),
                tags$li("walks, leaves, heads, starts, takes, is, sets forth, proceeds"),
                tags$li("out, again, along, back, in the rain, off, on his way, with the umbrella, (leaves) the house, the school bus")),
             
              tags$p('Note: Sometimes produced as "(3) Off (1) he (2) goes again.”', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Occasionally combined with MC 9, as in "He goes out with the umbrella." A person who says this should receive full credit for MCs 9 and 10.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


# sandwich
s1<-
  div(style = "line-height: 160%;",
      tags$ol(start = 1,
              tags$li(tags$em(tags$sup("1"), tags$strong("Get"),HTML('&nbsp;'), tags$sup("2"), tags$strong("bread"),HTML('&nbsp;'), tags$sup("3"), tags$strong("out"), "of the pantry/cupboard/refrigerator/freezer/etc.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Take out, remove, grab, find, pull out"),
                tags$li("Loaf, bread loaf, bread bag"),
                tags$li("From (which if used, must be followed by a location from the speaker)")),
             
              tags$p('Note: For concepts 1-5, "put" is not an acceptable verb. For each of those concepts there was a similar relevant concept (i.e., "put the bread on the counter"), however, none of these relevant concepts reached criterion. In these cases, the speaker would receive a score of absent, and any information associated with the verb “put” should be treated as extra information that is not scored.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

s2<-
  div(style = "line-height: 160%;",
      tags$ol(start = 2,
              tags$li(tags$em(tags$sup("1"), tags$strong("Get"),HTML('&nbsp;'), tags$sup("2"), tags$strong("two slices"),HTML('&nbsp;'), tags$sup("3"), tags$strong("of bread"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 1.1"),
                tags$li("A couple of slices, two pieces")),
             
              tags$p('Note: Most speakers indicated more than one piece of bread. However, if the speaker uses one piece of bread throughout the story, and for #8 uses "fold" or "close"", or indicates in some way that they made a half sandwich, then one slice/piece of bread is allowed. Speakers must be consistent throughout the telling for this to be counted as correct.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: If the speaker received full credit for the first concept, they do not necessarily have to repeat “of bread” to be counted correct/complete for this concept. For example, a speaker could say “You take the bread out of the pantry and get two slices.”', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

s3<-
  div(style = "line-height: 160%;",
      tags$ol(start = 3,
              tags$li(tags$em(tags$sup("1"), tags$strong("Get"),HTML('&nbsp;'), tags$sup("2"), tags$strong("the peanut butter"),HTML('&nbsp;'))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 1.1"),
                tags$li("A jar of peanut butter")),
             
              tags$p('Note: A concept like “take off the lid on the peanut butter” or "open the peanut butter" cannot be used for this concept. This was a relevant concept that did not reach significance.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

s4<-
  div(style = "line-height: 160%;",
      tags$ol(start = 4,
              tags$li(tags$em(tags$sup("1"), tags$strong("Get"),HTML('&nbsp;'), tags$sup("2"), tags$strong("the jelly"),HTML('&nbsp;'))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 1.1"),
                tags$li("A jar of jelly, jam, preserves, honey")),
             
              tags$p('Note: See note for Number 3.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

s5<-
  div(style = "line-height: 160%;",
      tags$ol(start = 5,
              tags$li(tags$em(tags$sup("1"), tags$strong("Get"),HTML('&nbsp;'), tags$sup("2"), tags$strong("a knife"),HTML('&nbsp;'))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 1.1"),
                tags$li("Spatula"))
      )
  )

s6<-
  div(style = "line-height: 160%;",
      tags$ol(start = 6,
              tags$li(tags$em(tags$sup("1"), tags$strong("Put"),HTML('&nbsp;'), tags$sup("2"), tags$strong("the bread"),HTML('&nbsp;'), tags$sup("3"), tags$strong("on a plate"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Place, set, lay"),
                tags$li("The slices, the pieces, it, them (It must be clear that the individual is referring only to the bread, not the jelly, peanut butter, or knife)"),
                tags$li("Counter, breadboard, cutting board, napkin, down (on a surface)"))
      )
  )

s7<-
  div(style = "line-height: 160%;",
      tags$ol(start = 7,
              tags$li(tags$em(tags$sup("1"), tags$strong("Put"),HTML('&nbsp;'), tags$sup("2"), tags$strong("peanut butter"),HTML('&nbsp;'), tags$sup("3"), tags$strong("on bread"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Place, smear, spread, slap, slather, spoon out, cover"),
                tags$li("No alternative for “peanut butter was produced"),
                tags$li("On top of jelly, on the other piece of bread, on one slice, on one side, on one half"))
      )
  )

s8<-
  div(style = "line-height: 160%;",
      tags$ol(start = 8,
              tags$li(tags$em(tags$sup("1"), tags$strong("Put"),HTML('&nbsp;'), tags$sup("2"), tags$strong("jelly"),HTML('&nbsp;'), tags$sup("3"), tags$strong("on bread"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 7.1"),
                tags$li("See 4.2"),
                tags$li("See 7.3"))
      )
  )

s9<-
  div(style = "line-height: 160%;",
      tags$ol(start = 9,
              tags$li(tags$em(tags$sup("1"), tags$strong("Put"),HTML('&nbsp;'), tags$sup("2"), tags$strong("the two pieces"),HTML('&nbsp;'), tags$sup("3"), tags$strong("together"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Place, smash, slap, smack, stick"),
                tags$li("The bread, the two slices of bread, the two sides, the peanut butter and jelly, the two, the two halves, them"),
                tags$li('If the speaker does not say "together" they must give some indication that the two pieces become one (i.e., "Put one piece on top of the other", "Combine the pieces of bread", "Put the second piece of bread on top.")')),
             
              tags$p('Note: The verbs “fold” and “close” cannot be used for concept 3, unless the speaker tells the entire story with one piece of bread as if making half of a sandwich, see 2.2', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

s10<-
  div(style = "line-height: 160%;",
      tags$ol(start = 2,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cut"),HTML('&nbsp;'), tags$sup("2"), tags$strong("the sandwich"),HTML('&nbsp;'), tags$sup("3"), tags$strong("in pieces"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Slice"),
                tags$li("The bread, it"),
                tags$li("In half, in quarters, in two, diagonally, across, on the bias, down the middle, with an x, however you like")),
             
              tags$p('Note: Most speakers indicated more than one piece of bread. However, if the speaker uses one piece of bread throughout the story, and for #8 uses "fold" or "close"", or indicates in some way that they made a half sandwich, then one slice/piece of bread is allowed. Speakers must be consistent throughout the telling for this to be counted as correct.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

# cinderella
c1<-
  div(style = "line-height: 160%;",
      tags$ol(start = 1,
              tags$li(tags$em(tags$sup("1"), tags$strong("Dad"),HTML('&nbsp;'), tags$sup("2"), tags$strong("remarried"),HTML('&nbsp;'), tags$sup("3"), tags$strong("a woman"), "with two daughters.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Daddy/Father"),
                tags$li("Got married to, got remarried, married again"),
                tags$li("A lady"))
      )
  )
  
c2<-
  div(style = "line-height: 160%;",
      tags$ol(start = 2,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cinderella"),HTML('&nbsp;'), tags$sup("2"), tags$strong("lives with"),HTML('&nbsp;'), tags$sup("3"), tags$strong("stepmother/stepsisters"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("She*"),
                tags$li("Is left with, moves in with, grows up with, has"),
                tags$li("Stepfamily, new family, the women, they. If they do not mention the word “step”, there must be a clear indication that the stepmother and stepsisters (the lady and her two daughters, the mean woman and her beautiful daughters) are a unit separate from Cinderella.")),
             
              tags$p('Note: After Cinderella has been introduced into the story “she” is an acceptable alternative as long as there is a clear pronoun referent.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: After the stepmother and stepsisters have been introduced into the story “they” is an acceptable alternative as long as there is a clear pronoun referent.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c3<-
  div(style = "line-height: 160%;",
      tags$ol(start = 3,
              tags$li(tags$em(tags$sup("1"), tags$strong("Stepmother/stepsisters"),HTML('&nbsp;'), tags$sup("2"), tags$strong("were mean"),HTML('&nbsp;'), tags$sup("3"), tags$strong("to Cinderella"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 2.3"),
                tags$li("Were cruel, were wicked, treated Cinderella poorly, were awful, hated")
              )
      )
  )


c4<-
  div(style = "line-height: 160%;",
      tags$ol(start = 4,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cinderella"),HTML('&nbsp;'), tags$sup("2"), tags$strong("was"),HTML('&nbsp;'), tags$sup("3"), tags$strong("a servant"), "to the stepmother and stepsisters.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("She"),
                tags$li("Was forced to be, had to be"),
                tags$li("Maid, slave, domestic ")),
             
              tags$p('Note: If they say the sentence in another way that expresses servitude, for example “had to wait on”, they must include stepmother and/or stepsisters, because the verb requires an object. This would be the only time they are essential for this concept.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c5<-
  div(style = "line-height: 160%;",
      tags$ol(start = 5,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cinderella"),HTML('&nbsp;'), tags$sup("2"), tags$strong("has to do"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the homework."))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("She"),
                tags$li("Is forced to do, must do, has to take care of"),
                tags$li("Chores, cleaning, taking care of the house, everything"))
      )
  )


c6<-
  div(style = "line-height: 160%;",
      tags$ol(start = 6,
              tags$li("The kind thinks", tags$em(tags$sup("1"), tags$strong("the prince"),HTML('&nbsp;'), tags$sup("2"), tags$strong("should get married."),HTML('&nbsp;'))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("He*"),
                tags$li("Needs to get married/find a wife, must get married, has to get married")),
             
              tags$p('Note: After the prince has been introduced into the story “he” is an acceptable alternative as long as there is a clear pronoun referent.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c7<-
  div(style = "line-height: 160%;",
      tags$ol(start = 7,
              tags$li("the king announces", tags$em(tags$sup("1"), tags$strong("there is going to be"),HTML('&nbsp;'), tags$sup("2"), tags$strong("a ball"),HTML('&nbsp;'), "in honor of son who needs to find a wife.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Will be, is to be, is"),
                tags$li("Dance, big party, celebration, gala")),
             
              tags$p('Note: Occasionally this concept was combined with number 8 in a statement like, “They got an invitation to the ball the king was hosting for his son.” This should receive full credit for concepts 7 and 8.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c8<-
  div(style = "line-height: 160%;",
      tags$ol(start = 8,
              tags$li(tags$em(tags$sup("1"), tags$strong("They"),HTML('&nbsp;'), tags$sup("2"), tags$strong("got"),HTML('&nbsp;'), tags$sup("3"), tags$strong("an invitation"), "*to the ball.*")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("The women, the stepmothers and/or stepsisters and/or Cinderella, everyone in the household, the household"),
                tags$li("Received, was delivered (if word order altered so that the invitation is delivered to the women)"),
                tags$li('No alternatives were produced for "invitation"')),
             
              tags$p('Note: Not essential if clear from context or previously stated; otherwise see note for number 7. ', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: Alternatively, the speaker could say “They were invited to the ball” or something similar.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c9<-
  div(style = "line-height: 160%;",
      tags$ol(start = 9,
              tags$li(tags$em(tags$sup("1"), tags$strong("They"),HTML('&nbsp;'), tags$sup("2"), tags$strong("are excited"),HTML('&nbsp;'), tags$sup("3"), "*about the ball.*")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 8.1"),
                tags$li("Are happy, are pleased")),
             
              tags$p('Note: *See number 8.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: If they say something like “They are looking forward to”, they must include “the ball” because an object is required.', style = "color:black; font-size: .9em;line-height: 120%;")
              
      )
  )


c10<-
  div(style = "line-height: 160%;",
      tags$ol(start = 10,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cinderella"), "is told by the stepmother she", HTML('&nbsp;'), tags$sup("2"), tags$strong("cannot go"),HTML('&nbsp;'), "*to the ball* unless/because (insert reason).")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("She"),
                tags$li("Could not go, has to stay home, is not allowed to go")),
             
              tags$p('Note: *See number 8.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: An alternative is "If Cinderella could get all of her chores done, she could go to the ball"', style = "color:black; font-size: .9em;line-height: 120%;")
              
      )
  )


c11<-
  div(style = "line-height: 160%;",
      tags$ol(start = 11,
              tags$li(tags$em(tags$sup("1"), tags$strong("The stepsisters"),HTML('&nbsp;'), tags$sup("2"), tags$strong("tore"),HTML('&nbsp;'), tags$sup("3"), tags$strong("Cinderella's dress"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("They"),
                tags$li("Ruined, destroyed, ripped up, shredded"),
                tags$li("Her dress"))
      )
  )


c12<-
  div(style = "line-height: 160%;",
      tags$ol(start = 12,
              tags$li(tags$em(tags$sup("1"), tags$strong("Stepmother/Stepsisters"),HTML('&nbsp;'), tags$sup("2"), tags$strong("went"),HTML('&nbsp;'), tags$sup("3"), "*to the ball*")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li('Everyone but Cinderella. If "They went to the ball" is the sentence, the "they" must clearly exclude Cinderella in the context'),
                tags$li("Go, left, departed")),
             
              tags$p('Note: *See number 8.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c13<-
  div(style = "line-height: 160%;",
      tags$ol(start = 13,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cinderella"),HTML('&nbsp;'), tags$sup("2"), tags$strong("was"),HTML('&nbsp;'), tags$sup("3"), tags$strong("upset"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("She"),
                tags$li("Is"),
                tags$li("Crying, sad, disappointed"))
      )
  )


c14<-
  div(style = "line-height: 160%;",
      tags$ol(start = 14,
              tags$li(tags$em(tags$sup("1"), tags$strong("A fairy godmother"),HTML('&nbsp;'), tags$sup("2"), tags$strong("appeared"),HTML('&nbsp;'), "to Cinderella.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("No alternative for “fairy godmother” was produced"),
                tags$li("Shows up, appears, surprises, comes"),
                tags$li('Some may say "Cinderella sees" or "meets" or "finds", in which case Cinderella then becomes an essential element')),
             
              tags$p('Note: Another popular way of expressing this is "Along came a fairy godmother" (which is basically "appeared a fairy godmother)."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c15<-
  div(style = "line-height: 160%;",
      tags$ol(start = 15,
              tags$li(tags$em(tags$sup("1"), tags$strong("The fairy godmother"),HTML('&nbsp;'), tags$sup("2"), tags$strong("makes"),HTML('&nbsp;'), tags$sup("3"), tags$strong("{item(s)} turn into {items}."))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 14.1"),
                tags$li("Turns, creates, changes, any other verb indicating transformation/creation. Must be a verb that indicates some kind of transformation or creation."),
                tags$li("Pumpkin and mice OR carriage/coach and horses (see note)")),
             
              tags$p('Note: When producing this concept, only one pair needs to be mentioned, however, it must be correctly paired to receive full credit', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$ol(
                tags$li("Pumpkin to carriage/coach (and horses)"),
                tags$li("Mice to horses (and carriage)"),
                tags$li("If they initially mention both pumpkin and mice, they do not necessarily have to mention both after the transformation occurs in order to receive full credit, and only one needs to be accurate"),
                tags$li("Do not take points off for mentioning other transformations, such as dog to coachman as these are not incorrect, they simply did not reach significance.")
              )
      )
  )


c16<-
  div(style = "line-height: 160%;",
      tags$ol(start = 16,
              tags$li(tags$em(tags$sup("1"), tags$strong("The fairy godmother"),HTML('&nbsp;'), tags$sup("2"), tags$strong("makes"),HTML('&nbsp;'), tags$sup("3"), tags$strong("Cinderella"), tags$sup("4"), tags$strong("into a beautiful princess"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 14.1"),
                tags$li("Turns, creates, changes, gives"),
                tags$li("The regular girl, her regular clothes"),
                tags$li("Dress/shoes into gown/slippers, beautiful")),
             
              tags$p('Note: If they say the sentence in another way that expresses servitude, for example “had to wait on”, they must include stepmother and/or stepsisters, because the verb requires an object. This would be the only time they are essential for this concept.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c17<-
  div(style = "line-height: 160%;",
      tags$ol(start = 17,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cinderella"),HTML('&nbsp;'), tags$sup("2"), tags$strong("went"),HTML('&nbsp;'), tags$sup("3"), tags$strong("to the ball"), "in the coach.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("She"),
                tags$li("Goes, arrives, reaches"),
                tags$li("See 7.2")),
             
              tags$p('Note: If they say the sentence in another way that expresses servitude, for example “had to wait on”, they must include stepmother and/or stepsisters, because the verb requires an object. This would be the only time they are essential for this concept.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c18<-
  div(style = "line-height: 160%;",
      tags$ol(start = 18,
              tags$li("She knew", tags$em(tags$sup("1"), tags$strong("she"),HTML('&nbsp;'), tags$sup("2"), tags$strong("had to be"),HTML('&nbsp;'), tags$sup("3"), tags$strong("home by midnight"), "because everything will turn back at midnight.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Cinderella"),
                tags$li("Must be, needs to be, must return"),
                tags$li("Leave by midnight")),
             
              tags$p('Note: An alternative could be "The fairy godmother told her that if she wasn’t home by midnight, XXX would happen" or something similar.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c19<-
  div(style = "line-height: 160%;",
      tags$ol(start = 19,
              tags$li(tags$em(tags$sup("1"), tags$strong("The prince and Cinderella"),HTML('&nbsp;'), tags$sup("2"), tags$strong("danced"),HTML('&nbsp;'), "around the room/all night/with no one else.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("They"),
                tags$li("Were dancing, kept dancing"))
      )
  )


c20<-
  div(style = "line-height: 160%;",
      tags$ol(start = 20,
              tags$li(tags$em(tags$sup("1"), tags$strong("Prince"),HTML('&nbsp;'), tags$sup("2"), tags$strong("falls in love"),HTML('&nbsp;'), tags$sup("3"), tags$strong("with Cinderella"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("He"),
                tags$li("Is enamored with, is delighted with, is awestruck by, likes, is hooked on"),
                tags$li("Her")),
             
              tags$p('Note: If someone says "Prince/They fall in love at first sight" that individual can receive credit only if Cinderella has been mentioned before or it is clearly indicated who "they" are.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c21<-
  div(style = "line-height: 160%;",
      tags$ol(start = 21,
              tags$li("Cinderella realized", tags$em(tags$sup("1"), tags$strong("it"),HTML('&nbsp;'), tags$sup("2"), tags$strong("is"),HTML('&nbsp;'), tags$sup("3"), tags$strong("midnight"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Clock, something indicates that it is"),
                tags$li("Is, gets to be, rings, strikes"),
                tags$li("Twelve o’clock, twelve midnight, almost midnight"))
      )
  )


c22<-
  div(style = "line-height: 160%;",
      tags$ol(start = 22,
              tags$li(tags$em(tags$sup("1"), tags$strong("She"),HTML('&nbsp;'), tags$sup("2"), tags$strong("ran"),HTML('&nbsp;'), tags$sup("3"), tags$strong("down the stairs"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 18.1"),
                tags$li("Was running, flew, rushed, sprinted, left, was leaving"),
                tags$li("Out of the ball/castle, away from the ball/castle/prince, out, away"))
      )
  )


c23<-
  div(style = "line-height: 160%;",
      tags$ol(start = 23,
              tags$li("As she was running down the stairs", tags$em(tags$sup("1"), tags$strong("she"),HTML('&nbsp;'), tags$sup("2"), tags$strong("lost one of the"),HTML('&nbsp;'), tags$sup("3"), tags$strong("glass slippers"), "to the stepmother and stepsisters.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 18.1"),
                tags$li("Leaves, steps out of"),
                tags$li("Shoes, glass shoe"))
      )
  )



c24<-
  div(style = "line-height: 160%;",
      tags$ol(start = 24,
              tags$li(tags$em(tags$sup("1"), tags$strong("Prince"),HTML('&nbsp;'), tags$sup("2"), tags$strong("finds"),HTML('&nbsp;'), tags$sup("3"), tags$strong("Cinderella's shoe."))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Any other royal figure, king, servant, duke, prime minister, chamberlain"),
                tags$li("Had, got, retrieved, was brought"),
                tags$li("See 23.3")),
             
              tags$p('Note: An alternative way to say this is "The servant brings the slipper to the prince."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c25<-
  div(style = "line-height: 160%;",
      tags$ol(start = 25,
              tags$li(tags$em(tags$sup("1"), tags$strong("Everything"),HTML('&nbsp;'), tags$sup("2"), tags$strong("turns back"),HTML('&nbsp;'), tags$sup("3"), tags$strong("to its original form"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("Pumpkin, mice, and/or clothes/dress"),
                tags$li('Goes back, returns, disappears. If the speaker uses "disappears" they do not have to specify what disappears, for example, "Everything disappears."'),
                tags$li("To normal (can specify what it turns back into)")),
             
              tags$p('Note: The addition of "again" at the end of the sentence paired with a verb that does not indicate change is acceptable because it implies a return to the original state (e.g.,"she got home and the dress was old again."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c26<-
  div(style = "line-height: 160%;",
      tags$ol(start = 26,
              tags$li(tags$em(tags$sup("1"), tags$strong("She"),HTML('&nbsp;'), tags$sup("2"), tags$strong("returned"),HTML('&nbsp;'), tags$sup("3"), tags$strong("home"), "in time.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 18.1"),
                tags$li("Gets, makes it, goes"),
                tags$li("to the house")),
             
              tags$p('Note: If they say the sentence in another way that expresses servitude, for example “had to wait on”, they must include stepmother and/or stepsisters, because the verb requires an object. This would be the only time they are essential for this concept.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c27<-
  div(style = "line-height: 160%;",
      tags$ol(start = 27,
              tags$li(tags$em(tags$sup("1"), tags$strong("The prince"),HTML('&nbsp;'), tags$sup("2"), tags$strong("searched"),HTML('&nbsp;'),"door to door", tags$sup("3"), tags$strong("for Cinderella"), "to the stepmother and stepsisters.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 24.1, the servant"),
                tags$li("Was trying to find, looked for"),
                tags$li("For the person who would fit into the glass slipper, for the girl from the ball")),
             
              tags$p('Note: Alternatively, this could be stated as "The prince/his servant was trying the slipper on all the girls."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c28<-
  div(style = "line-height: 160%;",
      tags$ol(start = 28,
              tags$li(tags$em(tags$sup("1"), tags$strong("Prince"),HTML('&nbsp;'), tags$sup("2"), tags$strong("comes"),HTML('&nbsp;'), tags$sup("3"), tags$strong("to Cinderella's house"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 27.1"),
                tags$li("Arrives at, went, found, shows up at"),
                tags$li("Her")),
             
              tags$p('Note: If they say the sentence in another way that expresses servitude, for example “had to wait on”, they must include stepmother and/or stepsisters, because the verb requires an object. This would be the only time they are essential for this concept.', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c29<-
  div(style = "line-height: 160%;",
      tags$ol(start = 29,
              tags$li(tags$em(tags$sup("1"), tags$strong("The stepsisters"),HTML('&nbsp;'), tags$sup("2"), tags$strong("try on"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the glass slipper"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("The stepsisters and stepmother, the other girls (if the reference to the stepsisters is clear), etc. For this concept, speakers may include the stepmother. However, the stepmother alone is not sufficient."),
                tags$li("Attempt to put on, cram, try to fit into, try their foot in"),
                tags$li("See 23.3"))
      )
  )


c30<-
  div(style = "line-height: 160%;",
      tags$ol(start = 30,
              tags$li(tags$em(tags$sup("1"), tags$strong("The slipper"),HTML('&nbsp;'), tags$sup("2"), tags$strong("didn't fit"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the stepsisters"), "to the stepmother and stepsisters.")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 23.3"),
                tags$li("Would not go on the feet of, did not work for, couldn’t fit on"),
                tags$li("See 29.1"))
      )
  )


c31<-
  div(style = "line-height: 160%;",
      tags$ol(start = 31,
              tags$li(tags$em(tags$sup("1"), tags$strong("He"),HTML('&nbsp;'), tags$sup("2"), tags$strong("put"),HTML('&nbsp;'), tags$sup("3"), tags$strong("the slipper on"), "Cinderella's foot")),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 27.1"),
                tags$li("Tried, slid, slipped, placed"),
                tags$li("See 23.3")),
             
              tags$p('Note: Though most did not specifically mention Cinderella in these concepts (31, 32), it was clear that at this point in the story, they were referring to Cinderella.', style = "color:black; font-size: .9em;line-height: 120%;"),
              tags$p('Note: An alternate way to say this is "Cinderella tried on the slipper."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c32<-
  div(style = "line-height: 160%;",
      tags$ol(start = 32,
              tags$li(tags$em(tags$sup("1"), tags$strong("The slipper"),HTML('&nbsp;'), tags$sup("2"), tags$strong("fits"),HTML('&nbsp;'), tags$sup("3"), tags$strong("Cinderella perfectly"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 23.3"),
                tags$li("Belonged to")),
             
              tags$p('Note: An alternate way to say this is "The shoe slid easily onto her foot" or "The slipper was Cinderella’s"', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c33<-
  div(style = "line-height: 160%;",
      tags$ol(start = 33,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cinderella and the prince"),HTML('&nbsp;'), tags$sup("2"), tags$strong("were married"),HTML('&nbsp;'))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 19.1"),
                tags$li("Got married, were wed, had a wedding, had a marriage celebration")),
             
              tags$p('Note: An alternate way to say this is, "The prince took Cinderella as his bride."', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )


c34<-
  div(style = "line-height: 160%;",
      tags$ol(start = 34,
              tags$li(tags$em(tags$sup("1"), tags$strong("Cinderella and the prince"),HTML('&nbsp;'), tags$sup("2"), tags$strong("lived happily ever after"))),
              tags$ol(style = "padding-bottom:5px;",
                tags$li("See 19.1. The speaker must include or refer to both Cinderella and the prince in order to receive an accurate and complete score"),
                tags$li('Lived forever, lived a long time, were happy for life. The speaker must indicate an extended length of time in order to receive an accurate and complete score. For example, "ever after," "forever," "a long time," "life"')),
             
              tags$p('Note: Variations can include "They lived happily every after", "They were together forever", "They had a wonderful life"', style = "color:black; font-size: .9em;line-height: 120%;")
      )
  )

















































































