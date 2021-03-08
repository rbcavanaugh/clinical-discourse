##### main concept ####
library(tidyverse)

# broken window main concepts:
main_concepts <- read_csv("www/main_concepts.csv")%>%
  group_by(task) %>% mutate(id = row_number())

main_concepts$concept_length = 4-rowSums(is.na(main_concepts))


# bw_mca = tibble(
#   c1 = c("The boy", "was", "outside"),
#   c2 = c("the boy", "playing", "soccer"),
#   c3 = c("The ball", "breaks", "window"),
#   c4 = c("The man", "is sitting", "hide"),
#   c5 = c("The man", "was startled", "hide"),
#   c6 = c("The ball", "broke", "a lamp"),
#   c7 = c("The man", "picked up", "the ball"),
#   c8 = c("The man", "looked", "out of the window")
# )

scoring_mca = tibble(
  Result = c("AC", "AI", "IC", "II", "Absent"),
  score = c(3, 2, 2, 1, 0)
)

# broken window
bw1 <-
  HTML('<p class="c13"><span class="c6">1)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">A/The boy </span><span class="c9 c15">2</span><span class="c9">was </span><span class="c9 c15">3</span><span class="c9">outside</span><span class="c9">.</span></p>
    <p class="c10"><span class="c6">1.</span><span class="c6">&nbsp; &nbsp; </span><span class="c3">He since referent is unambiguous; some give the boy a name</span></p>
    <p class="c10"><span class="c6">2.</span><span class="c6">&nbsp; &nbsp; </span><span class="c3">Is, decided to go</span></p>
    <p class="c10"><span class="c6">3.</span><span class="c6">&nbsp; &nbsp; </span><span class="c3">In his front yard, on the lawn, out of the house, etc.</span></p>
    <p class="c20"><span class="c0">Note: Sometimes, this concept was combined with number 2 in a statement like The boy was playing soccer outside or The boy was kicking the ball in the yard. These statements would receive full credit for both concept 1 and 2.</span></p>')
bw2<-
  HTML('<p class="c13"><span class="c6">2)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">A/The boy </span><span class="c9 c15">2</span><span class="c9">was playing </span><span class="c9 c15">3</span><span class="c9">soccer.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">See 1.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Played, is kicking, kicks, is practicing, etc.</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">With the soccer ball, with the ball, with the football* (*if not from U.S.)</span></p>
      <p class="c20"><span class="c0">Note: He has a ball or He has a soccer ball did not count towards this concept because it does not imply any kind of action with the soccer ball, and boy-action-ball was the concept that met criterion.</span></p>')
bw3 <- 
  HTML('<p class="c13"><span class="c6">3)</span><span class="c1">&nbsp; &nbsp; </span><span>&nbsp;</span><span class="c9 c15">1</span><span class="c9">The ball</span><span class="c9 c15">2</span><span class="c9">breaks</span><span class="c9 c15">3</span><span class="c9">the man’s/neighbor’s window.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Soccer ball, football*</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Goes through, went through, crashes through/into, flew through, sails through/into, shattered, is kicked through glass</span></p>')
bw4 <- 
  HTML('<p class="c13"><span class="c6">4)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">The man </span><span class="c9 c15">2</span><span class="c9">is sitting</span><span class="c9">&nbsp;</span><span class="c11">in a chair and/or inside the house.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">His dad, his father, the father, the neighbor, the guy; some give the man a name</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Lounging, resting, relaxing, inside</span></p>
      <p class="c20"><span class="c6">Note: Most common were The man is sitting, The man is inside, The man is sitting inside</span></p>
      <p class="c20"><span class="c0">Note: The man is watching TV or something similar did not count for this concept; that was a separate relevant concept that did not meet criterion. However, if an individual said, The man is sitting watching TV then they would receive credit for this concept since they included sitting.</span></p>')
bw5 <-
  HTML('<p class="c13"><span class="c6">5)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">The man </span><span class="c9 c15">2</span><span class="c9">was startled.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">See 4.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Surprised, amazed, afraid, astonished, freaked out, stunned, shocked, angry, upset, not happy, mad</span></p>
      <p class="c20"><span class="c0">Note: Occasionally, this concept was combined with number 3, in a statement such as The ball crashed through the window and startled the man.</span></p>')
bw6 <-
  HTML('<p class="c13"><span class="c6">6)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">The ball </span><span class="c9 c15">2</span><span class="c9">broke </span><span class="c9 c15">3</span><span class="c9">a lamp.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">See 3.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Knocks down/over, smashes into, breaks, hit</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">No alternative for lamp was produced</span></p>')
bw7 <-
  HTML('<p class="c13"><span class="c6">7)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">The man </span><span class="c9 c15">2</span><span class="c9">picked up </span><span class="c9 c15">3</span><span class="c9">the ball.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">See 4.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Grabs, gets, holds/is holding, catches, captures, has</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">See 3.1</span></p>
      <p class="c20"><span class="c0">Note: Occasionally, The man stands up with the ball and The man jumps up with the ball was used to express this concept, expressing that the man had performed some action to hold on to the ball. </span></p>')
bw8 <-
  HTML('<p class="c13"><span class="c6">8)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">The man </span><span class="c9 c15">2</span><span class="c9">looked </span><span class="c9 c15">3</span><span class="c9">out of the window.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">See 4.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Looks, is looking</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp; </span><span class="c3">Outside, out, out of the glass</span></p>
      <p class="c20"><span class="c0">Note: The man goes to the window, The man went to the window, or The man goes outside, etc., did not count towards this concept. These were separate relevant concepts that did not meet criterion.</span></p>')
# cat rescue
cr1 <-
  HTML('<p class="c13"><span class="c6">1)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The little girl</span><span class="c15 c9">2</span><span class="c9">was riding </span><span class="c15 c9">3</span><span class="c9">her bicycle.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">She (if appropriate referent), the girl, the child, any girl’s name</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Rode, rides, was on, is playing on, stopped riding, got off, was beside, has</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">Bike, tricycle, trike, it (if appropriate referent)</span></p>')

cr2<-
  HTML('<p class="c13"><span class="c6">2)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The cat</span><span class="c15 c9">2</span><span class="c9">was in</span><span class="c9 c15">3</span><span class="c9">the tree because the dog chased it.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">Kitty, kitten, it (if appropriate referent), any cat name</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Was up, was stuck in, got stuck in, climbed up, ran up, goes up, gets in, was caught in, ends up in, was on, was chased up, was scared up</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">The tree limb, limb</span></p>
      <p class="c20"><span class="c0">Note: Sometimes expressed as The dog</span><span class="c9">&nbsp;</span><span class="c15 c9">2</span><span class="c9">chased </span><span class="c15 c9">1</span><span class="c9">the cat </span><span class="c15 c9">3</span><span class="c9">up the tree.</span><span class="c6"> or The girl </span><span class="c15 c9">2</span><span class="c9">saw</span><span class="c15 c9">1</span><span class="c9">the cat </span><span class="c15 c9">3</span><span class="c9">in the tree</span><span class="c0">.</span></p>')
cr3<-
  HTML('<p class="c13"><span class="c6">3)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The dog </span><span class="c9 c15">2</span><span class="c9">was barking up the tree. </span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">It (if appropriate referent), puppy, pup, any dog name</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Barks, is barking, barked, is yelping</span></p>
      <p class="c20"><span class="c0">Note: The dog chased the cat should not apply to this statement as it was a separate relevant concept that did not meet threshold but was occasionally combined with additional elements that could apply to MC2 above.</span></p>')
cr4<-
  HTML('<p class="c13"><span class="c6">4)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The man </span><span class="c15 c9">2</span><span class="c9">climbed up</span><span class="c15 c9">3</span><span class="c9">the tree.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">The neighbor, the father, dad, daddy, someone older, big brother, he (if appropriate referent), any man’s name</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Was climbing, climbed, climbs, ran up, goes up into, got up on, crawls in/on</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">The branch, the limb, the ladder, it (if appropriate referent), there</span></p>')
cr5<-
  HTML('<p class="c13"><span class="c6">5)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The man </span><span class="c15 c9">2</span><span class="c9">tries to rescue </span><span class="c15 c9">3</span><span class="c9">the cat.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">See 4.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Wants to help, wants to rescue, tries to get, attempts to get, tries to reach, goes to get, tries to retrieve, went up after, comes to rescue</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">See 2.1</span></p>
      <p class="c20"><span class="c0">Note 1: Frequently combined with MC 4 as in The man climbed up the tree to get the cat.; a person who says this should receive full credit for MCs 4 and 5.</span></p>
      <p class="c20"><span class="c0">Note 2: Occasionally combined with MC 7 as in He’s stuck in the tree trying to get the cat.; a person who says this should receive full credit for MCs 5 and 7.</span></p>')
cr6<-
  HTML('<p class="c13"><span class="c6">6)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The ladder </span><span class="c15 c9">2</span><span class="c9">fell down.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">It (if appropriate referent)</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Is down, falls, fell, has fallen, has fallen down, got away from him, is on the ground, has slipped away, has dropped away, fell off, has been knocked down</span></p>
      <p class="c20"><span class="c0">Note: Sometimes expressed with an agent that caused the ladder to fall, such as the wind or dog (e.g., the dog knocked the ladder down).</span></p>')
cr7<-
  HTML('<p class="c13"><span class="c6">7)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The father </span><span class="c15 c9">2</span><span class="c9">is stuck </span><span class="c15 c9">3</span><span class="c9">in the tree with the cat.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">See 4.1, the man and the cat, they (if appropriate referents)</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Is up, is, is stranded, is caught, ended up, is marooned, is sitting</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">On the branch, on the limb, up there</span></p>
      <p class="c20"><span class="c0">Note: Sometimes expressed as: </span><span class="c15 c9">1</span><span class="c9">The man </span><span class="c15 c9">2</span><span class="c9">couldn’t</span><span class="c15 c9">3</span><span class="c9">get down.</span></p>')
cr8<-
  HTML('<p class="c13"><span class="c6">8)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">Someone</span><span class="c15 c9">2</span><span class="c9">called</span><span class="c15 c9">3</span><span class="c9">the fire department.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">The mother, the neighbor, the lady next door, the girl, the father, a passerby, an onlooker, he/she/they</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Notifies, alerts, got</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">The firemen, 911</span></p>
      <p class="c20"><span class="c0">Note 1: Sometimes expressed as a passive such as: </span><span class="c15 c9">3</span><span class="c9">The fire department </span><span class="c15 c9">2</span><span class="c9">has been called.</span></p>
      <p class="c20"><span class="c0">Note 2: For this concept, a pronoun without a preceding referent is scored as AC since this action is not depicted in the picture stimuli.</span></p>
      ')
cr9<-
  HTML('<p class="c13"><span class="c6">9)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The fire department </span><span class="c15 c9">2</span><span class="c9">comes</span><span class="c15 c9">3</span><span class="c9">with a ladder.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">The firefighters, the firemen, the fire truck, they (if appropriate referent or if includes ladder or other context so that the referent is not ambiguous)</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Is on the way, is/are coming, came, have arrived, rushes out, brings</span></p>
      <p class="c20"><span class="c0">Note 1: Sometimes combined with MC 8 as in The mother called the fire department to come with their ladder. A person who says this should receive full credit for MCs 8 and 9.</span></p>
      <p class="c20"><span class="c0">Note 2: While the first two essential elements met 66% threshold, the final element with a ladder was only produced by 33% of the sample. </span></p>
      ')
cr10<-
  HTML('<p class="c13"><span class="c6">10)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The fire department </span><span class="c15 c9">2</span><span class="c9">rescues</span><span class="c15 c9">3</span><span class="c9">them.</span></p>
      <p class="c10"><span class="c6">1. &nbsp;See 9.1 (but not fire truck)</span></p>
      <p class="c10"><span class="c6">2. &nbsp;Saves, is going to get, helps, gets, will take </span></p>
      <p class="c10"><span class="c6">3. &nbsp;The man, the cat, the man and the cat</span></p>
      <p class="c20"><span class="c0">Note 1: Often combined with MC 9 as in The fire department comes with a ladder to rescue them. A person who says this should receive full credit for MCs 9 and 10.</span></p>
      <p class="c20"><span class="c0">Note 2: Sometimes combined with MC 8 and MC 9 as in The mother called the fire department to come and rescue the father with a ladder. A person who says this should receive full credit for MCs 8, 9, and 10.</span></p>')
# umbrella
u1<-
  HTML('<p class="c13"><span class="c6">1)</span><span class="c1">&nbsp; &nbsp;</span><span class="c9">The mother says</span><span class="c15 c9">1</span><span class="c9">it’s going to </span><span class="c15 c9">2</span><span class="c9">rain today.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">It’s supposed to, it might, it’s predicted, it looks like, there’s a chance</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Sprinkle, drizzle, storm</span></p>
      <p class="c20"><span class="c0">Note 1: Occasionally produced as </span><span class="c15 c9">2</span><span class="c9">Rain</span><span class="c14 c6">&nbsp;</span><span class="c15 c9">1</span><span class="c9">is in the forecast</span><span class="c0">. </span></p>
      <p class="c20"><span class="c0">Note 2: Statements that implied bad weather was on the way e.g. the weather was looking gray and cloudy outside do not count towards this MC as it was another relevant concept that did not meet threshold. </span></p>
      <p class="c20"><span class="c0">Note 3: The statement It is raining. does not apply to this MC; see MC 5.</span></p>
      ')
u2<-
  HTML('<p class="c13"><span class="c6">2)</span><span class="c1">&nbsp; &nbsp;</span><span class="c6">The mother says </span><span class="c15 c9">1</span><span class="c9">you </span><span class="c15 c9">2</span><span class="c9">need to take </span><span class="c15 c9">3</span><span class="c9">the</span><span class="c14 c6">&nbsp;</span><span class="c9">umbrella</span><span class="c2">.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">He (if appropriate referent), the boy, (male name)</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Carry, take, have, need, should have, might need, might want </span></p>
      <p class="c20"><span class="c0">Note 1: Sometimes produced as a command with the subject implied, e.g., take this umbrella these statements were considered AC since English allows the subject to be dropped in a command.</span></p>
      <p class="c20"><span class="c0">Note 2: Sometimes produced as </span><span class="c15 c9">1</span><span class="c9">his mother </span><span class="c15 c9">2</span><span class="c9">offers</span><span class="c6">&nbsp;him </span><span class="c15 c9">3</span><span class="c9">an umbrella</span><span class="c0">. or similar.</span></p>
      <p class="c20"><span class="c0">Note 3: Occasionally produced as a question </span><span class="c9">dont &nbsp;</span><span class="c15 c9">1</span><span class="c9">you &nbsp;</span><span class="c15 c9">2</span><span class="c9">want to take</span><span class="c15 c9">3</span><span class="c9">this umbrella</span><span class="c0">?</span></p>
      <p class="c20"><span class="c0">Note 4: Sometimes produced </span><span class="c15 c9">2</span><span class="c9">here is</span><span class="c15 c9">1</span><span class="c9">your &nbsp;</span><span class="c15 c9">3</span><span class="c9">umbrella</span><span class="c0">.</span></p>')
u3<-
  HTML('<p class="c13"><span class="c6">3)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The boy</span><span class="c15 c9">2</span><span class="c9">(does something to refuse)</span><span class="c15 c9">3</span><span class="c9">the umbrella.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">He (if appropriate referent), the boy, (male name), I (if reported speech)</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Doesn’t want, refuses, won’t/is not going to take, declines, says no, says he’ll be ok without</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">It (if appropriate referent)</span></p>
      <p class="c20"><span class="c0">Note: Occasionally this concept was stated as He won’t do it. in reference to the mother trying to make him take the umbrella, so the action he won’t do is take the umbrella and this should receive an AC as long as the referent is produced.</span></p>
      ')
u4<-
  HTML('<p class="c13"><span class="c6">4)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The boy </span><span class="c15 c9">2</span><span class="c9">walks</span><span class="c15 c9">3</span><span class="c9">to school.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">See 3.1, a child</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Goes, leaves, heads, takes off, starts, sets</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">Outside, out of the house, out, to/for/towards [location], down the road, off, out of the door, further, forth, down, in the rain</span></p>
      <p class="c20"><span class="c0">Note: Sometimes the order of elements was switched, e.g. </span><span class="c9 c15">3</span><span class="c9">Off to school </span><span class="c9 c15">1</span><span class="c9">he </span><span class="c9 c15">2</span><span class="c9">goes</span><span class="c0"> </span></p>
      ')
u5<-
  HTML('<p class="c13"><span class="c6">5)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">It </span><span class="c15 c9">2</span><span class="c9">is raining.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">The rain, the deluge</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Starts to pour, starts coming down, is falling, is sprinkling, gets harder, gets heavier, is raining, begins to rain, starts to rain, starts falling, comes, is coming down, starts raining, started sprinkling, started, rained</span></p>
      <p class="c20"><span class="c0">Note 1: Sometimes produced as a colloquialism, The sky opens up or We have a downpour.</span></p>
      <p class="c20"><span class="c0">Note 2: Occasionally produced as Here </span><span class="c15 c9">2</span><span class="c9">comes</span><span class="c14 c6">&nbsp;</span><span class="c15 c9">1</span><span class="c9">the rain</span><span class="c0">.</span></p>
      <p class="c20"><span class="c0">Note 3: Do not count utterances about rain increasing in severity (e.g., It starts to rain harder.).</span></p>
      ')
u6<-
  HTML('<p class="c13"><span class="c6">6)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The boy </span><span class="c15 c9">2</span><span class="c9">gets</span><span class="c15 c9">3</span><span class="c9">soaking wet.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">See 3.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Is, looks, stands there</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">Soaked, drenched, dripping, very wet</span></p>
      <p class="c20"><span class="c0">Note: Sometimes speakers would use first person (e.g. </span><span class="c9 c15">1</span><span class="c9">I </span><span class="c9 c15">2</span><span class="c9">am </span><span class="c9 c15">3</span><span class="c9">all wet</span><span class="c0">)</span></p>
      ')
u7<-
  HTML('<p class="c13"><span class="c6">7)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The boy </span><span class="c15 c9">2</span><span class="c9">runs </span><span class="c15 c9">3</span><span class="c9">back.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">See 3.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">Goes, heads, returns, turns around, races, rushes, comes, gets, arrives, shows</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">Home, inside</span></p>
      <p class="c20"><span class="c0">Note: Occasionally combined with MC 6 as in, The boy runs back soaking wet. A person </span></p>
      <p class="c22"><span class="c0">who says this should receive full credit for MCs 6 and 7.</span></p>')
u8<-
  HTML(' <p class="c13"><span class="c6">8)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The mother</span><span class="c15 c9">2</span><span class="c9">is</span><span class="c15 c9">3</span><span class="c9">(negative emotional state).</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">The woman, she, the lady, mom </span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">looks, feels</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">unhappy, mad, angry, upset, annoyed, frustrated, concerned, cross, disappointed</span></p>
      <p class="c20"><span class="c0">Note 1: Sometimes reported as his mother doesn’t look happy.</span></p>
      <p class="c20"><span class="c0">Note 2: Statements about physical stance/nonverbal expression do not count, e.g., She’s scowling.</span></p>
      <p class="c20"><span class="c0">Note 3: Occasionally combined with MC 6 and MC 7 as in When the boy came back home, mom was mad because he was all wet. A person who says this should receive full credit for MCs 6, 7, and 8.</span></p>
      ')

u9<-
  HTML('<p class="c13"><span class="c6">9)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The boy</span><span class="c15 c9">2</span><span class="c9">gets</span><span class="c15 c9">3</span><span class="c9">an umbrella.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">see 3.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">takes, receives, has, asks for, carries, retrieves, picks up, holds</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">it (if appropriate referent)</span></p>
      <p class="c20"><span class="c0">Note: Sometimes produced as The mother </span><span class="c15 c6">2</span><span class="c9">gives</span><span class="c15 c6">1</span><span class="c9">the boy </span><span class="c15 c6">3</span><span class="c9">an</span><span class="c14 c6">&nbsp;</span><span class="c9">umbrella</span><span class="c14 c6">.</span><span class="c6"></span><span class="c14 c6">&nbsp;</span><span class="c6">Or she</span><span class="c14 c6">&nbsp;</span><span class="c9 c15">2</span><span class="c9">gave </span><span class="c9 c15">3</span><span class="c9">it to </span><span class="c9 c15">1</span><span class="c9">him.</span><span class="c6"></span><span class="c14 c6">&nbsp;</span><span class="c0">(if appropriate referents). </span></p>
      ')
u10<-
  HTML(' <p class="c13"><span class="c6">10)</span><span class="c1">&nbsp; &nbsp;</span><span class="c15 c9">1</span><span class="c9">The boy</span><span class="c15 c9">2</span><span class="c9">goes</span><span class="c15 c9">3</span><span class="c9">back to school.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; </span><span class="c0">see 3.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; </span><span class="c0">walks, leaves, heads, starts, takes, is, sets forth, proceeds</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; </span><span class="c0">out, again, along, back, in the rain, off, on his way, with the umbrella, (leaves) the house, the school bus</span></p>
      <p class="c20"><span class="c0">Note 1: Sometimes produced as </span><span class="c15 c6">3</span><span class="c9">Off</span><span class="c15 c6">1</span><span class="c9">he</span><span class="c15 c6">2</span><span class="c9">goes</span><span class="c0">&nbsp;again.</span></p>
      <p class="c20"><span class="c0">Note 2: Occasionally combined with MC 9, as in He goes out with the umbrella. A person who says this should receive full credit for MCs 9 and 10.</span></p>')
# sandwich
s1<-
  HTML('<p class="c13"><span class="c6">1)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Get </span><span class="c9 c15">2</span><span class="c9">bread </span><span class="c9 c15">3</span><span class="c9">out</span><span class="c0">&nbsp;of the pantry/cupboard/refrigerator/freezer/etc.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">Take out, remove, grab, find, pull out</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">Loaf, bread loaf, bread bag</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">From (which if used, must be followed by a location from the speaker)</span></p>')
s2<-
  HTML('<p class="c13"><span class="c6">2)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Get </span><span class="c9 c15">2</span><span class="c9">two slices </span><span class="c9 c15">3</span><span class="c9">of bread.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">See 1.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">A couple of slices, two pieces</span></p>
      <p class="c22 c26"><span class="c6">i.</span><span class="c1">&nbsp; &nbsp; </span><span class="c0">Most speakers indicated more than one piece of bread. However, if the speaker uses one piece of bread throughout the story, and for #8 uses fold or close, or indicates in some way that they made a half sandwich, then one slice/piece of bread is allowed. Speakers must be consistent throughout the telling for this to be counted as correct.</span></p>
      <p class="c12"><span class="c6">3. &nbsp; </span><span class="c0">If the speaker received full credit for the first concept, they do not necessarily have to repeat of bread to be counted correct/complete for this concept. For example, a speaker could say You take the bread out of the pantry and get two slices.</span></p>')
s3<-
  HTML(' <p class="c13"><span class="c6">3)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Get</span><span class="c15 c9">2</span><span class="c9">the peanut butter.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">See 1.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">A jar of peanut butter</span></p>
      <p class="c20"><span class="c0">Note: A concept like take off the lid on the peanut butter or open the peanut butter cannot be used for this concept. This was a relevant concept that did not reach significance.</span></p>')
s4<-
  HTML('<p class="c13"><span class="c6">4)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Get </span><span class="c9 c15">2</span><span class="c9">the jelly.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">See 1.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">A jar of jelly, jam, preserves, honey</span></p>
      <p class="c20"><span class="c0">Note: See note for number 3.</span></p>')
s5<-
  HTML('<p class="c13"><span class="c6">5)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Get </span><span class="c9 c15">2</span><span class="c9">a knife.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">See 1.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">Spatula</span></p>')
s6<-
  HTML('<p class="c13"><span class="c6">6)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Put </span><span class="c9 c15">2</span><span class="c9">the bread </span><span class="c9 c15">3</span><span class="c9">on the plate.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">Place, set, lay</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">The slices, the pieces, it, them</span></p>
      <p class="c22 c26"><span class="c6">i.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">It must be clear that the individual is referring only to the bread, not the jelly, peanut butter, or knife</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">Counter, breadboard, cutting board, napkin, down (on a surface)</span></p>')
s7<-
  HTML('<p class="c13"><span class="c6">7)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Put </span><span class="c9 c15">2</span><span class="c9">peanut butter </span><span class="c9 c15">3</span><span class="c9">on bread.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">Place, smear, spread, slap, slather, spoon out, cover</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">No alternative for peanut butter was produced</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c0">&nbsp; &nbsp;On top of jelly, on the other piece of bread, on one slice, on one side, on one half</span></p>')
s8<-
  HTML('<p class="c13"><span class="c6">8)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Put </span><span class="c9 c15">2</span><span class="c9">jelly </span><span class="c9 c15">3</span><span class="c9">on bread.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">See 7.1</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">See 4.2</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">See 7.3 </span></p>')
s9<-
  HTML(' <p class="c13"><span class="c6">9)</span><span class="c1">&nbsp; &nbsp; </span><span class="c9 c15">1</span><span class="c9">Put </span><span class="c9 c15">2</span><span class="c9">the two pieces </span><span class="c9 c15">3</span><span class="c9">together.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">Place, smash, slap, smack, stick</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">The bread, the two slices of bread, the two sides, the peanut butter and jelly, the two, the two halves, them</span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">If the speaker does not say together they must give some indication that the two pieces become one (i.e., Put one piece on top of the other, Combine the pieces of bread, Put the second piece of bread on top.)</span></p>
      <p class="c22 c26"><span class="c6">i.</span>&nbsp;&nbsp; The verbs fold and close cannot be used for this concept, unless the speaker tells the entire story with one piece of bread as if making half of a sandwich, see 2.2.i.</span></p>')
s10<-
  HTML('<p class="c13"><span class="c6">10)</span><span class="c9 c15">1</span><span class="c9">Cut </span><span class="c9 c15">2</span><span class="c9">the sandwich </span><span class="c9 c15">3</span><span class="c9">in pieces.</span></p>
      <p class="c10"><span class="c6">1.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">Slice</span></p>
      <p class="c10"><span class="c6">2.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">The bread, it </span></p>
      <p class="c10"><span class="c6">3.</span><span class="c1">&nbsp; &nbsp;</span><span class="c0">In half, in quarters, in two, diagonally, across, on the bias, down the middle, with an x, however you like</span></p>
      <p class="c22"><span class="c0">Note: For concepts 1-5, put is not an acceptable verb. For each of those concepts there was a similar relevant concept (i.e., put the bread on the counter), however, none of these relevant concepts reached criterion. In these cases, the speaker would receive a score of absent, and any information associated with the verb put should be treated as extra information that is not scored.</span></p>')
# cinderella
c1<-
  HTML('<p class="c13"><span class="c6">1)</span><span class="c9 c15">1</span><span class="c9">Dad </span><span class="c9 c15">2</span><span class="c9">remarried </span><span class="c9 c15">3</span><span class="c9">a woman</span><span class="c0">&nbsp;with two daughters.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">Daddy/Father</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Got married to, got remarried, married again</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">A lady</span></p>')
c2<-
  HTML('<p class="c13"><span class="c6">2)</span><span class="c9 c15">1</span><span class="c9">Cinderella </span><span class="c9 c15">2</span><span class="c9">lives with </span><span class="c9 c15">3</span><span class="c9">stepmother/stepsisters.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">She*</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0"> Is left with, moves in with, grows up with, has</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0"> Stepfamily, new family, the women, they</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;<span class="c0">If they do not mention the word step, there must be a clear indication that the stepmother and stepsisters (the lady and her two daughters, the mean woman and her beautiful daughters) are a unit separate from Cinderella.</span></p>
      <p class="c20"><span class="c0">Note 1: After Cinderella has been introduced into the story she is an acceptable alternative as long as there is a clear pronoun referent.</span></p>
      <p class="c20"><span class="c0">Note 2: After the stepmother and stepsisters have been introduced into the story they is an acceptable alternative as long as there is a clear pronoun referent.</span></p>')
c3<-
  HTML('<p class="c13"><span class="c6">3)</span><span class="c9 c15">1</span><span class="c9">Stepmother/stepsisters </span><span class="c9 c15">2</span><span class="c9">were mean </span><span class="c9 c15">3</span><span class="c9">to Cinderella.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 2.3</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Were cruel, were wicked, treated Cinderella poorly, were awful, hated</span></p>')
c4<-
  HTML('<p class="c13"><span class="c6">4)</span><span class="c9 c15">1</span><span class="c9">Cinderella </span><span class="c9 c15">2</span><span class="c9">was </span><span class="c9 c15">3</span><span class="c9">a servant</span><span class="c9 c6">&nbsp;</span><span class="c0">to the stepmother and stepsisters.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">She</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0"> Was forced to be, had to be</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0"> Maid, slave, domestic </span></p>
      <p class="c20"><span class="c0">Note: If they say the sentence in another way that expresses servitude, for example had to wait on, they must include stepmother and/or stepsisters, because the verb requires an object. This would be the only time they are essential for this concept.</span></p>')
c5<-
  HTML('<p class="c13"><span class="c6">5)</span><span class="c9 c15">1</span><span class="c9">Cinderella </span><span class="c9 c15">2</span><span class="c9">has to do</span><span class="c6 c14">&nbsp;</span><span class="c15 c9">3</span><span class="c9">the housework.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">She</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0"> Is forced to do, must do, has to take care of</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0"> Chores, cleaning, taking care of the house, everything</span></p>')
c6<-
  HTML('<p class="c13"><span class="c6">6)</span><span class="c6">The king thinks </span><span class="c9 c15">1</span><span class="c9">the prince </span><span class="c9 c15">2</span><span class="c9">should get married.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">He*</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">&nbsp; &nbsp;Needs to get married/find a wife, must get married, has to get married</span></p>
      <p class="c20"><span class="c0">Note: After the prince has been introduced into the story he is an acceptable alternative as long as there is a clear pronoun referent.</span></p>')
c7<-
  HTML('<p class="c13"><span class="c6">7)</span><span class="c6">King announces </span><span class="c9 c15">1</span><span class="c9">there is going to be </span><span class="c9 c15">2</span><span class="c9">a ball</span><span class="c9 c6">&nbsp;</span><span class="c0">in honor of son who needs to find a wife.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">Will be, is to be, is</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Dance, big party, celebration, gala</span></p>
      <p class="c20"><span class="c0">Note: Occasionally this concept was combined with number 8 in a statement like, They got an invitation to the ball the king was hosting for his son. This should receive full credit for concepts 7 and 8.</span></p>')
c8<-
  HTML(' <p class="c13"><span class="c6">8)</span><span class="c9 c15">1</span><span class="c9">They </span><span class="c9 c15">2</span><span class="c9">got </span><span class="c9 c15">3</span><span class="c9">an invitation</span><span class="c9 c6">&nbsp;*</span><span class="c0">to the ball*.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">The women, the stepmothers and/or stepsisters and/or Cinderella, everyone in the household, the household</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Received, was delivered (if word order altered so that the invitation is delivered to the women)</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">No alternatives were produced for invitation</span></p>
      <p class="c20"><span class="c0">Note 1: *Not essential if clear from context or previously stated; otherwise see note for number 7. </span></p>
      <p class="c20"><span class="c0">Note 2: Alternatively, the speaker could say They were invited to the ball or something similar.</span></p>')
c9<-
  HTML('<p class="c13"><span class="c6">9)</span><span class="c9 c15">1</span><span class="c9">They </span><span class="c9 c15">2</span><span class="c9">are excited</span><span class="c9 c6">&nbsp;</span><span class="c0">*about the ball*.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 8.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Are happy, are pleased</span></p>
      <p class="c20"><span class="c0">Note 1: *See number 8.</span></p>
      <p class="c20"><span class="c0">Note 2: If they say something like They are looking forward to, they must include the ball because an object is required.</span></p>')
c10<-
  HTML('<p class="c13"><span class="c6">10)</span><span class="c9 c15">1</span><span class="c9">Cinderella</span><span class="c9 c6">&nbsp;</span><span class="c6">is told by the stepmother she</span><span class="c9 c6">&nbsp;</span><span class="c9 c15">2</span><span class="c9">cannot go </span><span class="c0">*to the ball* unless/because (insert reason).</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">She</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Could not go, has to stay home, is not allowed to go</span></p>
      <p class="c20"><span class="c0">Note 1: *See number 8.</span></p>
      <p class="c20"><span class="c0">Note 2: An alternative is If Cinderella could get all of her chores done, she could go to the ball</span></p>')
c11<-
  HTML('<p class="c13"><span class="c6">11)</span><span class="c9 c15">1</span><span class="c9">The stepsisters </span><span class="c9 c15">2</span><span class="c9">tore </span><span class="c9 c15">3</span><span class="c7 c6">Cinderella’s dress.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">They</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Ruined, destroyed, ripped up, shredded</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Her dress</span></p>')
c12<-
  HTML('<p class="c13"><span class="c6">12)</span><span class="c9 c15">1</span><span class="c9">Stepmother/stepsisters </span><span class="c9 c15">2</span><span class="c9">went</span><span class="c9 c6">&nbsp;</span><span class="c0">*to the ball*.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">Everyone but Cinderella</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;</span><span class="c0">If They went to the ball is the sentence, the they must clearly exclude Cinderella in the context</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Go, left, departed</span></p>
      <p class="c20"><span class="c0">Note: *See number 8.</span></p>')
c13<-
  HTML('<p class="c13"><span class="c6">13)</span><span class="c9 c15">1</span><span class="c9">Cinderella </span><span class="c9 c15">2</span><span class="c9">was </span><span class="c9 c15">3</span><span class="c7 c6">upset.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">She</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Is</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Crying, sad, disappointed</span></p>')
c14<-
  HTML('<p class="c13"><span class="c6">14)</span><span class="c9 c15">1</span><span class="c9">A fairy godmother </span><span class="c9 c15">2</span><span class="c9">appeared</span><span class="c9 c6">&nbsp;</span><span class="c0">to Cinderella.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">No alternative for fairy godmother was produced</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Shows up, appears, surprises, comes</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;</span><span class="c0">Some may say Cinderella sees or meets or finds, in which case Cinderella then becomes an essential element</span></p>
      <p class="c20"><span class="c0">Note: Another popular way of expressing this is Along came a fairy godmother (which is basically appeared a fairy godmother)</span></p>')
c15<-
  HTML('<p class="c13"><span class="c6">15)</span><span class="c9 c15">1</span><span class="c9">The fairy godmother </span><span class="c9 c15">2</span><span class="c9">makes </span><span class="c9 c15">3</span><span class="c7 c6">{item(s)} turn into {items}.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 14.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Turns, creates, changes, any other verb indicating transformation/creation</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;</span><span class="c0">Must be a verb that indicates some kind of transformation or creation</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Pumpkin and mice OR carriage/coach and horses</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;</span><span class="c0">When producing this concept, only one pair needs to be mentioned, however, it must be correctly paired to receive full credit</span></p>
      <p class="c22 c26"><span class="c6">a.&nbsp;</span><span class="c0">Pumpkin carriage/coach (and horses) or Mice horses (and carriage)</span></p>
      <p class="c22 c26"><span class="c6">c.&nbsp;</span><span class="c0">If they initially mention both pumpkin and mice, they do not necessarily have to mention both after the transformation occurs in order to receive full credit, and only one needs to be accurate</span></p>
      <p class="c22 c26"><span class="c6">d.&nbsp;</span><span class="c0">Do not take points off for mentioning other transformations, such as dog &nbsp;coachman as these are not incorrect, they simply did not reach significance.</span></p>')
c16<-
  HTML(' <p class="c13"><span class="c6">16)</span><span class="c9 c15">1</span><span class="c9">The fairy godmother </span><span class="c9 c15">2</span><span class="c9">makes </span><span class="c9 c15">3</span><span class="c9">Cinderella </span><span class="c9 c15">4</span><span class="c7 c6">into a beautiful princess.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 14.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Turns, creates, changes, gives</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">The regular girl, her regular clothes</span></p>
      <p class="c10"><span class="c6">4.&nbsp; &nbsp;</span><span class="c0">Dress/shoes into gown/slippers, beautiful</span></p>')
c17<-
  HTML('<p class="c13"><span class="c6">17)</span><span class="c9 c15">1</span><span class="c9">Cinderella </span><span class="c9 c15">2</span><span class="c9">went </span><span class="c9 c15">3</span><span class="c9">to the ball</span><span class="c9 c6">&nbsp;</span><span class="c0">in the coach.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">She</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Goes, arrives, reaches</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">See 7.2</span></p>')
c18<-
  HTML('<p class="c13"><span class="c6">18)She knew </span><span class="c15 c9">1</span><span class="c9">she </span><span class="c9 c15">2</span><span class="c9">had to be </span><span class="c9 c15">3</span><span class="c9">home by midnight</span><span class="c9 c6">&nbsp;</span><span class="c0">because everything will turn back at midnight.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">Cinderella</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Must be, needs to be, must return</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Leave by midnight</span></p>
      <p class="c20"><span class="c0">Note: An alternative could be The fairy godmother told her that if she wasn’t home by midnight, XXX would happen or something similar.</span></p>')
c19<-
  HTML('<p class="c13"><span class="c6">19)</span><span class="c9 c15">1</span><span class="c9">The prince and Cinderella </span><span class="c9 c15">2</span><span class="c9">danced</span><span class="c9 c6">&nbsp;</span><span class="c0">around the room/all night/with no one else.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">They</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Were dancing, kept dancing</span></p>')
c20<-
  HTML('<p class="c13"><span class="c6">20)</span><span class="c9 c15">1</span><span class="c9">Prince </span><span class="c9 c15">2</span><span class="c9">falls in love </span><span class="c9 c15">3</span><span class="c7 c6">with Cinderella.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">He</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Is enamored with, is delighted with, is awestruck by, likes, is hooked on</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Her</span></p>
      <p class="c20"><span class="c0">Note: If someone says Prince/They fall in love at first sight that individual can receive credit only if Cinderella has been mentioned before or it is clearly indicated who they are.</span></p>')
c21<-
  HTML('<p class="c13"><span class="c6">21)Cinderella realized </span><span class="c9 c15">1</span><span class="c9">it </span><span class="c9 c15">2</span><span class="c9">is </span><span class="c9 c15">3</span><span class="c7 c6">midnight.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">Clock, something indicates that it is</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Is, gets to be, rings, strikes</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Twelve o’clock, twelve midnight, almost midnight</span></p>')
c22<-
  HTML('<p class="c13"><span class="c6">22)</span><span class="c9 c15">1</span><span class="c9">She </span><span class="c9 c15">2</span><span class="c9">ran </span><span class="c9 c15">3</span><span class="c9">down the stairs</span><span class="c2">.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 18.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Was running, flew, rushed, sprinted, left, was leaving</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Out of the ball/castle, away from the ball/castle/prince, out, away</span></p>')
c23<-
  HTML(' <p class="c13"><span class="c6">23)As she was running down the stairs </span><span class="c9 c15">1</span><span class="c9">she </span><span class="c9 c15">2</span><span class="c9">lost one of the </span><span class="c9 c15">3</span><span class="c7 c6">glass slippers.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 18.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Leaves, steps out of</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Shoes, glass shoe</span></p>')

c24<-
  HTML(' <p class="c13"><span class="c6">24)</span><span class="c9 c15">1</span><span class="c9">Prince </span><span class="c9 c15">2</span><span class="c9">finds </span><span class="c9 c15">3</span><span class="c7 c6">Cinderella’s shoe.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">Any other royal figure, king, servant, duke, prime minister, chamberlain</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Had, got, retrieved, was brought</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">See 23.3</span></p>
      <p class="c20"><span class="c0">Note: An alternative way to say this is The servant brings the slipper to the prince.</span></p>')
c25<-
  HTML('<p class="c13"><span class="c6">25)</span><span class="c9 c15">1</span><span class="c9">Everything </span><span class="c9 c15">2</span><span class="c9">turns back </span><span class="c9 c15">3</span><span class="c6 c7">to its original form.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">Pumpkin, mice, and/or clothes/dress</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Goes back, returns, disappears</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;</span><span class="c0">If the speaker uses disappears they do not have to specify what disappears, for example, Everything disappears.</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">To normal (can specify what it turns back into)</span></p>
      <p class="c20"><span class="c0">Note: The addition of again at the end of the sentence paired with a verb that does not indicate change is acceptable because it implies a return to the original state (e.g.,she got home and the dress was old again.</span></p>')
c26<-
  HTML('<p class="c13"><span class="c6">26)</span><span class="c9 c15">1</span><span class="c9">She </span><span class="c9 c15">2</span><span class="c9">returned </span><span class="c9 c15">3</span><span class="c9">home</span><span class="c9 c6">&nbsp;</span><span class="c0">in time.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 18.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Gets, makes it, goes</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">to the house</span></p>')
c27<-
  HTML('<p class="c13"><span class="c6">27)</span><span class="c9 c15">1</span><span class="c9">The prince </span><span class="c9 c15">2</span><span class="c9">searched</span><span class="c9 c6">&nbsp;</span><span class="c6">door to door </span><span class="c15 c9">3</span><span class="c7 c6">for Cinderella.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 24.1, the servant</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Was trying to find, looked for</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">For the person who would fit into the glass slipper, for the girl from the ball</span></p>
      <p class="c20"><span class="c0">Note: Alternatively, this could be stated as The prince/his servant was trying the slipper on all the girls.</span></p>')
c28<-
  HTML('<p class="c13"><span class="c6">28)</span><span class="c9 c15">1</span><span class="c9">Prince </span><span class="c9 c15">2</span><span class="c9">comes </span><span class="c9 c15">3</span><span class="c7 c6">to Cinderella’s house.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 27.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Arrives at, went, found, shows up at</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">Her</span></p>')
c29<-
  HTML('<p class="c13"><span class="c6">29)</span><span class="c9 c15">1</span><span class="c9">The stepsisters </span><span class="c9 c15">2</span><span class="c9">try on </span><span class="c9 c15">3</span><span class="c7 c6">the glass slipper.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">The stepsisters and stepmother, the other girls (if the reference to the stepsisters is clear), etc.</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;</span><span class="c0">For this concept, speakers may include the stepmother. However, the stepmother alone is not sufficient.</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Attempt to put on, cram, try to fit into, try their foot in</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">See 23.3</span></p>')
c30<-
  HTML('<p class="c13"><span class="c6">30)</span><span class="c9 c15">1</span><span class="c9">The slipper </span><span class="c9 c15">2</span><span class="c9">didn’t fit </span><span class="c9 c15">3</span><span class="c7 c6">the stepsisters.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 23.3</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Would not go on the feet of, did not work for, couldn’t fit on</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">See 29.1</span></p>')
c31<-
  HTML('<p class="c13"><span class="c6">31)</span><span class="c9 c15">1</span><span class="c9">He </span><span class="c9 c15">2</span><span class="c9">put </span><span class="c9 c15">3</span><span class="c9">the slipper on</span><span class="c9 c6">&nbsp;</span><span class="c0">Cinderella’s foot.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 27.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Tried, slid, slipped, placed</span></p>
      <p class="c10"><span class="c6">3.&nbsp; &nbsp;</span><span class="c0">See 23.3</span></p>
      <p class="c20"><span class="c0">Note 1: Though most did not specifically mention Cinderella in these concepts (31, 32), it was clear that at this point in the story, they were referring to Cinderella.</span></p>
      <p class="c20"><span class="c0">Note 2: An alternate way to say this is Cinderella tried on the slipper.</span></p>')
c32<-
  HTML('<p class="c13"><span class="c6">32)</span><span class="c9 c15">1</span><span class="c9">The slipper </span><span class="c9 c15">2</span><span class="c9">fits</span><span class="c9 c6">&nbsp;</span><span class="c0">Cinderella perfectly.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 23.3</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Belonged to</span></p>
      <p class="c20"><span class="c0">Note: An alternate way to say this is The shoe slid easily onto her foot or The slipper was Cinderella’s</span></p>')
c33<-
  HTML('<p class="c13"><span class="c6">33)</span><span class="c9 c15">1</span><span class="c9">Cinderella and the prince </span><span class="c9 c15">2</span><span class="c7 c6">were married.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 19.1</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Got married, were wed, had a wedding, had a marriage celebration</span></p>
      <p class="c20"><span class="c0">Note: An alternate way to say this is, The prince took Cinderella as his bride.</span></p>')
c34<-
  HTML('<p class="c13"><span class="c6">34)</span><span class="c9 c15">1</span><span class="c9">Cinderella and the prince </span><span class="c9 c15">2</span><span class="c7 c6">lived happily ever after.</span></p>
      <p class="c10"><span class="c6">1.&nbsp; &nbsp;</span><span class="c0">See 19.1</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;</span><span class="c0">The speaker must include or refer to both Cinderella and the prince in order to receive an accurate and complete score</span></p>
      <p class="c10"><span class="c6">2.&nbsp; &nbsp;</span><span class="c0">Lived forever, lived a long time, were happy for life.</span></p>
      <p class="c22 c26"><span class="c6">i.&nbsp; &nbsp;</span><span class="c0">The speaker must indicate an extended length of time in order to receive an accurate and complete score. For example, ever after, forever, a long time, life</span></p>
      <p class="c20"><span class="c0">Note: Variations can include They lived happily every after, They were together forever, They had a wonderful life</span></p>')
















































































