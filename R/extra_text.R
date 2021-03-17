##### saving other text

library(shinyglide)


controls <- tagList(
  glideControls(
    prevButton(class = "btn btn-default"),
    nextButton(class = "btn btn-default")
  ),
  tags$div(class="glide__bullets", `data-glide-el`="controls[nav]",
           tags$span(class="glide__bullet"),
           tags$span(class="glide__bullet"),
           tags$span(class="glide__bullet"),
           tags$span(class="glide__bullet"),
           tags$span(class="glide__bullet"),
           tags$span(class="glide__bullet")
      
  )
)


gettingStarted <- 
  div(
    h4("Main Concept Analysis", align = "center"),
    p("Main Concept Analysis is a discourse assessment that measures the informativeness of a discourse sample about a given topic. Each main concept consists of several essential elements, corresponding to the subject, main verb, object (if appropriate), and any subordinate clauses. Concepts are scored by accuracy and completeness."),
      p("Main concept analysis has shown good sensitivity in differentiating between controls and individuals with communication disorders. It is correlated with standardized measures of overall aphasia severity, confrontation naming (for some subtypes), listener perceptions, conversational abilities, communication confidence, and life participation. Changes in informativeness are associated with listener’s perceptions of communication quality."), 
    p("Main concept analysis was originally developed in 1995 (Nicholas & Brookshire). Drs. Richardson and Dalton have recently developed main concept checklists based on large language corpora",  a("(primarily, AphasiaBank).", href = "https://www.talkbank.org", target = "_blank"), "By using checklists, main concept analysis overcomes many challenges to discourse evaluation in clinical practice. The goal of this web-app is to provide clinicians with an easy-to-use interface for scoring main concepts, further improving the implementation of main concept analysis in routine clinical practice."),
  h4("Instructions"),
    tags$ol(
      tags$li("Start by reviewing the instructions for scoring main concept (hit next). Review the reference manual for additional details and examples if needed. You'll also be able to access this information while you score your transcript. We encourage you to use the", a("full scoring manual", href = "https://drive.google.com/file/d/1HFaKg8kHGW3JZhNr1nSGVJHLZhnnQiK-/view?usp=sharing", target = "_blank"),"and", a("specific guideles for Cinderella", href = "https://docs.google.com/document/d/1dx2aPGi6z_sPl1CCC0vbyNyVECwFRBLkHBXo9UrOWyg/edit?usp=sharing", target = "_blank")),
      tags$li("When you're ready, select the stimulus you used to elicit the discourse sample and type in your transcript in the box on the left. Make sure to capitalize the first letter of each sentence and use periods to end sentences. WHAT ARE THE TRANSCRIPTION RULES????"),
      tags$li('After typing in your transcript, hit "Start Scoring" to start scoring'),
      tags$li('Selecting the sentences to go along with each concept is not necessary, but may help with evaluting the parts of the transcript relevant to each concept. In the future, this will  provide information on discourse sequencing.'),
      tags$li("When you're done scoring, the results will be shown on the last page. You will also be able to download a summary spreadsheet with your transcript and the concept scores.")
    )
  )


coding <-
  div(
    div(align = "center", 
        h3("Coding")
        ),
  p("The standard coding for Main Concept Analysis is below. Normally, clinicians would need to code each concept with the following codes. In this app, you'll be asked to rate each essential element as accuracy, inaccurate, or absent, and we'll do the math for you"),
tags$ul(
  tags$li(
    "AC = accurate and complete: contains all elements of the main concept with no incorrect information."
  ),
  tags$li(
    "AI = accurate and incomplete: contains no incorrect information, but leaves out at least one essential element."
  ),
  tags$li(
    "IC = inaccurate and complete: contains at least one incorrect piece of essential information but includes all essential elements."
  ),
  tags$li(
    "II = inaccurate and incomplete: clearly corresponds with a main concept but includes at least one incorrect essential element and fails to include at least one essential element."
  ),
  tags$li(
    "AB = absent: the main concept was not produced."
  )
)
)

accuracy_html <- 
  div(
    div(align = "center",
    h3("Accuracy")
    ),
    h4("Wording"),
    tags$ul(
      tags$li("The wording of essential information does not have to be the same as that of the listed main concept, but the general meaning must be the same. Figures of speech and colloquialisms are acceptable.")
    ),
    h4("Grammatical form and word order"),
    tags$ul(
      tags$li("Essential information does not have to be spoken in standard grammatical form or standard word order, as long as deviations would not lead to miscomprehension of the essential meaning of the main concept.")
    ),
    h4("Articulation"),
    tags$ul(
      tags$li("Essential words do not have to be correctly articulated to be considered accurate, as long as they would be intelligible to a listener as the target words in the context of what the speaker is saying. Assume that the listener has seen the stimulus picture.")
    ),
    h4("Inaccurate words in accurate main concepts"),
    tags$ul(
      tags$li("If the essential information in a main concept is accurate, but inaccurate words also are included in the concept, consider the main concept accurate unless the inaccurate words alter essential information to make it inaccurate.")
    ),
    h4("Effects of statement form on essential information"),
    tags$ul(
      tags$li("If the essential information in a main concept is accurate, but inaccurate words also are included in the concept, consider the main concept accurate unless the inaccurate words alter essential information to make it inaccurate.")
    )
  )


completeness_html <-
  div(
    div(align = "center",
    h3("Completeness")
    ),
    h4("Missing essential information"),
    tags$ul(
      tags$li("If only some of the essential information (bold italicized) for a main concept is mentioned, either in its list form or in a form that has the same general meaning, consider the main concept incomplete.")
    ),
    h4("Statements containing some of the essential information"),
    tags$ul(
      tags$li("If a statement that is not listed as a main concept contains some of the essential information for a main concept, consider the main concept incomplete.")
    ),
    h4("Nonspecific words"),
    tags$ul(
      tags$li("If nonspecific words are spoken in place of essential information, consider the main concept incomplete."),
      tags$li('*BUT* In some cases there may be legitimate uncertainty about a specific person or element in a picture (e.g., "Someone is fishing" [WAB Picnic Scene]). In such instances, the nonspecific word will appear on the main concept list and its use will not render the main concept incomplete.')
    ),
    h4("Pronoun referents"),
    tags$ul(
      tags$li("If the referent for a pronoun is ambiguous, consider the main concept that contains the ambiguous pronoun incomplete, but only for the first use of the pronoun for that referent."),
      tags$li('Do not consider a main concept incomplete if there is no antecedent referent for a pronoun but the referent is clear from context/unambiguous. For example, if there is only one "she" or "he" or "they" in a picture, or only one figure is completing a specific action, the pronoun would be unambiguous without an antecedent referent.')
    )
  )



notes_scoring <-
  div(
    div(align = "center",
    h4("Frequently asked questions")
    ),
    p(tags$strong("Q:"), "Do main concepts have to be spoken in the same order as the checklist?", br(),
      tags$strong("A:"), "No - concepts can be produced in any order."),
    p(tags$strong("Q:"), "Which concept do I score if the same concept is produced multiple times?", br(),
      tags$strong("A:"), "If a main concept is spoken several times, ALWAYS score the final version, even if the later utterance results in an error not present in the first."),
    p(tags$strong("Q:"), "How do I score a concept that is produced across multiple utterances?", br(),
      tags$strong("A:"), "Segmenting transcripts into utterances can be complex and time consuming. Therefore - if essential elements are produced within a reasonable distance within a string of words, regardless of where utterance boundaries may be placed, they can still count towards a single main concept. In this app, we've segmented transcripts into sentences based on punctuation for consistency and simplicity, not based on any specific segmenting rules."),
    p(tags$strong("Q:"), "What if the verb tense is wrong?", br(),
      tags$strong("A:"), "The verb tense used in the production should not impact the code received. Any inflected form of the verbs listed in the checklist should be allowed."),
    p(tags$strong("Q:"), "Can two concepts occur in the same sentence or utterance?", br(),
      tags$strong("A:"), "Yes - One utterance can be used to produce two or more main concepts"),
    div(align = "center",
      div(`data-glide-el`="controls",
          tags$button(`data-glide-dir`="<<", href="#", "Back to start", class = "btn btn-default")
      )
    )
  )


glide_div <- 
  div(
    glide(custom_controls = controls,
          screen(
            gettingStarted
          ),
          screen(br(), br(), br(), br(), br(), br(),
                 h4("this will be a page about transcription", align = "center")
          ),
          screen(
            coding
          ),
          screen(
            accuracy_html
          ),
          screen(
            completeness_html
          ),
          screen(
            notes_scoring
          )
    )
  )






