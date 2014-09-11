#################################
# FORMAT DATA FOR USE IN APPLET #
#################################

require(magrittr)
require(dplyr)
require(reshape2)

#############
# LOAD DATA #
#############

duke10 <- read.csv("duke10.csv")
identities_behaviors <- read.csv("identities_behaviors.csv")
events <- read.csv("events.csv")


# -------FORMAT DATA IN LONG FORMAT-------- 

#here we're treating each person's individual E,P, or A rating of an identity as an observation
#of course, one could then collapse and treat people/identity/event, etc. as an observational unit...
names(duke10)
duke10.long <- melt(duke10, id.vars = names(duke10)[1:21])

#view data
head(duke10.long)


# -------BREAK UP VARIABLE COLUMN----------

#we wish to break up the variable column into "Context = event[event1, event2, ...]/none," "Element = actor/behavior/object/identity," "Name," " Dimension = evaluation/potency/activity."  This uniquely identifies each rating for each person.

names(duke10)

# functions for performing break up operations on the variable column

# ----------VARIABLE Context, character-----------

#obtain events from events.csv
event_names <- events[c("Variable.Name","Event.Wording","A.B.O.I.Rated.","Actor","Behavior","Object")]
head(event_names)

#make Event.Wording in event_names lowercase
substrLeft <- function(string){
  string = unlist(as.character(string))
  return(gsub("_"," ",tolower(substr(string,1,nchar(string)-5))))
}

#mutate Event.Wording
event_names <- mutate(event_names, "eventWording" = substrLeft(event_names$Event.Wording)) 

#fix Variable.name to match duke10

fixEventNames <- function(string){
  string = unlist(as.character(string))
  lengthString <- nchar(string)
  return(paste(substr(string,1,1),substr(string,lengthString-1,lengthString),substr(string,2,lengthString-2),sep=""))
}

event_names <- mutate(event_names, "variableName" = fixEventNames(event_names$Variable.Name))

#function for assigning context variable
variableContext <- function(string,event_names){
  string = unlist(as.character(string))
  return(ifelse(substr(string,1,1)=="e",event_names$eventWording[match(string,event_names$variableName)],"none"))
}

variableContext("eae1",event_names)
match("eae1",event_names$variableNames)

duke10.long <- mutate(duke10.long, "Context" = variableContext(duke10.long$variable,event_names))

#check if correct
sample_n(duke10.long[,c("variable","Context")],size=20)
# 
# variable                                  Context
# 13678     eae45             gangster praying with priest
# 41715    eaa108               retiree waiting on patient
# 73893     eba89                physician scolding outlaw
# 50111     ebe75           murderer surprising chatterbox
# 64722    ebp113                teenager escaping toddler
# 62901     ebp92 psychotic competing with serial murderer
# 116426     ip15                                     none
# 117291     ip24                                     none
# 111313      ie2                                     none
# 80199     eoe33          do nothing jesting with dropout
# 15777     eae69                 lunatic pinching patient
# 23547     eap29            deadbeat dad scolding toddler
# 84271     eoe79                    outlaw slugging bully
# 79590     eoe26     deadbeat dad chewing out grandparent
# 65386    ebp121        unemployed person soothing outlaw
# 112219     ie13                                     none
# 36045     eaa43               gangster comforting gunman
# 68310     eba26     deadbeat dad chewing out grandparent
# 83673     eoe72        murderer interrupting interviewee
# 48686     ebe59          hothead obeying serial murderer

# ---------------- VARIABLE Element, character = {actor, behavior, object, identity}
variableElement <- function(string,context){
  string = unlist(as.character(string))
  context = unlist(as.character(context))
  return( ifelse(context=="none", 
         ifelse(substr(string,1,1) == "i", "identity", "behavior"), 
         c("actor", "behavior", "object")[match(substr(string,2,2),c("a","b","o"))])
         )
}
variableElement(c("ea12","ia12"),c("asdf","none"))

duke10.long <- mutate(duke10.long, "Element" = variableElement(duke10.long$variable,duke10.long$Context))

# check if correct
sample_n(duke10.long[,c("variable","Element")],size=20)

#       variable  Element
# 33038      eaa9    actor
# 11753     eae23    actor
# 10748     eae12    actor
# 31231    eap116    actor
# 35767     eaa40    actor
# 33438     eaa13    actor
# 20800    eae126    actor
# 98993    eop118   object
# 93527     eop56   object
# 34661     eaa27    actor
# 24653     eap42    actor
# 118973     ip43 identity
# 44547     ebe12   action
# 83407     eoe69   object
# 25245     eap48    actor
# 107564    eoa88   object
# 2312       be27 behavior
# 72171     eba70   action
# 51698     ebe93   action
# 22644     eap19    actor

# -------------------- VARIABLE name, names 

identities_behaviors_names <- identities_behaviors[,c("Variable.Name","Event.Wording")]

#fix variable names to match duke10

fixIdenBehavNames <- function(string){
  string <- unlist(as.character(string))
  lengthString <- nchar(string)
  return(paste(substr(string,1,1),substr(string,lengthString,lengthString),substr(string,2,lengthString-1),sep=""))
}

identities_behaviors_names <- mutate(identities_behaviors_names, "variableName" = fixIdenBehavNames(identities_behaviors_names$Variable.Name))

#fix event wording

fixEventWording <- function(string){
  string = unlist(as.character(string))
  lengthString = nchar(string)
  return(gsub("_"," ",paste(substr(string,1,lengthString-5))))
}

identities_behaviors_names <- mutate(identities_behaviors_names, "eventWording" = fixEventWording(identities_behaviors_names$Event.Wording))
# 
# #identitify event elements
# elements <- rep(NA,dim(event_names)[1])
# for(i in 1:dim(event_names)[1]){
#   if(event_names$A.B.O.I.Rated.[i] == "a"){
#     elements[i] = as.character(event_names$Actor)[i]
#     print(event_names$Actor[i])
#   }
#   else if(event_names$A.B.O.I.Rated.[i] == "b"){
#     elements[i] = as.character(event_names$Behavior)[i]
#   }
#   else{
#     elements[i] = as.character(event_names$Object)[i]
#   }
# }
# event_names <- cbind(event_names,"Elements"=elements)

event_names <- mutate(event_names, "Element" = ifelse(event_names$A.B.O.I.Rated == "a", as.character(event_names$Actor), ifelse(event_names$A.B.O.I.Rated=="b", as.character(event_names$Behavior), as.character(event_names$Object))))



variableName <- function(string,context,identities_behaviors_names,event_names){
  string = unlist(as.character(string))
  context = unlist(as.character(context))
  return(ifelse(context == "none", 
                identities_behaviors_names$eventWording[match(string,identities_behaviors_names$variableName)],
                event_names$Element[match(string,event_names$variableName)]
                )
         )
}


duke10.long <- mutate(duke10.long, "Name" = variableName(duke10.long$variable,duke10.long$Context,identities_behaviors_names,event_names))

#check if correct

sample_n(duke10.long[,c("variable","Name")],size=20)
# variable          Name
# 15517     eae66        loafer
# 478         be6 laughing with
# 7848       ba16      slugging
# 36770     eaa51   grandparent
# 60296     ebp63   lusting for
# 118225     ip35  deadbeat dad
# 40192     eaa90        priest
# 51098     ebe86   chatting up
# 116433     ip15       patient
# 106800    eoa79         bully
# 36851     eaa52   grandparent
# 121365     ia25        genius
# 74379     eba95    monitoring
# 66833      eba9      watching
# 89695     eop13        gossip
# 89459     eop10      champion
# 27050     eap69       lunatic
# 88119    eoe123        loafer
# 46668     ebe36    comforting
# 4331       bp13  interrupting

# ----------VARIABLE Dimension ---------------------------

variableRating <- function(string, context){
  string = unlist(as.character(string))
  context = unlist(as.character(context))
  return(ifelse(context=="none", c("Evaluation", "Potency","Activity")[match(substr(string,2,2),c("e","p","a"))], c("Evaluation", "Potency","Activity")[match(substr(string,3,3),c("e","p","a"))]))
}

variableRating(c("eop84","ba14"),c("cat","none"))

duke10.long <- mutate(duke10.long, "Dimension" = variableRating(duke10.long$variable,duke10.long$Context))

#check if correct
sample_n(duke10.long[,c("variable","Rating")],size=20)


# #get variable names
# variableNames <- identities_behaviors$Variable.Name
# 
# fixNames <- function(string){
#   lengthString <- nchar(string)
#   return(paste(substr(string,1,1),substr(string,lengthString,lengthString),substr(string,2,lengthString-1),sep=""))
# }
# 
# #fix variable names to match Duke 10
# variableNamesFixed <- as.character(lapply(as.character(variableNames), fixNames))
# 
# #now match the positions of the variables in the key in duke10
# variablePositions <- match(variableNamesFixed, names(duke10))
# 
# fixEventWording <- function(string){
#   lengthString <- nchar(string)
#   return(paste(substr(string,1,lengthString-5),substr(string,lengthString,lengthString)))
# }
# 
# EventWording <- as.character(lapply(as.character(identities_behaviors$Event.Wording), fixEventWording))
# 
# names(duke10)[variablePositions] <- gsub("_"," ",EventWording)
# 

#---------------CHANGE DEMOGRAPHIC CODING--------------

duke10.long <- duke10.long %>% mutate("Sex" = ifelse(duke10.long$sex==1,"Female","Male")) %>%
  mutate("Sex" = ifelse(duke10.long$sex==1,"Female","Male"))%>%
  mutate("Year" = c("First-Year", "Second-Year", "Third-Year", "Fourth-Year", "Fifth-Year", "Grad Student", "Not a Student")[duke10.long$year]) %>%
  mutate("Hispanic" = ifelse(duke10.long$hisp==1,"Hispanic", "Not Hispanic")) %>%
  mutate("Race" = c("White or Caucasian", "Black or African American", "Native American", "Asian or Asian American", "Other")[duke10.long$race1]) %>%
  mutate("Race2" = c("White or Caucasian", "Black or African American", "Native American", "Asian or Asian American", "Other", "No additional race information")[duke10.long$race1]) %>%
  mutate("Born_US" = ifelse(duke10.long$usborn==1, "Born in US", "Not Born in US")) %>%
  mutate("US_Lived" = c("0-10%", "11-20%", "21-30%", "31-40%", "41-50%", "51-60%", "61-70%", "71-80%", "81-90%", "91-100%")[duke10.long$uslived]) %>%
  mutate("Location" = c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central",  "West South Central",	"Mountain",	"Pacific", "Not in US")[duke10.long$location]) %>%
  mutate("First_Language" = c("English", "Spanish",  "Portugese", "Chinese",  "Japanese", "Korean", "Arabic", "Hindi", "Urdu or Bengali", "Russian" , "Other")[duke10.long$firstland]) %>%
  mutate("Family_Income" = c("Less than $9,999","$10,000 to $29,999", "$30,000 to $69,999", "$70,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 to $299,999", "$300,000 to $399,999", "$400,000 to $499,999", "$500,000 or more")[duke10.long$pincome]) %>%
  mutate("Parent_Marital" = c("Married", "Separated", "Divorced, but neither parent has remarried",  "Divorced, and one or both parents have remarried",	"Widowed, but not remarried",	"Widowed, but has remarried", "Both parents deceased",	"Never married", "Other")[duke10.long$pmarstat]) %>%
  mutate("Mom_Education" = 
           c("Did not graduate high school",  "High school graduate",  "Some college or vocational school",	"Bachelor's degree", "Master's degree",	"Law degree (e.g., LLB, JD)",	"Medical degree (e.g., MD, DDS, DVM)",	"Business degree (e.g., MBA, CPA)", "Doctoral degree", "Multiple post-graduate degrees")[duke10.long$momeduc]) %>%
  mutate("Dad_Education" = c("Did not graduate high school",  "High school graduate",  "Some college or vocational school",  "Bachelor's degree", "Master's degree",	"Law degree (e.g., LLB, JD)",	"Medical degree (e.g., MD, DDS, DVM)",	"Business degree (e.g., MBA, CPA)", "Doctoral degree", "Multiple post-graduate degrees")[duke10.long$dadeduc]) %>%
  mutate("Parents_Own_Home" = ifelse(duke10.long$ownhome1==1,"Yes","No")) %>%
  mutate("Parents_Own_Second_Home" = ifelse(duke10.long$ownhome2==1,"Yes","No")) %>%
  mutate("Parents_Own_Business" = ifelse(duke10.long$ownbus==1,"Yes","No"))
  
#-----------------FINALLY, SELECT COLUMNS AND SAVE EVERYTHING------------------

names(duke10.long)

duke10.final <- duke10.long[,c("id","Sex","Year","Race","Race2","Born_US", "US_Lived","Location","Family_Income","Parent_Marital","Mom_Education","Dad_Education","Parents_Own_Home","Parents_Own_Second_Home","Parents_Own_Business","value","Context","Element","Name","Dimension")] %>% dcast(id + Sex + Year + Race + Race2 + Born_US + US_Lived + Location + Family_Income + Parent_Marital + Mom_Education + Dad_Education + Parents_Own_Home+Parents_Own_Second_Home + Parents_Own_Business + Context + Element + Name ~ Dimension)

# #important to use UTF-8 file encoding
# #write.csv(duke10, file = "duke10fixed.csv", row.names=FALSE, fileEncoding = "UTF-8")

write.csv(duke10.final, file = "duke10long.csv", row.names=FALSE, fileEncoding = "UTF-8")

require(ggvis) 
duke10.final %>% filter(Context == "none", Element == "behavior", Name == "abandoning") %>%
  ggvis(x= ~Activity) %>% layer_histograms()

thing <- duke10.final %>% filter(Context == "none", Element == "identity") %>% select(Name)
levels(as.factor(thing$Name))

