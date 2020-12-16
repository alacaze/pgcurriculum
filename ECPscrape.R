library(rvest)
library(stringr)
library(tibble)
library(dplyr)
library(readr)

program <- read_html("https://my.uq.edu.au/programs-courses/program_list.html?acad_prog=2373")

# table <- html_nodes(program, "table")

# select the courses and the links 
courses <- html_nodes(program, ".courses a")

# pull out the course labels and sort
course_names <- html_text(courses, trim = TRUE) %>% unique()  %>% sort()
course_names <- course_names[-1]
# html_text(courses)
# unique(course_names)

# create links to the pages that hold the ECPs
base <- "https://my.uq.edu.au/programs-courses/course.html?course_code="
course_links <- paste0(base, course_names)


# now need to get to page that holds the ECPs
# course_links[1] %>% read_html() %>%  html_nodes("#course-offering-1-profile .profile-available") 

# biol1020 <- course_links[1] %>% read_html() %>%  html_nodes(".profile-available") %>% html_attr(.,'href') # %>% str_sub(-6,-1)
# biol1020
# biol1020[1]


# a function to go from course link to ECP code this will give the most recent ECP that is available; the link goes to section 1
ecp_code <- function(x){
  group <- read_html(x) %>%  html_nodes(".profile-available")   %>% html_attr(.,'href') #%>% str_sub(-6,-1)
  group[1]
}

# course_links[2] %>% ecp_code()
# course_links %>% ecp_code()
# test <- sapply(course_links, ecp_code)
# 
# course_links[6] %>% ecp_code()
# course_links[6]

# Creating ecp_links:

ecp_links <- 0

for (i in 1:length(course_links)){
  ecp_links[i] <- course_links[i] %>% ecp_code()
}

# next steps: convert 'section_1' to 'section_5' and create database with the assessment table

Bpharm <- tibble(course_names,course_links, ecp_links)
Bpharm
# write.csv(Bpharm, file = "bpharm.csv")


# The following will create a column with specific links to the Assessment section of the ECP
ecp_links_assessment <- ecp_links %>%  str_replace("section_1", "section_5")

# The following will create a column with specific links to the Learning Objectives section of the ECP
ecp_links_lo <- ecp_links %>%  str_replace("section_1", "section_2")

# Bpharm <- data.frame(course_names,course_links, ecp_links, ecp_links_assessment)

Bpharm <- tibble(course_names,course_links, ecp_links, ecp_links_assessment, ecp_links_lo)


# df1 <- ecp_links_assessment[1] %>% read_html() %>% html_nodes("table") %>% html_table(header = TRUE) #%>% as.data.frame()
# 
# task <- ecp_links_assessment[2] %>% read_html() %>% html_nodes("tbody .text-center:nth-child(1)") %>% html_text(trim=TRUE)  %>% str_replace("\\n[:space:]*\\n[:space:]*",":")
# 
# weight <- ecp_links_assessment[2] %>% read_html() %>% html_nodes("tbody .text-center:nth-child(3)") %>% html_text(trim=TRUE) %>% str_replace("%","") %>% as.numeric()/100

learning_obj <- ecp_links_lo[1] %>% read_html() %>% html_nodes(".objectives-list span") %>% html_text(trim=TRUE)
learning_obj <- NULL

# Above just picks out column 1; can modify this and then capture the rest

# functions to scrape "assessment tasks" and "weight"
assessment_task <- function(x){
  read_html(x) %>% html_nodes("tbody .text-center:nth-child(1)") %>% html_text(trim=TRUE)  %>% str_replace("\\n[:space:]*\\n[:space:]*",":")
}

assessment_weight <- function(x){
  read_html(x) %>% html_nodes("tbody .text-center:nth-child(3)") %>% html_text(trim=TRUE) %>% str_replace("%","") %>% as.numeric()/100
  # need to modify this to also get the text; not all data is in the format of a percentage
}

# functions to scrape "learning objectives"
learning_obj <- function(x){
  read_html(x) %>% html_nodes(".objectives-list span") %>% html_text(trim=TRUE)
}
# Script to build the assessment data frame/tibble:

task <- NULL
weight <- NULL
course <- NULL

for (i in 1:length(course_links)){
 task <- c(task, assessment_task(ecp_links_assessment[i]))
 weight <- c(weight, assessment_weight(ecp_links_assessment[i]))
 course <- c(course, rep(course_names[i], length(assessment_task(ecp_links_assessment[i]))))
}

######
Bpharm_assessment <- tibble(course, task, weight)

write.csv(Bpharm_assessment, file = "AssessmentDB.csv")
rep(course_names[i], length(assessment_task(ecp_links_assessment[i])))
test_task <- assessment_task(ecp_links_assessment[30])

#####

## Script to build learning objective data frame/tibble:

lo <- NULL
course <- NULL

for (i in 1:length(course_links)){
  lo <- c(lo, learning_obj(ecp_links_lo[i]))
  course <- c(course, rep(course_names[i], length(learning_obj(ecp_links_lo[i]))))
}

Bpharm_learningobj <- tibble(course, lo)

write.csv(Bpharm_learningobj, file = "BPharm_learningobj.csv")

#####

pharm_assessment <- read_csv("AssessmentDB_mod.csv")
pharm_assessment <- tibble(pharm_assessment)

check_weights <- pharm_assessment %>% 
  group_by(course) %>% 
  summarise(weight = sum(weight))

####

# pharm_assessment$exam <- ifelse(pharm_assessment$task)

pharm_assessment <- pharm_assessment %>% mutate(exam = case_when(
  str_detect(pharm_assessment$task, "Exam") ~ 1, 
  TRUE ~ 0)
)

# find Oral exams
oral_exam <- grep("Oral", pharm_assessment$descriptor)
oral_exam <- c(oral_exam, grep("Oral", pharm_assessment$descriptor2))
cons_exam <- grep("Consultation", pharm_assessment$descriptor)

pharm_assessment$course[cons_exam]
pharm_assessment$exam[cons_exam] <- 0
pharm_assessment$exam[oral_exam] <- 0

disp_exam <- grep("Dispensing Exam", pharm_assessment$descriptor)
pharm_assessment$exam[disp_exam] <- 0

assessment_types <- pharm_assessment %>% 
  group_by(task) %>% 
  summarise(
    count = n(),
    sumweight = sum(weight)
  )

total_weight <- sum(pharm_assessment$weight)

assessment_exam <- pharm_assessment %>% 
  group_by(exam) %>% 
  summarise(
    count = n(),
    percweight = sum(weight)/total_weight * 100
  )

assessment2_exam <- pharm_assessment %>% 
  group_by(course, exam) %>% 
  summarise(
    count = n(),
    percweight = sum(weight)/1 * 100
  )

assessment2_exam <- filter(assessment2_exam, exam == 1)
assessment2_exam <- arrange(assessment2_exam, desc(percweight))

assessment2_exam <- assessment2_exam %>% 
  mutate(
    link = paste0("https://www.library.uq.edu.au/exams/papers.php?stub=", course)
  )

write.csv(assessment2_exam, "examMCQ_original.csv")

###

# The following relies on the 2019 *final exam* (central). Some of the perc-weight is from additional assessment (midsem exam etc). This is an estimate.

assessment_mcq <- read_csv(file="examMCQ.csv")
assessment_mcq <- assessment_mcq %>% 
  mutate(
    # MCQ_weight = MCQ/(MCQ+SA) * 100,
    MCQ_courseweight = (percweight * MCQ_weight)/10000
  )

mcq_total <- sum(assessment_mcq$MCQ_courseweight)
mcq_total/38

### get a ranking of subjects based on MCQ contribution to assessment.

course_mcq_rank <- assessment_mcq %>% 
  select(course, count, MCQ_courseweight) %>%
  arrange(desc(MCQ_courseweight))
course_mcq_rank <-  course_mcq_rank %>% 
  mutate(perc_MCQ = MCQ_courseweight * 100) %>% 
  filter(perc_MCQ >= 40)
course_mcq_rank <- course_mcq_rank %>% 
  select(-MCQ_courseweight)

write_csv(course_mcq_rank, "course_mcq_rank.csv")
