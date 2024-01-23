
# OPTIMIZATION MODEL ######################################


## Run LP model to get matches-----------------------------------
#' LP model for matches
#'
#' @param scores dataframe with mentee Name, mentor name and total_score column
#' @param m_ee mentee data
#' @param m_or mentors data
#'
#' @return
#' @import lpSolve
#' @export
#'
#' @examples

get_LP_based_matches <- function(scores, m_ee, m_or,
                                 num_mentees_per_mentor = 1,
                                 num_mentors_per_mentee = 1,
                                 valIfNA = 0, score_column_name = "total_score"){
  require(lpSolve)


  score_matrix_temp <- scores %>% select(mentee_name, mentor_name, !!as.symbol(score_column_name)) %>%
    pivot_wider(names_from = mentor_name,
                values_from = !!as.symbol(score_column_name),
                  #total_score,
                values_fill = valIfNA)

  score_matrix <- score_matrix_temp %>%
    # remove mentee name column
    select(-mentee_name) %>%
    # convert to a matrix
    as.matrix()

  # Add row names to matrix
  rownames(score_matrix) <- score_matrix_temp$mentee_name


  n_mentees <- dim(score_matrix)[1]
  n_mentors <- dim(score_matrix)[2]
  if(n_mentees <= n_mentors){
    row.signs <- rep("=", n_mentees)
  }else{
    row.signs <- rep("<=", n_mentees)
  }
  row.rhs <- rep(num_mentors_per_mentee, n_mentees)

  if(n_mentors <= n_mentees){
    col.signs <- rep("=", n_mentors)
  } else{
    col.signs <- rep("<=", n_mentors)

  }
  col.rhs <- rep(num_mentees_per_mentor, n_mentors)

  lp_res <- lp.transport (score_matrix, "max", row.signs, row.rhs, col.signs, col.rhs)


  dimnames(lp_res$solution) = list(mentee_name=rownames(score_matrix), mentor_name=colnames(score_matrix))

  sol_all = lp_res$solution %>% as_tibble(rownames ="mentee_name")
  sol <- sol_all %>% pivot_longer(
    !mentee_name,
    names_to = "mentor_name",
    values_to = "match"
  ) %>% filter(match==1)
  #sol

  matches <-left_join(m_ee,  sol, by = "mentee_name")%>%
    left_join(m_or, by = "mentor_name")%>% select(-match)

  unmatched_mentees = get_unmatched_mentees(matches)
  unmatched_mentors = get_unmatched_mentors(matches, m_or )

  return(list(matches=matches, objval=lp_res$objval,
              status=lp_res$status,
              unmatched_mentees = unmatched_mentees,
              unmatched_mentors = unmatched_mentors))

}





# HEURISTIC######################################

#' Get unmatched mentees
#'
#' @param matches_tb a tibble of matches
#'
#' @return a tibble of unmatched mentees
#' @export
#'
#' @examples

get_unmatched_mentees <- function(matches_tb){
  ## Identify unassigned mentees
  matches_tb %>% filter(is.na(mentor_name)) %>%
    select(starts_with("mentee")) %>% #, -contains("n_mentee"), -starts_with("mentor")) %>%
    ungroup()
}


#' Get unmatched mentors
#'
#' @param matches_tb a tibble of matches
#' @param m_or_tb a tibble of mentors
#'
#' @return a tibble of unmatched mentors
#' @export
#'
#' @examples

get_unmatched_mentors <- function(matches_tb, m_or_tb){
  ## Identify unassigned mentors
  anti_join(m_or_tb, matches_tb, by = c("mentor_name" = 'mentor_name'))%>% #select(-n_mentor)
    ungroup()

}



#' Get rule-based matches
#'
#' @param m_ee_inn mentee dataframe. must contain `mentee_name` column as unique identifier of each mentee
#' @param m_or_inn mentor dataframe. must contain `mentor_name` column as unique identifier of each mentor
#' @param mentee_criteria_in vector of column names to match on
#' @param mentor_criteria_in vector of column names to match on
#' @param mentee_filter_in a string containing a filtering condition for mentee data, e.g. "gender_pref == 'no_pref' "
#' @param mentor_filter_in a string containing a filtering condition for mentor data
#' @param demo boolean, default = FALSE. If TRUE additional info gets returned.
#'
#' @details matches mentees and mentors based on column names provided in mentee_criteria_in and mentor_criteria_in.
#'
#' @return either a tibble of all mentees with their matches, or if demo =TRUE, a list of  matches + other things.
#' @export
#'
#' @examples
#'
get_rule_based_matches <- function(m_ee_inn, m_or_inn,
                                   mentee_criteria_in,
                                   mentor_criteria_in,
                                   mentee_filter_in = NULL,
                                   mentor_filter_in  = NULL,
                                   demo = FALSE){
  mentee_criteria_all <- c(mentee_criteria_in, "n_mentee")
  mentor_criteria_all <- c(mentor_criteria_in, "n_mentor")

  # ...................................................................
  # MENTEE DATA
  # if mentee_filter_in isn't null, then filter using criteria, otherwise use input data
  if(!is.null(mentee_filter_in)){
    m_ee1 <- m_ee_inn %>% filter(!! rlang::parse_expr(mentee_filter_in))
  }else{
    m_ee1 <- m_ee_inn
  }

  # sort mentee data based on matching criteria and add groups
  m_ee1 <- m_ee1 %>% group_by(across(all_of(mentee_criteria_in))) %>%
    arrange(across(all_of(mentee_criteria_in)))


  # if m_ee1 is not empty, then add row number within each group
  if(dim(m_ee1)[1]>0){
    m_ee1 <- m_ee1 %>% mutate(n_mentee = 1:n())
  }else{
    return(m_ee_inn %>% mutate(mentor_name =as.character(NA)))
  }


  # ...................................................................
  # MENTOR DATA
  # if mentor_filter_in isn't null, then filter using criteria, otherwise use input data
  if(!is.null(mentor_filter_in)){
    m_or1 <- m_or_inn %>% filter(!! rlang::parse_expr(mentor_filter_in))
  }else{
    m_or1 <- m_or_inn
  }


  # sort mentor data based on criteria
  m_or1 <- m_or1 %>% group_by(across(all_of(mentor_criteria_in))) %>%
    arrange(across(all_of(mentor_criteria_in)))

  # if m_or1 is not empty, then add row number within each group
  if(dim(m_or1)[1]>0){
    m_or1 <- m_or1 %>% mutate(n_mentor = 1:n())
  } else{
    return(m_ee1 %>% mutate(mentor_name =as.character(NA)))
  }

  # MATCHES ...................................................................

  ## Assign mentees and mentors who match based on column names in mentor_criteria_all and mentee_criteria_all
  # only includes mentees in filtered group
  matches_1aa <- m_ee1 %>% left_join(m_or1,
                                     by = setNames(mentor_criteria_all, mentee_criteria_all),
                                     keep = TRUE)
  # include all unmatched mentees
  matches_1a <- left_join(m_ee_inn, matches_1aa, by = colnames(m_ee_inn))

  if(demo){
    ## Table for demo purposes only
    matches_1a_tempa <- m_ee1 %>% full_join(m_or1,
                                            by = setNames(mentor_criteria_all, mentee_criteria_all),
                                            keep = TRUE)
    # include all unmatched mentees
    matches_1a_temp <- full_join(m_ee_inn, matches_1a_tempa)
  }

  ## Get unmatched people
  #unmatched_mentees_1a <- get_unmatched_mentees(matches_1a)
  #unmatched_mentors_1a <- get_unmatched_mentors(matches_1a, m_or1)


  # if(match_no_pref & dim(unmatched_mentees_1a %>% filter(gender_pref=="no_pref"))[1]>0 & dim(unmatched_mentors_1a)[1]>0){
  #
  #   mentee_criteria_in_b = mentee_criteria_in[-which(mentee_criteria_in == "gender_pref")]
  #   mentor_criteria_in_b = mentor_criteria_in[-which(mentor_criteria_in == "mentor_gender")]
  #   mentee_criteria_all <- c(mentee_criteria_in_b, "n_mentee")
  #   mentor_criteria_all <- c(mentor_criteria_in_b, "n_mentor")
  #
  #
  #   m_ee1b <- unmatched_mentees_1a %>%
  #     filter(gender_pref=="no_pref") %>%
  #     group_by(across(all_of(mentee_criteria_in_b))) %>%
  #     arrange(across(all_of(mentee_criteria_in_b)))
  #
  #   if(dim(m_ee1b)[1]>0){
  #     m_ee1b <- m_ee1b %>% mutate(n_mentee = 1:n())
  #   }else{
  #     break;
  #   }
  #
  #
  #   ## Sort mentor data by programme,  type
  #   m_or1b <- unmatched_mentors_1a %>% group_by(across(all_of(mentor_criteria_in_b))) %>%
  #     arrange(across(all_of(mentor_criteria_in_b)))
  #
  #   if(dim(m_or1b)[1]>0){
  #     m_or1b <- m_or1b %>% mutate(n_mentor = 1:n())
  #   }else{
  #     break;
  #   }
  #
  #
  #
  #   ## Assign mentees and mentors who match based on programme, year/mentor_type
  #   matches_1b <- m_ee1b %>% inner_join(m_or1b,
  #                                       by = setNames(mentor_criteria_all, mentee_criteria_all),
  #                                       keep = TRUE)
  #
  #   # Combine matches from with and without gender pref
  #   matches_1 <-
  #     bind_rows(
  #       matches_1a %>% filter(!(mentee_name %in% matches_1b$mentee_name)),
  #       matches_1b)
  #
  #
  #
  #
  # }else{
  #   matches_1 = matches_1a
  # }
  if(demo){
    # demo =TRUE means extra stuff gets returned
    return(list(matches = matches_1a, m_ee1=m_ee1, m_or1 =m_or1,
                matches_1a = matches_1a,
                matches_1a_temp=matches_1a_temp
                #unmatched_mentees_1a,m_ee1b=m_ee1b, m_or1b =m_or1b,
                #matches_1b = matches_1b
    ))
  }else{
    return(matches_1a)
  }
}





#' Title
#'
#' @param m_ee_in mentee dataframe. must contain `mentee_name` column as unique identifier of each mentee
#' @param m_or_in mentor dataframe. must contain `mentor_name` column as unique identifier of each mentor
#' @param mentee_criteria_list_in a list containing vectors of column names to match on
#' @param mentor_criteria_list_in a list containing vectors of column names to match on
#' @param mentee_filter_list_in a list containing NULL or a string containing a filtering condition for mentee data, e.g. "gender_pref == 'no_pref' "
#' @param mentor_filter_list_in a list containing NULL or a string containing a filtering condition for mentor data
#'
#' @details matches mentees and mentors based on column names provided in mentee_criteria_in and mentor_criteria_in.
#'
#' @return either a tibble of all mentees with their matches, or if demo =TRUE, a list of  matches + other things.
#' @export
#'
#' @examples
#'
#'
get_rule_based_matches_all <- function(m_ee_in, m_or_in,
                                       matching_criteria_in
                                       #mentee_criteria_list_in = NULL,
                                       #mentor_criteria_list_in = NULL ,
                                       #mentee_filter_list_in = NULL,
                                       #mentor_filter_list_in  = NULL
){

  mentee_criteria_list_in = matching_criteria_in$mentee_criteria
  mentor_criteria_list_in = matching_criteria_in$mentor_criteria
  mentee_filter_list_in  = matching_criteria_in$mentee_filter
  mentor_filter_list_in   = matching_criteria_in$mentor_filter

  if(is.null(mentee_criteria_list_in)) {
    stop("get_rule_based_matches_all: matching criteria not provided")
  }
  if(is.null(mentor_criteria_list_in)) {
    stop("get_rule_based_matches_all: matching criteria not provided")
  }

  iteration <- 1
  n_matching_criteria_mentee <- length(mentee_criteria_list_in)
  n_matching_criteria_mentor <- length(mentor_criteria_list_in)
  n_filter_criteria_mentee <- length(mentee_filter_list_in)
  n_filter_criteria_mentor <- length(mentor_filter_list_in)
  max_num_iterations <- min(n_matching_criteria_mentee, n_matching_criteria_mentor, n_filter_criteria_mentee,n_filter_criteria_mentor)

  m_i_list <- list()
  while(iteration <= max_num_iterations){

    #print(iteration)
    ## Match based on programe, year and gender
    mentee_criteria = mentee_criteria_list_in[[iteration]]
    mentor_criteria = mentor_criteria_list_in[[iteration]]
    mentee_filter = mentee_filter_list_in[[iteration]]
    mentor_filter = mentor_filter_list_in[[iteration]]



    if(iteration == 1){
      m_ee=m_ee_in
      m_or=m_or_in
    }else{
      m_ee=unmatched_mentees_i
      m_or=unmatched_mentors_i
    }

    if(FALSE){
      print(iteration)
      print(mentee_criteria)
      print(mentor_criteria)
      print(mentee_filter)
      print(mentor_filter)
     print(m_ee)
     print(m_or)
    }


    m_i <- get_rule_based_matches(m_ee_inn=m_ee,
                                  m_or_inn=m_or,
                                  mentee_criteria_in = mentee_criteria,
                                  mentor_criteria_in = mentor_criteria,
                                  mentee_filter_in = mentee_filter,
                                  mentor_filter_in = mentor_filter)
    unmatched_mentees_i <- get_unmatched_mentees(m_i)
    unmatched_mentors_i <- get_unmatched_mentors(m_i, m_or)

    m_i_list[[iteration]] <- m_i %>% mutate(iteration_number = iteration)

    iteration = iteration + 1
  } # END WHILE

  matches <- bind_rows(m_i_list) %>%
    filter(!is.na(mentor_name)) %>%
    arrange(mentee_name) %>% select(-n_mentee, -n_mentor)

  matching_filtering_criteria <- tibble(1:(iteration-1),
                                        mentee_criteria_list_in,
                                        mentor_criteria_list_in,
                                        mentee_filter_list_in,
                                        mentor_filter_list_in)

  return(list(matches = matches,
              unmatched_mentors=unmatched_mentors_i,
              unmatched_mentees=unmatched_mentees_i,
              matches_by_iteration =  m_i_list,
              matching_filtering_criteria = matching_filtering_criteria,
              max_iteration_num = matches$iteration_number %>% max()
              # mentee_criteria_list_in = mentee_criteria_list_in,
              # mentor_criteria_list_in = mentor_criteria_list_in ,
              # mentee_filter_list_in = mentee_filter_list_in,
              # mentor_filter_list_in  = mentor_filter_list_in
  ))

}




# EVALUATE MATCHES ######################################
calculate_criteria_score_unweighted <- function(match_data, scoring_criteria){

  match_data_scores <- match_data %>% rowwise()
  # create a new column with TRUE if mentee is matched and FALSE otherwise
  match_data_scores <- match_data_scores %>%
    mutate(matched_mentee = !is.na(mentor_name))

  n_criteria <- dim(scoring_criteria)[1]
  for(i in 1:n_criteria){

    # for each matching criteria,
    # create a new column with TRUE if mentee and mentor match on that column and FALSE otherwise

    if(scoring_criteria$match_type[i]=="exact"){
      match_data_scores <- match_data_scores %>%
        mutate(!!as.symbol(paste0("match_", scoring_criteria$match_name[i])) :=
                 (!!as.symbol(scoring_criteria$mentee_criteria[i]) ==!!as.symbol( scoring_criteria$mentor_criteria[i]))

        )
    } else if(scoring_criteria$match_type[i]=="subset"){
      match_data_scores <- match_data_scores %>% #select(mentee_name, mentor_name, mentee_major1, mentor_majors) %>%
        #replace_na(list(
        #  !!as.symbol(scoring_criteria$mentor_criteria[[i]]) := ""
        #as.character(!!as.symbol(scoring_criteria$mentee_criteria[[i]]))
        #) )%>%
        #replace_na(list(mentee_major1="", mentee_major2="", mentor_major = "")) %>%
        mutate(!!as.symbol(paste0("match_", scoring_criteria$match_name[i])) :=
                 #(!!as.symbol(scoring_criteria$mentee_criteria[i])
                 #  ==!!as.symbol( scoring_criteria$mentor_criteria[i]))
                 str_detect(as.character(!!as.symbol(scoring_criteria$mentor_criteria[[i]])),
                            as.character(!!as.symbol(scoring_criteria$mentee_criteria[[i]])))

        ,
        across(contains(paste0("match_", scoring_criteria$match_name[i])), ~ replace_na(.x, FALSE))

        )


    }
    # print(match_data_scores)

    # Mentee_filter
    # if there is a filtering criteria e.g. gender == "no_pref", then overwrite value to TRUE
    if(!is.na(scoring_criteria$mentee_filter[i])){
      # e.g. match_data_scores %>% mutate(match_gender_pref = if_else(gender_pref == "no_pref", TRUE, match_gender_pref))
      match_data_scores <-  match_data_scores %>%
        mutate(!!as.symbol(paste0("match_", scoring_criteria$match_name[i])) :=
                 if_else(!! rlang::parse_expr(scoring_criteria$mentee_filter[i]),
                         TRUE,
                         !!as.symbol(paste0("match_", scoring_criteria$match_name[i]))))
      # print(match_data_scores)
    }#END MENTEE FILTER


    # Mentor_filter
    # if there is a filtering criteria e.g. mentee_programme == NA, then overwrite value to TRUE
    if(  !is.na(scoring_criteria$mentor_filter[i])){
      # match_data_scores %>% mutate(match_gender_pref = if_else(gender_pref == "no_pref", TRUE, match_gender_pref))
      match_data_scores <-  match_data_scores %>%
        mutate(!!as.symbol(paste0("match_", scoring_criteria$match_name[i])) :=
                 if_else(!! rlang::parse_expr(scoring_criteria$mentor_filter[i]),
                         TRUE,
                         !!as.symbol(paste0("match_", scoring_criteria$match_name[i]))))
      #print(match_data_scores)
    }#END MENTOR FILTER



  }#END FOR

  # Convert boolean to numeric
  match_data_scores <-  match_data_scores %>%
    mutate(across(starts_with("match"), as.numeric))

  return(match_data_scores)

}


calculate_criteria_score <- function(match_data, scoring_criteria,
                                     match_data_scores  = NULL){
  #  mentee_criteria_list_in = mentee_criteria_list, mentor_criteria_list_in = mentor_criteria_list,

  if(is.null(match_data_scores)){
    #cat("-")
    match_data_scores <- calculate_criteria_score_unweighted(match_data=match_data,
                                                             scoring_criteria=scoring_criteria)
  }

  # Set weights
  if(any(str_detect(names(scoring_criteria), "weight"))){
    weight <- scoring_criteria$weight
  }else{
    weight = rep(1, n_criteria)
  }
  names(weight) <- paste0("match_",scoring_criteria$match_name)

  #cat("-start unweighted1")
  #match_data_scores_small <- match_data_scores %>% ungroup() %>% select(mentee_name, mentor_name, starts_with("match_"))

  match_data_scores_0 <- match_data_scores %>% ungroup() %>% select(mentee_name, mentor_name, starts_with("match_"))  %>%
    mutate(total_unweighted_score = rowSums(across(starts_with("match_")))) %>%
    select( mentee_name, mentor_name, total_unweighted_score, starts_with("match_") )



  # match_data_scores_1 <-  match_data_scores_temp %>%
  #   mutate(total_score = rowSums(across(paste0("match_",scoring_criteria$match_name),
  #                                       ~ .x * weight[cur_column()]
  #
  #          ), na.rm = TRUE))

  #cat("start weighted2")

  # data names
  if(all(match_data_scores_0 %>% select( contains(scoring_criteria$match_name)) %>% names() ==
    names(weight))){
    #weights are in same order as columns
    tempxx <- match_data_scores_0 %>% ungroup()
    for(ii in 1:length(names(weight))){
      tempxx[,3+ii] <- tempxx[,3+ii]*weight[ii]
    }
    tempxx$total_score <- rowSums(tempxx[,(3+1):(3+ii)])
    match_data_scores_1 <- match_data_scores_0
    match_data_scores_1$total_score <-  tempxx$total_score
  }else{

    warning("weights in wrong order. Using sloooow method.")

    match_data_scores_1 <-  match_data_scores_0 %>%

      mutate(total_score = rowSums(across(paste0("match_",scoring_criteria$match_name),
                                          ~ .x * weight[cur_column()]

      ), na.rm = TRUE))
  }

  # match_data_scores_1 <-  match_data_scores_small %>%
  #
  #   mutate(total_score = rowSums(across(paste0("match_",scoring_criteria$match_name),
  #                                       ~ .x * weight[cur_column()]
  #
  #   ), na.rm = TRUE))
  #
  # cat("end weighted start original")
  #
  # # Compute total score - THIS IS SLOW
  # match_data_scores_1 <-  match_data_scores_small %>%
  #   mutate(total_unweighted_score =          rowSums(across(paste0("match_",scoring_criteria$match_name))),
  #          total_score = rowSums(across(paste0("match_",scoring_criteria$match_name),
  #                                        ~ .x * weight[cur_column()]
  #
  #          ), na.rm = TRUE))
  #
  #cat("-end weighted")

  match_data_scores_2 <-  match_data_scores_1 %>% select(contains("name"), total_unweighted_score, total_score, starts_with("match_"))
                                                     #, everything())

  #cat("-")

  return(list(match_data_scores=match_data_scores_2, mentee_mentor_data = match_data))
}




calculate_match_score_summary <- function(match_data, scoring_criteria,
                                          n_mentees = NULL,
                                          match_data_scores  = NULL){

  #cat("+")
  match_score_data = calculate_criteria_score(match_data = match_data,
                                              scoring_criteria = scoring_criteria,
                                              match_data_scores = match_data_scores)$match_data_scores
  #cat("+")
  #match_score_data %>%
  #  select(contains("name"), starts_with("match")) %>%
  #  mutate(across(starts_with("match"), as.numeric))


  num_perc <- list(
    num = ~ sum(.x, na.rm = TRUE),
    perc = ~ 100*sum(.x, na.rm = TRUE)/length(.x))

    score_data <- match_score_data %>% ungroup() %>% filter(!is.na(mentor_name)) %>% #select(starts_with("match")) %>%
    summarise(across(starts_with("match"), num_perc))

  total_score = sum(match_score_data$total_score, na.rm = TRUE)
  mean_score = mean(match_score_data$total_score, na.rm = TRUE)
  min_score = min(match_score_data$total_score, na.rm = TRUE)
  max_score = max(match_score_data$total_score, na.rm = TRUE)
  lq_score = quantile(match_score_data$total_score, na.rm = TRUE, 0.25)
  uq_score = quantile(match_score_data$total_score, na.rm = TRUE, 0.75)

  median_score = median(match_score_data$total_score, na.rm = TRUE)
  matched_mentees_num = match_data %>% filter(!is.na(mentor_name)) %>% ungroup() %>% count() %>% pull()
  matched_mentees_perc = 100*matched_mentees_num/n_mentees

  return(list(match_score_data=match_score_data, score_data=score_data, total_score = total_score,
              mean_score = mean_score, median_score = median_score,
              min_score = min_score,
              max_score = max_score,
              lq_score = lq_score,
              uq_score = uq_score,
              matched_mentees_num=matched_mentees_num,
              matched_mentees_perc=matched_mentees_perc))

}








# WRAPPER FUNCTION ######################################

#' Get and evaluate matches using heuristic and LP methods
#'
#' @param m_ee df of mentee data
#' @param m_or df of mentor data
#' @param scoring_criteria df of criteria used for scoring matches
#' @param weights df weights given to each row in scoring criteria (optional)
#' @param matching_criteria df of criteria used in heuristic
#' @param valIfNA default = 0
#' @param compare_original boolean. If TRUE compares heuristic and LP to original data
#' @param original_matches_raw optional df containing matches
#' @param verbose boolean
#'
#' @return df showing performance of each method
#' @export
#'
#' @examples
get_and_evaluate_matches <- function(mentee_data, mentor_data,
                        scoring_criteria,
                        weights = NULL,
                        matching_criteria,
                        valIfNA = 0,
                        compare_original = FALSE, original_matches_clean = NULL,
                        original_name = "original",
                        verbose = TRUE,
                        saveAllData = FALSE,
                        saveTimes = FALSE){

  if(saveTimes){
  times_record <- tibble(start = Sys.time(), end = Sys.time(), time_diff = end-start,
                         alg = NA, name = NA, alg_id = NA, id = NA)
  indy <- 1
  }

  if(verbose){
    cat("Computing matches for", dim(mentee_data)[1], "mentees and", dim(mentor_data)[1], "mentors. \n")
  }

  # get heuristic based matches .....................................

  num_heuristics <- length(matching_criteria)
  if(verbose){
    cat("Computing matches using ",num_heuristics, "rule-based heuristics: ")
  }

  heur_match <- list()
  heur_scores <- list()
  heur_match_all_data <- list()
  heur_scoring_data <- list()

  for(i in 1:num_heuristics){
    if(verbose){
      cat(i, " ")

    }


    if(saveTimes) times_record[indy, "start"] = Sys.time()


  m_heuristic <- get_rule_based_matches_all(m_ee_in=mentee_data, m_or_in=mentor_data,
                                            matching_criteria_in = matching_criteria[[i]]
  )


  #evaluate match
  score_summary_heuristic <- calculate_match_score_summary(match_data = m_heuristic$matches,
                                                           scoring_criteria = scoring_criteria,
                                                           n_mentees = dim(mentee_data)[1])
  # score by mentee/mentor pair

  heur_match[[i]] = score_summary_heuristic$match_score_data
  # summary stats
  heur_scores[[i]] = bind_cols(alg = "rule-based",
                               name = paste(" ", dim(matching_criteria[[i]])[1]),
                              alg_id =   paste0("rule-based_", i),
                              id = dim(matching_criteria[[i]])[1],
                              total_score = score_summary_heuristic$total_score,
                              mean_score = score_summary_heuristic$mean_score,
                              median_score = score_summary_heuristic$median_score,
                              min_score = score_summary_heuristic$min_score,
                              max_score= score_summary_heuristic$max_score,
                              lq_score= score_summary_heuristic$lq_score,
                              uq_score= score_summary_heuristic$uq_score,
                              matched_mentees_num=score_summary_heuristic$matched_mentees_num,
                              matched_mentees_perc=score_summary_heuristic$matched_mentees_perc,
                              #obj_val = as.numeric(NA),
                              score_summary_heuristic$score_data,
                              num_iterations = m_heuristic$max_iteration_num)
  if(saveTimes){
  times_record[indy, c("alg", "name", "alg_id")] = heur_scores[[i]][c("alg", "name", "alg_id")]
  times_record$id[indy] = heur_scores[[i]] %>% pull(id)
  times_record$end[indy]  = Sys.time()
  indy = indy + 1
  }

  if(saveAllData){
    heur_match_all_data[[i]] = m_heuristic
    heur_scoring_data[[i]] = score_summary_heuristic
  }
  } #end heuristics
  if(verbose) cat("\n")
  #.................................................................


  # get LP based matches ...........................................

  lp_match <- list()
  lp_scores <- list()
  lp_scoring_data <- list()

  if(saveTimes)  times_record[indy, "start"] = Sys.time()

  # create pairs of all mentees/mentors
  all_pairs <- expand_grid(mentee_name = mentee_data$mentee_name,
                           mentor_name = mentor_data$mentor_name)
  all_pairs <- full_join(all_pairs, mentee_data, by = "mentee_name") %>%
    full_join( mentor_data, by = "mentor_name")

  # compute scores
  if(!is.null(weights)){

   # if(!is.null(scoring_criteria$weight)){
    # if(any(str_detect(names(scoring_criteria), "weight"))){
    # warning("Weights specified in two places. Using weights in weights and ignoring scoring_criteria$weight for")
    # }else{
    #   # using weights input
    # }
    if(!any(str_detect(names(weights), "id"))){
      weights <- weights %>% mutate(id = 1:n())
    }

  }else{
    #weights is null
    # check if scoring_criteria$weight is null - if it is null, use 1,1,1,1
    if(!any(str_detect(names(scoring_criteria), "weight"))){

      scoring_criteria$weight = 1
      #= rep(1, dim(scoring_criteria)[1]) %>%t()
    }

    # create weights tibble of required format
    #temp2 <- temp_weights
    #olnames(temp2) <- scoring_criteria$match_name
    #temp2 <- temp2 %>% as_tibble()
    temp2 <- scoring_criteria %>%
      select(match_name, weight) %>%
      pivot_wider(names_from = match_name, values_from = weight)
    temp2$id = 1
    temp2$name = NA
    weights = temp2

  }

  num_weights_combinations <-dim(weights)[1]
  if(verbose){
    #print(weights)
    cat("Running LP models with", num_weights_combinations, "combinations of weights: ")
  }


  match_data_scores_unweighted_i <- calculate_criteria_score_unweighted(
                                                    match_data = all_pairs,
                                                    scoring_criteria = scoring_criteria %>% select(-weight))
  # if(verbose){
  #   cat("*")
  #
  # }

  if(saveTimes){
  times_record$alg[indy] = "LP"
  times_record$name[indy] ="scoring"
  times_record$alg_id[indy] = "LP_scoring"
  times_record$id[indy] = NA
  times_record$end[indy]  = Sys.time()
  indy = indy + 1
  }

    for(i in 1: num_weights_combinations){
        if(verbose){
          cat(i, " ")

        }

      if(saveTimes)      times_record[indy, "start"] = Sys.time()

      # add weight for current combination to scoring critera
      scoring_criteria_temp <- scoring_criteria %>% select(-weight)
      temp1 <- weights %>% select(((where(is.numeric)))) %>% filter(id == i) %>% select(-id)
      temp1_long <- temp1 %>% pivot_longer(cols = everything(), names_to = "match_name", values_to = "weight")
      scoring_criteria_temp <- left_join(scoring_criteria_temp, temp1_long,
                                         by = "match_name")

      # if(verbose){
      #   cat(".")
      # }
      score_data <- calculate_criteria_score(match_data = all_pairs,
                                             scoring_criteria=scoring_criteria_temp,
                                             match_data_scores = match_data_scores_unweighted_i)
      #score_data$match_data_scores
      # if(verbose){
      #   cat("*")
      # }

      # do LP matching
      lp_match_i <-
        get_LP_based_matches( scores = score_data$match_data_scores, m_ee=mentee_data, m_or=mentor_data
        )
      lp_match[[i]] <- lp_match_i
      # if(verbose){
      #   cat(".")
      # }
      # use weights in original scoring criteria for comparability with other methods.
      # weights in weights are used to compute objective function.
      score_summary_lp <- calculate_match_score_summary(match_data = lp_match_i$matches,
                                                        scoring_criteria = scoring_criteria,
                                                        n_mentees = dim(mentee_data)[1])

      # if(verbose){
      #   cat(".")
      # }




      if(saveAllData){
        lp_scoring_data[[i]] <- score_summary_lp
      }
      # score by mentee/mentor pair
      # score_summary_optimal$match_score_data
      # summary stats
      lp_scores[[i]]  =   bind_cols(alg = "LP",
                                    alg_id =   paste0("LP_", i),
                                    id = weights[i, "name"],
                                    #name = weights[i, "name"],
                                    total_score = score_summary_lp$total_score,
                                    mean_score = score_summary_lp$mean_score,
                                    median_score = score_summary_lp$median_score,
                                    min_score = score_summary_lp$min_score,
                                    max_score= score_summary_lp$max_score,
                                    lq_score= score_summary_lp$lq_score,
                                    uq_score= score_summary_lp$uq_score,
                                    matched_mentees_num=score_summary_lp$matched_mentees_num,
                                    matched_mentees_perc=score_summary_lp$matched_mentees_perc
                                    , obj_val = lp_match_i$objval
                                    , score_summary_lp$score_data,
                                    num_iterations = NA)


      if(saveTimes){
      times_record[indy, c("alg", "name", "alg_id")] = lp_scores[[i]][c("alg", "name", "alg_id")]
      times_record$id[indy] = i
      times_record$end[indy]  = Sys.time()
      indy = indy + 1
      }
    }#END FOR




  #.................................................................
  original_match <- list()
  original_scores <- list()
  original_scoring_data <- list()
  if(compare_original & !is.null(original_matches_clean)){
    if(verbose){
      cat("\nComputing score of actual matches.")
    }
    if(saveTimes)    times_record[indy, "start"] = Sys.time()

    # tidy original matches data
    # add columns from m_ee and m_or to match other methods
    original_matches <- mentee_data %>% left_join(
      original_matches_clean,
      by = c("mentee_name")) %>%
      left_join(mentor_data, by = c("mentor_name"))

    #evaluate match
    score_summary_original <- calculate_match_score_summary(match_data = original_matches,
                                                            scoring_criteria = scoring_criteria,
                                                            n_mentees = dim(mentee_data)[1])
    # score by mentee/mentor pair

    if(saveAllData){
      original_scoring_data[[1]] <- score_summary_original
    }

    original_match[[1]] <- original_matches
    original_scores[[1]] <- bind_cols(alg = paste0(original_name),
                                      total_score = score_summary_original$total_score,
                                      mean_score = score_summary_original$mean_score,
                                      median_score = score_summary_original$median_score,
                                      min_score = score_summary_original$min_score,
                                      max_score= score_summary_original$max_score,
                                      lq_score= score_summary_original$lq_score,
                                      uq_score= score_summary_original$uq_score,
                                      matched_mentees_num=score_summary_original$matched_mentees_num,
                                      matched_mentees_perc=score_summary_original$matched_mentees_perc,
                                   #, obj_val = NA,
                                   score_summary_original$score_data,
                                   num_iterations = NA)

    if(saveTimes){
    times_record[indy, c("alg")] = original_scores[[i]]["alg"]
    times_record$end[indy]  = Sys.time()
    indy = indy + 1
    }

  }#END compare original


  # turn results into a df
  df1 <- heur_scores %>% bind_rows()
  df2 <- lp_scores %>% bind_rows()
  df3 <- original_scores %>% bind_rows()
  results <- bind_rows(df1, df2) %>% bind_rows(df3)


  # extract columns to include only percentages or only raw numbers
  perc_results <- results %>% select(alg, name, total_score, obj_val, mean_score,
                                     min_score, contains("perc"))

  num_results <- results %>% select(alg, total_score,obj_val,  mean_score, min_score, contains("num"))

  if(saveTimes) {
    times_record$time_diff <- times_record$end - times_record$start
  }else{
    times_record = NA
  }

  return(list(all_results = results,
              perc_results = perc_results,
              num_results = num_results,
              times_record = times_record,
              matches = list(heur_match=heur_match,
                             heur_scores=heur_scores,
                             heur_scoring_data,
                             heur_match_all_data=heur_match_all_data,
                             lp_match= lp_match, lp_scores=lp_scores, lp_scoring_data =lp_scoring_data,
                             original_match =original_match, original_scores=original_scores
                             , original_scoring_data = original_scoring_data)
              ) )
}



#' get_best_match
#' after running algorithm on multiple versions of rule and LP
#'
#' @param results
#' @param criteria
#'
#' @return
#' @export
#'
#' @examples
get_best_matches <- function(results,   criteria = "mean_score"){

if(criteria !="mean_score"){stop("function only set to use criteria=mean_score")}

  best_match <-
    results$all_results %>%
    #slice_max(!!criteria, n=1)#
    arrange(desc(mean_score)) %>% slice_head(n=1)

  if(best_match$alg=="rule-based"){

    alg_version = gsub("rule_based_", "", x=  best_match$alg_id) %>% as.numeric()
    m=  results$matches$heur_match_all_data[[alg_version]]$matches
    unmatched_mentees = results$matches$heur_match_all_data[[alg_version]]$unmatched_mentors
    unmatched_mentors = results$matches$heur_match_all_data[[alg_version]]$unmatched_mentees

  }else if(best_match$alg=="LP"){
    alg_version = gsub("LP_", "", x=  best_match$alg_id) %>% as.numeric()
    m=  results$matches$lp_match[[alg_version]]$matches %>%
      #add scores
      left_join(results$matches$lp_scoring_data[[alg_version]]$match_score_data) %>%
      # arrange columns
      select(mentee_name, mentor_name, total_score, contains("mentee"), contains("mentor"), everything())

    unmatched_mentees =  results$matches$lp_match[[alg_version]]$unmatched_mentees
    unmatched_mentors = results$matches$lp_match[[alg_version]]$unmatched_mentors
  }

  return(list(matches = m,
              unmatched_mentees =unmatched_mentees,
              unmatched_mentors = unmatched_mentors))


}
