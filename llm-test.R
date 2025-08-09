library(ellmer)

# ...existing code...

# Function to convert natural language taxonomic requests to GBIF taxa selections
nlp_to_taxa_selection <- function(user_request, max_attempts = 3) {
  # Create a system prompt that guides the LLM
  system_prompt <- "
You are a taxonomic expert helping users find the correct GBIF taxonomic classification.
Given a user's request for a species or taxonomic group, you need to:

1. Identify the most specific taxonomic level that matches their request
2. Provide the exact scientific name used in GBIF taxonomy
3. Return a structured response with the taxonomic hierarchy

For example:
- 'birds' -> Kingdom: Animalia, Class: Aves
- 'crows' -> Kingdom: Animalia, Class: Aves, Family: Corvidae
- 'mammals' -> Kingdom: Animalia, Class: Mammalia
- 'Homo sapiens' -> Kingdom: Animalia, Class: Mammalia, Order: Primates, Family: Hominidae, Genus: Homo, Species: Homo sapiens

Always use proper scientific nomenclature and standard taxonomic hierarchy.
Respond with a JSON object containing the taxonomic path from kingdom down to the most specific level.
"

  user_prompt <- glue(
    "
Please identify the taxonomic classification for: '{user_request}'

Return a JSON object with the taxonomic hierarchy, like:
{{
  \"kingdom\": \"Animalia\",
  \"class\": \"Aves\",
  \"family\": \"Corvidae\"
}}

Only include levels that are relevant and specific to the request.
"
  )

  attempt <- 1

  while (attempt <= max_attempts) {
    tryCatch(
      {
        # Get LLM response
        response <- chat(
          system = system_prompt,
          user = user_prompt,
          turns = 1
        )

        # Parse JSON response
        taxa_hierarchy <- jsonlite::fromJSON(response)

        # Validate the hierarchy against GBIF data
        validated_hierarchy <- validate_and_refine_hierarchy(taxa_hierarchy)

        if (!is.null(validated_hierarchy)) {
          return(validated_hierarchy)
        }

        attempt <- attempt + 1
      },
      error = function(e) {
        warning(glue("Attempt {attempt} failed: {e$message}"))
        attempt <<- attempt + 1
      }
    )
  }

  # Fallback: return empty selection if all attempts fail
  warning(glue("Could not process taxonomic request: '{user_request}'"))
  return(list())
}

# Helper function to validate and refine the LLM's taxonomic hierarchy
validate_and_refine_hierarchy <- function(taxa_hierarchy) {
  # Start with kingdom and work down the hierarchy
  current_selections <- list()

  for (rank in taxonomic_ranks) {
    if (rank %in% names(taxa_hierarchy)) {
      proposed_name <- taxa_hierarchy[[rank]]

      # For kingdom, validate directly
      if (rank == "kingdom") {
        available_kingdoms <- taxa |>
          distinct(kingdom) |>
          filter(!is.na(kingdom)) |>
          pull(kingdom)

        if (proposed_name %in% available_kingdoms) {
          current_selections[[rank]] <- proposed_name
        } else {
          # Try fuzzy matching
          match_idx <- agrep(
            proposed_name,
            available_kingdoms,
            max.distance = 0.2
          )
          if (length(match_idx) > 0) {
            current_selections[[rank]] <- available_kingdoms[match_idx[1]]
          } else {
            warning(glue("Kingdom '{proposed_name}' not found in GBIF data"))
            return(NULL)
          }
        }
      } else {
        # For other ranks, use child_taxa to validate
        parent_rank <- taxonomic_ranks[which(taxonomic_ranks == rank) - 1]

        if (parent_rank %in% names(current_selections)) {
          available_children <- child_taxa(
            parent_rank,
            current_selections[[parent_rank]]
          )

          if (proposed_name %in% available_children) {
            current_selections[[rank]] <- proposed_name
          } else {
            # Try fuzzy matching
            match_idx <- agrep(
              proposed_name,
              available_children,
              max.distance = 0.2
            )
            if (length(match_idx) > 0) {
              current_selections[[rank]] <- available_children[match_idx[1]]
              message(glue(
                "Fuzzy matched '{proposed_name}' to '{available_children[match_idx[1]]}'"
              ))
            } else {
              warning(glue(
                "{str_to_title(rank)} '{proposed_name}' not found under {parent_rank} '{current_selections[[parent_rank]]}'"
              ))
              # Stop here but return what we have so far
              break
            }
          }
        }
      }
    }
  }

  return(current_selections)
}

# Convenience function to directly update a taxonomic selector with NLP input
update_selector_from_nlp <- function(session, module_id, user_request) {
  taxa_selection <- nlp_to_taxa_selection(user_request)

  if (length(taxa_selection) > 0) {
    # Update each level in the selector
    for (rank in names(taxa_selection)) {
      updateSelectInput(
        session,
        paste0(module_id, "-", rank),
        selected = taxa_selection[[rank]]
      )
    }

    message(glue(
      "Updated taxonomic selector with: {paste(names(taxa_selection), taxa_selection, sep = '=', collapse = ', ')}"
    ))
    return(taxa_selection)
  } else {
    warning("Could not process the taxonomic request")
    return(list())
  }
}
