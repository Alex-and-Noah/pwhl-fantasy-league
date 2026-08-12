library(blastula)

make_email <- function(df) {
    compose_email(
        body = md(
            glue::glue(
                "
## Hello {df$name}

This is an email.
"
            )
        ),
        footer = md(
            "
Code by Alex <3
"
        )
    )
}
