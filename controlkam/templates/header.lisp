
(PROGN
 (ECO-TEMPLATE:DEFTEMPLATE ECO-TEMPLATE::HEADER (ECO-TEMPLATE::TITLE) NIL
                           (WRITE-STRING "

<link href=\"https://cdn.jsdelivr.net/npm/tailwindcss/dist/preflight.min.css\" rel=\"stylesheet\">

<!-- Any of your own CSS would go here -->

<link href=\"https://cdn.jsdelivr.net/npm/tailwindcss/dist/utilities.min.css\" rel=\"stylesheet\">
<title>"
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE::TITLE) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "</title>

"
                                         ECO-TEMPLATE::ECO-STREAM))) 