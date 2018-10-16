
(PROGN
 (ECO-TEMPLATE:DEFTEMPLATE ECO-TEMPLATE::HOME (ECO-TEMPLATE::TITLE) NIL
                           (WRITE-STRING "
<div class=\"card\" style=\"width: 18rem;\">
  <img class=\"card-img-top\" src=\"...\" alt=\"Card image cap\">
  <div class=\"card-body\">
    <h5 class=\"card-title\">"
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE::TITLE) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "</h5>
    <p class=\"card-text\">Some quick example text to build on the card title and make up the bulk of the card's content.</p>
    <a href=\"/hello\" class=\"btn btn-primary\">Go somewhere</a>
  </div>
</div>
"
                                         ECO-TEMPLATE::ECO-STREAM))) 