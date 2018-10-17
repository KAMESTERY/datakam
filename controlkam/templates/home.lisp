
(PROGN
 (ECO-TEMPLATE:DEFTEMPLATE ECO-TEMPLATE::HOME (ECO-TEMPLATE::TITLE) NIL
                           (WRITE-STRING "

<!-- Main jumbotron for a primary marketing message or call to action -->
<div class=\"jumbotron\">
  <div class=\"container\">
    <h1 class=\"display-3\">"
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE::TITLE) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "</h1>
    <p>ControlKam Home</p>
    <p><a class=\"btn btn-primary btn-lg\" href=\"#\" role=\"button\">Learn more &raquo;</a></p>
  </div>
</div>

<div class=\"container\">
  <div class=\"card\" style=\"width: 18rem;\">
    <img class=\"card-img-top\" src=\"https://farm2.staticflickr.com/1966/44984758922_52e761d2c5_z.jpg\" alt=\"Multi Genius\">
    <div class=\"card-body\">
      <h5 class=\"card-title\">"
                                         ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING (ECO-TEMPLATE:E ECO-TEMPLATE::TITLE) ECO-TEMPLATE::ECO-STREAM)
                           (WRITE-STRING "</h5>
      <p class=\"card-text\">Imhotep was an Egyptian scholar and scholar who delved into medicine and astronomy, before becoming the first architect. Learn more at <a href=\"https://flic.kr/p/2bx9DzL\">here</a></p>
      <a href=\"/hello\" class=\"btn btn-primary\">Say Hello!</a>
    </div>
  </div>
</div>

"
                                         ECO-TEMPLATE::ECO-STREAM))) 