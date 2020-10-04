package com.kamestery.security

import io.quarkus.test.junit.QuarkusTest
import io.restassured.RestAssured.given
import org.hamcrest.CoreMatchers.`is`
import org.junit.jupiter.api.Test

@QuarkusTest
class AuthTest {

    @Test
    fun testHelloEndpoint() {
        given()
          .`when`().get("/")
          .then()
             .statusCode(200)
             .body(`is`("hello"))
    }

}