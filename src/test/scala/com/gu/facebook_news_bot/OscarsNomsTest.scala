package com.gu.facebook_news_bot

import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.gu.facebook_news_bot.models.MessageToFacebook
import com.gu.facebook_news_bot.util.JsonHelpers
import com.gu.facebook_news_bot.util.JsonHelpers._
import de.heikoseeberger.akkahttpcirce.CirceSupport
import io.circe.generic.auto._
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FunSpec, Matchers}

class OscarsNomsTest extends FunSpec with Matchers with ScalatestRouteTest with MockitoSugar with CirceSupport {
  val TestName = "oscars_noms_test"
  LocalDynamoDB.createUsersTable(TestName)
  LocalDynamoDB.createUserNomsTable(s"$TestName-oscarNoms")

  private def routeTest(inputFile: String, outputFile: String) = {
    val service = new TestService(TestName)
    val request = service.getRequest(loadFile(inputFile))

    request ~> service.routes ~> check {
      status should equal(OK)

      val expectedMessage = JsonHelpers.decodeFromFile[MessageToFacebook](outputFile)
      verify(service.facebook, timeout(5000)).send(List(expectedMessage))
    }
  }

  it("should ask a new user if they want to submit their Oscar winners predictions") {
    routeTest(
      "src/test/resources/facebookRequests/oscarsNoms/newUser.json",
      "src/test/resources/facebookResponses/oscarsNoms/newUser.json"
    )
  }

  it("should respond positively if a user wants to play the Oscars game") {
    routeTest(
      "src/test/resources/facebookRequests/oscarsNoms/enterNoms.json",
      "src/test/resources/facebookResponses/oscarsNoms/enterNoms.json"
    )
  }

  it("should accept Moonlight and ask user to confirm selection.") {
    routeTest(
      "src/test/resources/facebookRequests/oscarsNoms/enterBestFilm.json",
      "src/test/resources/facebookResponses/oscarsNoms/enterBestFilm.json"
    )
  }

}
