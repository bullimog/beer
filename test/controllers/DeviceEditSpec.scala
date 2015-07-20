package controllers

import model.{Device, ComponentCollection}
import org.specs2.mutable.Specification
import play.api.mvc.Result
import play.api.test.Helpers._
import play.api.test.{FakeRequest, WithApplication, FakeApplication}

import scala.concurrent.{Await, Future}


class DeviceEditSpec extends Specification with DeviceEdit {

//  override def fetchDevice(deviceId: Int):Device = {
//    Device(1, "Pump", 2, 1, None, None, None, None)
//  }

  "Calling the present method" should {
    "present the page" in running(FakeApplication()) {

      //val result = deviceEdit.present(1)
      val result:Future[Result] = route(FakeRequest(GET, "/device-edit?deviceId=1")).get
 //     val result= DeviceEdit.present(fakeRequest.withFormUrlEncodedBody("username"->"test", "password"->"test"))

      status(result) mustEqual OK
      contentType(result) must beSome.which(_ == "text/html")
      contentAsString(result) must contain("Thermometer")
    }
  }

  "Calling the submit method" should {
    "update the device" in new WithApplication {

      val request = FakeRequest(POST, "/device-edit")
        .withFormUrlEncodedBody(
          "id" -> "1",
          "description" -> "My stupid description",
          "deviceType" -> "1",
          "port" -> "1",
          "units" -> "%"
        ).withSession("just" -> "ignore me")

      val Some(result:Future[Result]) = route(request)

//      result.map { edval =>
  //      println("edval="+edval)
        status(result) must equalTo(OK)
        contentType(result) must beSome.which(_ == "text/html")
        contentAsString(result) must contain("stupid")
 //     }
    }
  }
}
