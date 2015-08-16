package controllers


import forms.ConfigurationSaveAsForm
import play.api.mvc.{Action, Controller}
import scala.concurrent.Future
import forms.ConfigurationSaveAsForm._


trait ComponentsSaveAsController extends Controller{

  def present = Action.async{
    Future.successful(Ok(views.html.components_saveas(configurationSaveAsForm)))
  }

  def submit() = Action.async { implicit request =>
    configurationSaveAsForm.bindFromRequest.fold(
      errors => Future.successful(Ok(views.html.components_saveas(errors))),
      value => Future.successful(Ok(views.html.components_saveas(configurationSaveAsForm)))
    )
  }

}

object ComponentsSaveAsController extends ComponentsSaveAsController