
  function highlightSteps(current){

  //console.debug("current="+current)

//    if(current >= 0) {
      var steps = $(".program").children().children();
      for(var i=1; i<current; i++){
        $("#"+steps[i].id).removeClass("to-run");
        $("#"+steps[i].id).removeClass("running");
        $("#"+steps[i].id).addClass("ran");
      }

      $("#step"+current).removeClass("to-run");
      $("#step"+current).addClass("running");
      $("#step"+current).removeClass("ran");

      for(var i=current+1; i<steps.length; i++){
         $("#"+steps[i].id).addClass("to-run");
         $("#"+steps[i].id).removeClass("running");
         $("#"+steps[i].id).removeClass("ran");
      }
 //   }
  }

  function highlightStartStopButtons(running){
    if(running==true){
       $("#startButton").addClass("btn-disable")
       $("#startButton").removeClass("btn-enable")
       $("#stopButton").addClass("btn-enable")
       $("#stopButton").removeClass("btn-disable")
    }
    else{
       $("#startButton").addClass("btn-enable")
       $("#startButton").removeClass("btn-disable")
       $("#stopButton").addClass("btn-disable")
       $("#stopButton").removeClass("btn-enable")
    }
  }

  function updateDevices(ss){
     for(i=0; i<ss.componentStatuses.length; i++){
        var compId = ss.componentStatuses[i].componentId
        var compType = ss.componentStatuses[i].componentType
        var compValue = ss.componentStatuses[i].componentValue
        var compUnit = ss.componentStatuses[i].componentUnit
        //console.debug("comp: "+ compId + " - "  + compType +" - " + compValue);

        switch(compType){
          case 0: {$("#device-val"+compId).text(formatTimer(compValue)); break;} //TIMER
          case 1: {$("#device-val"+compId).text(compValue+" "+compUnit); break;} //ANALOGUE_IN / Thermometer
          case 2: {$("#device-val"+compId).text(compValue+" "+compUnit); break;} //ANALOGUE OUT / Heater
          case 4: {                                                              //DIGITAL_OUT
            if(compValue == "true") $("#device-true"+compId).prop("checked", true)
            else $("#device-false"+compId).prop("checked", true);
            break;
          }
        }
     }
  }

  function formatTimer(totalSeconds){
    hours = Math.floor(totalSeconds / 3600);
    totalSeconds %= 3600;
    mins= Math.floor(totalSeconds / 60);
    secs = totalSeconds % 60;
    return (""+padZero(hours)+":"+padZero(mins)+":"+padZero(secs))
  }

  function padZero(num){
    if(num<10) return "0"+num
    else return num
  }

  function updateMonitors(ss){
    for(i=0; i<ss.monitorStatuses.length; i++){
       var monitorId = ss.monitorStatuses[i].componentId
       var monitorEnabled = ss.monitorStatuses[i].enabled
       var monitorTemp = ss.monitorStatuses[i].temperature
       var sensorTemp = ss.monitorStatuses[i].sensorStatus.componentValue
       var increaserPower = ss.monitorStatuses[i].increaserStatus.componentValue
       var sensorUnit = ss.monitorStatuses[i].sensorStatus.componentUnit
       var increaserUnit = ss.monitorStatuses[i].increaserStatus.componentUnit

       if(monitorEnabled) $("#monitor-true"+monitorId).prop("checked", true)
            else $("#monitor-false"+monitorId).prop("checked", true);

       $("#monitor-temperature"+monitorId).text(monitorTemp+" "+sensorUnit);
       $("#monitor-sensor"+monitorId).text(sensorTemp+" "+ sensorUnit);
       $("#monitor-increaser"+monitorId).text(increaserPower+" "+increaserUnit);
    }
  }

  function startSequencer(){jsRoutes.controllers.StatusController.startSequencer(null).ajax();};
  function stopSequencer(){jsRoutes.controllers.StatusController.stopSequencer(null).ajax();};

$(function() {
    console.debug("Initialising...")
    sequencerStatusCall();
});

function initEvents(){

}

function makeCall(componentId, state){
    //console.debug("componentId="+componentId+ "   state="+state)
    jsRoutes.controllers.StatusController.setComponentState(componentId, state).ajax();
}



var sequencerStatusCall = function() {
    var ajaxCallBack = {
        success : onSuccess,
        error : onError
    }
    jsRoutes.controllers.StatusController.sequencerStatus().ajax(ajaxCallBack);

    setTimeout(sequencerStatusCall, 1000);
};

var  onSuccess = function(strSequenceStatus) {
    var ss = JSON.parse(strSequenceStatus);
    highlightStartStopButtons(ss.running)
    highlightSteps(ss.currentStep)
    updateDevices(ss)
    updateMonitors(ss)
}

var onError = function(error) {
    console.debug("Error in ajax Call");
    console.debug(error);
}
