# beer
Thoughts about automation...

Main Components
~~~~~~~~~~~~~~~~
StatusController: Controls Status page, services Ajax calls
ComponentManager: A range of methods to locate/identify components, and set/get their status
MonitorActor:     Models monitor (e.g. a thermostat)
Sequencer:        holds sequencer state and provides a set of steps that can be performed
SequencerActor:   steps through a list of steps, invoking the sequencer.


/sequencer -> controllers.StatusController.present
    controllers.StatusController.sequenceToReadableSequence
        formatTarget
        formatPeriod
    controllers.cCToReadableCc



#### AJAX CALLS ####
GET     /sequencerStatus        ->  controllers.StatusController.sequencerStatus
    controllers.StatusController.compileComponentsStatuses
    controllers.StatusController.compileMonitorStatuses
    model.SequencerStatus.apply

GET     /startSequencer         ->  controllers.StatusController.startSequencer
    sequencer.Sequencer.runSequence
        sequencer.SequencerActor
        sequencer.getStepFromList
        sequencer.performStep

GET     /stopSequencer          ->  controllers.StatusController.stopSequencer
    sequencer.abortSequence
    sequencer.SequencerActor.receive("stop")

GET     /setComponentState      ->  controllers.StatusController.setComponentState(component:String, value:String)
    controllers.componentManager.componentFromId
        controllers.StatusController.setMonitorState
        controllers.StatusController.setDeviceState
        controllers.ComponentManager.on
        controllers.ComponentManager.off
        controllers.ComponentManager.setPower
        controllers.ComponentManager.getPower

GET     /javascriptRoutes       ->  controllers.StatusController.javascriptRoutes




