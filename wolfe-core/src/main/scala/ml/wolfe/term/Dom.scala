package ml.wolfe.term

import ml.wolfe.fg20.{State, Setting, Clique}
//
///**
// * @author riedel
// */
//trait Dom {
//  type Value
//  def vars: Clique
//  def settingToState(setting: Setting): State = vars.toState(setting)
//  def settingToValue(setting: Setting): Value = stateToValue(settingToState(setting))
//  def valueToSetting(value: Value, result: Setting) = stateToSetting(valueToState(value),result)
//  def valueToState(value: Value): State
//  def stateToValue(state:State):Value
//  def stateToSetting(state: State, result: Setting) = result := vars.toPartialSetting(state)
//  def createSetting() = vars.createSetting()
//  def createPartialSetting(value: Value) = vars.createPartialSetting(valueToState(value))
//  def values: Iterator[Value] = ???
//}


