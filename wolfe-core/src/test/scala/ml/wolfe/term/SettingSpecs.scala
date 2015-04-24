package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class SettingSpecs extends WolfeSpec {

  "A setting object" should {
    "should store integers" in {
      val setting = new Setting(numDisc=1)
      setting.disc(0) = 5
      setting.disc(0) should be (5)
    }

    "should remember updates" in {
      val setting = new Setting(numCont=2)
      val changes = new SettingChangeRecorder(setting, false)
      setting.cont(0) = 4
      changes.cont.changes should be (Set(0))

    }
  }

}
