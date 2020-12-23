/**
 * Copyright (C) 2016 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.lib.taskqueue

import better.files.File
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PassiveTaskQueueSpec extends AnyFlatSpec with Matchers {

  "process" should "cause previously queued tasks to be processed" in {
    val taskQueue = new PassiveTaskQueue[File]()
    val tasks = List(
      FileTriggerTask(),
      FileTriggerTask(),
      FileTriggerTask(),
    )

    tasks.foreach(taskQueue.add)
    taskQueue.process()

    tasks(0).triggered shouldBe true
    tasks(1).triggered shouldBe true
    tasks(2).triggered shouldBe true
  }

  it should "cause all other tasks to be processed if one task fails" in {
    val taskQueue = new PassiveTaskQueue[File]()
    val tasks = List(
      FileTriggerTask(),
      FileTriggerTask(shouldFail = true),
      FileTriggerTask(),
    )

    tasks.foreach(taskQueue.add)
    taskQueue.process()

    tasks(0).triggered shouldBe true
    tasks(1).triggered shouldBe true
    tasks(2).triggered shouldBe true
  }

  it should "not fail on an empty task queue" in {
    val taskQueue = new PassiveTaskQueue[File]()
    taskQueue.process()
  }
}
