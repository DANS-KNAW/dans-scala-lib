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
import org.scalatest.OneInstancePerTest
import org.scalatest.concurrent.{ Eventually, IntegrationPatience }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class ActiveTaskQueueSpec extends AnyFlatSpec with Matchers with OneInstancePerTest with Eventually with IntegrationPatience {

  "start" should "process previously queued tasks" in {
    val taskQueue = new ActiveTaskQueue[File]()
    val triggeredTasks = List(
      FileTriggerTask(),
      FileTriggerTask(),
      FileTriggerTask(),
    )

    triggeredTasks.foreach(taskQueue.add)
    taskQueue.start()

    eventually {
      triggeredTasks(0).triggered shouldBe true
      triggeredTasks(1).triggered shouldBe true
      triggeredTasks(2).triggered shouldBe true
    }

    taskQueue.stop()
  }

  it should "process other tasks if one task fails" in {
    val taskQueue = new ActiveTaskQueue[File]()
    val triggeredTasks = List(
      FileTriggerTask(),
      FileTriggerTask(shouldFail = true),
      FileTriggerTask(),
    )

    triggeredTasks.foreach(taskQueue.add)
    taskQueue.start()

    eventually {
      triggeredTasks(0).triggered shouldBe true
      triggeredTasks(1).triggered shouldBe true
      triggeredTasks(2).triggered shouldBe true
    }

    taskQueue.stop()
  }

  it should "process incoming tasks" in {
    val taskQueue = new ActiveTaskQueue[File]()
    val triggeredTasks = List(
      FileTriggerTask(),
      FileTriggerTask(),
      FileTriggerTask(),
    )

    // starting the taskQueue before adding tasks
    taskQueue.start()

    triggeredTasks.foreach(taskQueue.add)

    eventually {
      triggeredTasks(0).triggered shouldBe true
      triggeredTasks(1).triggered shouldBe true
      triggeredTasks(2).triggered shouldBe true
    }

    taskQueue.stop()
  }

  it should "not fail on an empty task queue" in {
    val taskQueue = new ActiveTaskQueue[File]()
    taskQueue.start()
    taskQueue.stop()
  }

  "stop" should "cancel all pending tasks" in {
    val taskQueue = new ActiveTaskQueue[File]()

    // this task will stop the ActiveTaskQueue when it gets processed
    // therefore all tasks following it should not be triggered
    val stopTask = new FileTriggerTask {
      override def run(): Try[Unit] = {
        taskQueue.stop()
        super.run()
      }
    }

    val triggeredTasks = List(
      FileTriggerTask(),
      stopTask,
      FileTriggerTask(),
      FileTriggerTask(),
    )

    triggeredTasks.foreach(taskQueue.add)
    taskQueue.start()

    eventually {
      triggeredTasks(0).triggered shouldBe true
      triggeredTasks(1).triggered shouldBe true
    }

    Thread.sleep(1000)

    triggeredTasks(2).triggered shouldBe false
    triggeredTasks(3).triggered shouldBe false
  }
}
