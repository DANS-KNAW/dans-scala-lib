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
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.{ Eventually, IntegrationPatience }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer
import scala.util.Success

class InboxSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach with Eventually with IntegrationPatience with MockFactory {

  val testDir: File = File.currentWorkingDirectory / "target" / "test" / getClass.getSimpleName
  val inboxDir: File = testDir / "inbox"

  // Inbox implementation for testing purposes which creates tasks for files with extension ".succeed" and failing tasks for files with extension ".fail"
  case class TestInbox() extends AbstractInbox[File](inboxDir) {
    val createdTasks: ListBuffer[FileTriggerTask] = ListBuffer[FileTriggerTask]()

    override def createTask(f: File): Option[Task[File]] = f.extension match {
      case Some(".succeed") | Some(".fail") => {
        val task = FileTriggerTask(target = f, shouldFail = f.extension.contains(".fail"))
        createdTasks += task
        Some(task)
      }

      case _ => None
    }
  }

  val fileNameAlphabeticalTaskSorter: TaskSorter[File] = (tasks: List[Task[File]]) => {
    tasks.sortBy(_.getTarget.name)
  }

  override def beforeEach(): Unit = {
    inboxDir.createDirectories()
    inboxDir.clear()
  }

  "start" should "process files already in inbox" in {
    val file1: File = (inboxDir / "file1.succeed").createFile()
    val file2: File = (inboxDir / "file2.succeed").createFile()

    val testInbox = TestInbox()
    val inboxWatcher = new InboxWatcher[File](testInbox)

    inboxWatcher.start()

    eventually {
      testInbox.createdTasks.size shouldBe 2

      testInbox.createdTasks.map(_.getTarget) should contain allOf(file1, file2)

      testInbox.createdTasks(0).triggered shouldBe true
      testInbox.createdTasks(1).triggered shouldBe true
    }

    inboxWatcher.stop()
  }

  it should "process incoming files" in {
    val testInbox = TestInbox()
    val inboxWatcher = new InboxWatcher[File](testInbox)

    inboxWatcher.start()

    // create files after starting watcher
    val file1: File = (inboxDir / "file1.succeed").createFile()
    val file2: File = (inboxDir / "file2.succeed").createFile()

    eventually {
      testInbox.createdTasks.size shouldBe 2

      testInbox.createdTasks.map(_.getTarget) should contain allOf(file1, file2)

      testInbox.createdTasks(0).triggered shouldBe true
      testInbox.createdTasks(1).triggered shouldBe true
    }

    inboxWatcher.stop()
  }

  it should "process only the actionable files" in {
    val file1: File = (inboxDir / "file1.succeed").createFile()
    val file2: File = (inboxDir / "file2.noaction").createFile()
    val file3: File = (inboxDir / "file3.succeed").createFile()

    val testInbox = TestInbox()
    val inboxWatcher = new InboxWatcher[File](testInbox)

    inboxWatcher.start()

    eventually {
      testInbox.createdTasks.size shouldBe 2

      testInbox.createdTasks.map(_.getTarget) should contain allOf(file1, file3)

      testInbox.createdTasks(0).triggered shouldBe true
      testInbox.createdTasks(1).triggered shouldBe true
    }

    inboxWatcher.stop()
  }

  it should "use a defined TaskSorter to process a single task" in {
    val file1: File = (inboxDir / "file1.succeed").createFile()

    val testInbox = TestInbox()
    val inboxWatcher = new InboxWatcher[File](testInbox)

    inboxWatcher.start(Some(fileNameAlphabeticalTaskSorter))

    eventually {
      testInbox.createdTasks.size shouldBe 1

      val task = testInbox.createdTasks.head

      task.getTarget shouldBe file1

      task.triggered shouldBe true
    }

    inboxWatcher.stop()
  }

  it should "use a defined TaskSorter to process tasks in the correct order" in {
    case class SortingTestInbox() extends AbstractInbox[File](inboxDir) {
      val createdTasks: ListBuffer[FileTriggerTask] = ListBuffer[FileTriggerTask]()

      override def createTask(f: File): Option[Task[File]] = {
        val task = stub[FileTriggerTask]
        task.run _ when () returns Success(())
        task.getTarget _ when () returns f

        createdTasks += task
        Some(task)
      }
    }

    val file1: File = (inboxDir / "d-file1.succeed").createFile()
    val file2: File = (inboxDir / "c-file2.succeed").createFile()
    val file3: File = (inboxDir / "b-file3.succeed").createFile()
    val file4: File = (inboxDir / "a-file4.succeed").createFile()

    val sortingTestInbox = SortingTestInbox()
    val inboxWatcher = new InboxWatcher[File](sortingTestInbox)

    inboxWatcher.start(Some(fileNameAlphabeticalTaskSorter))

    eventually {
      sortingTestInbox.createdTasks.size shouldBe 4

      val List(task1, task2, task3, task4): List[Task[File]] = fileNameAlphabeticalTaskSorter.sort(sortingTestInbox.createdTasks.toList)

      inSequence {
        (task1.run _).verify()
        (task2.run _).verify()
        (task3.run _).verify()
        (task4.run _).verify()
      }
    }

    inboxWatcher.stop()
  }

  it should "process other tasks if a task fails" in {
    // the TestInbox defined above creates a failing task for .jpg extensions
    val file1: File = (inboxDir / "file1.succeed").createFile()
    val file2: File = (inboxDir / "file2.fail").createFile()
    val file3: File = (inboxDir / "file3.succeed").createFile()

    val testInbox = TestInbox()
    val inboxWatcher = new InboxWatcher[File](testInbox)

    inboxWatcher.start()

    eventually {
      testInbox.createdTasks.size shouldBe 3

      testInbox.createdTasks.map(_.getTarget) should contain allOf(file1, file2, file3)

      testInbox.createdTasks(0).triggered shouldBe true
      testInbox.createdTasks(1).triggered shouldBe true
      testInbox.createdTasks(2).triggered shouldBe true
    }

    inboxWatcher.stop()
  }
}
