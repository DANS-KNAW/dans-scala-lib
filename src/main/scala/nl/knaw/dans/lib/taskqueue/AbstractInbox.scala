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

import java.nio.file.Path
import java.util
import java.util.{ Collections, Comparator }

import collection.JavaConverters._
import better.files.{ File, FileMonitor }
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

abstract class AbstractInbox(dir: File, comparator: Comparator[Path]) extends DebugEnhancedLogging {
  private val files = dir.list(_ => true, maxDepth = 1).toList

  /**
   * Directly enqueue the deposit directories currently present as deposits on the queue
   *
   * @param q the TaskQueue to put the DepositIngestTasks on
   */
  def enqueue(q: TaskQueue): Unit = {
    val paths = files.map(_.path).asJava
    paths.sort(comparator)
    for(p <- paths.asScala) {
      debug(s"Adding $p")
      createTask(p).foreach(q.add)
    }
  }

  /**
   * Creates and returns a FileMonitor that enqueues new deposits as they appear in
   * the inbox directory. Note that the caller is responsible for starting the FileMonitor
   * in the ExecutionContext of its choice.
   *
   * @param q the TaskQueue to put the DepositIngestTasks on
   * @return the FileMonitor
   */
  def createFileMonitor(q: TaskQueue): FileMonitor = {
    new FileMonitor(dir, maxDepth = 1) {
      override def onCreate(f: File, count: Int): Unit = {
        trace(f, count)
        createTask(f).foreach(q.add)
      }
    }
  }

  protected def createTask(f: File): Option[Task]
}
