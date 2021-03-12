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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import java.util.concurrent.LinkedBlockingDeque
import scala.util.Try

abstract class AbstractTaskQueue[T] extends TaskQueue[T] with DebugEnhancedLogging {
  protected val tasks = new LinkedBlockingDeque[Option[Task[T]]]

  /**
   * Adds a new task to the queue.
   *
   * @param t the task to add
   */
  def add(t: Task[T]): Try[Unit] = Try {
    trace(t)
    tasks.put(Some(t))
    debug("Task added to queue")
  }

  protected def runTask(t: Option[Task[T]]): Boolean = {
    t.map(_.run().recover {
      case e: Throwable => logger.warn(s"Task $t failed", e);
    }).isDefined
  }
}
