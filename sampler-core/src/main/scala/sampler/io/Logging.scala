/*
 * Copyright (c) 2012-15 Crown Copyright 
 *                       Animal and Plant Health Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sampler.io

import org.slf4j.LoggerFactory

trait Logging {
  private[this] val logger = LoggerFactory.getLogger(getClass.getName)

  def trace(msg: => String) { if (logger.isTraceEnabled) logger.trace(msg) }
  def debug(msg: => String) { if (logger.isDebugEnabled) logger.debug(msg) }
  def info(msg: => String) { if (logger.isInfoEnabled) logger.info(msg) }
  def warn(msg: => String) { if (logger.isWarnEnabled) logger.warn(msg) }
  def error(msg: => String) { if (logger.isErrorEnabled) logger.error(msg) }

  def logLocation() = {
    val ste: StackTraceElement = Thread.currentThread.getStackTrace()(3)
    logger.debug(s"Location: ${ste.getClassName}:${ste.getMethodName}:${ste.getLineNumber}")
  }
}