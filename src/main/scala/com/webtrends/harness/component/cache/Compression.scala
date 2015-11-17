/*
 * Copyright 2015 Webtrends (http://www.webtrends.com)
 *
 * See the LICENCE.txt file distributed with this work for additional
 * information regarding copyright ownership.
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
package com.webtrends.harness.component.cache

import java.nio.charset.StandardCharsets
import java.util.zip.{Deflater, Inflater}

import net.liftweb.json.Extraction._
import net.liftweb.json._

import scala.collection.mutable.ArrayBuffer

/**
 * Mixin trait that adds compression to default Cacheable behavior
 */
trait Compression[T] {
  this: Cacheable[T] =>

  @transient private lazy val deflater = new Deflater(level)
  @transient private lazy val inflater = new Inflater()

  protected def level:Int = Deflater.DEFAULT_COMPRESSION

  override protected def extract(obj: Array[Byte])(implicit m: Manifest[T]): Option[T] = {
    implicit val buffer = new Array[Byte](obj.length)
    inflater.reset()
    inflater.setInput(obj)
    Some(JsonParser.parse(new String(inflate(), StandardCharsets.UTF_8)).extract[T])
  }

  private def inflate(result: ArrayBuffer[Byte] = ArrayBuffer.empty[Byte])(implicit buffer: Array[Byte]): Array[Byte] = {
    val len = inflater.inflate(buffer)

    if (inflater.finished) {
      (result ++= buffer.take(len)).toArray
    } else {
      inflate(result ++= buffer.take(len))
    }
  }

  override protected def getBytes: Array[Byte] = {
    implicit val buffer = new Array[Byte](65536)
    deflater.reset()
    deflater.setInput(compactRender(decompose(this)).getBytes(StandardCharsets.UTF_8))
    deflater.finish()
    deflate()
  }

  private def deflate(result: ArrayBuffer[Byte] = ArrayBuffer.empty[Byte])(implicit buffer:Array[Byte]): Array[Byte] = {
    val len = deflater.deflate(buffer)

    if (deflater.finished()) {
      (result ++= buffer.take(len)).toArray
    }
    else {
      deflate(result ++= buffer.take(len))
    }
  }

  def compressionRatio: Option[Double] =
    if (deflater.finished() && deflater.getBytesWritten > 0)
      Some(deflater.getBytesRead.toDouble / deflater.getBytesWritten)
    else None

  def compressedSize: Option[Long] =
    if (deflater.finished()) Some(deflater.getBytesWritten)
    else None
}
