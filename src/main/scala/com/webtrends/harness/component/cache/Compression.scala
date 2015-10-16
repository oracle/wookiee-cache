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

/**
 * Mixin trait that adds compression to default Cacheable behavior
 */
trait Compression[T] {
  this: Cacheable[T] =>

  @transient private lazy val deflater = new Deflater(level)
  @transient private lazy val inflater = new Inflater()
  @transient private lazy val iBuffer, oBuffer = new Array[Byte](2 ^ 16)

  protected def level:Int = Deflater.DEFAULT_COMPRESSION

  override protected def extract(obj: Array[Byte])(implicit m: Manifest[T]): Option[T] = {
    inflater.setInput(obj)
    Some(JsonParser.parse(new String(inflate(), StandardCharsets.UTF_8)).extract[T])
  }

  private def inflate(result: Array[Byte] = Array.empty[Byte]): Array[Byte] = {
    val len = inflater.inflate(iBuffer)

    if (inflater.finished) {
      inflater.reset()
      result ++ iBuffer.take(len)
    } else {
      inflate(result ++ iBuffer.take(len))
    }
  }

  override protected def getBytes: Array[Byte] = {
    deflater.setInput(compactRender(decompose(this)).getBytes(StandardCharsets.UTF_8))
    deflater.finish()
    deflate()
  }

  def deflate(result: Array[Byte] = Array.empty[Byte]): Array[Byte] = {
    val len = deflater.deflate(oBuffer)

    if (deflater.finished()) {
      deflater.reset()
      result ++ oBuffer.take(len)
    }
    else {
      deflate(result ++ oBuffer.take(len))
    }
  }
}
