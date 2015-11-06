/*
 * Copyright 2015 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the 
 * HCI Group at the University of Würzburg. The project is funded by the German 
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.vrpn.converter

import org.json4s.jackson.JsonMethods._
import simx.core.helper.{Silence, Recognized, Hypothesized, SpeechRecognitionResult}

/**
 * Created by martin 
 * on 03/09/15.
 */
object Json {

  implicit val formats = org.json4s.DefaultFormats
  
  private case class SpeechResult(`type`: String, text: String, confidence: Float, timestamp: Long) {
    def toSpeechRecognitionResult = {
      `type` match {
        case "Hypothesized" => Hypothesized(text, confidence, timestamp)
        case "Recognized" => Recognized(text, confidence, timestamp)
        case _ => Silence()  
      }
    }
  }
  
  def parseSpeechRecognitionResult(msg: String) : SpeechRecognitionResult = {
    parse(msg).extract[SpeechResult].toSpeechRecognitionResult
  }
}
