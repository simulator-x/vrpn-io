/*
 * Copyright 2012 The SIRIS Project
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

package simx.components.vrpn.devices

import simx.core.ontology.types._
import simx.core.ontology.EntityDescription

/* author: dwiebusch
 * date: 25.09.2010
 */

case class WiiMote(wiiUrl : java.lang.String) extends EntityDescription(
  VRPNButton(Key_Home,     wiiUrl, Symbol("0")),
  VRPNButton(Key_1,     wiiUrl, Symbol("1")),
  VRPNButton(Key_2,     wiiUrl, Symbol("2")),
  VRPNButton(Key_a,     wiiUrl, Symbol("3")),
  VRPNButton(Key_b,     wiiUrl, Symbol("4")),
  VRPNButton(Key_Minus,     wiiUrl, Symbol("5")),
  VRPNButton(Key_Plus,     wiiUrl, Symbol("6")),
  VRPNButton(Key_Left,  wiiUrl, Symbol("7")),
  VRPNButton(Key_Right, wiiUrl, Symbol("8")),
  VRPNButton(Key_Down,  wiiUrl, Symbol("9")),
  VRPNButton(Key_Up,    wiiUrl, Symbol("10")) )
{}

case class WiiMoteWithTracker(wiiUrl : java.lang.String, trackerUrl : java.lang.String, targetId : Symbol) {
//  def realize(handler : (Entity) => Any) {
    val desc = new EntityDescription(
      TrackingTarget(trackerUrl, targetId),
  VRPNButton(Key_Home,     wiiUrl, Symbol("0")),
  VRPNButton(Key_1,     wiiUrl, Symbol("1")),
  VRPNButton(Key_2,     wiiUrl, Symbol("2")),
  VRPNButton(Key_a,     wiiUrl, Symbol("3")),
  VRPNButton(Key_b,     wiiUrl, Symbol("4")),
  VRPNButton(Key_Minus,     wiiUrl, Symbol("5")),
  VRPNButton(Key_Plus,     wiiUrl, Symbol("6")),
  VRPNButton(Key_Left,  wiiUrl, Symbol("7")),
  VRPNButton(Key_Right, wiiUrl, Symbol("8")),
  VRPNButton(Key_Down,  wiiUrl, Symbol("9")),
  VRPNButton(Key_Up,    wiiUrl, Symbol("10"))
    )//.realize(handler)
//  }
}