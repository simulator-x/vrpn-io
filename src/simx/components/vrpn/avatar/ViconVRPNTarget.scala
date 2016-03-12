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

package simx.components.vrpn.avatar

import simx.core.entity.description.SValSet
import simx.core.helper.chirality
import simx.core.ontology.{EntityDescription, GroundedSymbol, Symbols, types}

/**
 *
 * Created by dennis on 12/6/15.
 */
object ViconVRPNTarget {
  def head(url : String, updateRate : Long, csName : Option[GroundedSymbol]) =
    ViconBodyParts.getBodyPart(BodyPart.HEAD).map(_.toDesc(url, updateRate, csName, Some(Symbols.vicon)))

  def spine(url : String, updateRate : Long, csName : Option[GroundedSymbol]) =
    ViconBodyParts.getBodyPart(BodyPart.SPINE).map(_.toDesc(url, updateRate, csName, Some(Symbols.vicon)))

  def leftHand(url : String, updateRate : Long, csName : Option[GroundedSymbol]) =
    ViconBodyParts.getBodyPart(BodyPart.HAND, chirality.Left).map(_.toDesc(url, updateRate, csName, Some(Symbols.vicon)))

  def rightHand(url : String, updateRate : Long, csName : Option[GroundedSymbol]) =
    ViconBodyParts.getBodyPart(BodyPart.HAND, chirality.Right).map(_.toDesc(url, updateRate, csName, Some(Symbols.vicon)))

  def user(headName : String, spineName : String, lhName : String, rhName : String, ip : String, updateRate : Long = 16L, csName : Option[GroundedSymbol] = None) =
    new EntityDescription(
      name    = 'User,
      aspects = List(
        head(headName + "@" + ip, updateRate, csName ),
        spine(spineName + "@" + ip, updateRate, csName),
        leftHand(lhName + "@" + ip, updateRate, csName),
        rightHand(rhName + "@" + ip, updateRate, csName)
      ).map(_.get),
      additionalProperties = SValSet(types.EntityType.asConst(Symbols.user))
  )
}
