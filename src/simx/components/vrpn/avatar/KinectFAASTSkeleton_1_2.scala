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

import simx.core.entity.Entity
import simx.core.entity.component.EntityCreationHandling
import simx.core.entity.description.{EntityAspect, SValSet}
import simx.core.helper.chirality
import simx.core.ontology._

/**
 * Works with FAAST 1.1 & 1.2
 * Derived from simx.components.io.j4k.local.Skeleton.scala (Anke, Giebler-Schubert)
 * @author Martin Fischbach
 */
object KinectFAASTSkeleton_1_2 {

  val bodyParts = Set[BodyPart](
    BodyPart describedBy Symbols.head                                  accessedViaId 0,
    BodyPart describedBy Symbols.shoulder andChirality chirality.None  accessedViaId 1  hasSubParts shoulderSubParts,
    BodyPart describedBy Symbols.spine                                 accessedViaId 2,
    BodyPart describedBy Symbols.hip      andChirality chirality.None  accessedViaId 3  hasSubParts hipSubParts,
    BodyPart describedBy Symbols.elbow    andChirality chirality.Left  accessedViaId 5,
    BodyPart describedBy Symbols.elbow    andChirality chirality.Right accessedViaId 9,
    BodyPart describedBy Symbols.wrist    andChirality chirality.Left  accessedViaId 6,
    BodyPart describedBy Symbols.wrist    andChirality chirality.Right accessedViaId 10,
    BodyPart describedBy Symbols.hand     andChirality chirality.Left  accessedViaId 7,
    BodyPart describedBy Symbols.hand     andChirality chirality.Right accessedViaId 11,
    BodyPart describedBy Symbols.knee     andChirality chirality.Left  accessedViaId 13,
    BodyPart describedBy Symbols.knee     andChirality chirality.Right accessedViaId 17,
    BodyPart describedBy Symbols.ankle    andChirality chirality.Left  accessedViaId 14,
    BodyPart describedBy Symbols.ankle    andChirality chirality.Right accessedViaId 18,
    BodyPart describedBy Symbols.foot     andChirality chirality.Left  accessedViaId 15,
    BodyPart describedBy Symbols.foot     andChirality chirality.Right accessedViaId 19
  )

  private def shoulderSubParts = Set(
    BodyPart describedBy Symbols.shoulder andChirality chirality.Left  accessedViaId 4,
    BodyPart describedBy Symbols.shoulder andChirality chirality.Right accessedViaId 8
  )

  private def hipSubParts = Set(
    BodyPart describedBy Symbols.hip andChirality chirality.Left  accessedViaId 12,
    BodyPart describedBy Symbols.hip andChirality chirality.Right accessedViaId 16
  )

  def simpleUpperBodySymbols = Seq(Symbols.head, Symbols.spine, Symbols.hand)

  def simpleUpperBody = getSubset(simpleUpperBodySymbols:_*)

  def simpleUpperBodyUserDescription(vrpnServerUrl: String, csName : Option[GroundedSymbol] = None) =
    userDescription(vrpnServerUrl, csName, simpleUpperBodySymbols:_*)

  @deprecated("Use KinectFAASTSkeleton_1_2.simpleUpperBodyUserDescription instead", "06-07-2015")
  def simpleUserDescription(vrpnServerUrl: String) = simpleUpperBodyUserDescription(vrpnServerUrl)

  def upperBodySymbols =
    Seq(Symbols.head, Symbols.shoulder, Symbols.spine, Symbols.hip, Symbols.elbow, Symbols.wrist, Symbols.hand)

  def upperBody = getSubset(upperBodySymbols:_*)

  def upperBodyUserDescription(vrpnServerUrl: String, csName : Option[GroundedSymbol] = None) =
    userDescription(vrpnServerUrl, csName, upperBodySymbols:_*)

  def userDescription(vrpnServerUrl: String, csName : Option[GroundedSymbol], bodyPartDescriptions: GroundedSymbol*) = {
    val usedParts = if(bodyPartDescriptions.nonEmpty) getSubset(bodyPartDescriptions:_*) else bodyParts
    new EntityDescription(
      List[EntityAspect](usedParts.map(_.toDesc(vrpnServerUrl, csName = csName, deviceName = Some(Symbols.kinect))).toSeq:_*),
      'User,
      List[Symbol](),
      Set[simx.core.ontology.Annotation](),
      additionalProperties = SValSet(types.EntityType.asConst(Symbols.user))
    )
  }

  private def getSubset(descriptions: GroundedSymbol*): Set[BodyPart] =
    descriptions.map(types.EntityType.apply).map(d => bodyParts.filter(_.properties.contains(d))).flatten.toSet

  //  protected val mapping: Map[JointType, Symbol] = immutable.Map(
  //    Skeleton.HEAD-> Symbol("0"),
  //    Skeleton.SHOULDER_CENTER-> Symbol("1"),
  //    Skeleton.SPINE-> Symbol("2"),
  //    Skeleton.HIP_CENTER-> Symbol("3"),
  //    Skeleton.LEFT_SHOULDER-> Symbol("4"),
  //    Skeleton.LEFT_ELBOW-> Symbol("5"),
  //    Skeleton.LEFT_WRIST-> Symbol("6"),
  //    Skeleton.LEFT_HAND-> Symbol("7"),
  //    Skeleton.RIGHT_SHOULDER-> Symbol("8"),
  //    Skeleton.RIGHT_ELBOW-> Symbol("9"),
  //    Skeleton.RIGHT_WRIST-> Symbol("10"),
  //    Skeleton.RIGHT_HAND-> Symbol("11"),
  //    Skeleton.HIP_LEFT-> Symbol("12"),
  //    Skeleton.KNEE_LEFT-> Symbol("13"),
  //    Skeleton.ANKLE_LEFT-> Symbol("14"),
  //    Skeleton.FOOT_LEFT-> Symbol("15"),
  //    Skeleton.HIP_RIGHT-> Symbol("16"),
  //    Skeleton.KNEE_RIGHT-> Symbol("17"),
  //    Skeleton.ANKLE_RIGHT-> Symbol("18"),
  //    Skeleton.FOOT_RIGHT-> Symbol("19")
  //  )
}