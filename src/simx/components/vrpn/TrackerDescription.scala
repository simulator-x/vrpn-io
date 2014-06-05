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

/*
  * Created by IntelliJ IDEA.
 * User: martin
 * Date: 4/7/11
 * Time: 12:57 PM
 */
package simx.components.vrpn

import xml.Node
import java.io.File
import devices.TrackingTarget
import simx.core.component.ComponentDoesNotExistException
import simx.core.entity.Entity
import simx.core.ontology.{types => gt}
import simx.core.svaractor.{SVarActor, SVar}
import simx.core.entity.component.EntityCreationHandling
import simx.core.ontology.EntityDescription
import simplex3d.math.floatx.{Vec3f, Mat4x3f, ConstMat4f, functions}

/**
 *
 * Manages the data of the tracker described in descFile.
 * @see   ./SIRIS_ROOT/config/tracker/trackerDescription.xsd for more information on the description file.
 */
class TrackerDescription(descFile: File)(implicit creationContext : EntityCreationHandling) {

  /**
   * Creates entities for all targets.
   */
  def init( onFinish : (()=>Unit) ) {
    try {
      if (!entitiesCreated) {
        entitiesCreated = true
        for ((name, id) <- targets)
          new EntityDescription(
            TrackingTarget(url = url, id = Symbol(id))
          ).realize{ e => {
            targetEntities += name -> e
            onFinish()
          } }
      }
    }
    catch {
      case e: ComponentDoesNotExistException =>
        println("TrackerDescription(" + descFile.getName + "): VRPN Connector has to be created before a call to init.")
    }
  }

  /**
   *  Returns the entity corresponding to the targetName specified in the description file.
   *  A call to init should already have taken place.
   */
  def getEntityFor(targetName: String) = targetEntities.get(targetName)

  /**
   *        Propagates the position and orientation of the target from to the svar to.
   *
   *     Thereby the data is transformed from the tracking coordinate system to the display
   *              coordinate system. This is achieved using the observe mechanism. The actual handlers
   *              are registered at the calling SVarActor.
   *
   * @param from  The name of the target from which position and orientation has to be propagated.
   *              This string referes to the names given in the description file.
   *
   * @param to    The svar to write the data into.
   */
  def propagateDisplayCoordinates(from: String, to: SVar[gt.Transformation.dataType])
                                 (implicit actorContext : SVarActor){
    targetEntities.get(from).collect{case targetEntity: Entity =>
      val targetSvar = targetEntity.get(VRPN.oriAndPos).head
      addTransformationProperagtion(targetSvar, to, trackerToDisplay)
    }
  }

  private def addTransformationProperagtion(source: SVar[gt.Transformation.dataType],
                                            sink: SVar[gt.Transformation.dataType],
                                            sourceToSinkTransform: ConstMat4f)(implicit self : SVarActor) {
    source.observe((srcTrans) => {sink.set(removeScale( functions.inverse(sourceToSinkTransform) * srcTrans) ) })
  }

  private def removeScale( mat : ConstMat4f ) : ConstMat4f = {
    val u = functions.normalize( Vec3f( mat.m00, mat.m10, mat.m20 ) )
    val v = functions.normalize( Vec3f( mat.m01, mat.m11, mat.m21 ) )
    val w = functions.normalize( Vec3f( mat.m02, mat.m12, mat.m22 ) )
    val m = ConstMat4f( u.x, u.y, u.z, 0.0f, v.x, v.y, v.z, 0.0f, w.x, w.y, w.z, 0.0f, mat.m30, mat.m31, mat.m32, 1.0f  )
    m
  }

  private val doc = simx.core.helper.SchemaAwareXML.loadFile(descFile)

  //Currently unchecked since vrpn is the only choice
  //private val connection = (doc \ "connection").text

  private val url = (doc \ "url").text

  private val trackerToDisplay: ConstMat4f = readTransformFromXML((doc \ "trackerCoordinateSystemToDisplayCoordinateSystemTransformation").head)

  //Name -> entity
  private val targetEntities = collection.mutable.Map[String, Entity]()
  //Name -> Id
  private val targets = collection.mutable.Map[String, String]()

  (doc \\ "target").foreach((n: Node) => {
    val name = (n \ "name").text
    val id = (n \ "id").text
    targets += (name -> id)
  })

  private var entitiesCreated = false

  private def readTransformFromXML(n: Node): ConstMat4f = {
    val rotateX = (n \ "rotateX").text.toFloat
    val rotateY = (n \ "rotateY").text.toFloat
    val rotateZ = (n \ "rotateZ").text.toFloat

    val scale = (n \ "scale").text.toFloat

    val translateX = (n \ "translateX").text.toFloat
    val translateY = (n \ "translateY").text.toFloat
    val translateZ = (n \ "translateZ").text.toFloat

    ConstMat4f(
      Mat4x3f.rotateX(functions.radians(rotateX)).rotateY(functions.radians(rotateY)).rotateZ(functions.radians(rotateZ)).
        scale(scale).translate(Vec3f(translateX, translateY, translateZ))
    )
  }
}