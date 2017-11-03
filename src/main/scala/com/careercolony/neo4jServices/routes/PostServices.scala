package com.careercolony.neo4jServices.routes

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import akka.stream.ActorMaterializer
import com.careercolony.neo4jServices.factories.{DatabaseAccess, CreatePost, GetPost}
import spray.json.DefaultJsonProtocol
import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings

import akka.http.scaladsl.model.HttpMethods._
import scala.collection.immutable

import scala.collection.mutable.MutableList;
import spray.json._;



object UserJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val CreatePostFormats = jsonFormat7(CreatePost)
  implicit val GetPostFormats = jsonFormat8(GetPost)
  
}

trait PostService extends DatabaseAccess {

  import UserJsonSupport._

  implicit val system: ActorSystem
  implicit val materializer: ActorMaterializer
  val logger = Logging(system, getClass)

  implicit def myExceptionHandler = {
    ExceptionHandler {
      case e: ArithmeticException =>
        extractUri { uri =>
          complete(HttpResponse(StatusCodes.InternalServerError,
            entity = s"Data is not persisted and something went wrong"))
        }
    }
  }
  
  val settings = CorsSettings.defaultSettings.copy(allowedMethods = immutable.Seq(GET, PUT, POST, HEAD, OPTIONS))
  val postRoutes: Route = cors(settings){
    post {
      path("new-post") {
        entity(as[CreatePost]) { entity =>
          complete {
            try {
              val isPersisted: MutableList[GetPost] = insertPost(entity)
              isPersisted match {
                case _: MutableList[_] =>
                  var response: StringBuilder = new StringBuilder("[")
                  isPersisted.foreach(
                      x => response.append(x.toJson).append(",")
                    )
                  response.deleteCharAt(response.length - 1)
                  response.append("]");  
                  HttpResponse(StatusCodes.OK, entity = response.toString()) //data.toString())
                  case _ => HttpResponse(StatusCodes.BadRequest,
                   entity = s"User already exist")
              }
            } catch {
              case ex: Throwable =>
                logger.error(ex, ex.getMessage)
                HttpResponse(StatusCodes.InternalServerError,
                  entity = "Error while persisting data, please try again")
            }
          }
        }
      }
    }  ~ path("get" / "post" / Segment) { (postID: String) =>
      get {
        complete {
          try {
            val idAsRDD: MutableList[GetPost] = retrievePost(postID.toInt)
            idAsRDD match {
              //case GetCompany(_,_,_,_,_,_) => //Some(data) =>
              case _: MutableList[_] =>
                var response: StringBuilder = new StringBuilder("[")
                idAsRDD.foreach(
                    x => response.append(x.toJson).append(",")
                  )
                response.deleteCharAt(response.length - 1)
                response.append("]");  
                HttpResponse(StatusCodes.OK, entity = response.toString()) //data.toString())
              //case None => HttpResponse(StatusCodes.InternalServerError,
              //  entity = s"No user found")
            }
          } catch {
            case ex: Throwable =>
              logger.error(ex, ex.getMessage)
              HttpResponse(StatusCodes.InternalServerError,
                entity = s"Data is not fetched and something went wrong")
          }
        }
      }
    } ~ path("update-post") {
      put {
         entity(as[GetPost]) { entity =>
          complete {
            try {
              val isPersisted = updatepost(entity)
              isPersisted match {
                case true => HttpResponse(StatusCodes.Created,
                entity = s"Data is successfully persisted")
              case false => HttpResponse(StatusCodes.InternalServerError,
                entity = s"Error found for post")
              }
            } catch {
              case ex: Throwable =>
                logger.error(ex, ex.getMessage)
                HttpResponse(StatusCodes.InternalServerError,
                  entity = "Error while persisting data, please try again")
            }
          }
        }
      }
    }  ~ path("delete" / "postID" / Segment) { (postID: String) =>
      get {
        complete {
          try {
            val idAsRDD = deleteRecord(postID)
            idAsRDD match {
              case 1 => HttpResponse(StatusCodes.OK, entity = "Data is successfully deleted")
              case 0 => HttpResponse(StatusCodes.InternalServerError,
                entity = s"Data is not deleted and something went wrong")
            }
          } catch {
            case ex: Throwable =>
              logger.error(ex, ex.getMessage)
              HttpResponse(StatusCodes.InternalServerError,
                entity = s"Error found for post")
          }
        }
      }
    }
  }
}
