package com.careercolony.neo4jServices.routes

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{ExceptionHandler, Directive1, Route}



import akka.stream.ActorMaterializer
import com.careercolony.neo4jServices.factories.{DatabaseAccess, Credentials, User, User2, User3, ResponseStatus, BioData, GetJobTitle, Experience }
import spray.json.DefaultJsonProtocol

import ch.megard.akka.http.cors.CorsDirectives._
import ch.megard.akka.http.cors.CorsSettings

import akka.http.scaladsl.model.HttpMethods._
import scala.collection.immutable

import scala.collection.mutable.MutableList;
import spray.json._;

import java.util.concurrent.TimeUnit





object UserJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  //implicit val UserFormats = jsonFormat4(User)
  implicit val User2Formats = jsonFormat4(User2)
  implicit val User3Formats = jsonFormat7(User3)
  implicit val BiodataFormats = jsonFormat11(BioData)
  implicit val CredentialsFormats = jsonFormat2(Credentials)
  implicit val JobtitleFormats = jsonFormat1(GetJobTitle)
  implicit val ExpFormats = jsonFormat3(Experience)
  implicit val ResponseFormat = jsonFormat3(ResponseStatus.apply)


  
  
  
}

trait AuthService extends DatabaseAccess {

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

  import authentikat.jwt._

  private val tokenExpiryPeriodInDays = 1
  private val secretKey               = "super_secret_key"
  private val header                  = JwtHeader("HS256")

  
  private def securedContent = get {
    authenticated { claims =>
      complete(s"User ${claims.getOrElse("user", "")} accessed secured content!")
    }
  }

  private def authenticated: Directive1[Map[String, Any]] =
    optionalHeaderValueByName("Authorization").flatMap {
      case Some(jwt) if isTokenExpired(jwt) =>
        complete(StatusCodes.Unauthorized -> "Token expired.")
        
      case Some(jwt) if JsonWebToken.validate(jwt, secretKey) =>
        provide(getClaims(jwt).getOrElse(Map.empty[String, Any]))

      case _ => complete(StatusCodes.Unauthorized)
    }

  private def setClaims(email: String, expiryPeriodInDays: Long) = JwtClaimsSet(
    Map("user" -> email,
        "expiredAt" -> (System.currentTimeMillis() + TimeUnit.DAYS
          .toMillis(expiryPeriodInDays)))
  )

  private def getClaims(jwt: String) = jwt match {
    case JsonWebToken(_, claims, _) => claims.asSimpleMap.toOption
    case _                          => None
  }

  private def isTokenExpired(jwt: String) = getClaims(jwt) match {
    case Some(claims) =>
      claims.get("expiredAt") match {
        case Some(value) => value.toLong < System.currentTimeMillis()
        case None        => false
      }
    case None => false
  }


  val settings = CorsSettings.defaultSettings.copy(allowedMethods = immutable.Seq(GET, PUT, POST, HEAD, OPTIONS))
  val authRoutes: Route = cors(settings){
    post {
      path("login") {
        entity(as[Credentials]) { entity =>
          complete {
            try {
              val isPersisted: MutableList[User2] = login(entity)
              val claims = setClaims("flavoursoft@yahoo.com", tokenExpiryPeriodInDays)
              
              isPersisted match {
                case _: MutableList[_] =>
                  var response: StringBuilder = new StringBuilder("[")
                  isPersisted.foreach(
                      x => response.append(x.toJson).append(",")
                    )
                  
                  // If records exist
                  if(response.length > 1) response.deleteCharAt(response.length - 1);
                  response.append("]"); 
                  HttpResponse(StatusCodes.OK, entity = response.toString()) //data.toString())
                  case _ => HttpResponse(StatusCodes.BadRequest,
                   entity = s"User does not exist")
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
     } 
    
  }
}
