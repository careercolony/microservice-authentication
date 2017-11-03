package com.careercolony.neo4jServices

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.careercolony.neo4jServices.routes.PostService

import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.{StatusCodes, HttpResponse}
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}
import com.typesafe.config.ConfigFactory


class StartNeo4jServer(implicit val system: ActorSystem,
                       implicit val materializer: ActorMaterializer) extends PostService {
  
  lazy val allowedOrigin = {
    val config = ConfigFactory.load()
    val sAllowedOrigin = config.getString("cors.allowed-origin")
    HttpOrigin(sAllowedOrigin)
  }

  //this directive adds access control headers to normal responses
  private def addAccessControlHeaders: Directive0 = {
    respondWithHeaders(
      `Access-Control-Allow-Origin`(allowedOrigin),
      `Access-Control-Allow-Credentials`(true),
      `Access-Control-Allow-Headers`("Authorization", "Content-Type", "X-Requested-With")
    )
  }

  //this handles preflight OPTIONS requests.
  private def preflightRequestHandler: Route = options {
    complete(HttpResponse(StatusCodes.OK).withHeaders(`Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE)))
  }
  
  def corsHandler(r: Route) = addAccessControlHeaders {
    preflightRequestHandler ~ r
  }

  def startServer(address: String, port: Int) = {
    Http().bindAndHandle(postRoutes, address, port)
  }
}

object StartApplication extends App {
  StartApp
}

object StartApp {
  implicit val system: ActorSystem = ActorSystem("Neo4j-Akka-Service")
  implicit val executor = system.dispatcher
  implicit val materializer = ActorMaterializer()
  val server = new StartNeo4jServer()
  val config = server.config
  val serverUrl = config.getString("http.interface")
  val port = config.getInt("http.port")
  server.startServer(serverUrl, port)
}