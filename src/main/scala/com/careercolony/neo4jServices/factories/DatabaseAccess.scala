package com.careercolony.neo4jServices.factories

import org.neo4j.driver.v1._
import com.typesafe.config.ConfigFactory
import java.security.MessageDigest

import scala.collection.mutable.MutableList;





case class User2(memberID: Int, firstname: String, lastname: String, email: String)
case class User3(memberID: Int, firstname: String, lastname: String, email: String, interest: String, employmentstatus: String, avatar: String)
case class Credentials(email: String, password: String)
case class User(firstname: String, lastname: String, email: String, password: String)
case class Wizard(email: String, current_employment_status: String, interest: String, current_job_title: String, current_employer: String)
case class ListUser(companies: MutableList[User2])
case class ResponseStatus(status: Int, message: String, details: String)
case class GetJobTitle(position: String)
case class BioData(memberID: Int, userIP: Option[String], email:String, country:String, interest:String, employmentstatus:String, employer_name:Option[String], position:Option[String], industry:Option[String], degree:Option[String], school_name:Option[String])
case class Experience(memberID: String, employer: String, position: String)

trait DatabaseAccess {
  

  val config = ConfigFactory.load("application.conf")
  val neo4jUrl = config.getString("neo4j.url")
  val userName = config.getString("neo4j.userName")

  val userPassword = config.getString("neo4j.userPassword")

  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes)
  }

  

  def login(l: Credentials) = {
    
    val driver = GraphDatabase.driver(neo4jUrl, AuthTokens.basic(userName, userPassword))
    val session = driver.session
    val script = s"OPTIONAL MATCH (a:Members {email:'${ l.email}',  password:'${ l.password}'}) RETURN a.memberID AS memberID, a.firstname AS firstname, a.lastname AS lastname, a.email AS email"
    val result: StatementResult = session.run(script)
    val records = MutableList[User2]()
   if(result.hasNext()){
        while (result.hasNext()) {
          val record = result.next()
          val user: User2 = new User2(record.get("memberID").asInt, record.get("firstname").asString(), record.get("lastname").asString(), record.get("email").asString())
          
          records += user
        } 

        session.close()
        driver.close()
        records
    }else{
        println("User does not exist");
      
         session.close()
         driver.close()
         records
    }
  }

  

  
}

object DatabaseAccess extends DatabaseAccess

