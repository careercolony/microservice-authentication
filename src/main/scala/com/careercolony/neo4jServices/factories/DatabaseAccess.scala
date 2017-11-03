package com.careercolony.neo4jServices.factories

import org.neo4j.driver.v1._
import com.typesafe.config.ConfigFactory

import scala.collection.mutable.MutableList;

case class CreatePost(memberID: Int, post_title: String, post_description: String, post_email: String, media: String, media_type: String, post_date: String )
case class GetPost(postID: Int, memberID: Int, post_title: String, post_description: String, post_email: String, media: String, media_type: String, post_date: String )

trait DatabaseAccess {

  val config = ConfigFactory.load("application.conf")
  val neo4jUrl = config.getString("neo4j.url")
  val userName = config.getString("neo4j.userName")

  val userPassword = config.getString("neo4j.userPassword")


  def insertPost(p: CreatePost): MutableList[GetPost] = {
    val driver = GraphDatabase.driver(neo4jUrl, AuthTokens.basic(userName, userPassword))
    val session = driver.session
    
      val script = s"MERGE (id:UniqueId{name:'Posts'}) ON CREATE SET id.count = 1 ON MATCH SET id.count = id.count + 1 WITH id.count AS pid CREATE(p:Posts {postID:pid, post_title:'', post_description:'', post_date: TIMESTAMP(), post_email:'', memberID:'', media:'', media_type:'' }) RETURN p.postID AS postID, p.post_title AS post_title, p.post_description AS post_description, p.media AS media, p.media_type AS media_type, p.memberID AS memberID "
      val result: StatementResult = session.run(script)
      val records = MutableList[GetPost]()

      while (result.hasNext()) {
        val record = result.next()
        val post: GetPost = new GetPost(record.get("postID").asInt, record.get("memerID").asInt(), record.get("post_title").asString(), record.get("post_description").asString(), record.get("post_email").asString(), record.get("post_date").asString(), record.get("media").asString(), record.get("media_type").asString())
        
        records += post
      } 

      session.close()
      driver.close()
      records
  
}

def retrievePost(postID: Int): MutableList[GetPost] = {
    val driver = GraphDatabase.driver(neo4jUrl, AuthTokens.basic(userName, userPassword))
    val session = driver.session
    val script = s"MATCH (a:Posts) WHERE a.postID = $postID RETURN  a.postID AS postID, a.memberID AS memberID, a.post_title AS post_title, a.post_description AS post_description, a.media AS media, a.media_type AS media_type, a.post_date AS post_date"
    val result = session.run(script)
    
    val records = MutableList[GetPost]()
   
     while (result.hasNext()) {
          val record = result.next()
          val post: GetPost = new GetPost(record.get("postID").asInt, record.get("memerID").asInt(), record.get("post_title").asString(), record.get("post_description").asString(), record.get("post_email").asString(), record.get("post_date").asString(), record.get("media").asString(), record.get("media_type").asString())
          records += post
     } 
     

    session.close()
    driver.close()
    records
  }
  def updatepost(b:GetPost): Boolean = {
    val driver = GraphDatabase.driver(neo4jUrl, AuthTokens.basic(userName, userPassword))
    val session = driver.session
    val script =
      s"MATCH (s:Posts) where s.postID ='${b.postID}' SET s.post_title = '${b.post_title}', s.post_description = '${b.post_description}' , s.post_email ='${b.post_email}', s.media='${b.media}', s.media_type='${b.media_type}'  RETURN s.postID AS postID" 
    val result = session.run(script)
    session.close()
    driver.close()
    result.consume().counters().containsUpdates()
  }
  

  def deleteRecord(postID: String): Int = {
    val driver = GraphDatabase.driver(neo4jUrl, AuthTokens.basic(userName, userPassword))
    val session = driver.session
    val script = s"MATCH (p:Posts) where p.postID ='$postID' Delete Posts"
    val result = session.run(script)
    session.close()
    driver.close()
    result.consume().counters().nodesDeleted()
  }

/**
  def createNodesWithRelation(p: String, other_member_email:String) = {
    val driver = GraphDatabase.driver(neo4jUrl, AuthTokens.basic(userName, userPassword))
    val session = driver.session
    //val nameOfFriends = "\"" + userList.mkString("\", \"") + "\""
    //val script = s"MATCH (user:Users {firstname: '$user_name'}) FOREACH (firstname in [$nameOfFriends] | " +
                 //s"CREATE (user)-[:$relation]->(:Users {firstname:name}))"

    val script = s"MATCH(a:Members  {email:'${email}'}), (b:Members {email:'${other_member_email}'}) CREATE(a)-[:FRIENDS {status:'active', date:'2017-09-07'}]->(b) "
    val result = session.run(script)
    session.close()
    driver.close()
    result.consume().counters().relationshipsCreated()
  }
  */

  
}

object DatabaseAccess extends DatabaseAccess

