package com.github.bigwheel.youseibox

import scalikejdbc._

object Main {
  def main(args: Array[String]): Unit = {
    println("hello")
  }

  def db(ipAddress: String): Unit = {
    Class.forName("com.mysql.jdbc.Driver")
    ConnectionPool.singleton(
      s"jdbc:mysql://$ipAddress/youseibox_test?characterEncoding=UTF-8", "youseibox", "youseibox")
    implicit val session = AutoSession

    DB autoCommit { implicit session =>
      /*Seq(
        "drop table if exists artist",
        "drop table if exists artist_kana",
        "drop table if exists music",
        "drop table if exists content"
      ).foreach(SQL(_).execute.apply())

      Seq(
        """
          create table artist (
            id   INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
            name TEXT NOT NULL
          )
        """,
        """
          create table artist_kana (
            artist_id INT  NOT NULL PRIMARY KEY,
            kana      TEXT NOT NULL
          )
        """,
        """
          create table music (
            id        INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
            artist_id INT  NOT NULL,
            name      TEXT NOT NULL,
            INDEX index_artist_id(artist_id)
          )
        """,
        """
          create table content (
            id       INT  NOT NULL PRIMARY KEY AUTO_INCREMENT,
            music_id INT  NOT NULL,
            name     TEXT NOT NULL,
            INDEX index_music_id(music_id)
          )
        """
      ).foreach(SQL(_).execute.apply())
      */
    }
  }
}
