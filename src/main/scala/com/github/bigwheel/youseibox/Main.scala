package com.github.bigwheel.youseibox

import scalikejdbc._

object Main {
  def main(args: Array[String]): Unit = {
    println("hello")
  }

  def db(ipAddress: String): Unit = {
    Class.forName("com.mysql.jdbc.Driver")
    ConnectionPool.singleton(
      s"jdbc:mysql://$ipAddress/youseibox_test?characterEncoding=UTF-8", "root", "root")
    implicit val session = AutoSession

    DB autoCommit { implicit session =>
      SQL("""
    create table members (
      id bigint primary key auto_increment,
      name varchar(30) not null,
      description varchar(1000),
      birthday date,
      created_at timestamp not null
    )
          """).execute.apply()
    }
  }
}
