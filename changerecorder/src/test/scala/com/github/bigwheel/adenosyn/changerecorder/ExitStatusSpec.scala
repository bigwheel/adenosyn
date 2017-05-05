package com.github.bigwheel.adenosyn.changerecorder

import java.security.Permission
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Suite

trait ExitStatusSpec extends BeforeAndAfterAll { this: Suite =>

  // how to test exit status in scala http://stackoverflow.com/a/39075827/4006322
  protected[this] sealed case class ExitException(status: Int) extends SecurityException("System.exit() is not allowed")

  protected[this] sealed class NoExitSecurityManager extends SecurityManager {
    override def checkPermission(perm: Permission): Unit = {}

    override def checkPermission(perm: Permission, context: Object): Unit = {}

    override def checkExit(status: Int): Unit = {
      super.checkExit(status)
      throw ExitException(status)
    }
  }

  override protected def beforeAll() = System.setSecurityManager(new NoExitSecurityManager())

  override protected def afterAll() = System.setSecurityManager(null)

}
