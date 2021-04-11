package playground

import zio.config._, ConfigDescriptor._

object ZioConfigTryOut {
  def main(args: Array[String]): Unit = {
    val systemEnv: Map[String, String] = sys.env

    /**
     * Conventional method of getting the config. This will FAILFAST
     * Use OPTION to catch absence of a value rather than a String
     */
    /*    val databaseHost: Option[String] = systemEnv.get("DATABASE_HOST")
        val port: Option[String] = systemEnv.get("PORT")
        val user: Option[String] = systemEnv.get("USER_NAME")
        val password: Option[String] = systemEnv.get("PASSWORD")*/

    //    This is a bad way of programming
    //    CoreApp.run(ApplicationConfig(databaseHost.get, port.get.toInt, user.get, password.get))

    //    This is a better way to get the config values and run the app
    /*    for {
          db <- databaseHost
          pt <- port
          un <- user
          pw <- password
        } yield CoreApp.run(ApplicationConfig(db, pt.toInt, un, pw))*/

    //
    /**
     * Use EITHER to catch the error if config value absent
     * This will still FAILFAST. Will not catch all the errors
     */
    /*    val databaseHost: Either[String, String] = systemEnv.get("DATABASE_HOST").toRight("Database host is absent")
        val port: Either[String, String] = systemEnv.get("PORT").toRight("Port is absent")
        val user: Either[String, String] = systemEnv.get("USER_NAME").toRight("username is absent")
        val password: Either[String, String] = systemEnv.get("PASSWORD").toRight("password is absent")

        val result: Either[String, Unit] = for {
          db <- databaseHost
          pt <- port
          un <- user
          pw <- password
        } yield CoreApp.run(ApplicationConfig(db, pt.toInt, un, pw))*/

    //    println(result)

    /**
     * Use Zio Config to catch all errors and not to FAILFAST
     * Also use zio config to not explicitly cast port from String to Int
     */

    val configDescriptor: ConfigDescriptor[ApplicationConfig] =
      (string("DATABASE_HOST") |@| int("PORT") |@| string("USER_NAME") |@| string("PASSWORD")) (ApplicationConfig.apply, ApplicationConfig.unapply)

    println(read(configDescriptor))

  }
}

object CoreApp {
  def run(appConfig: ApplicationConfig) = {
    println(s"Running the application Config App using the config $appConfig")
  }
}

final case class ApplicationConfig(host: String, port: Int, user: String, password: String)