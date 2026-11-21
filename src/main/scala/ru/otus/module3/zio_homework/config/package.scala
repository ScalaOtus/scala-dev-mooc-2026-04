package ru.otus.module3.zio_homework

import zio.config.magnolia.*
import zio.{Config, ConfigProvider, IO}


package object config {
  case class AppConfig(host: String, port: String)
  
  private val myConfigAutomatic: Config[AppConfig] = deriveConfig[AppConfig]

  object Configuration {
    val config: IO[Config.Error, AppConfig] = ConfigProvider.defaultProvider.load(myConfigAutomatic)

    val defaultConfig: AppConfig = AppConfig("localhost", "8080")
    
  }
}
