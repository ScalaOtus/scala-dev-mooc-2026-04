package ru.otus.module3.zio_homework

import ru.otus.module3.zio_homework.config.Configuration
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault, Console}

object ZioHomeWorkApp extends ZIOAppDefault {
  // задача 1
  // override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = guessProgram

  // задача 2
  // override def run: ZIO[Any, Throwable, Unit] =
  //   doWhile(guessProgram)(result => result)

  // задача 3
  // sbt "runMain ru.otus.module3.zio_homework.ZioHomeWorkApp"
  override def run: ZIO[Any, Throwable, Unit] =
    loadConfigOrDefault.flatMap { cfg =>
      Console.printLine(s"Подключаюсь к ${cfg.host}:${cfg.port}")
    }
}
