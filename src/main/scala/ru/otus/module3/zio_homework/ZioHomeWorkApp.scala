package ru.otus.module3.zio_homework

import zio.{Console, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object ZioHomeWorkApp extends ZIOAppDefault {
  // задача 1
  // override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = guessProgram

  // задача 2
  // override def run: ZIO[Any, Throwable, Unit] =
  //   doWhile(guessProgram)(result => result)

  // задача 3
  override def run: ZIO[Any, Throwable, Unit] = loadConfigOrDefault.unit
  // запуск с ошибкой и её обработкой
  //   sbt "runMain ru.otus.module3.zio_homework.ZioHomeWorkApp"
  // запуск без ошибки
  //   HOST=localhost PORT=8081 sbt "runMain ru.otus.module3.zio_homework.ZioHomeWorkApp"

  // задачи 4.1, 4.2 и 4.3
  // override def run: ZIO[Any, Throwable, Unit] = app.unit

  // задача 4.4
  // override def run: ZIO[Any, Throwable, Unit] = appSpeedUp.unit

  // задача 5
  // override def run: ZIO[Any, Throwable, Unit] =
  //  app5.provide(TimingService.live)

  // задача 6
  // override def run: ZIO[Any, Throwable, Unit] = runApp

}
