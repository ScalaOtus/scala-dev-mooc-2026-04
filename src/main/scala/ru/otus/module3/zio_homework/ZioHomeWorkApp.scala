package ru.otus.module3.zio_homework

//import ru.otus.module3.zioConcurrency
//import ru.otus.module3.zio_homework.Timing
import ru.otus.module3.zio_homework.config.Configuration
import zio.{Console, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object ZioHomeWorkApp extends ZIOAppDefault {
  // задача 1
  // override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = guessProgram

  // задача 2
  // override def run: ZIO[Any, Throwable, Unit] =
  //   doWhile(guessProgram)(result => result)

  // задача 3
  // sbt "runMain ru.otus.module3.zio_homework.ZioHomeWorkApp"
//  override def run: ZIO[Any, Throwable, Unit] =
//    loadConfigOrDefault.flatMap { cfg =>
//      Console.printLine(s"Подключаюсь к ${cfg.host}:${cfg.port}")
//    }
  // запуск с ошибкой и её обработкой
  // sbt "runMain ru.otus.module3.zio_homework.ZioHomeWorkApp"

  // запуск без ошибки
  // HOST=localhost PORT=8080 sbt "runMain ru.otus.module3.zio_homework.ZioHomeWorkApp"


  // задачи 4.1, 4.2 и 4.3
  // override def run: ZIO[Any, Throwable, Unit] = app.unit

  // задача 4.4
  // override def run: ZIO[Any, Throwable, Unit] = appSpeedUp.unit

  // задача 5
//  override def run: ZIO[Any, Throwable, Unit] =
//    ZIO.unit

  override def run: ZIO[Any, Throwable, Unit] =
    app5.provide(Timing.live)

}
