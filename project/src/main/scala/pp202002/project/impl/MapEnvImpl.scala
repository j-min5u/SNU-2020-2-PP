package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._
import pp202002.project.impl.ExprInterpreter.InterpreterException

import scala.annotation.tailrec

object MapEnvImpl {
  implicit val mapEnvImpl: EnvOps[MapEnv, Value[MapEnv]] =
    new EnvOps[MapEnv, Value[MapEnv]] {
      def emptyEnv(): MapEnv = new MapEnv(Nil)

      def pushEmptyFrame(env: MapEnv): MapEnv = new MapEnv(Map[String,LazyVal[MapEnv, Value[MapEnv]]]()::env.frames)

      def popFrame(env: MapEnv): MapEnv = env.frames match{
        case Nil=>env
        case _::tl=>new MapEnv(tl)
      }

      def setItem(
          env: MapEnv,
          name: String,
          item: EnvVal
      ): MapEnv = {
        env.frames match{
          case Nil=>env
          case hd::tl=>{
            val changed=hd+(name->item)
            new MapEnv(changed::tl)
          }
        }
      }

      def findItem(
          env: MapEnv,
          name: String
      ): Option[EnvVal] = {
        env.frames match{
          case Nil=>None
          case hd::_=>hd.get(name) match {
            case None=>findItem(popFrame(env),name)
            case _=>hd.get(name)
          }
        }
      }
    }
}

