package pp202002.project.impl

import pp202002.project.common._
import pp202002.project.common.Environment._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object ExprInterpreter {
  class InterpreterException(val reason: String) extends Exception {
    override def getMessage: String = reason
  }

  implicit def exprInterpreter[Env](implicit
      envOps: EnvOps[Env, Value[Env]]
  ): Interpreter[Expr, Value[Env]] = new Interpreter[Expr, Value[Env]] {
    def interp(expr: Expr): Try[Value[Env]] = {
      val startenv=envOps.emptyEnv()
      envOps.pushEmptyFrame(startenv)
      def intEnv(exp:Expr,env:Env):Try[Value[Env]]={
        exp match{
          case EInt(n)=>{
            Success(VInt(n))
          }
          case EName(x)=> {
            envOps.findItem(env,x) match{
              case Some(lv@LVLazy(exprt,envt,evalutated))=>{
                lv.evaluated match{
                  case Some(v)=>Success(v)
                  case None=>{
                    intEnv(exprt,envt) match{
                      case Success(v)=>{
                        lv.evaluated=Some(v)
                        Success(v)
                      }
                      case _=>{
                        Failure(new InterpreterException("int except1"))
                      }
                    }
                  }
                }
              }
              case Some(LVVal(v))=>{
                Success(v)
              }
              case _=>{
                Failure(new InterpreterException(x+"int except2"))
              }
            }
          }
          case EInL(e)=>{
            intEnv(e,env) match{
              case Success(ret)=>Success(VLeft(ret))
              case _=>Failure(new InterpreterException("int except3"))
            }
          }
          case EInR(e)=>{
            intEnv(e,env) match{
              case Success(ret)=>Success(VRight(ret))
              case _=>Failure(new InterpreterException("int except4"))
            }
          }
          case EMatch(v,lN,lC,rN,rC)=>{
            intEnv(v,env) match{
              case Success(VLeft(ret))=>{
                val nextenv=envOps.setItem(env,lN,LVVal(ret))
                intEnv(lC,nextenv)
              }
              case Success(VRight(ret))=>{
                val nextenv=envOps.setItem(env,rN,LVVal(ret))
                intEnv(rC,nextenv)
              }
              case _=>Failure(new InterpreterException("int except5"))
            }
          }
          case ENil=>Success(VNil)
          case ECons(h,t)=>{
            intEnv(h,env) match{
              case Success(headret)=>{
                intEnv(t,env) match{
                  case Success(tailret)=>{
                    Success(VCons(headret,tailret))
                  }
                  case _=>Failure(new InterpreterException("int except6"))
                }
              }
              case _=>Failure(new InterpreterException("int except7"))
            }
          }
          case EFst(e)=>{
            intEnv(e,env) match{
              case Success(ret)=>{
                ret match{
                  case VCons(first,_)=>{
                    Success(first)
                  }
                  case _=>Failure(new InterpreterException("int except8"))
                }
              }
              case _=>Failure(new InterpreterException("int except9"))
            }
          }
          case ESnd(e)=>{
            intEnv(e,env) match{
              case Success(ret)=>{
                ret match{
                  case VCons(_,second)=>{
                    Success(second)
                  }
                  case _=>Failure(new InterpreterException("int except10"))
                }
              }
              case _=>Failure(new InterpreterException("int except11"))
            }
          }
          case EApp(f,a)=>{
            intEnv(f,env) match{
              case Success(VFunc(name,ps,bd,envv))=>{
                @tailrec
                def storeparam(ev:Env,parms:List[Arg],args:List[Expr]):Env={
                  parms match{
                    case pahd::patl=>{

                      args match{
                        case arhd::artl=>{
                          val nextenv=pahd match{
                            case AVName(pn)=>{
                              intEnv(arhd,env) match{
                                case Success(v)=>envOps.setItem(ev,pn,LVVal(v))
                                case _=>ev
                              }
                            }
                            case ANName(pn)=>{
                              envOps.setItem(ev,pn,LVLazy(arhd,env,None))
                            }
                          }
                          storeparam(nextenv,patl,artl)
                        }
                        case Nil=>ev
                      }
                    }
                    case Nil=>{
                      ev
                    }
                  }
                }
                val evself=envOps.setItem(envv,name,LVVal(VFunc(name,ps,bd,envv)))
                val paddedenv=storeparam(evself,ps,a)

                intEnv(bd,paddedenv)
              }
              case _=>intEnv(f,env)
            }

          }
          case ELet(bs,e)=>{
            @tailrec
            def intELetrec(binds:List[Bind],envin:Env):Env={
              binds match{
                case Nil=>{
                  envin
                }
                case hd::tl=>{
                  val nextenv=hd match{
                    case BDef(f,ps,bd)=>{
                      envOps.setItem(envin,f,LVVal(VFunc(f,ps,bd,envin)))
                    }
                    case BVal(x,e)=>{
                      intEnv(e,envin) match{
                        case Success(v)=>{
                          envOps.setItem(envin,x,LVVal(v))
                        }
                        case _=>envin
                      }

                    }
                    case BLVal(x,e)=>{
                      envOps.setItem(envin,x,LVLazy(e,envin,None))
                    }
                    case _=>envin
                  }
                  intELetrec(tl,nextenv)
                }
                case _=>envin

              }

            }
            val addedenv=envOps.pushEmptyFrame(env)
            val bindedenv=intELetrec(bs,addedenv)
            intEnv(e,bindedenv)
          }
          case ENilP(e)=>{
            intEnv(e,env) match{
              case Success(v)=>{
                v match{
                  case VNil=>Success(VRight(VInt(0)))
                  case _=>Success(VLeft(VInt(0)))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case EIntP(e)=>{
            intEnv(e,env) match{
              case Success(v)=>{
                v match{
                  case VInt(_)=>Success(VRight(VInt(0)))
                  case _=>Success(VLeft(VInt(0)))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case ESumP(e)=>{
            intEnv(e,env) match{
              case Success(v)=>{
                v match{
                  case VRight(_)=>Success(VRight(VInt(0)))
                  case VLeft(_)=>Success(VRight(VInt(0)))
                  case _=>Success(VLeft(VInt(0)))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case EProdP(e)=>{
            intEnv(e,env) match{
              case Success(v)=>{
                v match{
                  case VCons(_,_)=>Success(VRight(VInt(0)))
                  case _=>Success(VLeft(VInt(0)))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case EPlus(l,r)=>{
            intEnv(l,env) match{
              case Success(VInt(n1))=>{
                intEnv(r,env) match{
                  case Success(VInt(n2))=>Success(VInt(n1+n2))
                  case _=>Failure(new InterpreterException("int except"))
                }
              }
              case _=>intEnv(l,env)
            }
          }
          case EMinus(l,r)=>{
            intEnv(l,env) match{
              case Success(VInt(n1))=>{
                intEnv(r,env) match{
                  case Success(VInt(n2))=>Success(VInt(n1-n2))
                  case _=>Failure(new InterpreterException("int except"))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case EMul(l,r)=>{
            intEnv(l,env) match{
              case Success(VInt(n1))=>{
                intEnv(r,env) match{
                  case Success(VInt(n2))=>Success(VInt(n1*n2))
                  case _=>Failure(new InterpreterException("int except"))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case EDiv(l,r)=>{
            intEnv(l,env) match{
              case Success(VInt(n1))=>{
                intEnv(r,env) match{
                  case Success(VInt(n2))=>Success(VInt(n1/n2))
                  case _=>Failure(new InterpreterException("int except"))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case EMod(l,r)=>{
            intEnv(l,env) match{
              case Success(VInt(n1))=>{
                intEnv(r,env) match{
                  case Success(VInt(n2))=>Success(VInt(n1%n2))
                  case _=>Failure(new InterpreterException("int except"))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case EEq(l,r)=>{
            intEnv(l,env) match{
              case Success(VInt(n1))=>{
                intEnv(r,env) match{
                  case Success(VInt(n2))=> {
                    if(n1==n2) Success(VRight(VInt(0)))
                    else Success(VLeft(VInt(0)))
                  }
                  case _=>Failure(new InterpreterException("int except"))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case ELt(l,r)=>{
            intEnv(l,env) match{
              case Success(VInt(n1))=>{
                intEnv(r,env) match{
                  case Success(VInt(n2))=>{
                    if(n1<n2) Success(VRight(VInt(0)))
                    else Success(VLeft(VInt(0)))
                  }
                  case _=>Failure(new InterpreterException("int except"))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }
          case EGt(l,r)=>{
            intEnv(l,env) match{
              case Success(VInt(n1))=>{
                intEnv(r,env) match{
                  case Success(VInt(n2))=>{
                    if(n1>n2) Success(VRight(VInt(0)))
                    else Success(VLeft(VInt(0)))
                  }
                  case _=>Failure(new InterpreterException("int except"))
                }
              }
              case _=>Failure(new InterpreterException("int except"))
            }
          }

        }
      }
      intEnv(expr,startenv)
    }


  }
}

