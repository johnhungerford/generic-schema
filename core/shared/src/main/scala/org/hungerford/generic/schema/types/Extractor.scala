package org.hungerford.generic.schema.types

import scala.compiletime.{erasedValue, summonInline}


trait SimpleExtractor[ Source, Using ] {
   type Out

   def extract( from : Source, using : Using ) : Out
}

object SimpleExtractor {
   type Aux[ Source, Using, Out0 ] = SimpleExtractor[ Source, Using ] { type Out = Out0 }
}

trait Extractor[ Source, Target, Using ] {
   type Out

   def extract( from : Source, to : Target, using : Using ) : Out
}

object Extractor {
   type Aux[ Source, Target, Using, Out0 ] = Extractor[ Source, Target, Using ] { type Out = Out0 }

   given hnilExtractor[ Source, Target ] : Extractor.Aux[ Source, Target, EmptyTuple, Target ] = {
       new Extractor[ Source, Target, EmptyTuple ] {
           override type Out = Target

           override def extract( from : Source, to : Target, informedBy : EmptyTuple ) : Out = to
       }
   }

   given genericHlistExtractor[ Source, Target <: Tuple, UHead, UTail <: Tuple, SimpleRes, Res <: Tuple ](
       using
       se : SimpleExtractor.Aux[ Source, UHead, SimpleRes ],
       next : Extractor.Aux[ Source, Tuple.Concat[ Target, SimpleRes *: EmptyTuple ], UTail, Res ],
   ) : Extractor.Aux[ Source, Target, UHead *: UTail, Res ] = {
       new Extractor[ Source, Target, UHead *: UTail ] {
           override type Out = Res

           override def extract( from : Source, to : Target, informedBy : UHead *: UTail ) : Out = {
               val simpleRes : SimpleRes = se.extract( from, informedBy.head )
               val nextTarget = to ++ (simpleRes *: EmptyTuple)
               next.extract( from, nextTarget, informedBy.tail )
           }
       }
   }

   transparent inline def extractor[ Source, Target <: Tuple, Using <: Tuple, Res <: Tuple ] : Extractor.Aux[ Source, Target, Using, Res ] = {
       inline erasedValue[ Using ] match {
           case _ : EmptyTuple =>
               inline erasedValue[ Target ] match {
                   case _ : Res =>
                       new Extractor[ Source, Target, Using ] {
                            type Out = Res

                            def extract( from : Source, to : Target, using : Using ) : Res = to.asInstanceOf[ Res ]
                        }

                    case _ => throw new Exception( "asdfasdfa" )
               }
               
            case _ : (h *: ts) =>
                new Extractor[ Source, Target, Using ] {
                    val se = summonInline[ SimpleExtractor[ Source, h ] ]
                    val nextExtractor = extractor[ Source, Tuple.Concat[ Target, se.Out *: EmptyTuple ], ts, Res ]
                    type Out = Res

                    def extract( from : Source, to : Target, informedBy : Using ) : Out = {
                        val informedByCorrected = informedBy.asInstanceOf[ h *: ts ]
                        val simpleRes = se.extract( from, informedByCorrected.head )
                        nextExtractor.extract( from, to ++ (simpleRes *: EmptyTuple), informedByCorrected.tail )
                    }
                }
       }
   }
}
