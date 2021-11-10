//package org.hungerford.generic.schema.types
//
//
//trait SimpleExtractor[ Source, Using ] {
//    type Out
//
//    def extract( from : Source, using : Using ) : Out
//}
//
//object SimpleExtractor {
//    type Aux[ Source, Using, Out0 ] = SimpleExtractor[ Source, Using ] { type Out = Out0 }
//}
//
//trait Extractor[ Source, Target, Using ] {
//    type Out
//
//    def extract( from : Source, to : Target, using : Using ) : Out
//}
//
//object Extractor {
//    type Aux[ Source, Target, Using, Out0 ] = Extractor[ Source, Target, Using ] { type Out = Out0 }
//
//    implicit def hlistAppenderFromSimpleExtractor[ Source, Target <: Tuple, Using, SimpleRes, Res <: Tuple ](
//        implicit
//        se : SimpleExtractor.Aux[ Source, Using, SimpleRes ],
//        prep : Prepend.Aux[ Target, SimpleRes *: EmptyTuple, Res ],
//    ) : Extractor.Aux[ Source, Target, Using, Res ] = new Extractor[ Source, Target, Using ] {
//        override type Out = Res
//
//        override def extract( from : Source, to : Target, using : Using ) : Out = {
//            val simpleRes : SimpleRes = se.extract( from, using )
//            to :+ simpleRes
//        }
//    }
//
//    implicit def hnilExtractor[ Source, Target ] : Extractor.Aux[ Source, Target, EmptyTuple, Target ] = {
//        new Extractor[ Source, Target, EmptyTuple ] {
//            override type Out = Target
//
//            override def extract( from : Source, to : Target, using : EmptyTuple ) : Out = to
//        }
//    }
//
//    implicit def genericHlistExtractor[ Source, Target <: Tuple, UHead, UTail <: Tuple, Intermediate <: Tuple, Res ](
//        implicit
//        se : Lazy[ Extractor.Aux[ Source, Target, UHead, Intermediate ] ],
//        next : Extractor.Aux[ Source, Intermediate, UTail, Res ],
//    ) : Extractor.Aux[ Source, Target, UHead *: UTail, Res ] = {
//        new Extractor[ Source, Target, UHead *: UTail ] {
//            override type Out = Res
//
//            override def extract( from : Source, to : Target, using : UHead *: UTail ) : Out = {
//                val intermediateRes : Intermediate = se.value.extract( from, to, using.head )
//                next.extract( from, intermediateRes, using.tail )
//            }
//        }
//    }
//}
