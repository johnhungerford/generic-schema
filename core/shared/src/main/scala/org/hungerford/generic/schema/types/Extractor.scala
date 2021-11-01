package org.hungerford.generic.schema.types

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
}
