package org.hungerford.generic.schema.types


trait Injector[ T, Target, Using ] {
   type Out

   def inject( value : T, into : Target, using : Using ) : Out
}

object Injector {
   type Aux[ T, Target, Using, Out0 ] = Injector[ T, Target, Using ] { type Out = Out0 }

   def simpleInjector[ T, Target, Using ](
       injector : (T, Target, Using) => Target,
   ) : Injector.Aux[ T, Target, Using, Target ] = new Injector[ T, Target, Using ] {
       type Out = Target

       override def inject( value : T, into : Target, using : Using ) : Target = injector( value, into, using )
   }

   /**
    * Completes injection of hlist of fields into some object using an hlist of field description
    * by simply returning current state of target object when having arrived at the end of the lists
    */
   implicit def hnilInjector[ Target ] : Injector.Aux[ EmptyTuple, Target, EmptyTuple, Target ] = new Injector[ EmptyTuple, Target, EmptyTuple ] {
       override type Out = Target

       override def inject( value : EmptyTuple, into : Target, using : EmptyTuple ) : Out = into
   }

   /**
    * Recursive injector instance to build some target value from elements of an hlist
    */
   implicit def genericTupleInjector[ FromHead, FromTail <: Tuple, Target, UsingHead, UsingTail <: Tuple ](
       implicit
       simpleInjector : Injector.Aux[ FromHead, Target, UsingHead, Target ],
       nextInjector : Injector.Aux[ FromTail, Target, UsingTail, Target ],
   ) : Injector.Aux[ FromHead *: FromTail, Target, UsingHead *: UsingTail, Target ] = {
       new Injector[ FromHead *: FromTail, Target, UsingHead *: UsingTail ] {
           type Out = Target

           override def inject( value : FromHead *: FromTail, into : Target, using : UsingHead *: UsingTail ) : Target = {
               val nextTarget = simpleInjector.inject( value.head, into, using.head )
               nextInjector.inject( value.tail, nextTarget, using.tail )
           }
       }
   }
}
