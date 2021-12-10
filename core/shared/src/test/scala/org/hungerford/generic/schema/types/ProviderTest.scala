package org.hungerford.generic.schema.types

import org.hungerford.generic.schema.product.field.FieldBuilder
import org.hungerford.generic.schema.{Schema, SchemaProvider}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.ClassTag

class ProviderTest extends AnyFlatSpecLike with Matchers {

   behavior of "Provider"

   it should "permit an instance that simply provides from a given instance" in {
       implicit def provideInstance[ T ](
           implicit t : T,
       ) : Provider[ T ] = new Provider[T] {
           override def provide : T = t
       }

       implicit val tInst : Int = 5

       Provider[ Int ].provide shouldBe 5
   }

   it should "permit an instance that provides from an implicit deriver" in {
       trait Stringer[ T ] {
           def string : String
       }

       implicit def provideInstance[ From, T ](
           implicit der : Deriver.Aux[ From, T ],
       ) : Provider[ T ] = new Provider[ T ] {
           override def provide : T = der.derive
       }

       implicit def deriveInstance[ T ](
           implicit ct : ClassTag[ T ]
       ) : Deriver.Aux[ T, Stringer[ T ] ] = new Deriver[ T ] {
           type Out = Stringer[ T ]

           override def derive : Out = new Stringer[ T ] {
               override def string : String =  ct.runtimeClass.getSimpleName
           }
       }

       Provider[ Stringer[ Int ] ].provide.string shouldBe "int"
   }

   it should "prefer an instance to a derivation" in {
       trait Stringer[ T ] {
           def string : String
       }

       trait LowPriorityInstance {
           implicit def provideDerivation[ From, T ](
               implicit der : Deriver.Aux[ From, T ],
           ) : Provider[ T ] = new Provider[T] {
               override def provide : T = der.derive
           }
       }

       object Instances extends LowPriorityInstance {
           implicit def provideInstance[ T ](
               implicit t : T,
           ) : Provider[ T ] = new Provider[T] {
               override def provide : T = t
           }
       }

       import Instances._

       implicit def deriveInstance[ T ](
           implicit ct : ClassTag[ T ]
       ) : Deriver.Aux[ T, Stringer[ T ] ] = new Deriver[ T ] {
           override type Out = Stringer[ T ]

           override def derive : Out = new Stringer[ T ] {
               override def string : String =  ct.runtimeClass.getSimpleName
           }
       }

       implicit val stringerInst : Stringer[ Int ] = new Stringer[ Int ] {
           override def string : String = "hello"
       }

       Provider[ Stringer[ Int ] ].provide.string shouldBe "hello"
   }

}
