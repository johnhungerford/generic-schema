package org.hungerford.generic.schema

object Component {
	trait Tpe[T]
	
	trait Of[Outer]
	
	trait TypeOf[Outer, T] extends Of[Outer]

	trait Shaped[T, S] {
		def schema: Schema.Aux[T, S]
	}

	trait Lazy[T] extends Tpe[T] {
		def schema(
			using
			provider: SchemaProvider[T]
		): Schema.Aux[T, provider.Shape] = provider.provide
		
		def schemaFrom[Outer](
			using
			outer: Schema[Outer],
			extr: SchemaExtractor[T, Schema.Aux[ Outer, outer.Shape]]
		): Schema.Aux[T, extr.Shape] = extr.extract(outer)
	}

	trait Named[N <: String & Singleton] {
		def name: N
	}
	
	trait IsLazy
	trait NonLazy
	
	trait WithSchema[C, Outer] {
		type Tpe
		type Shape
		def schemaOf(component: C): Schema.Aux[Tpe, Shape]
	}
	
	object WithSchema {
		type Tpe[C, Outer, T] = WithSchema[C, Outer] { type Tpe = T }
		type Shaped[C, Outer, S] = WithSchema[C, Outer] { type Shape = S }
		type Aux[C, Outer, T, S] = WithSchema[C, Outer] { type Tpe = T; type Shape = S }
		
		given shaped[T, S, C <: Component.Shaped[T, S], Outer]: WithSchema[C, Outer] with {
			type Tpe = T
			type Shape = S
			override def schemaOf(component: C): Schema.Aux[T, S] = component.schema
		}
		given lazyComponent[Outer, OuterS, T, S, C <: Component.Lazy[T]](
			using
			outerSch: Schema.Aux[Outer, OuterS],
			extr: SchemaExtractor.Aux[T, Schema.Aux[Outer, OuterS], S],
		): WithSchema[C, Outer] with {
			type Tpe = T
			type Shape = S
			override def schemaOf(component: C): Schema.Aux[T, S] = extr.extract(outerSch)
		}
	}
}
