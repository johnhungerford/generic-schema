# Generic Schema

A library for generating comprehensive, fully-typed descriptions of data types in 
Scala 3.

## Features

### Schema building API

#### Coproduct type
```scala
import org.hungerford.generic.schema.Default.dsl.*

sealed trait HttpMethod
case object Get extends HttpMethod
case object Post extends HttpMethod
case object Put extends HttpMethod
case object Delete extends HttpMethod

val httpMethodSchema = Schema.coproductBuilder[HttpMethod]
  .name("HttpMethod")
  .description("Http method for a REST request")
  .examples(Get, Post, Put, Delete)
  .buildSubtype[Get.type](
      _.typeName("Get")
        .singleton
        .fromSuper({ case Get => Some(Get); case _ => None })
        .build 
   )
  .buildSubtype[Post.type ](
      _.typeName("Post")
        .singleton
        .fromSuper({ case Post => Some(Post); case _ => None })
        .build
      )
  .buildSubtype[Put.type](
      _.typeName("Put")
        .singleton
        .fromSuper({ case Put => Some(Put); case _ => None })
        .build
      )
  .buildSubtype[Delete.type](
      _.typeName("Delete")
        .singleton
        .fromSuper({ case Delete => Some(Delete); case _ => None })
        .build
      )
  .build
```

#### Product type

```scala
case class HttpRequest(url: String, method: HttpMethod, body: Option[String])

val httpRequestSchema = Schema.productBuilder[HttpRequest]
  .name("HttpRequest")
  .description("Simple REST request model")
  .buildField[String](
      _.name("url")
        .extractor(_.url)
        .description("Destination URL for REST request")
        .examples("http://foo.bar.org:8080", "https://host.name/endpoint")
        .primitive
        .validate(Validator.regex("""[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)
"""))
        .build
  )
  .buildField[HttpMethod](
      _.name("method")
        .extractor(_.method)
        .fromSchema(httpMethodSchema)
        .build
  )
  .buildField[Option[String]](
      _.name("string_body")
        .extractor(_.body)
        .primitive
        .description("Optional body of the REST request")
        .examples(None, Some("""{"json":"payload"}"""), Some("Simple text body"))
        .build
  )
  .construct((url, method, body) => HttpRequest(url, method, body))
  .build

```

#### Open product type

```scala

case class UserData(userId: UUID, userName: String, userData: Map[String, String])

val userDataSchema = Schema.productBuilder[UserData]
  .name("UserData")
  .description("Data type for describing arbitrary metadata associated with a user")
  .addField(Field.primitive[UserData, UUID]("id", _.userId))
  .addField(Field.primitive[UserData, String]("name", _.userName))
  .additionalFields[String].primitive(_.userData)
  .construct(((id, name), af) => UserData(id, name, af))
  .build

```

### Translation to third party codecs/schemas

The principle motivation behind this project is to allow you to generate a single, 
comprehensive description of a data types that can inform a variety of different 
I/O tasks like serialization and documentation generation with guaranteed 
consistency.

In order to support this without making generic-schema itself responsible for 
serialization and API doc generation, it supports translation to type classes from 
other libraries that can be used for these purposes. Currently supported are 
json codecs from Circe and uPickle, as well as Tapir's own `Schema` type, which it 
uses to describe input and output values in its endpoint definitions.

#### Circe
```scala
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.circe.CirceSchemaTranslation.given
import io.circe.*
import io.circe.parser.*

given httpRequestCodec : Codec[HttpRequest] =
    SchemaTranslator.translate(httpRequestSchema)

val req = HttpRequest("http://example.url", Get, None)

println(req.asJson.noSpaces.toString)
// output:
// {"url":"http://example.url","method":"Get","string_body":[]}

parse("""{"url":"http://another.url","method":"Post","string_body":["hello world"]}""")
  .toOption.get.as[HttpRequest]
// result:
// HttpRequest("http://another.url", Post, Some("hello world"))

val userDataCodec : Codec[UserData] =
    SchemaTranslator.translate(userDataSchema)

val ud = UserData(
    UUID.randomUUID(),
    "john_doe",
    Map("interests" -> "skiing, golf", "height" -> "5\'2\"", "email" -> "john.doe@gmail.com")
)

println(ud.asJson.noSpaces.toString)
// output:
// {"id":"a2c51aa1-6951-40d6-9be8-2ed98fbaffe6","name":"john_doe","interests":"skiing, golf","height":"5\'2\"","email":"john.doe@gmail.com"}

parse("""{"pets":"cat, dog, hamster", "name":"jane_doe", "phone_number":"444-444-4444","id":""1cfde5d9-0876-4669-9131-09b3cdb8df4c 
}""")
  .toOption.get.as[UserData]
// result:
// UserData(1cfde5d9-0876-4669-9131-09b3cdb8df4c, "jane_doe", Map("pets" -> "cat, dog, hamster", "phone_number" -> "444-444-4444"))
```

#### Upickle
```scala
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.upickle.UPickleSchemaTranslation.given
import upickle.default.*

given httpRequestRW : ReadWriter[HttpRequest] =
    SchemaTranslator.translate(httpRequestSchema)

val req = HttpRequest("http://example.url", Get, None)

println(write(req))
// {"url":"http://example.url","method":"Get","string_body":[]}
// Note that by default uPickle serializes Options differently from circe

read[HttpRequest]("""{"url":"http://another.url","method":"Post","string_body":["hello world"]}""")
// result:
// HttpRequest("http://another.url", Post, Some("hello world"))

val userDataRW : ReadWriter[UserData] =
    SchemaTranslator.translate(userDataSchema)

val ud = UserData(
    UUID.randomUUID(),
    "john_doe",
    Map("interests" -> "skiing, golf", "height" -> "5\'2\"", "email" -> "john.doe@gmail.com")
)

println(write(ud))
// {"id":"a2c51aa1-6951-40d6-9be8-2ed98fbaffe6","name":"john_doe","interests":"skiing, golf","height":"5\'2\"","email":"john.doe@gmail.com"}

read[UserData]("""{"pets":"cat, dog, hamster", "name":"jane_doe", "phone_number":"444-444-4444","id":""1cfde5d9-0876-4669-9131-09b3cdb8df4c 
}""")
// result:
// UserData(1cfde5d9-0876-4669-9131-09b3cdb8df4c, "jane_doe", Map("pets" -> "cat, dog, hamster", "phone_number" -> "444-444-4444"))
```

#### Tapir schema

A single generic schema instance can derive both a tapir-supported
json codec and a tapir schema, making it easy to establish consistency 
between your REST API definition and your implementation.

```scala
import org.hungerford.generic.schema.translation.SchemaTranslator
import org.hungerford.generic.schema.tapir.TapirSchemaTranslation.given
import org.hungerford.generic.schema.circe.CirceSchemaTranslation.given

import io.circe.*

import sttp.tapir.{*, Schema as TapirSchema}
import sttp.tapir.json.circe.*

import sttp.tapir.openapi.OpenAPI
import sttp.tapir.docs.openapi.OpenAPIDocsInterpreter
import sttp.tapir.openapi.circe.yaml._

given httpRequestTapir : TapirSchema[HttpRequest] =
    SchemaTranslator.translate(httpRequestSchema)

// Needed for tapir's jsonBody input
given httpRequestCodec : Codec[HttpRequest] =
    SchemaTranslator.translate(httpRequestSchema)

// An imagined REST endpoint to remotely execute an http request
val remoteHttpEndpoint = endpoint
  .post
  .in("remote-http")
  .in(jsonBody[ HttpRequest ])

val remoteHttpSpec = OpenAPIDocsInterpreter()
  .toOpenAPI( remoteHttpEndpoint, "Transactions", "1.0" )
  .toYaml

println(remoteHttpSpec)
// output:
// ...
//  schemas:
//    HttpMethod:
//      type: string
//      description: Http method for a REST request
//      example: Get
//      enum:
//      - Get
//      - Post
//      - Put
//      - Delete
//    HttpRequest:
//      required:
//      - url
//      - method
//      type: object
//      properties:
//        url:
//          type: string
//          pattern: |
//            [-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)
//        method:
//          $ref: '#/components/schemas/HttpMethod'
//        string_body:
//          type: string
//      description: Simple REST request model
// ...

given userDataTapir : TapirSchema[UserData] =
    SchemaTranslator.translate(userDataSchema)

given userDataCodec : Codec[UserData] =
    SchemaTranslator.translate(userDataSchema)

// REST endpoint to get user data by id
val userDataEndpoint = endpoint
  .get
  .in("users" / path[UUID]("id") / "data")
  .out(jsonBody[UserData])

val userDataSpec = OpenAPIDocsInterpreter()
  .toOpenAPI( userDataEndpoint, "Transactions", "1.0" )
  .toYaml

println(userDataSpec)
// output:
// ...
//  schemas:
//    UserData:
//      required:
//      - id
//      - name
//      type: object
//      properties:
//        id:
//          type: string
//          format: uuid
//        name:
//          type: string
//      description: Data type for describing arbitrary metadata associated with a user
// ...
```
Note that tapir does not support open products with fixed fields, so the `UserData`
entry in the above open api spec is missing an `additionalProperties` schema.

### Automatic derivation

Schemas can be derived, or derived and then built

```scala
import org.hungerford.generic.schema.Default.dsl.*

val httpRequestSchema = Schema.derived[HttpRequest]

// OR

val httpRequestSchema = Schema.derivedBuilder[HttpRequest]
  .description("Http method for a REST request")
  .examples(Get, Post, Put, Delete)
  .build
```

Convert a derived product to an open product:

```scala
// Since the field "userData" will automatically be recognized as a regular
// product field, we need to update the schema to make it function as an
// open product
val userDataSchema = Schema.derivedBuilder[UserData]
  .removeField("userData")
  .additionalFields[String].primitive
  .construct(((id, name), af) => UserData(id, name, af))
  .build
```

### Easy modification of nested types

One motivating use case for this project is the difficulty of generating a custom 
schema for a type without much boilerplate and without having to include the schema 
library as a dependency when defining the code (e.g., using annotations to 
associate schema information with classes and their fields).

Using type safe component selectors, we can easily retrieve and update fields 
and subtypes, however deeply these may be nested:

```scala
import org.hungerford.generic.schema.Default.dsl.*

// Update the description and the singleton identifier of the Put
// subtype of HttpMethod (this will have the effect of serializing
// HttpRequest("someurl", Put, None) as {"url":"someurl","method":"Upsert"})
val updatedHttpRequestSchema =
    httpRequestSchema.modifyComponent("method" /~ "Put")(
        _.withDescription("Create or update a record")
          .withName("Upsert")
    )
    
// --- OR ---

// Same as above, but using index instead field and subtype name. 
val updatedHttpRequestSchema =
    httpRequestSchema.modifyComponent( 1 /~ 2)(
        _.withDescription("Create or update a record")
          .withName("Upsert")
    )

// --- OR ---

// Same as above, but select by type. 
val updatedHttpRequestSchema =
    httpRequestSchema.modifyComponent( t[HttpMethod] /~ t[Put] )(
        _.withDescription("Create or update a record")
          .withName("Upsert")
    )

// Retrieve the "method" field description from the new http request schema
// defined above, and extract its schema
val updatedHttpMethodSchema = updatedHttpRequestSchema("method").schema
```
