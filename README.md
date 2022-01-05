# Generic Schema

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
        .singleton()
        .fromSuper({ case Delete => Some(Delete); case _ => None })
        .build
      )
  .build
```

#### Product type

```scala
case class HttpRequest(url: String, method: HttpMethod, body: Option[String])

// fully typed given instance of HttpMethod schema defined above
import httpMethodSchema.givenSchema

val httpRequestSchema = Schema.productBuilder[HttpRequest]
  .name("HttpRequest")
  .description("Simple REST request model")
  .addField(
      Field.builder[String]
        .fieldName("url")
        .description("Destination URL for REST request")
        .examples("http://foo.bar.org:8080", "https://host.name/endpoint")
        .primitive
        .validate(Validator.regex("""[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)
"""))
        .build
  )
  .addField(
      Field.builder[HttpMethod]
        .fieldName("method")
        .fromSchema // uses given instance imported from httpMethodSchema above
        .build
  )
  .addField(
      Field.builder[Option[String]]
        .fieldName("string_body")
        .primitive
        .description("Optional body of the REST request")
        .examples(None, Some("""{"json":"payload"}"""), Some("Simple text body"))
        .build
  )
  .construct((url, method, body) => HttpRequest(url, method, body))
  .deconstruct(req => (req.url, req.method, req.body))
  .build

```

#### Open product type

```scala

case class UserData(userId: UUID, userName: String, userData: Map[String, String])

val userDataSchema = Schema.productBuilder[UserData]
  .name("UserData")
  .description("Data type for describing arbitrary metadata associated with a user")
  .addField(Field.primitive[UUID]("id"))
  .addField(Field.primitive[String]("name"))
  .additionalFields[String].primitive
  .construct(((id, name), af) => UserData(id, name, af))
  .deconstruct(ud => ((ud.userId, ud.userName), ud.userData))
  .build

```

### Translation to third party codecs/schemas

#### Circe

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

read[HttpRequest]("""{"url":"http://another.url","method":"Post","string_body":["hello world"]}""")
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
// UserData(1cfde5d9-0876-4669-9131-09b3cdb8df4c, "jane_doe", Map("pets" -> "cat, dog, hamster", "phone_number" -> "444-444-4444"))


```