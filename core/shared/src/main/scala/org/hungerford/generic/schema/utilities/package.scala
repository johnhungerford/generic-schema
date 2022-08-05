package org.hungerford.generic.schema

import org.hungerford.generic.schema.utilities.{IsomorphismDsl, LensDsl, ValidationDsl}

trait UtilitiesDsl extends IsomorphismDsl with LensDsl with ValidationDsl

package object utilities { object dsl extends UtilitiesDsl }
