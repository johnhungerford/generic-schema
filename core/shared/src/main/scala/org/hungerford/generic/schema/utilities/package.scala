package org.hungerford.generic.schema

import org.hungerford.generic.schema.utilities.{IsomorphismDsl, LensDsl}

trait UtilitiesDsl extends IsomorphismDsl with LensDsl

package object utilities { object dsl extends UtilitiesDsl }
