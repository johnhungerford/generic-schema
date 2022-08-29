package org.hungerford.generic.schema

import org.hungerford.generic.schema.utilities.{LensDsl, MigrationDsl, ValidationDsl}

trait UtilitiesDsl extends MigrationDsl with LensDsl with ValidationDsl

package object utilities { object dsl extends UtilitiesDsl }
