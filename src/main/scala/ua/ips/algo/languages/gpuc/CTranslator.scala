package ua.ips.algo.languages.gpuc

import ua.ips.algo._

trait CTranslator {

  case class InputOutput(
    inputs: IndexedSeq[InputSchema],
    outputs: IndexedSeq[OutputSchema]
  )

  def translate(schema: Schema, flavor: CBase): C_Ast =
     ???




}

