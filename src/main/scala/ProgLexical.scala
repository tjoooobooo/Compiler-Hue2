object ProgLexical {


  private def matchRegex(r : Regex): Option[String] = {
    r.findPrefixMatchOf(input.subSequence)
    case Some(matchedMatch) =>
      val res = Some(input.subSequence(actOffset, input.length())) match {}
  }
}
