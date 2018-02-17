package fs2json

case class JawnParserFailure(message: String, cause: Option[Throwable] = None) extends RuntimeException(message, cause.orNull)
