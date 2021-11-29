package io.lambdaworks.workshop.exceptions

object PasswordChecker {

  def validate(password: String): Either[List[Throwable], String] = {
    val passwordCheck1 = minNumberOfChars(password, 5)
    val passwordCheck2 = containsUpperCase(password)
    val passwordCheck3 = containsLowerCase(password)
    val passwordCheck4 = containsNumber(password)
    val checkerList    = List(passwordCheck1, passwordCheck2, passwordCheck3, passwordCheck4)
    val errorList = checkerList
      .filter(c =>
        c match {
          case Left(value)  => true
          case Right(value) => false
      })
      .map(c => c.left.get)
    if (errorList.nonEmpty) Left(errorList)
    else Right(password)
  }

  private def minNumberOfChars(password: String, length: Int): Either[Throwable, String] = {
    if (password.length < length)
      Left(InvalidLength)
    else
      Right(password)
  }

  private def containsUpperCase(password: String): Either[Throwable, String] = {
    val upperCases = password.filter(c => c.isUpper)
    if (upperCases.length > 0)
      Right(password)
    else
      Left(MissingUppercase)
  }

  private def containsLowerCase(password: String): Either[Throwable, String] = {
    val lowerCases = password.filter(c => c.isLower)
    if (lowerCases.length > 0)
      Right(password)
    else
      Left(MissingLowercase)
  }

  private def containsNumber(password: String): Either[Throwable, String] = {
    val digits = password.filter(c => c.isDigit)
    if (digits.length > 0)
      Right(password)
    else
      Left(MissingNumber)
  }

  object InvalidLength    extends Throwable("Password must contain at least 5 characters.")
  object MissingUppercase extends Throwable("Password must contain uppercase letter.")
  object MissingLowercase extends Throwable("Password must contain lowercase letter.")
  object MissingNumber    extends Throwable("Password must contain number.")

}
