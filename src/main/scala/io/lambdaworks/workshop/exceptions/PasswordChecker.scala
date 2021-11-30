package io.lambdaworks.workshop.exceptions

object PasswordChecker {

  def validate(password: String): Either[List[Throwable], String] = {
    val passwordCheck1 = minNumberOfChars(password, 5)
    val passwordCheck2 = containsUpperCase(password)
    val passwordCheck3 = containsLowerCase(password)
    val passwordCheck4 = containsNumber(password)
    val checkerList    = List(passwordCheck1, passwordCheck2, passwordCheck3, passwordCheck4)
    val errorList = checkerList
      .collect({ case Left(x) => x })
    if (errorList.nonEmpty) Left(errorList)
    else Right(password)
  }

  private def minNumberOfChars(password: String, length: Int): Either[Throwable, String] =
    if (password.length < length)
      Left(InvalidLength)
    else
      Right(password)

  private def containsUpperCase(password: String): Either[Throwable, String] = {
    val hasUpperCase = password.exists(c => c.isUpper)
    if (hasUpperCase)
      Right(password)
    else
      Left(MissingUppercase)
  }

  private def containsLowerCase(password: String): Either[Throwable, String] = {
    val hasLowerCase = password.exists(c => c.isLower)
    if (hasLowerCase)
      Right(password)
    else
      Left(MissingLowercase)
  }

  private def containsNumber(password: String): Either[Throwable, String] = {
    val hasDigit = password.exists(c => c.isDigit)
    if (hasDigit)
      Right(password)
    else
      Left(MissingNumber)
  }

  object InvalidLength    extends Throwable("Password must contain at least 5 characters.")
  object MissingUppercase extends Throwable("Password must contain uppercase letter.")
  object MissingLowercase extends Throwable("Password must contain lowercase letter.")
  object MissingNumber    extends Throwable("Password must contain number.")

}
