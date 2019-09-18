package lambda

fun <A> identity(a: A) = a

sealed class Either<L, R> {
    data class Left<L, R>(val value: L) : Either<L, R>()
    data class Right<L, R>(val value: R) : Either<L, R>()

    fun <T, U> bimap(l: (L) -> U, r: (R) -> T): Either<U, T> = when (this) {
        is Left -> Left(l(value))
        is Right -> Right(r(value))
    }

    fun <T> map(r: (R) -> T): Either<L, T> = bimap(::identity, r)

    fun <T> flatMap(r: (R) -> Either<L, T>): Either<L, T> = when (this) {
        is Left -> Left(value)
        is Right -> r(value)
    }

    fun <T> fold(l: (L) -> T, r: (R) -> T): T = when (this) {
        is Left -> l(value)
        is Right -> r(value)
    }
}