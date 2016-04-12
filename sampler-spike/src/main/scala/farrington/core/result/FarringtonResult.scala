package farrington.core.result

case class FarringtonResult(
    results: ResultVector,
    flags: IndexedSeq[Int]
)

case class FarringtonResult2(
    results: IndexedSeq[Result],
    flags: IndexedSeq[Int]
)