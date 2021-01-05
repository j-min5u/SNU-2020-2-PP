package pp202002.project.impl

object Prime {
  // Problem 4: implement nthPrime.
  // nthPrime should return a function that calculates n-th prime number.
  val nthPrime: String =
    """
      |
      |(let ((def nthprime (n)
      |(let
      |((def isprime (k i)
      |(match (= k i)
      |((_) (match (= (% k i) 0)
      |((_) (app isprime k (+ i 1)))
      |((_) (inl 0))))
      |((_) (inr 0))))
      |(def pfn (kk)
      |(match (app isprime kk 2)
      |((_) (app pfn (+ kk 1)))
      |((_) kk)))
      |(def nprimerec (p num kkk)
      |(match (= p num)
      |((_) (app nprimerec (+ p 1) num (app pfn (+ kkk 1))))
      |((_) kkk))))
      |(app nprimerec 0 n 1))))
      |nthprime)
      |
      |""".stripMargin
}


