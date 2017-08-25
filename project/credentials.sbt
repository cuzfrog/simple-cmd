credentials ++= {
  val user = System.getenv("SONATYPE_USER")
  val password = System.getenv("SONATYPE_PASS")
  if (user == null || password == null) Nil
  else Seq(
    Credentials("Sonatype Nexus Repository Manager",
      "oss.sonatype.org", user, password)
  )
}