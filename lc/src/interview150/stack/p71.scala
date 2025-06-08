package p71

object Solution {
  def simplifyPath(path: String): String = {
    path
      .split("/")
      .foldLeft(List.empty[String]) {
        case (ls, ".")       => ls
        case (Nil, "..")     => Nil
        case (_ :: tl, "..") => tl
        case (ls, "")        => ls
        case (ls, part)      => part :: ls
      }
      .reverse
      .mkString("/", "/", "")
  }
}
