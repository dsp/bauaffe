/*
 * This file is part of Bauaffe
 *
 * Bauaffe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 2 of the License
 *
 * Bauaffe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Bauaffe.  If not, see <http://www.gnu.org/licenses/>.
 */
import java.io.File
import java.io.FileReader
import scala.util.parsing.combinator._

case class Env(val key: String, val value: String)
case class Define(val key: String, val value: String)
case class DefineBlock(val key: String, val value: List[Any])
case class BuildConfiguration(val name: String, val branchdir: String,
    val buildir : String, val buildopts : List[Any]) {
    name match {
        case "all" => throw new Exception("all is not allowed as a target name")
        case "list" => throw new Exception("list is not allowed as a target name")
        case _ =>
    }
}
class Target(val name : String, val srcdir : File, val dstdir: File,
    val opts : List[String], val env : List[Env])

case class Configuration(val name: String, val configs : List[Any]) {
    var sourcedir = ""
    var buildir = ""

/* TODO: fix dirty hack */
    val cfgs = configs.flatMap(
        c => c match {
            case t : List[_] => t
            case _ => List(c)
        }
    )

    for (c <- cfgs) {
        c match {
            case Define("source", v) => sourcedir = v
            case Define("build", v) => buildir= v
            case _ =>
        }
    }

    val blist = cfgs.filter(    
        c => c.isInstanceOf[BuildConfiguration]).
                asInstanceOf[List[BuildConfiguration]]

    private def opts(l : List[Any]) = {
        l.filter(c=>c.isInstanceOf[String]).
            asInstanceOf[List[String]]
    }

    private def env(l : List[Any]) = {
        l.filter(c=>c.isInstanceOf[Env]).asInstanceOf[List[Env]]
    }

    def targets : List[Target] = {
        blist.map(c => 
            new Target(c.name, new File(sourcedir + '/' + c.branchdir),
                new File(buildir + '/' + c.buildir),
                    opts(c.buildopts), env(c.buildopts)))
    }
}

class ConfigurationParser(file: String) extends JavaTokenParsers {
    val f = new FileReader(file)
    var defaults = new DefineBlock("defaults", List())

/* PARSER {{{ */
    def begin : Parser[Configuration] =
        "begin" ~ ("default" | stringLiteral) ~ "configuration" ~ rep(define | build) ^^ {
        case "begin"~name~"configuration"~confs => new Configuration(name, confs)
    }
    def define : Parser[Any] =
        "define" ~> (("source" | "build") ~ stringLiteral | "defaults" ~ defblck) ^^ {
        case name~list => {
            list match {
                case value : String => new Define(name, fixparse(value))
                case list : List[_] =>
                    val d = new DefineBlock("defaults", list);
                    defaults = d ; d
            }
        }
    }
    def defblck : Parser[List[Any]] =
        "as" ~> buildconfig
    def optlist : Parser[String] =
        ("enable" ~ stringLiteral | "with" ~ stringLiteral | "disable" ~ stringLiteral) ^^ {
        case k~v => "--" + k + "-" + fixparse(v)
    }
    def build : Parser[List[BuildConfiguration]] =
        "build" ~> ("trunk" | "branch" ~ stringLiteral) ~ "as" ~ rep(buildlist) ^^ {
        case "branch"~branch~"as"~buildlist =>
            gen("branches/" + fixparse(branch), buildlist)
        case "trunk"~"as"~buildlist =>
            gen("trunk", buildlist)
    } 
    def buildlist : Parser[(String, List[Any])] =
        stringLiteral ~ "using" ~ ("defaults" ~ opt(buildconfig) | buildconfig) ^^ {
        case buildir~"using"~("defaults"~config) => {
            config match {
                case None => (fixparse(buildir), defaults.value)
                case Some(list: List[_]) => (fixparse(buildir),
                    defaults.value ::: list)
            }
        }
        case buildir~"using"~config => {
            config match {
                /* TODO: fix config list */
                case list : List[_] => (fixparse(buildir), list)
            }
        }
    }
    def buildconfig : Parser[List[Any]] =
        rep(optlist | envlist) 
    def envlist : Parser[Any] =
        "environment" ~> env~"="~stringLiteral ^^ {
        case key~"="~value => new Env(key, value)
    }
    def env : Parser[String] =
        """[A-Za-z\-_]*""".r
/* }}} PARSER */

    def parse : Configuration =
        parseAll(begin, f) match {
            case Success(e, _) => e
            case f: NoSuccess => throw new Exception("Failed " + f)
        }

    private def fixparse(a: Any) = {
        val s = "" + a
        s.substring(1, s.length() - 1)
    }

    private def gen(branch: String,
        buildlist: List[Tuple2[String, List[Any]]]) : List[BuildConfiguration] = {
        /* okay let's create our list of build configs from the buildir and buildopts
           give in the config */
        buildlist.map(
            (tuple : Tuple2[String, List[Any]]) =>  {
                val (buildir, buildopts) = tuple
                new BuildConfiguration(buildir, branch, buildir, buildopts)
            }
        )
    }
}

// vim: set ts=4 sw=4 et:
