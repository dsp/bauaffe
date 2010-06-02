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
object Main {
    val Version = "3.0.0a2"
    val Name = "Bauaffee"

    def main(args : Array[String]) {
        val wanthelp = args.filter(s => s == "-h" || s == "--help").size > 0
        if (wanthelp) {
            help
            exit()
        }

        val quiet = args.filter(s => s == "-v").size == 0
        val force = args.filter(s => s == "-f").size > 0
        val verbose = ((s : String) => if (!quiet) println(s))

        verbose("parsing configuration")

        val home = System.getProperty("user.home")
        val config = if (home == null || !(new java.io.File(home + "/.bauaffe")).exists)
                        new ConfigurationParser("bauaffe.conf")
                     else
                        new ConfigurationParser(home + "/.bauaffe")

        verbose("generating targets")
        val targets = config.parse.targets

        val target = if (args.length > 0) args(0) else "help"

        def runcl (t : Target) = {
            verbose("build " + t.name)
            (new Runner(t)).run(force)
        }
        target match {
            case "help" =>
                help
            case "all" => 
                banner
                verbose("start requested builds")
                targets.foreach(t => runcl(t))
            case "list" =>
                printf("%-30s %s\n", "TARGET", "LAST BUILD")
                targets.foreach((t:Target) =>
                        printf("%-30s %s\n", t.name, lastbuild(t))
                )
            case tname: String =>
                banner
                verbose("start requested builds")
                val tlist = targets.filter((t:Target) => t.name == tname)
                if (tlist.size == 0)
                    println("No targets found")
                else 
                    tlist.foreach(t => runcl(t))
        }
    }

    def lastbuild(t:Target) = {
        val f = new java.io.File(t.dstdir + "/Makefile")
        if (f.lastModified != 0)
            new java.util.Date(f.lastModified)
        else
            None
    }

    def help = {
        banner
        println("Commands:")
        println("    list      List all targets")
        println("    all       Build all targets")
        println("    help      Show help")
        println("    [target]  Build target named [target]")
    }

    def banner = {
        println(Name + " " + Version)
        println("    configure and build multiple targets of the PHP source tree")
        println("    Licensed under the terms of the BSD License")
        println("    (c) 2010 David Soria Parra")
        println("")
    }    
}

// vim: set ts=4 sw=4 et:
