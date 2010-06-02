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

class ExecEnvironment(val wd: java.io.File, val envs : List[Env]) {
    def execute(cmd : String*) = {
        val pb = new ProcessBuilder(cmd: _*)
        for (e <- envs)
            pb.environment.put(e.key, e.value)
        pb.directory(wd)
        pb.redirectErrorStream(true)
        val process = pb.start()
        val instream = new java.io.DataInputStream(process.getInputStream())
        var istr : String = null
        do {
            istr = instream.readLine
            if (istr != null) {
                println(istr)
            }
        } while (istr != null)
    }

    def file(cmd : String) = (new File(wd, cmd))
}

class Runner(val target : Target) {
    def run(force : Boolean) = {
        if (!target.srcdir.exists) 
            throw new Exception("Source directory does not exists: "
                + target.srcdir)
        if (!target.dstdir.exists)
            target.dstdir.mkdirs
        
        if (target.srcdir.file("configure").exists && force)
           target.srcdir.file("configure").delete 
        if (!target.srcdir.file("configure").exists)
            target.srcdir.execute("./buildconf", "--force")

        val procCount = Runtime.getRuntime.availableProcessors
        val opts = (List(target.srcdir.getAbsolutePath + "/configure")
            ::: target.opts)
        val opta = opts.toArray[String]
        target.dstdir.execute("make", "clean")
        target.dstdir.execute(opta : _*) 
        target.dstdir.execute("make", "-j", procCount.toString)
    }

    implicit def fileToExec(dir: java.io.File) : ExecEnvironment =
        new ExecEnvironment(dir, target.env)
}

// vim: set ts=4 sw=4 et:
