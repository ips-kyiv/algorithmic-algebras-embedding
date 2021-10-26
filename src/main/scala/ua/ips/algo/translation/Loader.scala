package ua.ips.algo.translation

import scala.concurrent.*
import ua.ips.algo.*


trait Loader(target: Target) {

    /**
     * run prepare step after genration bundle.
     * This can be call of makefile or external build tools.
     * On error - complete returned future with exception.
     **/
    def prepare(signature: DataSortSignature, path: String, variant: Seq[String]): Future[Unit]

    /**
     * load generated code.
     *@param signature - signature of schema which was compiled into target.
     *@param path - path, where compiled output bundle was build.
     *@param variant - variant of choosen set of optimizations.
     *@return interpretation, which can interpret the main schema of the bundle.
     **/
    def load(signature: DataSortSignature, path: String, variant: Seq[String]): Interpretation 

}
