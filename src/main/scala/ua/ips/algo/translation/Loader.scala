package ua.ips.algo.translation

import ua.ips.algo._

trait Loader(val target: Target) {

    /**
     * load generated code.
     *@param signature - signature of schema which was compiled into target.
     *@param path - path, where compiled output bundle was build.
     *@return interpretation, which can interpret the main schema of the bundle.
     **/
    def load(signature: DataSortSignature, path: String): Interpretation 

}
