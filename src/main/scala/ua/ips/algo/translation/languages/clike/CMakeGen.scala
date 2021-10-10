package ua.ips.algo.translation.languages.clike


object CMakeGen:

   def lib(name: String): StringLob =
    val tmpl = 
    s"""
    cmake_minimum_required(VERSION 3.10)

    project(${name})

    add_library($name SHARED ${name}.c) 
    """.stripMargin
    StringLob(tmpl)
