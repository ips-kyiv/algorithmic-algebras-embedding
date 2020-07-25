package ua.ips.algo


object StandardBase:

  lazy val instance = SchemaBase(
                   sorts = Set(
                     IntBasicRep.dataSort,
                     BooleanBasicRep.dataSort,
                   ),
                   signatures = Set.empty
                 )

  def apply() = instance



