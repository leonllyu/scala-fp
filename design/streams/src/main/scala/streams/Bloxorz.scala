package streams

/**
 * A main object that can be used to execute the Bloxorz solver
 */
object Bloxorz extends App {

  /**
   * A level constructed using the `InfiniteTerrain` trait which defines
   * the terrain to be valid at every position.
   */
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(1,3)
    val goal = Pos(5,8)
  }

  println(InfiniteLevel.solution)

  /**
   * A simple level constructed using the StringParserTerrain
   */
  abstract class Level extends Solver with StringParserTerrain

  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  println(Level0.solution)

  /**
   * Level 1 of the official Bloxorz game
   */
  object Level1 extends Level {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  println(Level1.solution)

  /**
   * Level 2 of the official Bloxorz game
   */
  object Level2 extends Level {
    val level =
     """------oooo--ooo
        oooo--oooo--oTo
        oooo--oooo--ooo
        oooo--oooo--ooo
        oSooooooooooooo
        oooo--oooo""".stripMargin
  }

  println(Level2.solution)
  
  /**
   * Level 3 of the official Bloxorz game
   */
  object Level3 extends Level {
    val level =
     """------ooooooo
        oooo--ooo--oo
        ooooooooo--oooo
        oSoo-------ooTo
        oooo-------oooo
        ------------ooo
        """.stripMargin
  }

  println(Level3.solution)
  
  /**
   * Level 4 of the official Bloxorz game
   * // . means it's light, cannot bear block standing on it.
   */
  object Level4 extends Level {
    val level =
     """---.......
        ---.......
        oooo-----ooo
        ooo-------oo
        ooo-------oo
        oSo--oooo.....
        ooo--oooo.....
        -----oTo--..o.
        -----ooo--....""".stripMargin
  }

  println(Level4.solution)
  
  /**
   * Level 5 of the official Bloxorz game
   */
  object Level5 extends Level {
    val level =
     """-----------oooo
        -ooooooooooooSo
        -oooo-------ooo
        -oooo
        -oooo
        ---oooooooooo
        ----------ooooo
        ooo-------ooooo
        oTooooooooooo
        oooo
        """.stripMargin
  }

  println(Level5.solution)
  
  /**
   * Level 6 of the official Bloxorz game
   */
  object Level6 extends Level {
    val level =
       """-----oooooo
          -----o--ooo
          -----o--ooooo
          Sooooo-----oooo
          ----ooo----ooTo
          ----ooo-----ooo
          ------o--oo
          ------ooooo
          ------ooooo
          -------ooo
          """.stripMargin
  }

  println(Level6.solution)
  
  /**
   * Level 7 of the official Bloxorz game
   */
  object Level7 extends Level {
    val level =
       """--------oooo
          --------oooo
          ooo-----o--oooo
          oSooooooo---oTo
          ooo----ooo--ooo
          ooo----ooo--ooo
          -ooo---o
          --oooooo
          """.stripMargin
  }

  println(Level7.solution)
  
  /**
   * Level 8 of the official Bloxorz game
   */
  object Level8 extends Level {
    val level =
     """-oooo
        -oToo
        -ooo
        -o---oooooo
        -o---oo--oo
        Soooooo--ooo
        -----o-----o
        -----oooo--o
        -----ooooooo
        --------ooo
        """.stripMargin
  }

  println(Level8.solution)
  
  /**
   * Level 9 of the official Bloxorz game
   */
  object Level9 extends Level {
    val level =
     """ooo.oooo.oooo
        oo--------ooo
        oo---------ooo
        ooo---ooo--oSo
        ooo...oTo--ooo
        ooo--.ooo--o
        --o--.....oo
        --ooo..o...
        ---oo......
        ---ooo--oo  """.stripMargin
  }

  println(Level9.solution)  
}


