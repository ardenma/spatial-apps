import spatial.dsl._

@spatial object PGMChain extends SpatialApp {


  def main(args: Array[String]): Void = {
    val num_inferences = 2

    // Simple BN chain: A <- B <- C
    val p_a_b_dist = List(
      List(0.1, 0, 0),
      List(0.9, 1, 0),
      List(0.5, 0, 1),
      List(0.5, 1, 1)
    )
    val p_a_b_idxs = List(
      List(0, 0),
      List(1, 0),
      List(0, 1),
      List(1, 1)
    )

    val p_b_c_dist = List(
      List(0.8, 0, 0),
      List(0.2, 1, 0),
      List(0.3, 0, 1),
      List(0.7, 1, 1)
    )
    val p_b_c_idxs = List(
      List(0, 0),
      List(1, 0),
      List(0, 1),
      List(1, 1)
    )

    val p_c_dist = List(
      List(0.6, 0),
      List(0.4, 1)
    )
    val p_c_idxs = List(
      List(0),
      List(1)
    )
    
    // TODO: figure out how to encode arbitrary RV values... use a map? Ideal: Map[Float][Int]
    // NOTE: encoding table size will grow O(n^2) based on number of conditional variables

    val a_vals = List(0, 1)
    val b_vals = List(0, 1)
    val c_vals = List(0, 1)

    // Generate input data (indices corresponding to values of A)
    val input_data = Array.tabulate[Int](num_inferences){i => (i % 2).to[Int]}
    
    // First let's compute P(A = 1)
    val input = DRAM[Int](num_inferences)  // Int corresponding to the array index in p(a) of the val
    val output = DRAM[Float](num_inferences)
    
    // Transfer data to memory
    setMem(input, input_data)

    Accel {
      // Use Scala lists defined above to populate LUT
      val p_a_b = LUT[Float](4,3)(p_a_b_dist.flatten.map(_.to[Float]):_*)
      val p_b_c = LUT[Float](4,3)(p_b_c_dist.flatten.map(_.to[Float]):_*)
      val p_c = LUT[Float](2,2)(p_c_dist.flatten.map(_.to[Float]):_*)

      // Creat index LUTs
      val p_a_b_i = LUT[Int](4,2)(p_a_b_idxs.flatten.map(_.to[Int]):_*)
      val p_b_c_i = LUT[Int](4,2)(p_b_c_idxs.flatten.map(_.to[Int]):_*)
      val p_c_i = LUT[Int](2,1)(p_c_idxs.flatten.map(_.to[Int]):_*)    
      
      // Marginal distributions
      val p_b = SRAM[Float](2,2)
      val p_a = SRAM[Float](2,2)

      // Loop over each element in SRAM, initializing to 0
      Foreach(2 by 1, 2 by 1){(i,j) => 
        p_b(i,j) = 0
        p_a(i,j) = 0
      }

      // Iterate over each row to marginalize over C
      Foreach(4 by 1){i =>
        var p: Float = p_b_c(i, 0)
        var b: Float = p_b_c(i, 1)
        var c: Float = p_b_c(i, 2)
        var b_idx: Int = p_b_c_i(i, 0)
        var c_idx: Int = p_b_c_i(i, 1)
        p_b(b_idx, 0) = p_b(b_idx, 0) + (p * p_c(c_idx, 0))  // need fast lookup based on value of c and b, this only works because c and b can only be 0 or 1
        p_a(b_idx, 1) = b
      }
   
      // Iterate over each row to marginalize over B
      Foreach(4 by 1){i =>
        var p: Float = p_a_b(i, 0)
        var a: Float = p_a_b(i, 1)
        var b: Float = p_a_b(i, 2)
        var a_idx: Int = p_a_b_i(i, 0)
        var b_idx: Int = p_a_b_i(i, 1)
        p_a(a_idx, 0) = p_a(a_idx, 0) + (p * p_b(b_idx, 0))  // need fast lookup based on value of a and b, this only works because a and b can only be 0 or 1
        p_a(a_idx, 1) = a
      }

      Foreach(2 by 1, 2 by 1){(i,j) => 
        println(r"P(a) is ${p_a(i,j)}")
        println(r"P(b) is ${p_b(i,j)}")
      }

      // Create local SRAM
      val x = SRAM[Int](num_inferences)
      x load input      
      
      // Populating output
      val out = SRAM[Float](num_inferences)
      Foreach(num_inferences by 1){i =>
        out(i) = p_a(x(i), 0)
      }

      // Saving to output
      output store out
    }
    // Report the answer
    println(r"Result is ${getArray(output)}")
  }
}