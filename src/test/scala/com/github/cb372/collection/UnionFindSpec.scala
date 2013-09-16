package com.github.cb372.collection

import org.scalatest._
import org.scalatest.matchers._

class UnionFindSpec extends FunSpec with ShouldMatchers {

  describe("UnionFind") {
    
    describe("empty") {
      val uf = UnionFind[Int]()

      it("should have size 0") {
        uf should have size(0) 
      }
    
      describe("find") {
        it("should return None") {
          val (group, uf2) = uf.find(123)
          group should be(None)
          uf2 should be(uf)
        }
      }

      describe("union") {
        it("should complain that it cannot find the given elements") {
          intercept[IllegalArgumentException] {
            uf.union(123, 456)
          }
        }
      }
    }

    describe("freshly initialised") {
      val uf = UnionFind(1,2,3,4,5)
      
      it("should have size equal to number of elements") {
        uf should have size(5)
      }

      describe("find") {
        it("should return the element that you give it") {
          for (i <- 1 to 5) {
            val (group, uf2) = uf.find(i)
            group should be(Some(i))
            uf2 should be(uf)
          }
        }
      }

      describe("union") {
        val uf2 = uf.union(1, 2)
      
        it("should reduce the number of groups by 1") {
          uf2 should have size(4)
        }

        it("should arbitrarily choose one argument to be the root") {
          if (uf2.find(1)._1.get == 1) {
            uf2.find(2)._1 should be(Some(1))
          } else {
            uf2.find(2)._1 should be(Some(2))
          }
        }
      }
      
    }

    describe("after a few unions") {
      val uf = UnionFind(1,2,3,4,5).union(1,2).union(3,2).union(4,5).union(4,1)

      it("should have size equal to number of groups") {
        uf should have size(1)
      }

      describe("find") {
        it("should return the representative of the group") {
          val (group, _) = uf.find(1)
          for (i <- 2 to 5) {
            val (g, _) = uf.find(i)
            g should be(group)
          }
        }
      }

      describe("union") {
        val uf2 = uf.union(1, 2)
      
        it("should not reduce the number of groups") {
          uf2 should have size(1)
        }
      }
      
    }
  
  }


}
