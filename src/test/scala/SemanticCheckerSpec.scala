import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable.ListBuffer
import testUtils.{assertTypeIdEquals}

class SemanticCheckerSpec extends AnyFlatSpec {
    var st: SymbolTable = SymbolTable()
    var log: ListBuffer[WaccError] = ListBuffer()
    var node: ASTNode = IntTypeNode()((0,0))

    def resetNode(n: ASTNode): Unit = {
        this.st = SymbolTable()
        this.log = ListBuffer()
        this.node = n
    }

    behavior of "semantic check of <type> nodes"
    it should "correctly check <base-type> node types" in {
        resetNode(IntTypeNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

        resetNode(BoolTypeNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(BoolType()), node.typeId, ListBuffer(), log)

        resetNode(CharTypeNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(CharType()), node.typeId, ListBuffer(), log)
        
        resetNode(StringTypeNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(StringType()), node.typeId, ListBuffer(), log)
    }
    it should "correctly check <pair-type> node types" in {
        // pair of 2 base types
        resetNode(PairTypeNode(IntTypeNode()((0,0)), 
                                BoolTypeNode()((0,0)))((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(PairType(IntType(), BoolType())), node.typeId,
                            ListBuffer(), log)

        // pair with nested pairs
        resetNode(PairTypeNode(PairElemTypePairNode()((0,0)), 
                                PairElemTypePairNode()((0,0)))((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(PairType(NestedPairType(), NestedPairType())),
                            node.typeId, ListBuffer(), log)

        // pair containing array type
        resetNode(PairTypeNode(ArrayTypeNode(IntTypeNode()((0,0)), 3)((0,0)),
                                StringTypeNode()((0,0)))((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(PairType(ArrayType(IntType(), 3), 
                                        StringType())),
                            node.typeId, ListBuffer(), log)
    }
    it should "correctly check <array-type> node types" in {
        // 1-dimensional char array type
        resetNode(ArrayTypeNode(CharTypeNode()((0,0)), 1)((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(ArrayType(CharType(), 1)), node.typeId,
                            ListBuffer(), log)
        
        // nested array type
        resetNode(ArrayTypeNode(BoolTypeNode()((0,0)), 10)((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(ArrayType(BoolType(), 10)), node.typeId,
                            ListBuffer(), log)

        // array containing pair type
        resetNode(ArrayTypeNode(PairTypeNode(ArrayTypeNode(
                                                IntTypeNode()((0,0)), 1)((0,0)),
                                        IntTypeNode()((0,0)))((0,0)), 2)((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(ArrayType(PairType(ArrayType(IntType(), 1),
                                                    IntType()), 2)),
                            node.typeId, ListBuffer(), log)
    }
    
    behavior of "semantic check of literals"
    it should "correctly check integer values" in {
        resetNode(IntLiterNode(3)((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)
    }
    it should "correctly check boolean values" in {
        resetNode(BoolLiterNode(true)((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(BoolType()), node.typeId, ListBuffer(), log)
    }
    it should "correctly check character literals" in {
        resetNode(CharLiterNode('p')((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(CharType()), node.typeId, ListBuffer(), log)
    }   
    it should "correctly check string literals" in {
        resetNode(StringLiterNode("some string")((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(StringType()), node.typeId, ListBuffer(), log)
    }
    it should "correctly check pair literals" in {
        resetNode(new PairLiterNode()((0,0)))
        node.check(st, log)
        assertTypeIdEquals(Some(NullPairType()), node.typeId, ListBuffer(), log)
    }
        
    behavior of "semantic check of variables"
    it should "get the type of variables in the current scope" in {
        resetNode(IdentNode("var1")((0,0)))
        st.add("var1", Variable(IntType()))
        node.check(st, log)
        assertTypeIdEquals(Some(Variable(IntType())), node.typeId, 
                        ListBuffer(), log)
    }
    it should "search for variables in the outer scope" in {
        resetNode(IdentNode("_var_2")((0,0)))
        val innerST = SymbolTable(st)
        st.add("_var_2", Variable(PairType(StringType(), IntType())))
        node.check(innerST, log)
        assertTypeIdEquals(Some(Variable(PairType(StringType(), IntType()))),
                            node.typeId, ListBuffer(), log)
    }
    it should "search for variables in nested scopes" in {
        resetNode(IdentNode("nested")((0,0)))
        val innerST = SymbolTable(st)
        val innerMostST = SymbolTable(innerST)
        st.add("nested", Variable(IntType()))
        node.check(innerMostST, log)
        assertTypeIdEquals(Some(Variable(IntType())), node.typeId, ListBuffer(),
                            log)
    }
    it should "get the type of the variable in the smallest scope" in {
        resetNode(IdentNode("inTwoPlaces")((0,0)))
        val innerST = SymbolTable(st)
        st.add("inTwoPlaces", Variable(BoolType()))
        innerST.add("inTwoPlaces", Variable(CharType()))
        node.check(innerST, log)
        assertTypeIdEquals(Some(Variable(CharType())), node.typeId, 
                            ListBuffer(), log)
    }
    it should "produce an error for variables not in scope" in {
        resetNode(IdentNode("id_2")((0,0)))
        node.check(st, log)
        assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((0,0), 
                                    "id_2 has not been defined in this scope")),
                            log)
    }
    it should "not get the type of variables in a smaller scope" in {
        resetNode(IdentNode("cannot_be_accessed")((1,1)))
        val innerST = SymbolTable(st)
        innerST.add("cannot_be_accessed", Variable(NullPairType()))
        node.check(st, log)
        assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((1,1),
                    "cannot_be_accessed has not been defined in this scope")),
                    log)
    }
    it should "not get the type of variables in an unenclosed scope" in {
        resetNode(IdentNode("inAnotherWorld")((0,1)))
        val thisST = SymbolTable(st)
        val otherST = SymbolTable(st)
        otherST.add("inAnotherWorld", Variable(StringType()))
        node.check(thisST, log)
        assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((0,1),
                        "inAnotherWorld has not been defined in this scope")),
                        log)
    }

    // behavior of "semantic check of unary operations"
    // it should "correctly verify the argument type passed in for '!'" in {
    //     // bool literal
    //     resetNode(Not(BoolLiterNode(false)((0,0)))((0,0)))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(BoolType()), node.typeId, ListBuffer(), log)

    //     // boolean variable
    //     resetNode(Not(IdentNode("a_bool")((0,0)))((0,0)))
    //     st.add("a_bool", Variable(BoolType()))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(BoolType()), node.typeId, ListBuffer(), log)
    // }
    // it should "produce an error for an invalid argument type for '!'" in {
    //     resetNode(Not(IntLiterNode(-2)((0,0)))((0,0)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((0,0),
    //         """expression -2's type is incompatible for the '!' operator 
    //         |(Expected: BOOL, Actual: INT)
    //         |""".stripMargin.replaceAll("\n", ""))),
    //         log)
        
    //     resetNode(Not(IdentNode("not_a_bool")((2,5)))((2,0)))
    //     st.add("not_a_bool", StringType())
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((2,5), 
    //         """expression not_a_bool's type is incompatible for the '!' operator
    //         | (Expected: BOOL, Actual: STRING)
    //         |""".stripMargin.replaceAll("\n", ""))),
    //         log)
    // }
    // it should "correctly verify the argument type passed in for '-'" in {
    //     resetNode(Neg(IntLiterNode(3)((0,0)))((0,0)))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

    //     resetNode(Neg(IdentNode("a_number")((0,0)))((0,0)))
    //     st.add("a_number", Variable(IntType()))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)
    // }
    // it should "produce an error for an invalid argument type for '-'" in {
    //     resetNode(Neg(BoolLiterNode(true)((1,6)))((1,0)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((1,6),
    //         """expression true's type is incompatible for the '-' operator 
    //         |(Expected: INT, Actual: BOOL)
    //         |""".stripMargin.replaceAll("\n", ""))),
    //         log)

    //     resetNode(Neg(IdentNode("not_an_int")((23, 9)))((22, 8)))
    //     st.add("not_an_int", CharType())
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((23, 9), 
    //         """expression not_an_int's type is incompatible for the '-' operator
    //         | (Expected: INT, Actual: CHAR)
    //         |""".stripMargin.replaceAll("\n", ""))),
    //         log)
    // }
    // it should "correctly verify the argument type passed in for 'len'" in {
    //     // array
    //     resetNode(Len(IdentNode("array")((2,4)))((2,0)))
    //     st.add("array", ArrayType(StringType(), 2))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

    //     // array-elem
    //     resetNode(Len(ArrayElemNode(IdentNode("_2d_array")((4,3)), 
    //                             List(IntLiterNode(2)((4,15))))((4,3)))((4,0)))
    //     st.add("_2d_array", Variable(ArrayType(BoolType(), 2)))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)
    // }
    // it should "produce an error for an invalid argument type for 'len'" in {
    //     // array-elem accessing 1D array
    //     resetNode(Len(ArrayElemNode(IdentNode("_1d_array")((4,3)), 
    //                             List(IntLiterNode(2)((4,15))))((4,3)))((4,0)))
    //     st.add("_1d_array", Variable(ArrayType(BoolType(), 1)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((4,3), 
    //         """expression _1d_array[2]'s type is incompatible for the 'len' 
    //         |operator (Expected: ANY[], Actual: BOOL)
    //         |""".stripMargin.replaceAll("\n", ""))), log)

    //     // not an array
    //     resetNode(Len(IdentNode("not_an_array")((6, 8)))((6, 0)))
    //     st.add("not_an_array", StringType())
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((6, 8), 
    //         """expression not_an_array's type is incompatible for the 'len' 
    //         |operator (Expected: ANY[], Actual: STRING)
    //         |""".stripMargin.replaceAll("\n", ""))),
    //         log)
    // }
    // it should "correctly verify the argument type passed in for 'ord'" in {
    //     // char literal
    //     resetNode(Ord(CharLiterNode('r')((0,5)))((0,0)))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

    //     // char variable
    //     resetNode(Ord(IdentNode("aChar")((0,5)))((0,0)))
    //     st.add("aChar", Variable(CharType()))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

    //     // array elem of char
    //     resetNode(Ord(ArrayElemNode(IdentNode("charArray")((0,5)), 
    //                             List(IntLiterNode(3)((0,16))))((0,5)))((0,0)))
    //     st.add("charArray", Variable(ArrayType(CharType(), 1)))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)
    // }
    // it should "produce an error for an invalid argument type for 'ord'" in {
    //     // string literal
    //     resetNode(Ord(StringLiterNode("aString")((0,5)))((0,0)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((0,5),
    //         """expression "aString"'s type is incompatible for the 'ord' 
    //         |operator (Expected: CHAR, Actual: STRING)
    //         |""".stripMargin.replaceAll("\n", ""))), 
    //         log)
    
    //     // array elem not char type
    //     resetNode(Ord(ArrayElemNode(IdentNode("notCharArray")((1,5)), 
    //                             List(IntLiterNode(1)((1,18))))((1,5)))((1,1)))
    //     st.add("notCharArray", Variable(ArrayType(IntType(), 1)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((1,5),
    //         """expression notCharArray[1]'s type is incompatible for the 'ord' 
    //         |operator (Expected: CHAR, Actual: INT)
    //         |""".stripMargin.replaceAll("\n", ""))),
    //         log)
    // }
    // it should "correctly verify the argument type passed in for 'chr'" in {
    //     // int literal
    //     resetNode(Chr(IntLiterNode(90)((0,5)))((0,0)))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(CharType()), node.typeId, ListBuffer(), log)

    //     // int variable
    //     resetNode(Chr(IdentNode("someInt")((0,5)))((0,0)))
    //     st.add("someInt", Variable(IntType()))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(CharType()), node.typeId, ListBuffer(), log)

    //     // array elem of int
    //     resetNode(Chr(ArrayElemNode(IdentNode("nestedIntArray")((0,5)), 
    //                             List(IntLiterNode(2)((0,16)),
    //                                 IntLiterNode(5)((0,19))))((0,5)))((0,0)))
    //     st.add("nestedIntArray", Variable(ArrayType(IntType(), 2)))
    //     node.check(st, log)
    //     assertTypeIdEquals(Some(CharType()), node.typeId, ListBuffer(), log)
    // }
    // it should "produce an error for an invalid argument type for 'chr'" in {
    //     // string literal
    //     resetNode(Chr(StringLiterNode("aString")((0,5)))((0,0)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((0,5),
    //         """expression "aString"'s type is incompatible for the 'chr' 
    //         |operator (Expected: INT, Actual: STRING)
    //         |""".stripMargin.replaceAll("\n", ""))), 
    //         log)
    
    //     // array elem not int type
    //     resetNode(Chr(ArrayElemNode(IdentNode("notIntArray")((1,5)), 
    //                             List(IntLiterNode(1)((1,18))))((1,5)))((1,1)))
    //     st.add("notIntArray", Variable(ArrayType(CharType(), 1)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((1,5),
    //         """expression notIntArray[1]'s type is incompatible for the 'chr' 
    //         |operator (Expected: INT, Actual: CHAR)
    //         |""".stripMargin.replaceAll("\n", ""))),
    //         log)
    // }

    behavior of "semantic check of binary operations"
    it should "correctly verify the argument types of *, /, %, +, -" in {
        resetNode(Mult(IntLiterNode(3)((2,3)), IdentNode("a")((2,5)))((2,4)))
        st.add("a", Variable(IntType()))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

        resetNode(Div(IdentNode("b")((5,2)), IntLiterNode(-2)((5,4)))((5,3)))
        st.add("b", Variable(IntType()))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

        resetNode(Mod(IdentNode("c")((5,2)), IntLiterNode(10)((5,4)))((5,3)))
        st.add("c", Variable(IntType()))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

        resetNode(Add(IntLiterNode(42)((5,2)), IntLiterNode(0)((5,4)))((5,3)))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

        resetNode(Sub(IntLiterNode(42)((5,2)), IntLiterNode(0)((5,4)))((5,3)))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)

        // combined expr
        resetNode(Add(Mod(IntLiterNode(10)((3,4)), 
                        IntLiterNode(3)((3,8)))((3,6)), 
                    Div(IdentNode("x")((3,10)), 
                        IntLiterNode(2)((3,14)))((3,12)))((3,9)))
        st.add("x", Variable(IntType()))
        node.check(st, log)
        assertTypeIdEquals(Some(IntType()), node.typeId, ListBuffer(), log)
    }
    // it should "produce an error for invalid semantics of *, /, %, +, -" in {
    //     resetNode(Mult(IntLiterNode(3)((2,3)), IdentNode("a")((2,5)))((2,4)))
    //     st.add("a", Variable(CharType()))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((2,5),
    //         """expression a's type is incompatible for '*' (Expected: INT, 
    //         |Actual: CHAR)""".stripMargin.replaceAll("\n", ""))), 
    //         log)

    //     resetNode(Div(IdentNode("b")((5,2)), 
    //                     StringLiterNode("str")((5,4)))((5,3)))
    //     st.add("b", Variable(IntType()))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((5,4),
    //         """expression "str"'s type is incompatible for '/' (Expected: INT,
    //         | Actual: STRING)""".stripMargin.replaceAll("\n", ""))), 
    //         log)

    //     resetNode(Mod(IdentNode("c")((5,2)), IntLiterNode(10)((5,4)))((5,3)))
    //     st.add("c", Variable(BoolType()))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((5,2),
    //         """expression c's type is incompatible for '%' (Expected: INT, 
    //         |Actual: BOOL)""".stripMargin.replaceAll("\n", ""))), 
    //         log)

    //     resetNode(Add(IntLiterNode(42)((5,2)), 
    //                 CharLiterNode('n')((5,4)))((5,3)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((5,4),
    //         """expression 'n''s type is incompatible for '+' (Expected: INT, 
    //         |Actual: CHAR)""".stripMargin.replaceAll("\n", ""))), 
    //         log)

    //     resetNode(Sub(new PairLiterNode()((5,2)), 
    //                 IntLiterNode(0)((5,4)))((5,3)))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((5,2),
    //         """expression null's type is incompatible for '-' (Expected: INT, 
    //         |Actual: PAIR)""".stripMargin.replaceAll("\n", ""))), 
    //         log)

    //     // combined expr
    //     resetNode(Add(Mod(IntLiterNode(10)((3,4)), 
    //                     IntLiterNode(3)((3,8)))((3,6)), 
    //                 Div(IdentNode("x")((3,10)), 
    //                     IntLiterNode(2)((3,14)))((3,12)))((3,9)))
    //     st.add("x", Variable(StringType()))
    //     node.check(st, log)
    //     assertTypeIdEquals(None, node.typeId, ListBuffer(WaccError((3,10),
    //         """expression x's type is incompatible for '/' (Expected: INT, 
    //         |Actual: STRING)""".stripMargin.replaceAll("\n", ""))), 
    //         log)
    // }
}
