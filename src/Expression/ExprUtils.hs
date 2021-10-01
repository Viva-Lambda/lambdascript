-- expression utils
module Expression.ExprUtils where

import Expression.Expression
import Expression.Literal
import Expression.Identifier

-- check for expression types
isNumericExpr :: Expr -> Bool
isNumericExpr (GExpr (GetLit (NumLit _ _))) = True
isNumericExpr _ = False

isBoolExpr :: Expr -> Bool
isBoolExpr (GExpr (GetLit (BLit _ _)) ) = True
isBoolExpr _ = False

-- default functions



fromTyExpr2Expr :: TypedExpr -> Expr
fromTyExpr2Expr texpr = case texpr of
                            (TypedLit lit) -> GExpr $ GetLit lit
                            (TypedId vn) -> GExpr $ GetVName vn
                            (TypedCall c) -> StmtExpr (CallStmt c)


reduceExpr :: [Expr] -> Expr
reduceExpr [] = EndExpr

reduceExpr (e:es) =
    let children = reduceExpr es
        seqe = SeqExpr {parent = e, child = children}
        stmt = SeqStmt seqe
    in StmtExpr stmt

fromExprToSeq :: [Expr] -> Sequence
fromExprToSeq [] = SeqExpr {parent = EndExpr, child = EndExpr}
fromExprToSeq (e:es) = SeqExpr {parent = e, child = reduceExpr es}

fromSeqToExprs :: Sequence -> [Expr]
-- end of sequence
fromSeqToExprs SeqExpr {parent = EndExpr, child=EndExpr} = []
-- skip parent sequence
fromSeqToExprs SeqExpr {parent = EndExpr, child=StmtExpr (SeqStmt s)} = fromSeqToExprs s
-- skip child sequence
fromSeqToExprs SeqExpr {parent = StmtExpr (SeqStmt s) , child=EndExpr} = fromSeqToExprs s
-- both are sequences
fromSeqToExprs SeqExpr {parent = StmtExpr (SeqStmt s) , child=StmtExpr (SeqStmt a) } = 
    fromSeqToExprs s ++ fromSeqToExprs a
-- only parent is a sequence
fromSeqToExprs SeqExpr {parent = StmtExpr (SeqStmt s), child=a} = 
    fromSeqToExprs s ++ [a]
-- only child is a sequence
fromSeqToExprs SeqExpr {parent = a, child=StmtExpr (SeqStmt s)} = [a] ++ fromSeqToExprs s
-- both are not a sequence
fromSeqToExprs SeqExpr {parent = a, child = b} = [a] ++ [b]


