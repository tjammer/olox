open Olox

let ref =
  [
    { Scanner.Token.kind = Scanner.Token.Identifier "package"; line = 1 };
    { kind = Identifier "main"; line = 1 };
    { kind = Identifier "import"; line = 3 };
    { kind = String "fmt"; line = 3 };
    { kind = Identifier "type"; line = 5 };
    { kind = Identifier "point"; line = 5 };
    { kind = Identifier "struct"; line = 5 };
    { kind = Left_brace; line = 5 };
    { kind = Identifier "x"; line = 6 };
    { kind = Comma; line = 6 };
    { kind = Identifier "y"; line = 6 };
    { kind = Identifier "float64"; line = 6 };
    { kind = Right_brace; line = 7 };
    { kind = Identifier "func"; line = 9 };
    { kind = Left_paren; line = 9 };
    { kind = Identifier "p"; line = 9 };
    { kind = Identifier "point"; line = 9 };
    { kind = Right_paren; line = 9 };
    { kind = Identifier "String"; line = 9 };
    { kind = Left_paren; line = 9 };
    { kind = Right_paren; line = 9 };
    { kind = Identifier "string"; line = 9 };
    { kind = Left_brace; line = 9 };
    { kind = Return; line = 10 };
    { kind = Identifier "fmt"; line = 10 };
    { kind = Dot; line = 10 };
    { kind = Identifier "Sprintf"; line = 10 };
    { kind = Left_paren; line = 10 };
    { kind = String "(%.1f, %.1f)"; line = 10 };
    { kind = Comma; line = 10 };
    { kind = Identifier "p"; line = 10 };
    { kind = Dot; line = 10 };
    { kind = Identifier "x"; line = 10 };
    { kind = Comma; line = 10 };
    { kind = Identifier "p"; line = 10 };
    { kind = Dot; line = 10 };
    { kind = Identifier "y"; line = 10 };
    { kind = Right_paren; line = 10 };
    { kind = Right_brace; line = 11 };
    { kind = Identifier "type"; line = 13 };
    { kind = Identifier "triangle"; line = 13 };
    { kind = Identifier "struct"; line = 13 };
    { kind = Left_brace; line = 13 };
    { kind = Identifier "p1"; line = 14 };
    { kind = Comma; line = 14 };
    { kind = Identifier "p2"; line = 14 };
    { kind = Comma; line = 14 };
    { kind = Identifier "p3"; line = 14 };
    { kind = Identifier "point"; line = 14 };
    { kind = Right_brace; line = 15 };
    { kind = Identifier "func"; line = 17 };
    { kind = Left_paren; line = 17 };
    { kind = Identifier "t"; line = 17 };
    { kind = Star; line = 17 };
    { kind = Identifier "triangle"; line = 17 };
    { kind = Right_paren; line = 17 };
    { kind = Identifier "String"; line = 17 };
    { kind = Left_paren; line = 17 };
    { kind = Right_paren; line = 17 };
    { kind = Identifier "string"; line = 17 };
    { kind = Left_brace; line = 17 };
    { kind = Return; line = 18 };
    { kind = Identifier "fmt"; line = 18 };
    { kind = Dot; line = 18 };
    { kind = Identifier "Sprintf"; line = 18 };
    { kind = Left_paren; line = 18 };
    { kind = String "Triangle %s, %s, %s"; line = 18 };
    { kind = Comma; line = 18 };
    { kind = Identifier "t"; line = 18 };
    { kind = Dot; line = 18 };
    { kind = Identifier "p1"; line = 18 };
    { kind = Comma; line = 18 };
    { kind = Identifier "t"; line = 18 };
    { kind = Dot; line = 18 };
    { kind = Identifier "p2"; line = 18 };
    { kind = Comma; line = 18 };
    { kind = Identifier "t"; line = 18 };
    { kind = Dot; line = 18 };
    { kind = Identifier "p3"; line = 18 };
    { kind = Right_paren; line = 18 };
    { kind = Right_brace; line = 19 };
    { kind = Identifier "func"; line = 21 };
    { kind = Left_paren; line = 21 };
    { kind = Identifier "t"; line = 21 };
    { kind = Star; line = 21 };
    { kind = Identifier "triangle"; line = 21 };
    { kind = Right_paren; line = 21 };
    { kind = Identifier "det2D"; line = 21 };
    { kind = Left_paren; line = 21 };
    { kind = Right_paren; line = 21 };
    { kind = Identifier "float64"; line = 21 };
    { kind = Left_brace; line = 21 };
    { kind = Return; line = 22 };
    { kind = Identifier "t"; line = 22 };
    { kind = Dot; line = 22 };
    { kind = Identifier "p1"; line = 22 };
    { kind = Dot; line = 22 };
    { kind = Identifier "x"; line = 22 };
    { kind = Star; line = 22 };
    { kind = Left_paren; line = 22 };
    { kind = Identifier "t"; line = 22 };
    { kind = Dot; line = 22 };
    { kind = Identifier "p2"; line = 22 };
    { kind = Dot; line = 22 };
    { kind = Identifier "y"; line = 22 };
    { kind = Minus; line = 22 };
    { kind = Identifier "t"; line = 22 };
    { kind = Dot; line = 22 };
    { kind = Identifier "p3"; line = 22 };
    { kind = Dot; line = 22 };
    { kind = Identifier "y"; line = 22 };
    { kind = Right_paren; line = 22 };
    { kind = Plus; line = 22 };
    { kind = Identifier "t"; line = 23 };
    { kind = Dot; line = 23 };
    { kind = Identifier "p2"; line = 23 };
    { kind = Dot; line = 23 };
    { kind = Identifier "x"; line = 23 };
    { kind = Star; line = 23 };
    { kind = Left_paren; line = 23 };
    { kind = Identifier "t"; line = 23 };
    { kind = Dot; line = 23 };
    { kind = Identifier "p3"; line = 23 };
    { kind = Dot; line = 23 };
    { kind = Identifier "y"; line = 23 };
    { kind = Minus; line = 23 };
    { kind = Identifier "t"; line = 23 };
    { kind = Dot; line = 23 };
    { kind = Identifier "p1"; line = 23 };
    { kind = Dot; line = 23 };
    { kind = Identifier "y"; line = 23 };
    { kind = Right_paren; line = 23 };
    { kind = Plus; line = 23 };
    { kind = Identifier "t"; line = 24 };
    { kind = Dot; line = 24 };
    { kind = Identifier "p3"; line = 24 };
    { kind = Dot; line = 24 };
    { kind = Identifier "x"; line = 24 };
    { kind = Star; line = 24 };
    { kind = Left_paren; line = 24 };
    { kind = Identifier "t"; line = 24 };
    { kind = Dot; line = 24 };
    { kind = Identifier "p1"; line = 24 };
    { kind = Dot; line = 24 };
    { kind = Identifier "y"; line = 24 };
    { kind = Minus; line = 24 };
    { kind = Identifier "t"; line = 24 };
    { kind = Dot; line = 24 };
    { kind = Identifier "p2"; line = 24 };
    { kind = Dot; line = 24 };
    { kind = Identifier "y"; line = 24 };
    { kind = Right_paren; line = 24 };
    { kind = Right_brace; line = 25 };
    { kind = Identifier "func"; line = 27 };
    { kind = Identifier "triTri2D"; line = 27 };
    { kind = Left_paren; line = 27 };
    { kind = Identifier "t1"; line = 27 };
    { kind = Comma; line = 27 };
    { kind = Identifier "t2"; line = 27 };
    { kind = Star; line = 27 };
    { kind = Identifier "triangle"; line = 27 };
    { kind = Comma; line = 27 };
    { kind = Identifier "eps"; line = 27 };
    { kind = Identifier "float64"; line = 27 };
    { kind = Comma; line = 27 };
    { kind = Identifier "allowReversed"; line = 27 };
    { kind = Comma; line = 27 };
    { kind = Identifier "onBoundary"; line = 27 };
    { kind = Identifier "bool"; line = 27 };
    { kind = Right_paren; line = 27 };
    { kind = Identifier "bool"; line = 27 };
    { kind = Left_brace; line = 27 };
    { kind = Identifier "t1"; line = 29 };
    { kind = Dot; line = 29 };
    { kind = Identifier "checkTriWinding"; line = 29 };
    { kind = Left_paren; line = 29 };
    { kind = Identifier "allowReversed"; line = 29 };
    { kind = Right_paren; line = 29 };
    { kind = Identifier "t2"; line = 30 };
    { kind = Dot; line = 30 };
    { kind = Identifier "checkTriWinding"; line = 30 };
    { kind = Left_paren; line = 30 };
    { kind = Identifier "allowReversed"; line = 30 };
    { kind = Right_paren; line = 30 };
    { kind = Var; line = 33 };
    { kind = Identifier "chkEdge"; line = 33 };
    { kind = Identifier "func"; line = 33 };
    { kind = Left_paren; line = 33 };
    { kind = Star; line = 33 };
    { kind = Identifier "triangle"; line = 33 };
    { kind = Comma; line = 33 };
    { kind = Identifier "float64"; line = 33 };
    { kind = Right_paren; line = 33 };
    { kind = Identifier "bool"; line = 33 };
    { kind = If; line = 34 };
    { kind = Identifier "onBoundary"; line = 34 };
    { kind = Left_brace; line = 34 };
    { kind = Identifier "chkEdge"; line = 35 };
    { kind = Equal; line = 35 };
    { kind = Identifier "boundaryCollideChk"; line = 35 };
    { kind = Right_brace; line = 36 };
    { kind = Else; line = 36 };
    { kind = Left_brace; line = 36 };
    { kind = Identifier "chkEdge"; line = 37 };
    { kind = Equal; line = 37 };
    { kind = Identifier "boundaryDoesntCollideChk"; line = 37 };
    { kind = Right_brace; line = 38 };
    { kind = Identifier "lp1"; line = 39 };
    { kind = Equal; line = 39 };
    { kind = Number 3.; line = 39 };
    { kind = Dot; line = 39 };
    { kind = Identifier "point"; line = 39 };
    { kind = Left_brace; line = 39 };
    { kind = Identifier "t1"; line = 39 };
    { kind = Dot; line = 39 };
    { kind = Identifier "p1"; line = 39 };
    { kind = Comma; line = 39 };
    { kind = Identifier "t1"; line = 39 };
    { kind = Dot; line = 39 };
    { kind = Identifier "p2"; line = 39 };
    { kind = Comma; line = 39 };
    { kind = Identifier "t1"; line = 39 };
    { kind = Dot; line = 39 };
    { kind = Identifier "p3"; line = 39 };
    { kind = Right_brace; line = 39 };
    { kind = Identifier "lp2"; line = 40 };
    { kind = Equal; line = 40 };
    { kind = Number 3.; line = 40 };
    { kind = Dot; line = 40 };
    { kind = Identifier "point"; line = 40 };
    { kind = Left_brace; line = 40 };
    { kind = Identifier "t2"; line = 40 };
    { kind = Dot; line = 40 };
    { kind = Identifier "p1"; line = 40 };
    { kind = Comma; line = 40 };
    { kind = Identifier "t2"; line = 40 };
    { kind = Dot; line = 40 };
    { kind = Identifier "p2"; line = 40 };
    { kind = Comma; line = 40 };
    { kind = Identifier "t2"; line = 40 };
    { kind = Dot; line = 40 };
    { kind = Identifier "p3"; line = 40 };
    { kind = Right_brace; line = 40 };
    { kind = Eof; line = 41 };
  ]

let src = "package main

import \"fmt\"

type point struct {
    x, y float64
}

func (p point) String() string {
    return fmt.Sprintf(\"(%.1f, %.1f)\", p.x, p.y)
}

type triangle struct {
    p1, p2, p3 point
}

func (t *triangle) String() string {
    return fmt.Sprintf(\"Triangle %s, %s, %s\", t.p1, t.p2, t.p3)
}

func (t *triangle) det2D() float64 {
    return t.p1.x * (t.p2.y - t.p3.y) +
           t.p2.x * (t.p3.y - t.p1.y) +
           t.p3.x * (t.p1.y - t.p2.y)
}

func triTri2D(t1, t2 *triangle, eps float64, allowReversed, onBoundary bool) bool {
    // Triangles must be expressed anti-clockwise.
    t1.checkTriWinding(allowReversed)
    t2.checkTriWinding(allowReversed)

    // 'onBoundary' determines whether points on boundary are considered as colliding or not.
    var chkEdge func (*triangle, float64) bool
    if onBoundary {
        chkEdge = boundaryCollideChk
    } else {
        chkEdge = boundaryDoesntCollideChk
    }
    lp1 = 3.point{t1.p1, t1.p2, t1.p3}
    lp2 = 3.point{t2.p1, t2.p2, t2.p3}
"
