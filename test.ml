let rec cst_to_ast t = match t with
| E1(t1, token, t2) -> (match token with
                       | Minus -> Sub(cst_to_ast t1, cst_to_ast t2))

| E2(t)             -> cst_to_ast(t)

| T1(tok)           -> (match tok with
                       | Ident(str) -> Id(str))

| T2(tree, token1, token2) -> (match (token1, token2) with
                              | (Star, Ident(str2)) -> Times(cst_to_ast tree, Id(str2))) 