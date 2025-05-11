%token <string> VAR
%token <string> FORMAT
%token NEWLINE
%token NEG AND OR XOR ITE EXISTS FORALL FREE OUTPUT
%token EQUAL LPAREN RPAREN COMMA SEMICOLON
%token EOF

%{
open QCIR.T
%}
%type <QCIR.T.file> file
%start file
%%

%inline soption(X):
|        { "" } /* nothing */
| x = X  { x }

lit:
| v = VAR { (true,v) }
| NEG v = VAR { (false,v) }

quant:
| EXISTS { Exists }
| FORALL { Forall }

gateStmt:
| v = VAR EQUAL AND LPAREN xs = separated_list(COMMA, lit) RPAREN { (v, Group (And, xs)) }
| v = VAR EQUAL OR LPAREN xs = separated_list(COMMA, lit) RPAREN { (v, Group (Or, xs)) }
| v = VAR EQUAL XOR LPAREN x = lit COMMA y = lit RPAREN { (v, Xor (x, y)) }
| v = VAR EQUAL ITE LPAREN x = lit COMMA y = lit COMMA z = lit RPAREN { (v, Ite (x, y, z)) }
| v = VAR EQUAL q = quant LPAREN vs = separated_nonempty_list(COMMA, VAR) SEMICOLON x = lit RPAREN { (v, Quantifier (q, vs, x)) }

outputStmt:
| OUTPUT LPAREN x = lit RPAREN NEWLINE { x }

qblockQuant:
| q = quant LPAREN vs = separated_nonempty_list(COMMA, VAR) RPAREN NEWLINE { (q,vs) }

qblockPrefix:
| LPAREN FREE LPAREN vs = separated_nonempty_list(COMMA, VAR) RPAREN NEWLINE qbs = list(qblockQuant) { (vs, qbs) }
| qbs = list(qblockQuant) { ([], qbs) }

formatId:
| f = FORMAT NEWLINE { f }


file:
| formatId qbp = qblockPrefix o = outputStmt gs = list(terminated(gateStmt,NEWLINE)) EOF
  { let (vs, qbs) = qbp in { free_vars = vs; qblocks = qbs; output = o; gates = gs } }
