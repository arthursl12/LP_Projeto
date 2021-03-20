(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

use "PlcChecker.sml";
use "PlcInterp.sml";

fun run (e:expr) : string =
    (let 
        val t = teval e []
        val res = eval e []
    in
        val2string(res) ^ " : " ^ type2string(t)
    end)
    handle  Impossible => "Erro na avaliacao! Expressao impossivel de avaliar"
        |   HDEmptySeq => "Erro na avaliacao! Operacao de head numa sequencia vazia"
        |   TLEmptySeq => "Erro na avaliacao! Operacao de tail numa sequencia vazia"
        |   ValueNotFoundInMatch => "Erro na avaliacao! Padrao nao encontrado para casamento no comando match"
        |   NotAFunc => "Erro na avaliacao! Simbolo usado para chamada nao eh uma funcao"
        |   UnknownType => "Erro na tipagem! Expressao possui tipo desconhecido ou nao eh possivel avaliar seu tipo"
        |   EmptySeq => "Erro na tipagem! Sequencia utilizada nao possui nenhum elemento"
        |   NotEqTypes => "Erro na tipagem! Operandos utilizados nao sao tipos de igualdade"
        |   WrongRetType => "Erro na tipagem! Tipo de retorno da funcao incompativel"
        |   DiffBrTypes => "Erro na tipagem! Tipos das branchs do If diferentes"
        |   IfCondNotBool => "Erro na tipagem! Condicao do comando If nao eh booleano"
        |   NoMatchResults => "Erro na tipagem! Comando match sem padroes para casamento"
        |   MatchResTypeDiff => "Erro na tipagem! Resultados dos casamentos de match de tipos diferentes"
        |   MatchCondTypesDiff => "Erro na tipagem! Tipos dos padroes para casamentos de match de tipos diferentes"
        |   CallTypeMisM => "Erro na tipagem! Parametro usado na chamada eh de tipo incompativel com o declarado na funcao"
        |   NotFunc => "Erro na tipagem! Tipo do simbolo usado para chamada nao eh uma funcao"
        |   ListOutOfRange => "Erro na tipagem! Indice requisitado fora do intervalo da lista"
        |   OpNonList => "Erro na tipagem! Operando do operador Item usado nao eh uma lista"
        |   SymbolNotFound => "Erro de ambiente! Simbolo requisitado nao foi encontrado no ambiente";
