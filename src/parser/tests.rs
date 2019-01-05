#[cfg(test)]
use super::*;
use crate::lexer::Lexer;
use std::any::Any;

#[test]
fn test_let_statements() {
    fn test(input: &str, expected_identifier: &str, expected_value: &Any) {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        test_let_statement(&statement, expected_identifier, expected_value);
    }

    test("let x = 5;", "x", &5);
    test("let y = true;", "y", &true);
    test("let foobar = y;", "foobar", &"y");
}

#[test]
fn test_return_statements() {
    fn test(input: &str, expected: &Any) {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = &program.statements[0];
        test_return_statement(&statement, expected);
    }

    test("return 5;", &5);
    test("return x;", &"x");
    test("return false;", &false);
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    check_parse_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    let statement = match &program.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", program.statements[0]),
    };
    test_identifier(&statement.expression, "foobar");
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program().unwrap();
    check_parse_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    let statement = match &program.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", program.statements[0]),
    };
    test_integer_literal(&statement.expression, &5);
}

#[test]
fn test_prefix_expressions() {
    fn test(input: &str, op: &str, right: &Any) {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };
        test_prefix_expression(&statement.expression, op, right);
    }

    let i64_tests: Vec<(&str, &str, i64)> = vec![("!5;", "!", 5), ("-15;", "-", 15)];
    for t in i64_tests {
        test(t.0, t.1, &t.2);
    }

    let bool_tests: Vec<(&str, &str, bool)> = vec![("!true;", "!", true), ("!false;", "!", false)];
    for t in bool_tests {
        test(t.0, t.1, &t.2);
    }
}

#[test]
fn test_infix_expressions() {
    fn test(input: &str, left: &Any, op: &str, right: &Any) {
        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };
        test_infix_expression(&statement.expression, left, op, right);
    }

    let i64_tests: Vec<(&str, i64, &str, i64)> = vec![
        ("5 + 6;", 5, "+", 6),
        ("5 - 6;", 5, "-", 6),
        ("5 * 6;", 5, "*", 6),
        ("5 / 6;", 5, "/", 6),
        ("5 > 6;", 5, ">", 6),
        ("5 < 6;", 5, "<", 6),
        ("5 == 6;", 5, "==", 6),
        ("5 != 6;", 5, "!=", 6),
    ];
    for t in i64_tests {
        test(t.0, &t.1, t.2, &t.3);
    }

    let bool_tests: Vec<(&str, bool, &str, bool)> = vec![
        ("true == true", true, "==", true),
        ("true != false", true, "!=", false),
        ("false == false", false, "==", false),
    ];
    for t in bool_tests {
        test(t.0, &t.1, t.2, &t.3);
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests: Vec<(&str, &str)> = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
    ];

    for t in tests {
        let mut parser = Parser::new(Lexer::new(t.0));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);
        assert_eq!(format!("{}", program), t.1);
    }
}

#[test]
fn test_boolean_expression() {
    let tests: Vec<(&str, bool)> = vec![("true;", true), ("false;", false)];

    for t in tests {
        let mut parser = Parser::new(Lexer::new(t.0));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };
        test_boolean_literal(&statement.expression, &t.1);
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";

    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program().unwrap();
    check_parse_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    let statement = match &program.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", program.statements[0]),
    };

    let if_exp = match &statement.expression {
        Expression::If(e) => e,
        _ => panic!("invalid variant: {:?}", &statement.expression),
    };

    test_infix_expression(if_exp.condition.as_ref(), &"x", "<", &"y");
    let cons = match if_exp.consequence.as_ref() {
        Statement::Block(b) => b,
        _ => panic!("invalid variant: {:?}", &if_exp.consequence),
    };
    assert_eq!(cons.statements.len(), 1);

    let consequence = match &cons.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", cons.statements[0]),
    };
    test_identifier(&consequence.expression, "x");

    assert!(if_exp.alternative.is_none());
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";

    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program().unwrap();
    check_parse_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    let statement = match &program.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", program.statements[0]),
    };

    let if_exp = match &statement.expression {
        Expression::If(e) => e,
        _ => panic!("invalid variant: {:?}", &statement.expression),
    };

    test_infix_expression(if_exp.condition.as_ref(), &"x", "<", &"y");
    let cons = match if_exp.consequence.as_ref() {
        Statement::Block(b) => b,
        _ => panic!("invalid variant: {:?}", &if_exp.consequence),
    };
    assert_eq!(cons.statements.len(), 1);

    let consequence = match &cons.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", cons.statements[0]),
    };
    test_identifier(&consequence.expression, "x");

    let alt = match if_exp.alternative.as_ref().unwrap().as_ref() {
        Statement::Block(b) => b,
        _ => panic!("invalid variant: {:?}", &if_exp.consequence),
    };
    let alternative = match &alt.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", alt.statements[0]),
    };
    test_identifier(&alternative.expression, "y");
}

#[test]
fn test_function_literal() {
    let input = "fn(x, y) { x + y; }";

    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program().unwrap();
    check_parse_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    let statement = match &program.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", program.statements[0]),
    };

    let func_exp = match &statement.expression {
        Expression::Function(e) => e,
        _ => panic!("invalid variant: {:?}", &statement.expression),
    };

    assert_eq!(func_exp.parameters.len(), 2);
    test_literal_expression(
        &Expression::Identifier(func_exp.parameters[0].clone()),
        &"x",
    );
    test_literal_expression(
        &Expression::Identifier(func_exp.parameters[1].clone()),
        &"y",
    );

    assert_eq!(func_exp.body.statements.len(), 1);
    let body = match &func_exp.body.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", func_exp.body.statements[0]),
    };
    test_infix_expression(&body.expression, &"x", "+", &"y");
}

#[test]
fn test_function_parameter_parsing() {
    let tests: Vec<(&str, Vec<&str>)> = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x"]),
        ("fn(x, y, z) {};", vec!["x", "y", "z"]),
    ];

    for t in tests {
        let mut parser = Parser::new(Lexer::new(t.0));
        let program = parser.parse_program().unwrap();
        check_parse_errors(&parser);

        let statement = match &program.statements[0] {
            Statement::Expression(s) => s,
            _ => panic!("invalid variant: {:?}", program.statements[0]),
        };
        let func_exp = match &statement.expression {
            Expression::Function(e) => e,
            _ => panic!("invalid variant: {:?}", &statement.expression),
        };

        assert_eq!(func_exp.parameters.len(), t.1.len());
        for (i, expect) in t.1.iter().enumerate() {
            let exp = Expression::Identifier(func_exp.parameters[i].clone());
            test_literal_expression(&exp, expect);
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program().unwrap();
    check_parse_errors(&parser);

    assert_eq!(program.statements.len(), 1);

    let statement = match &program.statements[0] {
        Statement::Expression(s) => s,
        _ => panic!("invalid variant: {:?}", program.statements[0]),
    };
    let call_exp = match &statement.expression {
        Expression::Call(e) => e,
        _ => panic!("invalid variant: {:?}", &statement.expression),
    };

    test_identifier(&call_exp.function, "add");
    assert_eq!(call_exp.arguments.len(), 3);
    test_literal_expression(&call_exp.arguments[0], &1);
    test_infix_expression(&call_exp.arguments[1], &2, "*", &3);
    test_infix_expression(&call_exp.arguments[2], &4, "+", &5);
}

fn test_let_statement(statement: &Statement, expected_identifier: &str, expected_value: &Any) {
    let let_statement = match statement {
        Statement::Let(s) => s,
        _ => panic!("invalid variant: {:?}", statement),
    };
    assert_eq!(let_statement.token.literal, "let");
    test_identifier(
        &Expression::Identifier(let_statement.name.clone()),
        expected_identifier,
    );
    test_literal_expression(&let_statement.value, expected_value);
}

fn test_return_statement(statement: &Statement, expected: &Any) {
    let return_statement = match statement {
        Statement::Return(s) => s,
        _ => panic!("invalid variant: {:?}", statement),
    };
    assert_eq!(return_statement.token.literal, "return");
    test_literal_expression(&return_statement.return_value, expected);
}

fn test_identifier(expression: &Expression, expected: &str) {
    let ident = match expression {
        Expression::Identifier(e) => e,
        _ => panic!("invalid variant: {:?}", expression),
    };
    assert_eq!(ident.value, expected);
    assert_eq!(ident.token.literal, expected);
}

fn test_integer_literal(expression: &Expression, expected: &i64) {
    let integer = match expression {
        Expression::IntegerLiteral(e) => e,
        _ => panic!("invalid variant: {:?}", expression),
    };
    assert_eq!(integer.value, *expected);
    assert_eq!(integer.token.literal, format!("{}", expected));
}

fn test_boolean_literal(expression: &Expression, expected: &bool) {
    let boolean = match expression {
        Expression::BooleanLiteral(e) => e,
        _ => panic!("invalid variant: {:?}", expression),
    };
    assert_eq!(boolean.value, *expected);
    assert_eq!(boolean.token.literal, format!("{}", expected));
}

fn test_literal_expression(expression: &Expression, expected: &dyn Any) {
    if let Some(expected) = expected.downcast_ref::<i64>() {
        test_integer_literal(expression, expected);
    } else if let Some(expected) = expected.downcast_ref::<i32>() {
        test_integer_literal(expression, &(*expected as i64));
    } else if let Some(expected) = expected.downcast_ref::<&str>() {
        test_identifier(expression, expected);
    } else if let Some(expected) = expected.downcast_ref::<bool>() {
        test_boolean_literal(expression, expected);
    } else {
        panic!("invalid type: {:?}", expected);
    }
}

fn test_prefix_expression(expression: &Expression, operator: &str, right: &Any) {
    let oe = match expression {
        Expression::Prefix(e) => e,
        _ => panic!("invalid variant: {:?}", expression),
    };
    assert_eq!(oe.operator, operator);
    test_literal_expression(&oe.right, right);
}

fn test_infix_expression(expression: &Expression, left: &Any, operator: &str, right: &Any) {
    let oe = match expression {
        Expression::Infix(e) => e,
        _ => panic!("invalid variant: {:?}", expression),
    };
    test_literal_expression(&oe.left, left);
    assert_eq!(oe.operator, operator);
    test_literal_expression(&oe.right, right);
}

fn check_parse_errors(parser: &Parser) {
    let errors = parser.get_errors();
    if errors.len() == 0 {
        return;
    }

    eprintln!("parser has {} errors", errors.len());
    for error in errors {
        eprintln!("{}", error)
    }

    panic!();
}
