#[cfg(test)]
use super::*;
use crate::lexer::Lexer;
use crate::parser::Parser;

#[test]
fn test_eval_integer_expression() {
    let tests: Vec<(&str, i64)> = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];
    for t in tests {
        let evaluated = test_eval(t.0).unwrap();
        test_integer_object(&evaluated, t.1);
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests: Vec<(&str, bool)> = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];
    for t in tests {
        let evaluated = test_eval(t.0).unwrap();
        test_boolean_object(&evaluated, t.1);
    }
}

#[test]
fn test_bang_operator() {
    let tests: Vec<(&str, bool)> = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];
    for t in tests {
        let evaluated = test_eval(t.0).unwrap();
        test_boolean_object(&evaluated, t.1);
    }
}

#[test]
fn test_if_expressions() {
    let int_tests: Vec<(&str, i64)> = vec![
        ("if (true) { 10 }", 10),
        ("if (1) { 10 }", 10),
        ("if (1 < 2) { 10 }", 10),
        ("if (1 > 2) { 10 } else { 20 }", 20),
        ("if (1 < 2) { 10 } else { 20 }", 10),
    ];
    for t in int_tests {
        let evaluated = test_eval(t.0).unwrap();
        test_integer_object(&evaluated, t.1);
    }

    let null_tests: Vec<&str> = vec!["if (false) { 10 }", "if (1 > 2) { 10 }"];
    for t in null_tests {
        let evaluated = test_eval(t).unwrap();
        test_null_object(&evaluated);
    }
}

#[test]
fn test_return_statements() {
    let tests: Vec<(&str, i64)> = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        (
            r#"
                if (10 > 1) {
                  if (10 > 1) {
                    return 10;
                  }
                  return 1;
                }
            "#,
            10,
        ),
    ];
    for t in tests {
        let evaluated = test_eval(t.0).unwrap();
        test_integer_object(&evaluated, t.1);
    }
}

#[test]
fn test_error_handling() {
    let tests: Vec<(&str, EvalError)> = vec![
        (
            "5 + true",
            EvalError::TypeMismatch("INTEGER + BOOLEAN".into()),
        ),
        (
            "5 + true; 5;",
            EvalError::TypeMismatch("INTEGER + BOOLEAN".into()),
        ),
        ("-true", EvalError::UnknownOperator("-BOOLEAN".into())),
        (
            "true + false;",
            EvalError::UnknownOperator("BOOLEAN + BOOLEAN".into()),
        ),
        (
            "5; true + false; 5;",
            EvalError::UnknownOperator("BOOLEAN + BOOLEAN".into()),
        ),
        (
            "if (10 > 1) { true + false; }",
            EvalError::UnknownOperator("BOOLEAN + BOOLEAN".into()),
        ),
        (
            r#"
                if (10 > 1) {
                  if (10 > 1) {
                    return true + false;
                  }
                  return 1;
                }
            "#,
            EvalError::UnknownOperator("BOOLEAN + BOOLEAN".into()),
        ),
    ];
    for t in tests {
        let evaluated = test_eval(t.0).unwrap_err();
        assert_eq!(evaluated, t.1);
    }
}

fn test_eval(input: &str) -> Result<Object> {
    let mut parser = Parser::new(Lexer::new(input));
    let program = parser.parse_program().unwrap();
    eval(&program)
}

fn test_integer_object(object: &Object, expected: i64) {
    let int = match object {
        Object::Integer(i) => i,
        _ => panic!("object is not Integer. got: {:?}", object),
    };
    assert_eq!(*int, expected);
}

fn test_boolean_object(object: &Object, expected: bool) {
    let boolean = match object {
        Object::Boolean(b) => b,
        _ => panic!("object is not Boolean. got: {:?}", object),
    };
    assert_eq!(*boolean, expected);
}

fn test_null_object(object: &Object) {
    match object {
        Object::Null => {}
        _ => panic!("object is not null. got: {:?}", object),
    };
}
