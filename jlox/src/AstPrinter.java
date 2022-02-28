package com.craftinginterpreters.lox;

import java.util.List;
import java.util.ArrayList;

class AstPrinter implements Expr.Visitor<String>, Stmt.Visitor<String> {
    String print(Expr expr) {
        return expr.accept(this);
    }

    String print(Stmt stmt) {
        return stmt.accept(this);
    }

    @Override
    public String visitBinaryExpr(Expr.Binary expr) {
        return parenthesize(expr.operator.lexeme, expr.left, expr.right);
    }

    @Override
    public String visitGroupingExpr(Expr.Grouping expr) {
        return parenthesize("group", expr.expression);
    }

    @Override
    public String visitLiteralExpr(Expr.Literal expr) {
        if (expr.value == null) return "nil";
        // If it's a string, enclose it in quotation marks to show that
        if (expr.value.equals(expr.value.toString())) return "\"" + expr.value + "\"";
        return expr.value.toString();
    }

    @Override
    public String visitUnaryExpr(Expr.Unary expr) {
        return parenthesize(expr.operator.lexeme, expr.right);
    }

    @Override
    public String visitVariableExpr(Expr.Variable expr) {
        return expr.name.lexeme;
    }

    @Override
    public String visitAssignExpr(Expr.Assign expr) {
        return parenthesize("assign " + expr.name.lexeme, expr.value);
    }

    @Override
    public String visitExpressionStmt(Stmt.Expression stmt) {
        return stmt.expression.accept(this);
    }

    @Override
    public String visitPrintStmt(Stmt.Print stmt) {
        return parenthesize("print", stmt.expression);
    }

    @Override
    public String visitVarStmt(Stmt.Var stmt) {
        if (stmt.initializer == null) return "(var " + stmt.name.lexeme + " nil)";
        return parenthesize("var " + stmt.name.lexeme, stmt.initializer);
    }

    @Override
    public String visitBlockStmt(Stmt.Block stmt) {
        StringBuilder builder = new StringBuilder();
        builder.append("(block\n    ");
        for (Stmt statement : stmt.statements) {
            builder.append(statement.accept(this).replace("\n", "\n    "));
            builder.append("\n    ");
        }
        // Remove the last 4 spaces and add a closing parenthesis
        return builder.substring(0, builder.length() - 4) + ")";
    }

    @Override
    public String visitLogicalExpr(Expr.Logical expr) {
        return parenthesize(expr.operator.lexeme, expr.left, expr.right);
    }

    @Override
    public String visitCallExpr(Expr.Call expr) {
        // Make an array with all of the call data (the callee expression plus the
        // arguments).
        List<Expr> call_data = new ArrayList<>(expr.arguments);
        call_data.add(0, expr.callee);
        return parenthesize("call", call_data.toArray(new Expr[0]));
    }

    @Override
    public String visitIfStmt(Stmt.If stmt) {
        StringBuilder builder = new StringBuilder();
        builder.append("(if ");
        builder.append(stmt.condition.accept(this));
        builder.append("\n    ");
        builder.append(stmt.thenBranch.accept(this).replace("\n", "\n    "));
        if (stmt.elseBranch != null) {
            builder.append("\nelse\n    ");
            builder.append(stmt.elseBranch.accept(this).replace("\n", "\n    "));
        }
        builder.append("\n)");

        return builder.toString();
    }

    @Override
    public String visitWhileStmt(Stmt.While stmt) {
        StringBuilder builder = new StringBuilder();
        builder.append("(while ");
        builder.append(stmt.condition.accept(this));
        builder.append("\n    ");
        builder.append(stmt.body.accept(this).replace("\n", "\n    "));
        builder.append("\n)");
        return builder.toString();
    }

    @Override
    public String visitFunctionStmt(Stmt.Function stmt) {
        StringBuilder builder = new StringBuilder();
        builder.append("(fn ");
        builder.append(stmt.name.lexeme);
        builder.append(" (");
        for (Token param : stmt.params) {
            if (param != stmt.params.get(0)) {
                builder.append(", ");
            }
            builder.append(param.lexeme);
        }
        builder.append(")\n    ");
        for (Stmt statement : stmt.body) {
            if (statement != stmt.body.get(0)) {
                builder.append("\n    ");
            }
            builder.append(statement.accept(this).replace("\n", "\n    "));
        }
        builder.append("\n)");
        return builder.toString();
    }

    @Override
    public String visitReturnStmt(Stmt.Return stmt) {
        if (stmt.value != null) {
            return parenthesize("return", stmt.value);
        }
        return "return";
    }

    @Override
    public String visitClassStmt(Stmt.Class stmt) {
        StringBuilder builder = new StringBuilder();
        builder.append("(class ");
        builder.append(stmt.name.lexeme);
        if (stmt.superclass != null) {
            builder.append("< " + stmt.superclass.name.lexeme);
        }
        builder.append("\n    ");

        for (Stmt.Function method : stmt.methods) {
            System.out.println(method.name.lexeme);
            if (method != stmt.methods.get(0)) {
                builder.append("\n    ");
            }
            builder.append(method.accept(this).replace("\n", "\n    "));
        }

        builder.append("\n)");
        return builder.toString();
    }

    @Override
    public String visitGetExpr(Expr.Get expr) {
        return "(get " + expr.object.accept(this) + "." + expr.name.lexeme + ")";
    }

    @Override
    public String visitSetExpr(Expr.Set expr) {
        return (
            "(set " + expr.object.accept(this) + "." + expr.name.lexeme + " " +
            expr.value.accept(this) + ")"
        );
    }

    @Override
    public String visitThisExpr(Expr.This expr) {
        return "this";
    }

    private String parenthesize(String name, Expr... exprs) {
        StringBuilder builder = new StringBuilder();

        builder.append("(").append(name);
        for (Expr expr : exprs) {
            builder.append(" ");
            builder.append(expr.accept(this));
        }
        builder.append(")");

        return builder.toString();
    }


}
