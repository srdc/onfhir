// Generated from FhirPathExpr.g4 by ANTLR 4.7
package io.onfhir.path.grammar;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link FhirPathExprParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface FhirPathExprVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by the {@code indexerExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndexerExpression(FhirPathExprParser.IndexerExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code polarityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPolarityExpression(FhirPathExprParser.PolarityExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code additiveExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAdditiveExpression(FhirPathExprParser.AdditiveExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code multiplicativeExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMultiplicativeExpression(FhirPathExprParser.MultiplicativeExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code unionExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnionExpression(FhirPathExprParser.UnionExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code orExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOrExpression(FhirPathExprParser.OrExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code andExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAndExpression(FhirPathExprParser.AndExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code membershipExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMembershipExpression(FhirPathExprParser.MembershipExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code inequalityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInequalityExpression(FhirPathExprParser.InequalityExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code invocationExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInvocationExpression(FhirPathExprParser.InvocationExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code equalityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEqualityExpression(FhirPathExprParser.EqualityExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code impliesExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitImpliesExpression(FhirPathExprParser.ImpliesExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code termExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTermExpression(FhirPathExprParser.TermExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code typeExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeExpression(FhirPathExprParser.TypeExpressionContext ctx);
	/**
	 * Visit a parse tree produced by the {@code invocationTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitInvocationTerm(FhirPathExprParser.InvocationTermContext ctx);
	/**
	 * Visit a parse tree produced by the {@code literalTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLiteralTerm(FhirPathExprParser.LiteralTermContext ctx);
	/**
	 * Visit a parse tree produced by the {@code externalConstantTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExternalConstantTerm(FhirPathExprParser.ExternalConstantTermContext ctx);
	/**
	 * Visit a parse tree produced by the {@code parenthesizedTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParenthesizedTerm(FhirPathExprParser.ParenthesizedTermContext ctx);
	/**
	 * Visit a parse tree produced by the {@code nullLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNullLiteral(FhirPathExprParser.NullLiteralContext ctx);
	/**
	 * Visit a parse tree produced by the {@code booleanLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBooleanLiteral(FhirPathExprParser.BooleanLiteralContext ctx);
	/**
	 * Visit a parse tree produced by the {@code stringLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStringLiteral(FhirPathExprParser.StringLiteralContext ctx);
	/**
	 * Visit a parse tree produced by the {@code numberLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNumberLiteral(FhirPathExprParser.NumberLiteralContext ctx);
	/**
	 * Visit a parse tree produced by the {@code dateTimeLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDateTimeLiteral(FhirPathExprParser.DateTimeLiteralContext ctx);
	/**
	 * Visit a parse tree produced by the {@code timeLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTimeLiteral(FhirPathExprParser.TimeLiteralContext ctx);
	/**
	 * Visit a parse tree produced by the {@code quantityLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQuantityLiteral(FhirPathExprParser.QuantityLiteralContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#externalConstant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExternalConstant(FhirPathExprParser.ExternalConstantContext ctx);
	/**
	 * Visit a parse tree produced by the {@code memberInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMemberInvocation(FhirPathExprParser.MemberInvocationContext ctx);
	/**
	 * Visit a parse tree produced by the {@code functionInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunctionInvocation(FhirPathExprParser.FunctionInvocationContext ctx);
	/**
	 * Visit a parse tree produced by the {@code thisInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitThisInvocation(FhirPathExprParser.ThisInvocationContext ctx);
	/**
	 * Visit a parse tree produced by the {@code indexInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIndexInvocation(FhirPathExprParser.IndexInvocationContext ctx);
	/**
	 * Visit a parse tree produced by the {@code totalInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTotalInvocation(FhirPathExprParser.TotalInvocationContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#function}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunction(FhirPathExprParser.FunctionContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#paramList}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamList(FhirPathExprParser.ParamListContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#quantity}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQuantity(FhirPathExprParser.QuantityContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#unit}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitUnit(FhirPathExprParser.UnitContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#dateTimePrecision}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDateTimePrecision(FhirPathExprParser.DateTimePrecisionContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#pluralDateTimePrecision}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPluralDateTimePrecision(FhirPathExprParser.PluralDateTimePrecisionContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#typeSpecifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTypeSpecifier(FhirPathExprParser.TypeSpecifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#qualifiedIdentifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitQualifiedIdentifier(FhirPathExprParser.QualifiedIdentifierContext ctx);
	/**
	 * Visit a parse tree produced by {@link FhirPathExprParser#identifier}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdentifier(FhirPathExprParser.IdentifierContext ctx);
}