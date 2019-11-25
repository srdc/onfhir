// Generated from FhirPathExpr.g4 by ANTLR 4.7
package io.onfhir.path.grammar;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link FhirPathExprParser}.
 */
public interface FhirPathExprListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by the {@code indexerExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterIndexerExpression(FhirPathExprParser.IndexerExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code indexerExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitIndexerExpression(FhirPathExprParser.IndexerExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code polarityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterPolarityExpression(FhirPathExprParser.PolarityExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code polarityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitPolarityExpression(FhirPathExprParser.PolarityExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code additiveExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterAdditiveExpression(FhirPathExprParser.AdditiveExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code additiveExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitAdditiveExpression(FhirPathExprParser.AdditiveExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code multiplicativeExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterMultiplicativeExpression(FhirPathExprParser.MultiplicativeExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code multiplicativeExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitMultiplicativeExpression(FhirPathExprParser.MultiplicativeExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code unionExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterUnionExpression(FhirPathExprParser.UnionExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code unionExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitUnionExpression(FhirPathExprParser.UnionExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code orExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterOrExpression(FhirPathExprParser.OrExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code orExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitOrExpression(FhirPathExprParser.OrExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code andExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterAndExpression(FhirPathExprParser.AndExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code andExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitAndExpression(FhirPathExprParser.AndExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code membershipExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterMembershipExpression(FhirPathExprParser.MembershipExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code membershipExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitMembershipExpression(FhirPathExprParser.MembershipExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code inequalityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterInequalityExpression(FhirPathExprParser.InequalityExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code inequalityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitInequalityExpression(FhirPathExprParser.InequalityExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code invocationExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterInvocationExpression(FhirPathExprParser.InvocationExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code invocationExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitInvocationExpression(FhirPathExprParser.InvocationExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code equalityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterEqualityExpression(FhirPathExprParser.EqualityExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code equalityExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitEqualityExpression(FhirPathExprParser.EqualityExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code impliesExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterImpliesExpression(FhirPathExprParser.ImpliesExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code impliesExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitImpliesExpression(FhirPathExprParser.ImpliesExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code termExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterTermExpression(FhirPathExprParser.TermExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code termExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitTermExpression(FhirPathExprParser.TermExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code typeExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void enterTypeExpression(FhirPathExprParser.TypeExpressionContext ctx);
	/**
	 * Exit a parse tree produced by the {@code typeExpression}
	 * labeled alternative in {@link FhirPathExprParser#expression}.
	 * @param ctx the parse tree
	 */
	void exitTypeExpression(FhirPathExprParser.TypeExpressionContext ctx);
	/**
	 * Enter a parse tree produced by the {@code invocationTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 */
	void enterInvocationTerm(FhirPathExprParser.InvocationTermContext ctx);
	/**
	 * Exit a parse tree produced by the {@code invocationTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 */
	void exitInvocationTerm(FhirPathExprParser.InvocationTermContext ctx);
	/**
	 * Enter a parse tree produced by the {@code literalTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 */
	void enterLiteralTerm(FhirPathExprParser.LiteralTermContext ctx);
	/**
	 * Exit a parse tree produced by the {@code literalTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 */
	void exitLiteralTerm(FhirPathExprParser.LiteralTermContext ctx);
	/**
	 * Enter a parse tree produced by the {@code externalConstantTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 */
	void enterExternalConstantTerm(FhirPathExprParser.ExternalConstantTermContext ctx);
	/**
	 * Exit a parse tree produced by the {@code externalConstantTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 */
	void exitExternalConstantTerm(FhirPathExprParser.ExternalConstantTermContext ctx);
	/**
	 * Enter a parse tree produced by the {@code parenthesizedTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 */
	void enterParenthesizedTerm(FhirPathExprParser.ParenthesizedTermContext ctx);
	/**
	 * Exit a parse tree produced by the {@code parenthesizedTerm}
	 * labeled alternative in {@link FhirPathExprParser#term}.
	 * @param ctx the parse tree
	 */
	void exitParenthesizedTerm(FhirPathExprParser.ParenthesizedTermContext ctx);
	/**
	 * Enter a parse tree produced by the {@code nullLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterNullLiteral(FhirPathExprParser.NullLiteralContext ctx);
	/**
	 * Exit a parse tree produced by the {@code nullLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitNullLiteral(FhirPathExprParser.NullLiteralContext ctx);
	/**
	 * Enter a parse tree produced by the {@code booleanLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterBooleanLiteral(FhirPathExprParser.BooleanLiteralContext ctx);
	/**
	 * Exit a parse tree produced by the {@code booleanLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitBooleanLiteral(FhirPathExprParser.BooleanLiteralContext ctx);
	/**
	 * Enter a parse tree produced by the {@code stringLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterStringLiteral(FhirPathExprParser.StringLiteralContext ctx);
	/**
	 * Exit a parse tree produced by the {@code stringLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitStringLiteral(FhirPathExprParser.StringLiteralContext ctx);
	/**
	 * Enter a parse tree produced by the {@code numberLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterNumberLiteral(FhirPathExprParser.NumberLiteralContext ctx);
	/**
	 * Exit a parse tree produced by the {@code numberLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitNumberLiteral(FhirPathExprParser.NumberLiteralContext ctx);
	/**
	 * Enter a parse tree produced by the {@code dateTimeLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterDateTimeLiteral(FhirPathExprParser.DateTimeLiteralContext ctx);
	/**
	 * Exit a parse tree produced by the {@code dateTimeLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitDateTimeLiteral(FhirPathExprParser.DateTimeLiteralContext ctx);
	/**
	 * Enter a parse tree produced by the {@code timeLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterTimeLiteral(FhirPathExprParser.TimeLiteralContext ctx);
	/**
	 * Exit a parse tree produced by the {@code timeLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitTimeLiteral(FhirPathExprParser.TimeLiteralContext ctx);
	/**
	 * Enter a parse tree produced by the {@code quantityLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterQuantityLiteral(FhirPathExprParser.QuantityLiteralContext ctx);
	/**
	 * Exit a parse tree produced by the {@code quantityLiteral}
	 * labeled alternative in {@link FhirPathExprParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitQuantityLiteral(FhirPathExprParser.QuantityLiteralContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#externalConstant}.
	 * @param ctx the parse tree
	 */
	void enterExternalConstant(FhirPathExprParser.ExternalConstantContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#externalConstant}.
	 * @param ctx the parse tree
	 */
	void exitExternalConstant(FhirPathExprParser.ExternalConstantContext ctx);
	/**
	 * Enter a parse tree produced by the {@code memberInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void enterMemberInvocation(FhirPathExprParser.MemberInvocationContext ctx);
	/**
	 * Exit a parse tree produced by the {@code memberInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void exitMemberInvocation(FhirPathExprParser.MemberInvocationContext ctx);
	/**
	 * Enter a parse tree produced by the {@code functionInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void enterFunctionInvocation(FhirPathExprParser.FunctionInvocationContext ctx);
	/**
	 * Exit a parse tree produced by the {@code functionInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void exitFunctionInvocation(FhirPathExprParser.FunctionInvocationContext ctx);
	/**
	 * Enter a parse tree produced by the {@code thisInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void enterThisInvocation(FhirPathExprParser.ThisInvocationContext ctx);
	/**
	 * Exit a parse tree produced by the {@code thisInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void exitThisInvocation(FhirPathExprParser.ThisInvocationContext ctx);
	/**
	 * Enter a parse tree produced by the {@code indexInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void enterIndexInvocation(FhirPathExprParser.IndexInvocationContext ctx);
	/**
	 * Exit a parse tree produced by the {@code indexInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void exitIndexInvocation(FhirPathExprParser.IndexInvocationContext ctx);
	/**
	 * Enter a parse tree produced by the {@code totalInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void enterTotalInvocation(FhirPathExprParser.TotalInvocationContext ctx);
	/**
	 * Exit a parse tree produced by the {@code totalInvocation}
	 * labeled alternative in {@link FhirPathExprParser#invocation}.
	 * @param ctx the parse tree
	 */
	void exitTotalInvocation(FhirPathExprParser.TotalInvocationContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#function}.
	 * @param ctx the parse tree
	 */
	void enterFunction(FhirPathExprParser.FunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#function}.
	 * @param ctx the parse tree
	 */
	void exitFunction(FhirPathExprParser.FunctionContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#paramList}.
	 * @param ctx the parse tree
	 */
	void enterParamList(FhirPathExprParser.ParamListContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#paramList}.
	 * @param ctx the parse tree
	 */
	void exitParamList(FhirPathExprParser.ParamListContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#quantity}.
	 * @param ctx the parse tree
	 */
	void enterQuantity(FhirPathExprParser.QuantityContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#quantity}.
	 * @param ctx the parse tree
	 */
	void exitQuantity(FhirPathExprParser.QuantityContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#unit}.
	 * @param ctx the parse tree
	 */
	void enterUnit(FhirPathExprParser.UnitContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#unit}.
	 * @param ctx the parse tree
	 */
	void exitUnit(FhirPathExprParser.UnitContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#dateTimePrecision}.
	 * @param ctx the parse tree
	 */
	void enterDateTimePrecision(FhirPathExprParser.DateTimePrecisionContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#dateTimePrecision}.
	 * @param ctx the parse tree
	 */
	void exitDateTimePrecision(FhirPathExprParser.DateTimePrecisionContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#pluralDateTimePrecision}.
	 * @param ctx the parse tree
	 */
	void enterPluralDateTimePrecision(FhirPathExprParser.PluralDateTimePrecisionContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#pluralDateTimePrecision}.
	 * @param ctx the parse tree
	 */
	void exitPluralDateTimePrecision(FhirPathExprParser.PluralDateTimePrecisionContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#typeSpecifier}.
	 * @param ctx the parse tree
	 */
	void enterTypeSpecifier(FhirPathExprParser.TypeSpecifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#typeSpecifier}.
	 * @param ctx the parse tree
	 */
	void exitTypeSpecifier(FhirPathExprParser.TypeSpecifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#qualifiedIdentifier}.
	 * @param ctx the parse tree
	 */
	void enterQualifiedIdentifier(FhirPathExprParser.QualifiedIdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#qualifiedIdentifier}.
	 * @param ctx the parse tree
	 */
	void exitQualifiedIdentifier(FhirPathExprParser.QualifiedIdentifierContext ctx);
	/**
	 * Enter a parse tree produced by {@link FhirPathExprParser#identifier}.
	 * @param ctx the parse tree
	 */
	void enterIdentifier(FhirPathExprParser.IdentifierContext ctx);
	/**
	 * Exit a parse tree produced by {@link FhirPathExprParser#identifier}.
	 * @param ctx the parse tree
	 */
	void exitIdentifier(FhirPathExprParser.IdentifierContext ctx);
}