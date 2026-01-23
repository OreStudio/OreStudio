<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" 
	xmlns="http://www.fpml.org/FpML-5/master"
	xmlns:master="http://www.fpml.org/FpML-5/master"
	xpath-default-namespace="http://www.fpml.org/FpML-5/master"
	xmlns:common="http://exslt.org/common" 
	xmlns:xsd="http://www.w3.org/2001/XMLSchema"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
	xmlns:saxon="http://saxon.sf.net/"
	exclude-result-prefixes="common xsd saxon xsi master"
	>
	<xsl:strip-space elements="*"/>
	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes" standalone="yes"/>


	<xsl:variable name="ns" select="'http://www.fpml.org/FpML-5/master'"/>

	<xsl:template match="@*" priority="-2">
		<xsl:copy-of select="."/>
	</xsl:template>	

	<xsl:template match="node() | @*" priority="-3">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="*" priority="-1">
		<xsl:element name="{local-name(.)}">
			<xsl:apply-templates select="@* | node()" />
		</xsl:element>
	</xsl:template>	

	<xsl:template match="publicExecutionReport/trade[not(../tradingEvent)]" priority="10">
		<xsl:copy-of select="."/>
		<tradingEvent>
			<eventType>Trade</eventType>
			<agreementDate>
				<xsl:value-of select="tradeHeader/tradeDate"/>
			</agreementDate>
			<xsl:copy-of select="(tradeHeader/partyTradeInformation/executionDateTime)[1]"/>
		</tradingEvent>
	</xsl:template>	

	<xsl:template match="originatingEvent" priority="10"/>

	<xsl:template match="trade[../originatingEvent and not(../tradingEvent)]" priority="10">
		<xsl:copy-of select="."/>
		<tradingEvent>
			<eventType><xsl:value-of select="../originatingEvent"/></eventType>
			<agreementDate>
				<xsl:value-of select="tradeHeader/tradeDate"/>
			</agreementDate>
			<xsl:copy-of select="(tradeHeader/partyTradeInformation/executionDateTime)[1]"/>
		</tradingEvent>
	</xsl:template>	


	<xsl:template match="amendment" priority="10">
		<xsl:apply-templates select="trade"/>
		<tradingEvent>
			<xsl:apply-templates select="." mode="event.id"/>>
			<eventType>Amendment</eventType>
			<xsl:apply-templates select="." mode="event.content"/>
		</tradingEvent>
	</xsl:template>	

	<xsl:template match="termination" priority="10">
		<xsl:apply-templates select="originalTrade"/>
		<tradingEvent>
			<xsl:apply-templates select="." mode="event.id"/>>
			<eventType>Termination</eventType>
			<xsl:apply-templates select="." mode="event.content"/>
		</tradingEvent>
	</xsl:template>	

	<xsl:template match="increase" priority="10">
		<xsl:apply-templates select="originalTrade"/>
		<tradingEvent>
			<xsl:apply-templates select="." mode="event.id"/>>
			<eventType>Increase</eventType>
			<xsl:apply-templates select="." mode="event.content"/>
		</tradingEvent>
	</xsl:template>	

	<xsl:template match="novation" priority="10">
		<xsl:apply-templates select="oldTrade"/>
		<tradingEvent>
			<xsl:apply-templates select="." mode="event.id"/>>
			<eventType>Novation</eventType>
			<xsl:apply-templates select="." mode="event.content"/>
		</tradingEvent>
	</xsl:template>	

	<xsl:template match="*" mode="event.id">
		<xsl:variable name="eid" select="//correlationId"/>
		<xsl:variable name="party" select="//onBehalfOf/partyReference"/>
		<xsl:if test="$eid and $party">
			<eventIdentifier>
				<xsl:copy-of select="//onBehalfOf/partyReference[1]"/>
				<eventId><xsl:value-of select="$eid"/></eventId>
			</eventIdentifier>
		</xsl:if>
	</xsl:template>	

	<xsl:template match="*" mode="event.content">
		<xsl:apply-templates select="agreementDate|executionDateTime|effectiveDate"/>
		<xsl:apply-templates select="." mode="size.change"/>
		<xsl:apply-templates select="payment[1]" mode="event.payments"/>
	</xsl:template>	


	<!-- normalize payment structure -->
	<xsl:template match="payment" mode="event.payments">
		<xsl:copy>
			<xsl:copy-of select="payerPartyReference"/>
			<xsl:copy-of select="receiverPartyReference"/>
			<xsl:apply-templates select="paymentDate" mode="event.payments"/>
			<xsl:copy-of select="paymentAmount"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="paymentDate[unadjustedDate | adjustedDate]" mode="event.payments" >
		<xsl:copy>
			<adjustableDate>
				<xsl:copy-of select="unadjustedDate"/>
				<xsl:copy-of select="adjustedDate"/>
			</adjustableDate>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="paymentDate" mode="payments">
		<xsl:copy-of select="."/>
	</xsl:template>	

	<!-- normalize notional change info structure -->
	<xsl:template match="*" mode="size.change">
		<xsl:copy-of select="changeInNotional|changeInNumberOfOptions|changeInQuantity"/>
	</xsl:template>	

	<xsl:template match="*[novationAmount]" mode="size.change">
		<xsl:apply-templates select="novationAmount" mode="size.change"/>
	</xsl:template>	

	<xsl:template match="*[changeInNotionalAmount]" mode="size.change">
		<xsl:apply-templates select="changeInNotionalAmount" mode="size.change"/>
	</xsl:template>	

	<xsl:template match="*[changeInNumberOfOptions]" mode="size.change">
		<xsl:apply-templates select="changeInNumberOfOptions" mode="size.change"/>
	</xsl:template>	

	<xsl:template match="*[changeInNumberOfUnits]" mode="size.change">
		<xsl:apply-templates select="changeInNumberOfUnits" mode="size.change"/>
	</xsl:template>	

	<xsl:template match="*[notionalChange]" mode="size.change">
		<changeInNotional>
			<xsl:copy-of select="//currency[1]"/>  <!-- this is a hack, should be first ccy in product -->
			<xsl:copy-of select="amount"/>
		</changeInNotional>
	</xsl:template>	

	<xsl:template match="*[novatedAmount]" mode="size.change">
		<changeInNotional>
			<xsl:copy-of select="//currency[1]"/>  <!-- this is a hack, should be first ccy in product -->
			<xsl:copy-of select="novatedAmount"/>
		</changeInNotional>
	</xsl:template>	

	<xsl:template match="*[novatedNumberOfOptions]" mode="size.change">
		<changeInNumberOfOptions>
			<xsl:value-of select="novatedNumberOfOptions"/>
		</changeInNumberOfOptions>
	</xsl:template>	

	<xsl:template match="*[novatedNumberOfUnits]" mode="size.change">
		<changeInQuantity>
			<xsl:value-of select="novatedNumberOfUnits"/>
		</changeInQuantity>
	</xsl:template>	

	<xsl:template match="*[sizeChange]" mode="size.change">
		<xsl:apply-templates mode="size.change" select="sizeChange/*"/>
	</xsl:template>	

	<xsl:template match="changeInNotionalAmount" mode="size.change">
		<changeInNotional>
			<xsl:copy-of select="currency|amount"/>
		</changeInNotional>
	</xsl:template>	

	<xsl:template match="changeInNumberOfOptions" mode="size.change">
		<xsl:copy-of select="."/>
	</xsl:template>	

	<!-- this is problematic, shows inconsistencies between sizechange and ReportingNotionalChange.model -->
	<xsl:template match="changeInNumberOfUnits" mode="size.change">
		<changeInQuantity>
			<quantity>
				<xsl:copy-of select="text()"/>
			</quantity>
		</changeInQuantity>
	</xsl:template>	


	<!-- normalize notional change info structure -->
	<xsl:template match="oldTrade|newTrade|feeTrade|originalTrade|resultingTrade" priority="10">
		<!--
		<xsl:apply-templates mode="get-submitting-party" select="."/>
		<xsl:apply-templates mode="get-trade-source" select="."/>
		<xsl:variable name="reportingParty">
			<xsl:apply-templates mode="get-reporting-party" select="."/>
		</xsl:variable>
		<xsl:copy-of select="$reportingParty"/>

		<xsl:apply-templates mode="get-counter-party" select="."/>
		<xsl:apply-templates mode="get-beneficiary" select="."/>

		<xsl:apply-templates mode="get-clearing-service" select="."/>
		<xsl:apply-templates mode="get-clearing-firm" select="."/>

		<xsl:apply-templates mode="get-related-parties" select=".">
			<xsl:with-param name="reportingParty" select="$reportingParty//@href"/>
		</xsl:apply-templates>

		<xsl:apply-templates mode="get-units-and-people" select=".">
			<xsl:with-param name="reportingParty" select="$reportingParty//@href"/>
		</xsl:apply-templates>
		<xsl:apply-templates mode="get-repository" select="tradeHeader/partyTradeInformation/*"/>
		<xsl:apply-templates mode="get-prior-repository" select="tradeHeader/partyTradeInformation/*"/>
		-->
		<!-- copy the trade -->
		<trade>
			<xsl:apply-templates select="node()"/>
		</trade>
	</xsl:template>

	<xsl:template match="partyTradeIdentifier[1][not(originatingTradeId) and not(blockTradeId)]" priority="10">
		<partyTradeIdentifier>
			<xsl:apply-templates select="@*"/>
			<xsl:apply-templates select="*"/>
	
			<xsl:choose>
				<xsl:when test="//novation/oldTrade">
					<originatingTradeId>
						<xsl:apply-templates select="//novation/oldTrade/tradeHeader/partyTradeIdentifier[1]/*"/>
					</originatingTradeId>
				</xsl:when>
			</xsl:choose>
		</partyTradeIdentifier>
	</xsl:template>

	<xsl:template match="termination/originalTrade//notionalStepSchedule/initialValue" priority="10">
		<initialValue>
			<xsl:value-of select="//termination/outstandingNotionalAmount/amount"/>
		</initialValue>
	</xsl:template>

	<xsl:template match="increase/originalTrade//notionalStepSchedule/initialValue" priority="10">
		<initialValue>
			<xsl:value-of select="//increase/outstandingNotionalAmount/amount"/>
		</initialValue>
	</xsl:template>


	<xsl:template mode="get-submitting-party" match="node()" priority="-1">
		<xsl:apply-templates mode="get-submitting-party" select="tradeHeader/partyTradeInformation"/>
	</xsl:template>

	<xsl:template mode="get-submitting-party" match="partyTradeInformation[relatedParty/role = 'TradeSource'][1]">
		<submittingPartyReference href="{partyReference/@href}"/>
	</xsl:template>


	<xsl:template mode="get-trade-source" match="node()" priority="-1">
		<xsl:apply-templates mode="get-trade-source" select="tradeHeader/partyTradeInformation"/>
	</xsl:template>

	<xsl:template mode="get-trade-source" match="partyTradeInformation[relatedParty/role = 'TradeSource'][1]">
		<tradeSourceReference href="{partyReference/@href}"/>
	</xsl:template>



	<xsl:template mode="get-reporting-party" match="*">
		<xsl:apply-templates mode="get-reporting-party" select="tradeHeader/partyTradeInformation"/>
	</xsl:template>

	<xsl:template mode="get-reporting-party" match="node()" priority="-1"/>

	<xsl:template mode="get-reporting-party" match="partyTradeInformation[reportingRegime/reportingRole = 'ReportingParty'][1]">
		<reportingPartyReference href="{partyReference/@href}"/>
	</xsl:template>


	<xsl:template mode="get-counter-party" match="*">
		<xsl:apply-templates mode="get-counter-party" select="tradeHeader/partyTradeInformation"/>
	</xsl:template>

	<xsl:template mode="get-counter-party" match="node()" priority="-1"/>

	<xsl:template mode="get-counter-party" match="partyTradeInformation">
		<xsl:apply-templates mode="get-counter-party" select="relatedParty"/>
	</xsl:template>

	<xsl:template mode="get-counter-party" match="partyTradeInformation/relatedParty[role= 'Counterparty'][1]">
		<counterPartyReference>
			<xsl:copy-of select="partyReference/@href"/>
		</counterPartyReference>
	</xsl:template>

	<xsl:template mode="get-beneficiary" match="*">
		<xsl:apply-templates mode="get-beneficiary" select="tradeHeader/partyTradeInformation"/>
	</xsl:template>

	<xsl:template mode="get-beneficiary" match="node()" priority="-1"/>

	<xsl:template mode="get-beneficiary" match="partyTradeInformation">
		<xsl:apply-templates mode="get-beneficiary" select="relatedParty"/>
	</xsl:template>

	<xsl:template mode="get-beneficiary" match="partyTradeInformation/relatedParty[role= 'Beneficiary'][1]">
		<beneficiaryReference>
			<xsl:copy-of select="partyReference/@href"/>
		</beneficiaryReference>
	</xsl:template>

	<xsl:template mode="get-clearing-service" match="*">
		<xsl:variable name="dco" select="tradeHeader/partyTradeInformation/relatedParty[role = 'ClearingOrganization'][1]/partyReference/@href"/>
		<xsl:if test="$dco">
			<clearingOrganizationPartyReference href="{$dco}"/>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="get-clearing-firm" match="*">
		<xsl:variable name="cmf" select="tradeHeader/partyTradeInformation/relatedParty[role = 'ClearingFirm'][1]/partyReference/@href"/>
		<xsl:if test="$cmf">
			<clearingFirmPartyReference href="{$cmf}"/>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="get-related-parties" match="*">
		<xsl:param name="reportingParty"/>
		<!-- allocation agent (in reporting party's partyTradeInfo block) -->
		<xsl:variable name="allocAgent" select="tradeHeader/partyTradeInformation[partyReference/@href= $reportingParty]/relatedParty[role='AllocationAgent']/partyReference/@href"/>
		<xsl:if test="$allocAgent">
			<allocationAgentReference href="{$allocAgent}"/>
		</xsl:if>

		<!-- execution facility (first in any party trade info block) -->
		<xsl:variable name="executionFacility" select="(tradeHeader/partyTradeInformation/relatedParty[role='ExecutionFacility'])[1]/partyReference/@href"/>
		<xsl:if test="$executionFacility">
			<executionVenueReference href="{$executionFacility}"/>
		</xsl:if>

		<!-- executing broker (first in any party trade info block) -->
		<xsl:variable name="executingBroker" select="(tradeHeader/partyTradeInformation/relatedParty[role='ExecutingBroker'])[1]/partyReference/@href"/>
		<xsl:if test="$executingBroker">
			<executingBrokerReference href="{$executingBroker}"/>
		</xsl:if>

		<!-- arranging broker (first in any party trade info block) -->
		<xsl:variable name="arrangingBroker" select="(tradeHeader/partyTradeInformation/relatedParty[role='ArrangingBroker'])[1]/partyReference/@href"/>
		<xsl:if test="$arrangingBroker">
			<arrangingBrokerReference href="{$arrangingBroker}"/>
		</xsl:if>


	</xsl:template>

	<xsl:template mode="get-units-and-people" match="*">
		<xsl:param name="reportingParty"/>
		<!-- units -->
		<xsl:variable name="branch" select="tradeHeader/partyTradeInformation[partyReference/@href= $reportingParty]/relatedBusinessUnit[role='RegisteredBranch']/businessUnitReference/@href"/>
		<xsl:if test="$branch">
			<branchReference href="{$branch}"/>
		</xsl:if>

		<xsl:variable name="desk" select="tradeHeader/partyTradeInformation[partyReference/@href= $reportingParty]/relatedBusinessUnit[role='TradingDesk']/businessUnitReference/@href"/>
		<xsl:if test="$desk">
			<deskReference href="{$desk}"/>
		</xsl:if>

		<!-- people -->
		<xsl:variable name="trader" select="tradeHeader/partyTradeInformation[partyReference/@href= $reportingParty]/relatedPerson[role='Trader']/personReference/@href"/>
		<xsl:if test="$trader">
			<traderReference href="{$trader}"/>
		</xsl:if>
		<xsl:variable name="broker" select="tradeHeader/partyTradeInformation[partyReference/@href= $reportingParty]/relatedPerson[role='SalesBroker']/personReference/@href"/>
		<xsl:if test="$broker">
			<salesBrokerReference href="{$broker}"/>
		</xsl:if>
		<xsl:variable name="tradingDecisionMaker" select="tradeHeader/partyTradeInformation[partyReference/@href= $reportingParty]/relatedPerson[role='TradingDecisionMaker']/personReference/@href"/>
		<xsl:if test="$tradingDecisionMaker">
			<tradingDecisionMakerReference href="{$tradingDecisionMaker}"/>
		</xsl:if>
		<xsl:variable name="investmentDecisionMaker" select="tradeHeader/partyTradeInformation[partyReference/@href= $reportingParty]/relatedPerson[role='InvestmentDecisionMaker']/personReference/@href"/>
		<xsl:if test="$investmentDecisionMaker">
			<investmentDecisionMakerReference href="{$investmentDecisionMaker}"/>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="get-repository" match="*" priority="1" />
	<xsl:template mode="get-prior-repository" match="*" priority="1" />

	<xsl:template mode="get-repository" match="relatedParty[role='TradeRepository']" priority="10">
		<tradeRepositoryReference href="{partyReference/@href}"/>
	</xsl:template>

	<xsl:template mode="get-prior-repository" match="relatedParty[role='PriorTradeRepository']" priority="10">
		<priorTradeRepositoryReference href="{partyReference/@href}"/>
	</xsl:template>

</xsl:stylesheet>
