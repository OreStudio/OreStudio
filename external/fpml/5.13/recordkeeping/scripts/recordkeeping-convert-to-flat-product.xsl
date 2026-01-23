<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" 
	xmlns="http://www.fpml.org/FpML-5/recordkeeping"
	xpath-default-namespace="http://www.fpml.org/FpML-5/recordkeeping"
	xmlns:rec="http://www.fpml.org/FpML-5/recordkeeping"
	xmlns:common="http://exslt.org/common" 
	xmlns:xsd="http://www.w3.org/2001/XMLSchema"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
	xmlns:prod="http://fpml.org/product-definition" 
	xmlns:saxon="http://saxon.sf.net/" 
	exclude-result-prefixes="common xsd saxon prod rec"
	>

	<xsl:import href="recordkeeping-utils.xsl"/>


	<xsl:strip-space elements="*"/>
	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes" standalone="yes"/>

	<xsl:param name="xpathdir" select="'../src/recordkeeping-view-examples'"/>
	<xsl:param name="productDefs-file" select="concat(translate($xpathdir,'\','/'), '/ProductData.xml')"/>
	<xsl:variable name="productDefs" select="document($productDefs-file)/prod:productValidationSet"/>

	<xsl:variable name="target.ns" select="'http://www.fpml.org/FpML-5/recordkeeping'"/>

	<xsl:variable name="reportingPartyRef" select="//reportingPartyReference/@href"/>
	<xsl:variable name="counterPartyRef" select="//counterPartyReference/@href"/>


	<xsl:variable name="templateTF">
		<regulatoryProductInfo>
			<primaryAssetClass copy=".//primaryAssetClass" copyatts="true"/>
			<secondaryAssetClass copy=".//secondaryAssetClass" copyatts="true"/>
			<productType copy=".//productType[1]" copyatts="true"/>
			<productId copy=".//productId[1]" copyatts="true"/>
			<embededOptionType copy=".//embededOptionType"/>

			<parties>
				<xsl:copy-of select=".//buyerPartyReference[1]"/>
				<xsl:copy-of select=".//sellerPartyReference[1]"/>
			</parties>

			<dates>
				<effectiveDate field="Effective date"/>
				<contractExpirationDate field="Termination date"/>
				<finalContractualSettlementDate field="Final Settlement Date"/>
			</dates>

			<sizes>
				<notional>
					<currency field="Notional currency 1 (for leg 1)"/>
					<amount field="Notional amount 1 (for leg 1)"/>
				</notional>
				<notional>
					<currency field="Notional currency 2 (for leg 2)"/>
					<amount field="Notional amount 2 (for leg 2)"/>
				</notional>
				<notionalSchedule children="true" copy="(.//notionalStepSchedule)[1]"/>
				<notionalSchedule children="true" copy="(.//notionalStepSchedule)[2]"/>
				<quantity field="Quantity"/>
			</sizes>

			<underlyers>
				<underlyer>
					<floatingRate context="Leg 1">
						<!--
						<floatingRateIndex field="Floating rate index name (leg 1)"/>
						<indexTenor>
							<periodMultiplier field="Floating rate period multiplier (leg 1)"/>
							<period	field="Floating rate period (leg 1)"/>
						</indexTenor>
						-->
						<floatingRateIndex copy=".//floatingRateIndex"/>
						<indexTenor copy=".//indexTenor"/>
						<floatingRateMultiplierSchedule copy=".//floatingRateMultiplierSchedule"/>
						<spreadSchedule copy=".//spreadSchedule"/>
						<capRateSchedule copy=".//capRateSchedule"/>
						<floorRateSchedule copy=".//floorRateSchedule"/>
					</floatingRate>
				</underlyer>
				<underlyer>
					<floatingRate context="Leg 2">
						<!--
						<floatingRateIndex field="Floating rate index name (leg 2)"/>
						<indexTenor>
							<periodMultiplier field="Floating rate period multiplier (leg 2)"/>
							<period	field="Floating rate period (leg 2)"/>
						</indexTenor>
						-->
						<floatingRateIndex copy=".//floatingRateIndex"/>
						<indexTenor copy=".//indexTenor"/>
						<floatingRateMultiplierSchedule copy=".//floatingRateMultiplierSchedule"/>
						<spreadSchedule copy=".//spreadSchedule"/>
						<capRateSchedule copy=".//capRateSchedule"/>
						<floorRateSchedule copy=".//floorRateSchedule"/>
					</floatingRate>
				</underlyer>
				<underlyer>
					<referenceEntity>
						<entityName field="Name of the reference entity"/>
						<entityId field="Id of the reference entity"/>
					</referenceEntity>
				</underlyer>
				<underlyer>
					<indexReferenceInformation structure="Index Information" nowrap="true"/>
				</underlyer>
				<underlyer context="Leg 1">
					<fixedRate children="true" copy=".//fixedRateSchedule"/>
					<fixedRate>
						<initialValue wrap="true" children="true" copy=".//fixedRate"/>
					</fixedRate>
				</underlyer>
				<underlyer context="Leg 2">
					<fixedRate children="true" copy=".//fixedRateSchedule"/>
				</underlyer>
				<underlyer structure="Underlying Asset"/>
				<underlyer structure="Underlying Asset 2"/>
			</underlyers>

			<priceTerms>
				<fixedRate field="Fixed rate (Leg 1)"/>
				<fixedRate field="Fixed rate (leg2)"/>
				<spread field="Floating rate spread"/>
				<spread field="Floating rate spread (leg 2)"/>
				<spreadCurrency field="Floating rate spread currency"/>
				<spreadCurrency field="Floating rate spread currency (leg 2)"/>
				<strikePrice children="true" wrap="true" copy=".//strike/rate"/>
				<strikePrice copy=".//strike/strikePrice"/>
				<exchangeRate copy=".//exchangeRate"/>
			</priceTerms>

			<otherCalculationTerms>
				<dayCountFraction copy="(//dayCountFraction)[1]"/>
				<dayCountFraction copy="(//dayCountFraction)[2]"/>
				<paymentFrequency copy="(//paymentFrequency)[1]"/>
				<paymentFrequency copy="(//paymentFrequency)[2]"/>
				<calculationFrequency copy="(//calculationFrequency)[1]"/>
				<calculationFrequency copy="(//calculationFrequency)[2]"/>
				<resetFrequency copy="(//resetFrequency)[1]"/>
				<resetFrequency copy="(//resetFrequency)[2]"/>
			</otherCalculationTerms>

			<optionDetails>
				<optionType  field="Option type"/>
				<firstExerciseDate  field="Commencement Date"/>
				<optionExpirationDate  field="Exercise period"/>
				<undlerlyerMaturityDate  field="Underlyer Maturity Date"/>
				<exerciseStyle  field="Option style"/>
				<feature coyp=".//feature"/>
			</optionDetails>

			<settlementDetails>
				<settlementMethod field="Settlement type"/>
				<settlementCurrency copy=".//settlementCurrency"/>
				<settlementLocation field="Settlement Location"/>
			</settlementDetails>

			<otherPayment children="true" wrap="true" paymentstructure="Option premium structure" />
			<!-- tbd:  additional payments -->

			<creditDetails>
				<attachmentPoint copy=".//attachmentPoint"/>
				<exhuastionPoint copy=".//exhuastionPoint"/>
				<seniority copy=".//seniority"/>
				<indexFactor copy=".//indexFactor"/>
				<indexSeries copy=".//indexSeries"/>
				<indexAnnexVersion copy=".//indexAnnexVersion"/>
				<fixedRecoverFinalPrice field="Fixed Recovery"/>
			</creditDetails>

			<fxDetails>
				<fxType field="FX Type"/>
				<deliveryCurrency children="true" copy=".//putCurrencyAmount/currency"/>
				<deliveryCurrency2 children="true" copy=".//callCurrencyAmount/currency"/>
				<deliveryCurrency children="true" copy=".//exchangedCurrency1/paymentAmount/currency"/>
				<deliveryCurrency2 children="true" copy=".//exchangedCurrency2/paymentAmount/currency"/>
			</fxDetails>

			<commodityDetails>
				<grade field="Grade"/>
				<!-- settlement periods -->
				<settlementPeriods>
				</settlementPeriods>

				<loadType field="Load Type"/>
				<quantity field="Quantity" />
				<quantityFrequency field="Quantity Frequency" />
				<!--
				<fixedPrice>
					<price	field="Fixed price"/>
				</fixedPrice>
				-->
				<interconnectionPoint field="Interconnection Point"/>

				<!--
				<commodityDetails field="Commodity Details"/>
				<priceCurrency field="Price currency"/>
				<priceUnits field="Price unit"/>
				<deliveryPoint field="Delivery point or zone"/>
				-->
			</commodityDetails>

		</regulatoryProductInfo>
		</xsl:variable>
	<xsl:variable name="template" select="common:node-set($templateTF)"/>

	<xsl:variable name="optiontemplateTF">
		<optionBuyerReference reference="Option buyer"/>
		<optionSellerReference reference="Option seller"/>
		<premium paymentstructure="Option premium structure" nowrap="true"/>
		<!-- underlyer -->
		<optionType  field="Option type"/>
		<exerciseStyle  field="Option style"/>
		<expirationDate  field="Exercise period"/>
		<commencementDate  field="Commencement Date"/>
		<strikePrice  field="Strike price (cap/floor rate)"/>
		<strikeCurrency  field="Strike Currency"/>
		<rate  field="Strike price (cap/floor rate)"/>
		<!-- payout -->
		<numberOfOptions  field="Number of options"/>
		<optionEntitlement  field="Option entitlement"/>
	</xsl:variable>
	<xsl:variable name="optiontemplate" select="common:node-set($optiontemplateTF)"/>

	<xsl:variable name="irtemplateTF">
		<!-- reporting party pays type -->
		<reportingPartyPaysType field="Reporting party pays type"/>
		<counterPartyPaysType field="Counterparty pays type"/>
		<floatingPaymentFrequency  field="Floating rate payment frequency"/>
		<floatingCalculationFrequency>  
			<periodMultiplier field="Floating rate calculation frequency multiplier"/>
			<period field="Floating rate calculation frequency period"/>
		</floatingCalculationFrequency>  
		<floatingResetFrequency>
			<periodMultiplier field="Floating rate reset frequency multiplier"/>
			<period field="Floating rate reset frequency period"/>
		</floatingResetFrequency>
		<floatingDayCountFraction field="Floating rate day count fraction"/>
		<floatingRateIndexTenor>
			<periodMultiplier field="Floating rate index rate period multiplier (leg 1)"/>
			<period field="Floating rate index rate period (leg 1)"/>
		</floatingRateIndexTenor>
		<floatingRateIndexTenor2>
			<periodMultiplier field="Floating rate index rate period multiplier (leg 2)"/>
			<period field="Floating rate index rate period (leg 2)"/>
		</floatingRateIndexTenor2>
		<floatingRateSpread field="Floating rate spread"/>
		<floatingRateSpread2 field="Floating rate spread (leg 2)"/>
		<fixedRate field="Fixed rate (Leg 1)"/>
		<fixedRate2 field="Fixed rate (leg2)"/>
		<fixedPaymentFrequency>
			<periodMultiplier field="Fixed leg payment frequency multiplier"/>
			<period field="Fixed leg payment frequency period"/>
		</fixedPaymentFrequency>
		<fixedCalculationFrequency>
			<periodMultiplier field="Fixed rate calculation frequency multiplier"/>
			<period field="Fixed rate calculation frequency period"/>
		</fixedCalculationFrequency>
		<fixedDayCountFraction field="Fixed rate day count fraction"/>
		<fraDiscounting field="FRA Discounting"/>
		<initialIndexLevel copy=".//initialIndexLevel"/>
		<indexSource copy=".//indexSource"/>
		<mainPublication copy=".//mainPublication"/>
		<interpolationMethod copy=".//interpolationMethod"/>
		<inflationLag copy=".//inflationLag"/>
		<initialPrincipalExchange field="Initial Principal Exchange Indicator"/>
		<finalPrincipalExchange field="Final Principal Exchange Indicator"/>
		<principalExchangeAmount1 field="Principal exchange amount 1"/>
		<principalExchangeCurrency1 field="Principal exchange currency 1"/>
		<principalExchangeAmount2 field="Principal exchange amount 2"/>
		<principalExchangeCurrency2 field="Principal exchange currency 2"/>
		<leg1Schedules context="Leg 1">
			<notionalStepSchedule copy=".//notionalStepSchedule"/>
			<fixedRateSchedule copy=".//fixedRateSchedule"/>
			<capRateSchedule copy=".//capRateSchedule"/>
			<floorRateSchedule copy=".//floorRateSchedule"/>
			<spreadSchedule copy=".//spreadSchedule"/>
			<floatingRateMultiplierSchedule copy=".//floatingRateMultiplierSchedule"/>
		</leg1Schedules>
		<leg2Schedules context="Leg 2">
			<notionalStepSchedule copy=".//notionalStepSchedule"/>
			<fixedRateSchedule copy=".//fixedRateSchedule"/>
			<capRateSchedule copy=".//capRateSchedule"/>
			<floorRateSchedule copy=".//floorRateSchedule"/>
			<spreadSchedule copy=".//spreadSchedule"/>
			<floatingRateMultiplierSchedule copy=".//floatingRateMultiplierSchedule"/>
		</leg2Schedules>
		<earlyTerminationProvision copy=".//earlyTerminationProvision"/>
		<cancelableProvision copy=".//cancelableProvision"/>
		<extendibleProvision copy=".//extendiblPerovision"/>
	</xsl:variable>
	<xsl:variable name="irtemplate" select="common:node-set($irtemplateTF)"/>

	<xsl:variable name="fxtemplateTF">
		<valueDate field="Value date"/>
		<exchangeRate structure="Exchange rate structure" nowrap="true"/>
		<!-- exchange rate -->
		<!-- exchange rate 2-->
	</xsl:variable>
	<xsl:variable name="fxtemplate" select="common:node-set($fxtemplateTF)"/>


	<xsl:variable name="credittemplateTF">
		<referenceObligation copy=".//referenceObligation"/>
		<protectionBuyerReference reference="Counterparty purchasing protection"/>
		<protectionSellerReference reference="Counterparty selling protection"/>
		<fixedRate field="Fixed rate for calculating payments to the seller of credit protection"/>
		<paymentFrequency>
			<periodMultiplier field="Payment frequency multiplier"/>
			<period field="Payment frequency period"/>
		</paymentFrequency>
		<priceMultipler field="Price Multipler"/>
		<indexSeries copy="//indexSeries"/>
		<indexAnnexVersion copy="//indexAnnexVersion"/>
		<indexFactor field="Index Factor"/>
		<seniority field="Seniority"/>
		<tranche copy="//tranche"/>
	</xsl:variable>
	<xsl:variable name="credittemplate" select="common:node-set($credittemplateTF)"/>


	<xsl:variable name="equitytemplateTF">
		<revaluationFrequency>
			<periodMultiplier field="Periodicity of underlying asset revaluation - multiplier"/>
			<period field="Periodicity of underlying asset revaluation - period"/>
		</revaluationFrequency>
		<paymentFrequency>
			<periodMultiplier field="Payment frequency multiplier"/>
			<period field="Payment frequency period"/>
		</paymentFrequency>
		<priceMultipler field="Price Multipler"/>
		<fixedPrice field="Fixed price"/>
		<dividendAmountPayerPartyReference reference="Dividend Amount Payer"/>
		<dividendAmountReceiverPartyReference reference="Dividend Amount Receiver"/>
		<dividendPaymentDate field="Dividend Payment Date"/>
		<dividendPeriodStartDate field="Dividend Period Start Date"/>
		<dividendPeriodEndDate field="Dividend Period End Date"/>
		<dividendPeriodStrike field="Dividend Period Strike"/>
		<dividendPeriodFixedPayment field="Dividend Period Fixed Payment"/>
		<specialDividend field="Special Dividends"/>
		<materialDividend field="Material Dividend"/>

		<fixedAmountPayerPartyReference reference="Fixed Amount Payer"/>
		<fixedAmountReceiverPartyReference reference="Fixed Amount Receiver"/>
		<fixedAmountPaymentDate reference="Fixed Amount Payment Date"/>
		<fixedPaymentAmount field="Fixed Payment Amount"/>

		<floatingRateIndex field="Floating Rate Index"/>
		<indexTenor>
			<periodMultiplier field="Index Tenor Period Multiplier"/>
			<period field="Index Tenor Period"/>
		</indexTenor>
		<finalValuationDate field="Final Valuation Date"/>
		<valuationDate field="Valuation Date"/>
		<returnType field="Return Type"/>
		<referenceCurrency field="Reference Currency"/>
		<optionalEarlyTermination field="Optional Early Termination"/>
		<optionalEarlyTerminationDate field="Optional Early Termination Date"/>
		<optionalEarlyTerminationElectingPartyReference reference="Optional Early Termination Electing Party"/>
		<optionalEarlyTermination field="Optional Early Termination Type"/>

		<observationStartDate field="Observation Start Date"/>
		<varianceAmount field="Variance Amount"/>
		<varianceCapFactor field="Variance Cap Factor"/>
		<varianceStrikePrice field="Variance Strike Price"/>
		<volatilityStrikePrice field="Volatility Strike Price"/>
	</xsl:variable>
	<xsl:variable name="equitytemplate" select="common:node-set($equitytemplateTF)"/>


	<xsl:variable name="commoditytemplateTF">
		<commodity structure="Commodity structure" nowrap="true"/>
		<!--
		<commodityDetails xpath="//productId[contains(@productIdScheme, 'http://www.dtcc.com/coding-scheme/external/commodity-details')]"/>
		-->
		<commodityDetails field="Commodity Details"/>
		<fixedPrice field="Fixed price"/>
		<priceCurrency field="Price currency"/>
		<priceUnits field="Price unit"/>
		<deliveryPoint field="Delivery point or zone"/>
		<interconnectionPoint field="Interconnection Point"/>
		<loadType field="Load Type"/>
		<applicableDay xpath=".//applicableDay"/>
		<deliveryType field="Delivery type"/>
		<deliveryCapacity field="Contract capacity"/>
		<deliveryPeriods field="Delivery Period"/>
		<!-- delivery periods -->
		<quantity structure="Quantity structure" nowrap="true"/>
		<type field="Type"/>
		<grade field="Grade"/>
		<periodQuantity field="Period quantity"/>
		<totalQuantity field="Total quantity"/>
		<quantityUnit field="Quantity unit"/>
	</xsl:variable>
	<xsl:variable name="commoditytemplate" select="common:node-set($commoditytemplateTF)"/>


	<!--
	<xsl:template match="*" priority="1">
		<xsl:comment> ** </xsl:comment>
		<xsl:copy>
			<xsl:apply-templates select="@*|node()"/>
		</xsl:copy>
	</xsl:template>	
	-->

	<xsl:template match="node() | @*" priority="-1">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()"/>
		</xsl:copy>
	</xsl:template>	

	<!-- prepend newline on initial comment -->
	<xsl:template match="comment()[contains(.,'Copyright')]" >
		<xsl:text>
</xsl:text>
		<xsl:copy-of select ="."/>
	</xsl:template>	

	<!-- convert party references to party IDs and names -->
	<!--
	<xsl:template match="*[contains(local-name(.),'Reference')]" >
		<xsl:variable name="oldtag" select="local-name(.)"/>
		<xsl:variable name="role" select="substring-before($oldtag,'Reference')"/>
		<xsl:variable name="newtag" select="concat($role,'Id')"/>
		<xsl:variable name="href" select="@href"/>
		<xsl:variable name="targ" select="//party[@id=$href]"/>
		<xsl:element name="{$newtag}">
			<xsl:copy-of select="$targ/partyId[1]/@*"/>
			<xsl:value-of select="$targ/partyId[1]"/>
		</xsl:element>
		/* party names if relevant */
		<xsl:if test="$role = 'reportingParty' or $role = 'counterParty' or $role = 'beneficiary'">
			<xsl:variable name="nametag" select="concat($role, 'Name')"/>
			<xsl:element name="{$nametag}">
				<xsl:value-of select="$targ/partyName"/>
			</xsl:element>
		</xsl:if>
		<xsl:if test="$role = 'reportingParty'">
			<xsl:variable name="emailtag" select="concat($role, 'Email')"/> just a pretty diagram that expresses that same opinion
			<xsl:element name="{$emailtag}">
				<xsl:value-of select="$targ/contactInfo/email[1]"/>
			</xsl:element>
		</xsl:if>
	</xsl:template>	

	<xsl:template match="branchReference"  priority="10">
		<xsl:variable name="href" select="@href"/>
		<xsl:variable name="targ" select="//businessUnit[@id=$href]"/>
		<branchId>
			<xsl:value-of select="$targ/name"/>
		</branchId>
	</xsl:template>	
	/* remove the party blocks */
	<xsl:template match="/regulatorReport/party" />
-->
<!-- MiFIR specific -->
	<xsl:template match="mifirInformation" priority="100">
		<xsl:copy>
			<xsl:copy-of select="*"/>
			<xsl:apply-templates mode="mifir-buyer" select="../../productInformation"/>
		</xsl:copy>
	</xsl:template>	

	<!-- skip if no product -->
	<xsl:template match="productInformation[primaryAssetClass]" priority="100" mode="mifir-buyer"/>

	<xsl:template match="productInformation" mode="mifir-buyer">
		<xsl:apply-templates mode="mifir-buyer" select="*[1]"/>
	</xsl:template>	

	<xsl:template match="*" mode="mifir-buyer" priority="-1" />

	<xsl:template match="*[.//buyerPartyReference]" mode="mifir-buyer">
		<xsl:copy-of select=".//buyerPartyReference[1]"/>
		<xsl:copy-of select=".//sellerPartyReference[1]"/>
	</xsl:template>	

	<xsl:template match="swap[swapStream//fixedRateSchedule]" mode="mifir-buyer">
		<xsl:variable name="fixedStream" select="swapStream[.//fixedRateSchedule][1]"/>
		<buyerPartyReference href="{$fixedStream/payerPartyReference/@href}"/>
		<sellerPartyReference href="{$fixedStream/receiverPartyReference/@href}"/>
	</xsl:template>	

	<xsl:template match="swap" mode="mifir-buyer">
		<xsl:variable name="tenor1" select="swapStream[1]//indexTenor/periodMultiplier"/>
		<xsl:variable name="tenor2" select="swapStream[2]//indexTenor/periodMultiplier"/>
		<xsl:choose>
			<xsl:when test="$tenor1 &gt; $tenor2">
				<buyerPartyReference href="{swapStream[1]/payerPartyReference/@href}"/>
				<sellerPartyReference href="{swapStream[1]/receiverPartyReference/@href}"/>
			</xsl:when>
			<xsl:otherwise>
				<buyerPartyReference href="{swapStream[2]/payerPartyReference/@href}"/>
				<sellerPartyReference href="{swapStream[2]/receiverPartyReference/@href}"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>	

	<xsl:template match="fxSingleLeg" mode="mifir-buyer">
		<buyerPartyReference href="{exchangedCurrency1/payerPartyReference/@href}"/>
		<sellerPartyReference href="{exchangedCurrency1/receiverPartyReference/@href}"/>
	</xsl:template>	
		
	<xsl:template match="varianceSwap" mode="mifir-buyer">
		<xsl:variable name="stream" select="varianceLeg"/>
		<buyerPartyReference href="{$stream/payerPartyReference/@href}"/>
		<sellerPartyReference href="{$stream/receiverPartyReference/@href}"/>
	</xsl:template>	

	<xsl:template match="commoditySwap[fixedLeg]" mode="mifir-buyer" priority="10">
		<xsl:variable name="stream" select="fixedLeg"/>
		<buyerPartyReference href="{$stream/payerPartyReference/@href}"/>
		<sellerPartyReference href="{$stream/receiverPartyReference/@href}"/>
	</xsl:template>	

	<xsl:template match="commoditySwap[floatingLeg]" mode="mifir-buyer" priority="2">
		<xsl:variable name="stream" select="floatingLeg"/>
		<buyerPartyReference href="{$stream/payerPartyReference/@href}"/>
		<sellerPartyReference href="{$stream/receiverPartyReference/@href}"/>
	</xsl:template>	


	<!-- skip if already converted -->
	<xsl:template match="productInformation[primaryAssetClass]" priority="100">
		<xsl:copy-of select="."/>
	</xsl:template>	

	<!-- convert full product to flattenedversion -->
	<xsl:template match="productInformation" priority="10">
		<xsl:copy>
			<xsl:variable name="product" select="*[1]/productType"/>
			<!--<xsl:message>product = <xsl:value-of select="$product"/></xsl:message>-->
			<!--<xsl:message>file = <xsl:value-of select="$productDefs-file"/></xsl:message>-->
			<!--<xsl:message>Num products = <xsl:value-of select="count($productDefs//prod:product)"/></xsl:message>-->
			<xsl:variable name="defs" select="$productDefs/prod:product[@product=$product][1]"/>
			<xsl:if test="$productDefs/prod:product[@product=$product][2]">
				<!--<xsl:message>******* MULTIPLE DEFS FOR <xsl:value-of select="$product"/></xsl:message>-->
			</xsl:if>
			<xsl:choose>
				<xsl:when test="count($defs) != 0">
					<xsl:comment>Found <xsl:value-of select="$product"/></xsl:comment>
					<xsl:apply-templates mode="xpath-product-processor" select=".">
						<xsl:with-param name="defs" select="$defs"/>
					</xsl:apply-templates>
				</xsl:when>
				<xsl:otherwise>
					<xsl:apply-templates mode="default-product-processor" select="."/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:copy>
	</xsl:template>
	
	<!-- convert full product to flattenedversion -->
	<xsl:template match="trade" priority="10">
		<xsl:comment>found trade</xsl:comment>
		<xsl:copy>
			<xsl:variable name="product" select="*[2]/productType"/>
			<!--<xsl:message>product = <xsl:value-of select="$product"/></xsl:message>-->
			<!--<xsl:message>file = <xsl:value-of select="$productDefs-file"/></xsl:message>-->
			<!--<xsl:message>Num products = <xsl:value-of select="count($productDefs//prod:product)"/></xsl:message>-->
			<xsl:variable name="defs" select="$productDefs/prod:product[@product=$product][1]"/>
			<xsl:if test="$productDefs/prod:product[@product=$product][2]">
				<!--<xsl:message>******* MULTIPLE DEFS FOR <xsl:value-of select="$product"/></xsl:message>-->
			</xsl:if>
			<xsl:choose>
				<xsl:when test="count($defs) != 0">
					<xsl:copy-of select="tradeHeader"/>
					<xsl:comment>Found <xsl:value-of select="$product"/></xsl:comment>
					<xsl:apply-templates mode="xpath-product-processor" select=".">
						<xsl:with-param name="defs" select="$defs"/>
					</xsl:apply-templates>
					<xsl:copy-of select="*[position() &gt; 2]"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:apply-templates mode="default-product-processor" select="."/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:copy>
	</xsl:template>
	
	<xsl:template match="trade" mode="xpath-product-processor" priority="1000">
		<xsl:param name="defs"/>
		<!--
		<xsl:apply-templates mode="copy-product-classification" select="*[2]/*"/>
		<xsl:copy-of select="nonStandardTerms"/>
		-->
		<xsl:apply-templates mode="fill-template" select="$template/*">
			<xsl:with-param name="doc" select=".."/>
			<xsl:with-param name="defs" select="$defs"/>
		</xsl:apply-templates>

		<!--
		<xsl:variable name="product" select="*[2]/productType[contains(@productTypeScheme, 'taxonomy')]"/>
		<xsl:if test="contains($product, 'ption')">
			<xsl:apply-templates mode="option" select=".">
				<xsl:with-param name="defs" select="$defs"/>
			</xsl:apply-templates>
		</xsl:if>
		-->

		<!--
		<xsl:comment> asset class specific </xsl:comment>
		<xsl:apply-templates mode="asset-class-specific" select=".">
			<xsl:with-param name="defs" select="$defs"/>
		</xsl:apply-templates>
		-->
	</xsl:template>

	<xsl:template match="productInformation" mode="xpath-product-processor" priority="1000">
		<xsl:param name="defs"/>
		<xsl:apply-templates mode="copy-product-classification" select="*[1]/*"/>
		<xsl:copy-of select="nonStandardTerms"/>
		<xsl:apply-templates mode="fill-template" select="$template/*">
			<xsl:with-param name="doc" select=".."/>
			<xsl:with-param name="defs" select="$defs"/>
		</xsl:apply-templates>

		<xsl:variable name="product" select="*[1]/productType[contains(@productTypeScheme, 'taxonomy')]"/>
		<xsl:if test="contains($product, 'ption')">
			<xsl:apply-templates mode="option" select=".">
				<xsl:with-param name="defs" select="$defs"/>
			</xsl:apply-templates>
		</xsl:if>

		<!--
		<xsl:comment> asset class specific </xsl:comment>
		-->
		<xsl:apply-templates mode="asset-class-specific" select=".">
			<xsl:with-param name="defs" select="$defs"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="productInformation" mode="option" >
		<xsl:param name="defs" />
		<optionDetails>
			<xsl:variable name="doc" select=".."/>
			<xsl:apply-templates mode="fill-template" select="$optiontemplate/*">
				<xsl:with-param name="doc" select="$doc"/>
				<xsl:with-param name="defs" select="$defs"/>
			</xsl:apply-templates>
		</optionDetails>
	</xsl:template>
	<xsl:template match="productInformation" mode="asset-class-specific" />

	<xsl:template match="productInformation[*/primaryAssetClass = 'InterestRate']" mode="asset-class-specific" priority="100" >
		<xsl:param name="defs" />
		<interestRateDetails>
			<xsl:variable name="doc" select=".."/>
			<xsl:apply-templates mode="fill-template" select="$irtemplate/*">
				<xsl:with-param name="doc" select="$doc"/>
				<xsl:with-param name="defs" select="$defs"/>
			</xsl:apply-templates>
		</interestRateDetails>
	</xsl:template>

	<xsl:template match="productInformation[*/primaryAssetClass = 'ForeignExchange']" mode="asset-class-specific"  priority="100">
		<xsl:param name="defs" />
		<fxDetails>
			<xsl:variable name="doc" select=".."/>
			<xsl:apply-templates mode="fill-template" select="$fxtemplate/*">
				<xsl:with-param name="doc" select="$doc"/>
				<xsl:with-param name="defs" select="$defs"/>
			</xsl:apply-templates>
		</fxDetails>
	</xsl:template>

	<xsl:template match="productInformation[*/primaryAssetClass = 'Equity']" mode="asset-class-specific"  priority="100">
		<xsl:param name="defs" />
		<!--
		<xsl:comment> equity class specific </xsl:comment>
		-->
		<equityDetails>
			<xsl:variable name="doc" select=".."/>
			<xsl:apply-templates mode="fill-template" select="$equitytemplate/*">
				<xsl:with-param name="doc" select="$doc"/>
				<xsl:with-param name="defs" select="$defs"/>
			</xsl:apply-templates>
		</equityDetails>
	</xsl:template>

	<xsl:template match="productInformation[*/primaryAssetClass = 'Commodity']" mode="asset-class-specific"  priority="100">
		<xsl:param name="defs" />
		<commodityDetails>
			<xsl:variable name="doc" select=".."/>
			<xsl:apply-templates mode="fill-template" select="$commoditytemplate/*">
				<xsl:with-param name="doc" select="$doc"/>
				<xsl:with-param name="defs" select="$defs"/>
			</xsl:apply-templates>
		</commodityDetails>
	</xsl:template>

	<xsl:template match="productInformation[*/primaryAssetClass = 'Credit']" mode="asset-class-specific"  priority="100">
		<xsl:param name="defs" />
		<creditDetails>
			<xsl:variable name="doc" select=".."/>
			<xsl:apply-templates mode="fill-template" select="$credittemplate/*">
				<xsl:with-param name="doc" select="$doc"/>
				<xsl:with-param name="defs" select="$defs"/>
			</xsl:apply-templates>
		</creditDetails>
	</xsl:template>

	<xsl:template match="productInformation" mode="fill-asset-template" >
		<xsl:param name="template"/>
		<xsl:apply-templates mode="fill-template" select="$template/*">
			<xsl:with-param name="doc" select=".."/>
			<xsl:with-param name="defs" select="$template"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="*[@context]" mode="fill-template" priority="200" >
		<xsl:param name="defs" />

		<xsl:variable name="context" select="@context"/>
		<xsl:variable name="xpath" select="($defs/prod:section/prod:field[@name=$context])[1]"/>

		<xsl:variable name="idval">
			<xsl:apply-templates select="$docWithIds" mode="evaluate-xpath">
				<xsl:with-param name="xpath" select="concat($xpath,'/@xxxid')"/>
			</xsl:apply-templates>
		</xsl:variable>

		<!-- 
		<xsl:comment>Context = <xsl:value-of select="$context"/>, xpath=<xsl:value-of select="$xpath"/>, idval=<xsl:value-of select="$idval"/></xsl:comment>
		-->

		<xsl:if test="string-length($idval) &gt; 0">
			<xsl:variable name="docWIds" select ="$docWithIds//*[@xxxid = $idval]"/>
			<xsl:variable name="doc" >
				<xsl:apply-templates mode="strip-id" select ="$docWithIds//*[@xxxid = $idval]"/>
			</xsl:variable>
			<!--
			<xsl:comment>Doc frag root is <xsl:value-of select="local-name($doc/*[1])"/></xsl:comment>
			-->

			<xsl:variable name="content">
				<xsl:apply-templates mode="fill-template" select="node()">
					<xsl:with-param name="doc" select="$doc"/>
					<xsl:with-param name="defs" select="$defs"/>
					<xsl:with-param name="docWIds" select="$docWIds"/>
				</xsl:apply-templates>
			</xsl:variable>
			<xsl:if test="$content/* or $content/text()">
				<xsl:copy>
					<xsl:copy-of select="$content"/>
				</xsl:copy>
			</xsl:if>
		</xsl:if> 
	</xsl:template>




	<xsl:template match="*[@reference]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:variable name="fieldname" select="@reference"/>
		<!--<xsl:message>[[[[[[ Working on <xsl:value-of select="$fieldname"/> ]]]]</xsl:message>-->
		<xsl:variable name="elem" select="local-name(.)"/>

		<xsl:apply-templates mode="get-xpath-reference" select="$doc">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
		</xsl:apply-templates>
	</xsl:template>


	<xsl:template match="*[@structure]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:param name="nowrap" select="@nowrap"/>
		<xsl:param name="children" select="@children"/>
		<xsl:variable name="fieldname" select="@structure"/>
		<!--
		<xsl:comment>Working on <xsl:value-of select="$fieldname"/>, wrap = <xsl:value-of select="not($nowrap)"/></xsl:comment>
		-->
		<xsl:variable name="elem" select="local-name(.)"/>

		<xsl:apply-templates mode="get-xpath-copy-structure" select="$docWithIds">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
			<xsl:with-param name="wrap" select="not($nowrap)"/>
			<xsl:with-param name="children" select="$children"/>
			<xsl:with-param name="debug" select="1"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="*[@paymentstructure]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:param name="children" select="@children"/>
		<xsl:param name="nowrap" select="@nowrap"/>
		<xsl:variable name="fieldname" select="@paymentstructure"/>
		<xsl:variable name="elem" select="local-name(.)"/>
		<!--<xsl:message>Working on payment structure <xsl:value-of select="$fieldname"/>, children=<xsl:value-of select="$children"/>, nowrap=<xsl:value-of select="$nowrap"/>, elem=<xsl:value-of select="$elem"/></xsl:message>-->

		<xsl:variable name="pmt">
			<xsl:apply-templates mode="get-xpath-copy-structure" select="$docWithIds">
				<xsl:with-param name="fieldname" select="$fieldname"/>
				<xsl:with-param name="defs" select="$defs"/>
				<xsl:with-param name="elem" select="$elem"/>
				<xsl:with-param name="children" select="$children"/>
				<xsl:with-param name="wrap" select="not($nowrap)"/>
				<xsl:with-param name="debug" select="1"/>
			</xsl:apply-templates>
		</xsl:variable>
		<xsl:apply-templates mode="cleanup-payment" select="$pmt">
			<xsl:with-param name="elem" select="$elem"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="*" mode="cleanup-payment" >
		<xsl:param name="elem" select="local-name(.)"/>
		<!--<xsl:message>Cleanup payment for <xsl:value-of select="local-name(.)"/> </xsl:message>-->
		<xsl:apply-templates mode="cleanup-payment" select="*">
			<xsl:with-param name="elem" select="$elem"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="otherPayment|premium|additionalPayment|payment1|payment2" mode="cleanup-payment" priority="2">
		<xsl:param name="elem" select="local-name(.)"/>
		<!--<xsl:message>Cleanup payment for <xsl:value-of select="local-name(.)"/> with elem=<xsl:value-of select="$elem"/></xsl:message>-->
		<xsl:element name="{$elem}" namespace="{$target.ns}">
			<xsl:copy-of select="payerPartyReference"/>
			<xsl:copy-of select="receiverPartyReference"/>
			<xsl:apply-templates mode="cleanup-payment" select="paymentDate"/>
			<xsl:copy-of select="paymentAmount"/>
			<xsl:copy-of select="paymentType"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="paymentDate" mode="cleanup-payment" >
		<xsl:copy>
			<adjustableDate>
				<unadjustedDate>
					<xsl:value-of select=".//unadjustedDate"/>
				</unadjustedDate>
			</adjustableDate>
			<!--
			<xsl:apply-templates mode="cleanup-payment" select="*"/>
			-->
		</xsl:copy>
	</xsl:template>

	<xsl:template match="unadjustedDate|dateAdjustments" mode="cleanup-payment" >
		<xsl:copy-of select="."/>
	</xsl:template>

	<xsl:template match="*" mode="get-xpath-reference" >
		<xsl:param name="defs"/>
		<xsl:param name="fieldname"/>
		<xsl:param name="elem"/>
		<xsl:variable name="xpath" select="$defs/prod:section/prod:field[@name=$fieldname]"/>
		<!--<xsl:message>[[[[[ Working on <xsl:value-of select="$fieldname"/>, xpath=<xsl:value-of select="$xpath"/> ]]]]]]</xsl:message>-->
		<!--
		<xsl:comment> Processing <xsl:value-of select="$fieldname"/> for <xsl:value-of select="$elem"/>, xpath = <xsl:value-of select="$xpath"/></xsl:comment>
		-->

		<xsl:if test="string-length($xpath) &gt; 1">
			<xsl:variable name="idval">
				<xsl:apply-templates select="." mode="evaluate-xpath">
					<xsl:with-param name="xpath" select="$xpath"/>
				</xsl:apply-templates>
			</xsl:variable>
			<!--<xsl:message>[[[[[[[[  idval=<xsl:value-of select="$idval"/> ]]]]]]]]</xsl:message>-->
			<xsl:if test="string-length($idval) &gt; 0">
				<xsl:element name="{$elem}">
					<xsl:attribute name="href" select="$idval"/>
				</xsl:element>
			</xsl:if>
		</xsl:if>
	</xsl:template>

	<xsl:template match="*" mode="get-xpath-copy-structure" >
		<xsl:param name="defs"/>
		<xsl:param name="fieldname"/>
		<xsl:param name="children"/>
		<xsl:param name="elem"/>
		<xsl:param name="wrap"/>
		<xsl:param name="skipelement" select="false"/>
		<xsl:param name="docWIds" select="$docWithIds"/>
		<xsl:param name="xpath" select="$defs/prod:section/prod:field[@name=$fieldname]"/>
		<xsl:param name="copyatts" select="1"/>
		<xsl:param name="debug" select="0"/>

		<xsl:if test="$debug=1">
			<!--<xsl:message><xsl:value-of select="$fieldname"/> <xsl:value-of select="$xpath"/></xsl:message>-->
		</xsl:if>

		<xsl:apply-templates mode="xpath-copy-structure" select=".">
			<xsl:with-param name="elem" select="$elem"/>
			<xsl:with-param name="xpath" select="$xpath"/>
			<xsl:with-param name="wrap" select="$wrap"/>
			<xsl:with-param name="skipelement" select="$skipelement"/>
			<xsl:with-param name="docWIds" select="$docWIds"/>
			<xsl:with-param name="copyatts" select="$copyatts"/>
			<xsl:with-param name="children" select="$children"/>
			<xsl:with-param name="debug" select="$debug"/>
		</xsl:apply-templates>
	</xsl:template>



	<xsl:template match="*" mode="evaluate-xpath" >
		<xsl:param name="xpath"/>
		<xsl:variable name="xp2" select="replace($xpath, 'rec:nonpublicExecutionReport/', '')"/>
		<!--
		<xsl:variable name="xp21" select="replace($xp2, 'rec:trade/rec:tradeHeader', 'rec:transactionIdentification')"/> 
		<xsl:variable name="xp3" select="replace($xp21, 'rec:trade/', 'rec:productInformation/')"/>
		-->
		<xsl:variable name="xp3" select="replace($xp2, 'rec:regulatoryDisclosure/', '')"/>
		<xsl:variable name="xp4">
			<xsl:call-template name="map-xpath-variables">
				<xsl:with-param name="xpath" select="$xp3"/>
			</xsl:call-template>
		</xsl:variable>
		<xsl:if test="contains($xpath, 'tradeHeader')">
			<!--<xsl:message>********* got <xsl:value-of select="$xp3"/>, <xsl:value-of select="$xp4"/> ****** <xsl:value-of select="saxon:evaluate($xp4)"/> </xsl:message>-->
		</xsl:if>
		<xsl:if test="contains($xpath, 'fixedRateSchedule')">
			<!--<xsl:message>********* got <xsl:value-of select="$xp3"/>, <xsl:value-of select="$xp4"/> ******  </xsl:message>-->
		</xsl:if>
		<xsl:choose>
			<xsl:when test="contains($xp4, 'if(')">
				<!--<xsl:message>**** evaluating <xsl:value-of select="$xp4"/> ****</xsl:message>-->
				<xsl:apply-templates mode="evaluate-if-xpath" select=".">
					<xsl:with-param name="xpath" select="$xp4"/>
				</xsl:apply-templates>
			</xsl:when>
			<xsl:otherwise>
				<xsl:if test="contains($xp4, 'additional')">
					<!--<xsl:message>Evaluating <xsl:value-of select="$xp4"/></xsl:message>	-->
				</xsl:if>
				<xsl:value-of select="saxon:evaluate($xp4)"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="*" mode="evaluate-if-xpath" >
		<xsl:param name="xpath"/>
		<xsl:variable name="strippif" select="substring-after($xpath, 'if(')"/>
		<xsl:variable name="strippiflen" select="string-length($strippif)"/>
		<xsl:variable name="args" select="substring($strippif,1, $strippiflen -1)"/>
		<!--
		<xsl:message> if args are <xsl:value-of select="$args"/></xsl:message>
		-->
		<xsl:variable name="chars" select="string-to-codepoints($args)"/>
		<!--
		<xsl:message> args has <xsl:value-of select="count($chars)"/> chars </xsl:message>
		-->
		<xsl:variable name="commaloc">
			<xsl:call-template name="find-comma-loc">
				<xsl:with-param name="firstchar" select="$chars[1]"/>
				<xsl:with-param name="nestlevel" select="0"/>
				<xsl:with-param name="chars"  select="$chars[position() &gt; 1]"/>
			</xsl:call-template>
		</xsl:variable>
		<xsl:variable name="test" select="substring($args, 1, string-length($args) - $commaloc - 1)"/>
		<!--<xsl:message> !!!! test has <xsl:value-of select="$test"/> </xsl:message>-->
		<xsl:variable name="rest" select="substring($args, string-length($args) - $commaloc + 1)"/>
		<xsl:variable name="restchars" select="string-to-codepoints($rest)"/>
		<xsl:variable name="commaloc2">
			<xsl:call-template name="find-comma-loc">
				<xsl:with-param name="firstchar" select="$restchars[1]"/>
				<xsl:with-param name="nestlevel" select="0"/>
				<xsl:with-param name="chars"  select="$restchars[position() &gt; 1]"/>
			</xsl:call-template>
		</xsl:variable>
		<xsl:variable name="trueval" select="substring($rest, 1, string-length($rest) - $commaloc2 - 1)"/>
		<!--<xsl:message> !!!! true val has <xsl:value-of select="$trueval"/> </xsl:message>-->
		<xsl:variable name="falseval" select="substring($rest, string-length($rest) - $commaloc2 + 1)"/>
		<!--<xsl:message> !!!! false val has <xsl:value-of select="$falseval"/> </xsl:message>-->
		<xsl:variable name="testval" >
			<xsl:apply-templates select="." mode="evaluate-xpath">
				<xsl:with-param name="xpath" select="$test"/>
			</xsl:apply-templates>
		</xsl:variable>

		<!--<xsl:message> !!!! test val has <xsl:value-of select="$testval"/> </xsl:message>-->
		<xsl:choose>
			<xsl:when test="string($testval) = 'true'">
				<!--<xsl:message> ! got true </xsl:message>-->
				<xsl:apply-templates select="." mode="evaluate-xpath">
					<xsl:with-param name="xpath" select="$trueval"/>
				</xsl:apply-templates>
			</xsl:when>
			<xsl:otherwise>
				<!--<xsl:message> ! got false </xsl:message>-->
				<xsl:apply-templates select="." mode="evaluate-xpath">
					<xsl:with-param name="xpath" select="$falseval"/>
				</xsl:apply-templates>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="find-comma-loc" >
		<xsl:param name="nestlevel" select="0"/>
		<xsl:param name="firstchar" />
		<xsl:param name="chars" />

		<xsl:choose>
			<xsl:when test="$firstchar = string-to-codepoints('(')">
				<xsl:call-template name="find-comma-loc">
					<xsl:with-param name="firstchar" select="$chars[1]"/>
					<xsl:with-param name="nestlevel" select="$nestlevel + 1"/>
					<xsl:with-param name="chars"  select="$chars[position() &gt; 1]"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="$firstchar = string-to-codepoints(')')">
				<xsl:call-template name="find-comma-loc">
					<xsl:with-param name="firstchar" select="$chars[1]"/>
					<xsl:with-param name="nestlevel" select="$nestlevel - 1"/>
					<xsl:with-param name="chars"  select="$chars[position() &gt; 1]"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="$firstchar = string-to-codepoints(',') and $nestlevel = 0">
				<xsl:value-of select="count($chars)"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="find-comma-loc">
					<xsl:with-param name="firstchar" select="$chars[1]"/>
					<xsl:with-param name="nestlevel" select="$nestlevel"/>
					<xsl:with-param name="chars"  select="$chars[position() &gt; 1]"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!--
	<xsl:template mode="find-comma-loc" match="node()">
		<xsl:param name="nestlevel" select="0"/>
		<xsl:param name="characters" />
			<xsl:apply-templates mode="find-comma-loc" select="$characters[1]">
				<xsl:with-param name="nestlevel" select="$nestlevel"/>
				<xsl:with-param name="characters"  select="$characters[position() &gt; 1]"/>
			</xsl:apply-templates>
	</xsl:template>

	<xsl:template mode="find-comma-loc" match="'('">
		<xsl:param name="nestlevel" select="0"/>
		<xsl:param name="characters" />
		<xsl:apply-templates mode="find-comma-loc" select="$characters[1]">
			<xsl:with-param name="nestlevel" select="$nestlevel+1"/>
			<xsl:with-param name="characters"  select="$characters[position() &gt; 1]"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template mode="find-comma-loc" match="')'">
		<xsl:param name="nestlevel" select="0"/>
		<xsl:param name="characters" />
		<xsl:apply-templates mode="find-comma-loc" select="$characters[1]">
			<xsl:with-param name="nestlevel" select="$nestlevel-1"/>
			<xsl:with-param name="characters"  select="$characters[position() &gt; 1]"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template mode="find-comma-loc" match="','">
		<xsl:param name="nestlevel" select="0"/>
		<xsl:param name="characters" />
		<xsl:choose>
			<xsl:when test="$nestlevel = 0">
				<xsl:value-of select="Count($chars)"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates mode="find-comma-loc" select="$characters[1]">
					<xsl:with-param name="nestlevel" select="$nestlevel"/>
					<xsl:with-param name="characters"  select="$characters"/>
				</xsl:apply-templates>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	-->

	<xsl:template name="map-xpath-variables">
		<xsl:param name="xpath"/>
		<xsl:variable name="has" select="contains($xpath, '$')"/>
		<xsl:variable name="xp0" select="replace($xpath, 'rec:\$reportingPartyRef', '//rec:reportingPartyReference/@href')"/>
		<xsl:variable name="xp1" select="replace($xp0, '$reportingPartyRef', '//rec:reportingPartyReference/@href')"/>
		<xsl:variable name="xp2" select="replace($xp1, 'rec:\$counterPartyRef', '//rec:counterPartyReference/@href')"/>
		<xsl:variable name="xp3" select="replace($xp2, '$counterPartyRef', '//rec:counterPartyReference/@href')"/>
		<xsl:if test="$has">
			<!--<xsl:message> !!! mapped xpath = <xsl:value-of select="$xp3"/> </xsl:message>-->
		</xsl:if>
		<xsl:value-of select="$xp3"/>
	</xsl:template>

	<xsl:template match="productInformation" mode="default-product-processor" priority="10">
		<xsl:apply-templates mode="copy-product-classification" select="*[1]/*"/>
		<xsl:copy-of select="nonStandardTerms"/>
		<xsl:apply-templates mode="get-effective-date" select="*[1]"/>
		<xsl:apply-templates mode="get-termination-date" select="*[1]"/>
		<xsl:apply-templates mode="get-notionals" select="*[1]"/>
		<xsl:apply-templates mode="get-reporting-party-side" select="*[1]"/>
		<xsl:apply-templates mode="get-underlying-asset" select="*[1]"/>
		<xsl:copy-of select=".//settlementCurrency"/>
		<xsl:apply-templates mode="process-option-data" select="*[1]"/>
		<xsl:apply-templates mode="get-product-details" select="*[1]"/>
	</xsl:template>

	<!-- product classification -->
	<xsl:template mode="copy-product-classification" match="*" priority="-1" />

	<xsl:template mode="copy-product-classification" match="primaryAssetClass|secondaryAssetClass|productType|productId|embeddedOptionType"  >
		<xsl:copy-of select="."/>
	</xsl:template>

	<!-- effective date -->
	<xsl:template mode="get-effective-date" match="*">
		<xsl:variable name="effDates">
			<xsl:for-each select=".//effectiveDate//unadjustedDate">
				<xsl:sort select="." order="ascending"/>
				<xsl:copy-of select="."/>
			</xsl:for-each>
		</xsl:variable>
		<effectiveDate>
			<xsl:choose>
				<xsl:when test="count($effDates/*) &gt; 0">					<!-- when there are effective dates specified -->
					<xsl:value-of select="$effDates/unadjustedDate[1]"/>			<!-- get the earliest -->
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="//tradeDate"/>	<!-- default with trade date -->
				</xsl:otherwise>
			</xsl:choose>
		</effectiveDate>
	</xsl:template>

	<xsl:template mode="get-effective-date" match="fra">
		<effectiveDate>
			<xsl:value-of select="adjustedEffectiveDate"/>
		</effectiveDate>
	</xsl:template>

	<!-- termination date -->
	<xsl:template mode="get-termination-date" match="*">
		<xsl:variable name="termDates">
			<xsl:for-each select=".//terminationDate//unadjustedDate|.//scheduledTerminationDate/unadjustedDate">
				<xsl:sort select="." order="descending"/>
				<xsl:copy-of select="."/>
			</xsl:for-each>
		</xsl:variable>
		<terminationDate>
			<xsl:choose>
				<xsl:when test="$termDates">					<!-- when there are termination dates specified -->
					<xsl:value-of select="$termDates/unadjustedDate[1]"/>			<!-- get the latest -->
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select=".//expirationDate/unadjustedDate"/>	<!-- default with expiration date -->
				</xsl:otherwise>
			</xsl:choose>
		</terminationDate>
	</xsl:template>

	<xsl:template mode="get-termination-date" match="fra">
		<terminationDate>
			<xsl:value-of select="adjustedTerminationDate"/>
		</terminationDate>
	</xsl:template>

	<xsl:template mode="get-termination-date" match="fxSingleLeg|fxOption">
		<terminationDate>
			<xsl:value-of select="(.//valueDate)[1]"/>
		</terminationDate>
	</xsl:template>

	<xsl:template mode="get-termination-date" match="equityOptionTransactionSupplement|varianceSwap|varianceOptionTransactionSupplement">
		<terminationDate>
			<xsl:value-of select="(.//expirationDate//unadjustedDate)[1]"/>
		</terminationDate>
	</xsl:template>

	<!-- notional extraction -->

	<xsl:template mode="get-notionals" match="*">
		<xsl:variable name="notls">
			<xsl:apply-templates select="*" mode="get-notls"/>
		</xsl:variable>
		<xsl:variable name="notl1" select="$notls/*[1]"/>
		<xsl:variable name="notl2" select="$notls/*[2]"/>

		<xsl:if test="$notl1">
			<notional1Amount><xsl:value-of select="$notl1/(amount|initialValue)"/></notional1Amount>
			<notional1Currency><xsl:value-of select="$notl1/currency"/></notional1Currency>
		</xsl:if>
		<xsl:if test="$notl2">
			<notional2Amount><xsl:value-of select="$notl2/(amount|initialValue)"/></notional2Amount>
			<notional2Currency><xsl:value-of select="$notl2/currency"/></notional2Currency>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="get-notls" match="*" priority="-1"/>

	<xsl:template mode="get-notls" match="swapStream">
		<xsl:copy-of select=".//notionalStepSchedule"/>
	</xsl:template>
		
	<xsl:template mode="get-notls" match="notional">
		<xsl:copy-of select="."/>
	</xsl:template>
		
	<!-- CDS -->
	<xsl:template mode="get-notls" match="protectionTerms">
		<xsl:copy-of select=".//calculationAmount"/>
	</xsl:template>
		
	<!-- FX products -->
	<xsl:template mode="get-notls" match="exchangedCurrency1|exchangedCurrency2">
		<xsl:copy-of select="paymentAmount"/>
	</xsl:template>
		
	<xsl:template mode="get-notls" match="putCurrencyAmount|callCurrencyAmount">
		<xsl:copy-of select="."/>
	</xsl:template>
		
	<!-- Equity products -->
	<xsl:template mode="get-notls" match="numberOfOptions">
		<notional>
			<amount>
				<xsl:value-of select=". * ../strike/strikePrice"/>  <!-- or should it be spot price? -->
			</amount>o	Provides a simple way for low-tech firms to submit reporting data using FpML, if TRs support it.
			<currency>
				<xsl:value-of select="../equityPremium/paymentAmount/currency"/>  <!-- or should it be spot price? -->
			</currency>
		</notional>
	</xsl:template>
		
	<xsl:template mode="get-notls" match="returnLeg">
		<xsl:copy-of select="notional/notionalAmount"/>
	</xsl:template>
		
	<xsl:template mode="get-notls" match="varianceLeg">
		<xsl:copy-of select=".//varianceAmount"/>
	</xsl:template>
		
	<!-- reporting party side -->
	<xsl:template mode="get-reporting-party-side" match="swap">
		<!--
		<xsl:variable name="reportingPartyPaysStream" select="swapStream[payerPartyReference/@href = $reportingPartyRef]"/>
		<reportingPartySide>
			<xsl:apply-templates mode="stream-type" select="$reportingPartyPaysStream"/>
		</reportingPartySide>
		-->
	</xsl:template>
		
	<xsl:template mode="get-reporting-party-side" match="*"/>

	<!-- underlying asset -->
	<xsl:template mode="get-underlying-asset" match="*"/>

	<xsl:template mode="get-underlying-asset" match="swap|fra">
		<underlyingAsset>
			<xsl:copy-of select=".//floatingRateIndex[1]"/>
			<xsl:variable name="tenor" select=".//indexTenor[1]"/>
			<xsl:if test="$tenor">
				<indexTenor><xsl:value-of select="$tenor"/></indexTenor>
			</xsl:if>
		</underlyingAsset>
	</xsl:template>

	<!-- Option info -->
	<xsl:template mode="process-option-data" match="*" priority="-1"/>

	<xsl:template mode="process-option-data" match="*[contains(local-name(.),'ption')]">
		<optionDetails>
			<xsl:copy-of select=".//underlyer"/>
			<xsl:apply-templates mode="premium" select=".//premium"/>
			<xsl:copy-of select=".//optionType"/>

			<exerciseStyle>
				<xsl:apply-templates mode="exerciseStyle" select="."/>
			</exerciseStyle>

			<expirationDate>
				<xsl:apply-templates mode="get-expiration-date" select="."/>
			</expirationDate>
			<xsl:apply-templates mode="commencement-date" select="."/>

			<xsl:apply-templates mode="strike" select="."/>
		</optionDetails>
	</xsl:template>

	<xsl:template mode="strike" match="*" priority="-1"/>

	<xsl:template mode="strike" match="*[.//strike]">
		<xsl:copy-of select=".//strike/rate"/>
		<xsl:copy-of select=".//strike/strikePrice"/>
	</xsl:template>


	<xsl:template mode="premium" match="premium">
		<xsl:copy>
			<xsl:copy-of select=".//paymentAmount"/>
			<paymentDate>
				<adjustableDate>
					<unadjustedDate>
						<xsl:value-of select=".//paymentDate//unadjustedDate"/>
					</unadjustedDate>
				</adjustableDate>
			</paymentDate>
		</xsl:copy>
	</xsl:template>

	<xsl:template mode="exerciseStyle" match="*">Undetermined</xsl:template>
	<xsl:template mode="exerciseStyle" match="*[.//*[contains(local-name(.), 'uropean')]]">European</xsl:template>
	<xsl:template mode="exerciseStyle" match="*[.//*[contains(local-name(.), 'merican')]]">American</xsl:template>
	<xsl:template mode="exerciseStyle" match="*[.//*[contains(local-name(.), 'ermuda')]]">Bermudan</xsl:template>

	<xsl:template mode="get-expiration-date" match="*" priority="-1">
		<xsl:value-of select=".//expirationDate//unadjustedDate"/>
	</xsl:template>
	
	<xsl:template mode="get-expiration-date" match="*[.//expiryDate]">
		<xsl:value-of select=".//expiryDate"/>
	</xsl:template>
	
	<xsl:template mode="commencement-date" match="*"/>
	<xsl:template mode="commencement-date" match="*[.//commencementDate]">
		<commencementDate>
			<xsl:value-of select=".//commencementDate//unadjustedDate"/>
		</commencementDate>
	</xsl:template>

	<!-- Stream info -->

	<xsl:template mode="process-streams" match="*[contains(local-name(.),'Option')]" priority="10"/>	<!-- skip options -->

	<xsl:template mode="process-streams" match="*">

		<reportingPartyPaysStream>
			<xsl:apply-templates mode="get-stream-info" select="*">
				<xsl:with-param name="payerPartyRef" select="$reportingPartyRef"/>
			</xsl:apply-templates>
		</reportingPartyPaysStream>
		<counterPartyPaysStream>
			<xsl:apply-templates mode="get-stream-info" select="*">
				<xsl:with-param name="payerPartyRef" select="$counterPartyRef"/>
			</xsl:apply-templates>
		</counterPartyPaysStream>
	</xsl:template>
	<xsl:template mode="get-stream-info" match="*" priority="-1"/>

	<xsl:template mode="get-stream-info" match="swapStream|fixedLeg|floatingLeg|returnLeg|interestLeg|exchangedCurrency1|exchangedCurrency2">
		<xsl:param name="payerPartyRef"/>
		<xsl:variable name="stream">
			<xsl:apply-templates mode="get-payer-stream" select=".">
				<xsl:with-param name="payerPartyRef" select="$payerPartyRef"/>
			</xsl:apply-templates>
		</xsl:variable>

		<xsl:if test="count($stream/*) &gt; 0">
			<streamType>
				<xsl:apply-templates mode="get-stream-type" select="$stream"/>
			</streamType>

			<!-- underlying asset -->
			<xsl:apply-templates mode="get-stream-underlyer" select="$stream"/>

			<!-- day count -->
			<xsl:variable name="dcf" select="$stream//dayCountFraction"/>
			<xsl:if test="$dcf">
				<dayCountFraction><xsl:value-of select="$dcf"/></dayCountFraction>
			</xsl:if>

			<xsl:copy-of select=".//paymentFrequency"/>
			<xsl:apply-templates select=".//interestLegPaymentDates" mode="payment-frequency"/>
			<xsl:copy-of select=".//calculationPeriodFrequency"/>
			<xsl:copy-of select=".//calculationPeriodSchedule"/>
			<xsl:copy-of select=".//resetFrequency"/>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="get-stream-info" match="generalTerms">
		<xsl:param name="payerPartyRef"/>
		<xsl:variable name="buyer" select="buyerPartyReference/@href"/>
		<xsl:message> checking general terms, buyer=<xsl:value-of select="$buyer"/>, payer = <xsl:value-of select="$payerPartyRef"/> </xsl:message>

		<xsl:if test="$buyer = $payerPartyRef">
			<streamType>Float</streamType>

			<!-- underlying asset -->
			<underlyer>
				<xsl:copy-of select=".//referenceEntity"/>
				<xsl:copy-of select=".//referenceObligation"/>
				<xsl:copy-of select=".//indexReferenceEntity"/>
			</underlyer>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="get-stream-info" match="feeLeg">
		<xsl:param name="payerPartyRef"/>
		<xsl:variable name="seller" select="../generalTerms/sellerPartyReference/@href"/>
		<xsl:message> checking fee leg, seller=<xsl:value-of select="$seller"/>, payer = <xsl:value-of select="$payerPartyRef"/> </xsl:message>

		<xsl:if test="$seller = $payerPartyRef">
			<streamType>Fixed</streamType>
			<xsl:copy-of select=".//fixedRate"/>

			<xsl:copy-of select=".//paymentFrequency"/>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="payment-frequency" match="*" priority="-1">
		<paymentFrequency>
			<xsl:value-of select=".//periodMultiplier"/>
			<xsl:value-of select=".//period"/>
		</paymentFrequency>
	</xsl:template>


	<xsl:template mode="get-stream-type" match="*" priority="-1"/>

	<xsl:template mode="get-payer-stream" match="*" priority="-1"/>


	<xsl:template mode="get-payer-stream" match="*[.//payerPartyReference]">
		<xsl:param name="payerPartyRef"/>
		<!--
		<xsl:message> get payer stream for <xsl:value-of select="$payerPartyRef"/>, stream payer is <xsl:value-of select=".//payerPartyReference/@href"/></xsl:message>
		-->
		<xsl:if test=".//payerPartyReference/@href = $payerPartyRef">
			<xsl:copy-of select="."/>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="get-payer-stream" match="feeLeg">
		<xsl:param name="payerPartyRef"/>
		<xsl:variable name="buyer" select="../generalTerms/byerPartyRefence/@href" />

		<xsl:if test="$payerPartyRef = $buyer">
			<xsl:copy-of select="."/>
		</xsl:if>
	</xsl:template>

	<xsl:template mode="get-payer-stream" match="protectionTerms">
		<xsl:param name="payerPartyRef"/>
		<xsl:variable name="seller" select="../generalTerms/sellerPartyRefence/@href" />

		<xsl:if test="$payerPartyRef = $seller">
			<xsl:copy-of select="."/>
		</xsl:if>
	</xsl:template>

	<!-- stream type (fixed or floating) -->
	<xsl:template mode="get-stream-type" match="*[.//fixedRate|..//fixedRateSchedule]">Fixed</xsl:template>
	<xsl:template mode="get-stream-type" match="fixedLeg">Fixed</xsl:template>
	<xsl:template mode="get-stream-type" match="interestLeg">Funding</xsl:template>

	<xsl:template mode="get-stream-type" match="*[.//floatingRateIndex]">Floating</xsl:template>
	<xsl:template mode="get-stream-type" match="floatingLeg">Floating</xsl:template>
	<xsl:template mode="get-stream-type" match="returnLeg">Return</xsl:template>
	<xsl:template mode="get-stream-type" match="exchangedCurrency1|exchangedCurrency2">Currency</xsl:template>

	<!-- stream underlyer -->

	<xsl:template mode="get-stream-underlyer" match="*" priority="-1"/>

	<xsl:template mode="get-stream-underlyer" match="*[.//fixedRate]" >
		<xsl:copy-of select=".//fixedRate"/>
	</xsl:template>

	<xsl:template mode="get-stream-underlyer" match="*[.//fixedRateSchedule]" >
		<fixedRate>
			<xsl:value-of select=".//fixedRateSchedule[1]//initialValue"/>
		</fixedRate>
	</xsl:template>

	<xsl:template mode="get-stream-underlyer" match="*[.//fixedPrice]" >
		<xsl:copy-of select=".//fixedPrice"/>
	</xsl:template>

	<xsl:template mode="get-stream-underlyer" match="*[.//floatingRateIndex]" >
		<xsl:copy-of select=".//floatingRateIndex|.//indexTenor"/>
	</xsl:template>

	<xsl:template mode="get-stream-underlyer" match="*[.//underlyer]" >
		<xsl:copy-of select=".//underlyer"/>
	</xsl:template>

	<xsl:template mode="get-stream-underlyer" match="exchangedCurrency1|exchangedCurrency2" >
		<underlyer>
			<xsl:copy-of select=".//currency"/>
		</underlyer>
	</xsl:template>

	<xsl:template mode="get-stream-underlyer" match="*[.//commodity]" >
		<underlyer>
			<xsl:copy-of select=".//commodity"/>
		</underlyer>
	</xsl:template>


	<!-- specialized stream processing logic -->
	<xsl:template mode="process-streams" match="fra">
		<xsl:variable name="fixed" select="$reportingPartyRef = sellerPartyReference/@href"/>

		<reportingPartyPaysStream>
			<xsl:apply-templates mode="get-fra-info" select=".">
				<xsl:with-param name="fixed" select="$fixed"/>
			</xsl:apply-templates>
		</reportingPartyPaysStream>
		<counterPartyPaysStream>
			<xsl:apply-templates mode="get-fra-info" select=".">
				<xsl:with-param name="fixed" select="not($fixed)"/>
			</xsl:apply-templates>
		</counterPartyPaysStream>
	</xsl:template>

	<xsl:template mode="get-fra-info" match="fra">
		<xsl:param name="fixed"/>
		<xsl:choose>
			<xsl:when test="$fixed">
				<streamType>Fixed</streamType>
				<xsl:copy-of select="fixedRate"/>
			</xsl:when>
			<xsl:otherwise>
				<streamType>Floating</streamType>
				<xsl:copy-of select="floatingRateIndex|indexTenor"/>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:copy-of select="dayCountFraction"/>
	</xsl:template>


	<xsl:template mode="process-streams" match="varianceSwap">
		<xsl:variable name="payer" select="$reportingPartyRef = payerPartyReference/@href"/>

		<reportingPartyPaysStream>
			<xsl:apply-templates mode="get-vs-info" select="varianceLeg">
				<xsl:with-param name="payer" select="$payer"/>
			</xsl:apply-templates>
		</reportingPartyPaysStream>
		<counterPartyPaysStream>
			<xsl:apply-templates mode="get-vs-info" select="varianceLeg">
				<xsl:with-param name="payer" select="not($payer)"/>
			</xsl:apply-templates>
		</counterPartyPaysStream>
	</xsl:template>

	<xsl:template mode="get-vs-info" match="varianceLeg">
		<xsl:param name="payer"/>
		<xsl:choose>
			<xsl:when test="$payer">
				<streamType>Floating</streamType>
				<xsl:copy-of select=".//underlyer"/>
			</xsl:when>
			<xsl:otherwise>
				<streamType>Fixed</streamType>
				<fixedRate>
					<xsl:value-of select=".//varianceStrikePrice"/>
				</fixedRate>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:copy-of select="dayCountFraction"/>
	</xsl:template>

	<!-- product details -->
	<xsl:template mode="get-product-details" match="*">
		<xsl:comment> get product details -> asset class specific </xsl:comment>
		<xsl:apply-templates mode="asset-class-specific" select="."/>
	</xsl:template>


	<xsl:template match="*" mode="asset-class-specific" priority="-1" />

	<!-- rates specific -->

	<xsl:template match="swap" mode="asset-class-specific" >
		<interestRateDetails>
			<xsl:variable name="reportingPartyPaysStream" select="swapStream[payerPartyReference/@href = $reportingPartyRef]"/>
			<reportingPartyPaysType>
				<xsl:apply-templates mode="stream-type" select="$reportingPartyPaysStream"/>
			</reportingPartyPaysType>
			<xsl:variable name="counterPartyRef" select="//counterPartyReference/@href"/>
			<xsl:variable name="counterPartyPaysStream" select="swapStream[payerPartyReference/@href = $counterPartyRef]"/>
			<counterPartyPaysType>
				<xsl:apply-templates mode="stream-type" select="$counterPartyPaysStream"/>
			</counterPartyPaysType>

			<xsl:variable name="floatingStream" select="swapStream[.//floatingRateCalculation][1]"/>
			<xsl:variable name="floatingStream2" select="swapStream[.//floatingRateCalculation][2]"/>
			<xsl:if test="$floatingStream">
				<floatingPaymentFrequency>
					<xsl:value-of select="$floatingStream//paymentFrequency"/>
				</floatingPaymentFrequency>
				<floatingCalculationFrequency>
					<xsl:variable name="freq">
						<xsl:value-of select="$floatingStream//calculationPeriodFrequency/(periodMultiplier|period)"/>
					</xsl:variable>
					<xsl:value-of select="translate($freq, ' ', '')"/>
				</floatingCalculationFrequency>
				<floatingResetFrequency>
					<xsl:value-of select="$floatingStream//resetFrequency"/>
				</floatingResetFrequency>
				<floatingDayCountFraction>
					<xsl:value-of select="$floatingStream//dayCountFraction"/>
				</floatingDayCountFraction>
				<xsl:if test="floatingStream//spreadSchedule/initialValue">
					<floatingRateSpread>
							<xsl:value-of select="$floatingStream//spreadSchedule/initialValue"/>
					</floatingRateSpread>
				</xsl:if>

				<xsl:if test="floatingStream2">
					<floatingRateSpread>
						<xsl:value-of select="$floatingStream2//spreadSchedule/initialValue"/>
					</floatingRateSpread>
				</xsl:if>
			</xsl:if>

			<xsl:variable name="fixedStream" select="swapStream[.//fixedRateSchedule][1]"/>
			<xsl:variable name="fixedStream2" select="swapStream[.//fixedRateSchedule][2]"/>
			<xsl:if test="$fixedStream">
				<fixedRate>
					<xsl:value-of select="$fixedStream//fixedRateSchedule[1]/initialValue"/>
				</fixedRate>
				<xsl:if test="$fixedStream2">
					<fixedRate2>
						<xsl:value-of select="$fixedStream2//fixedRateSchedule[1]/initialValue"/>
					</fixedRate2>
				</xsl:if>
				<fixedPaymentFrequency>
					<xsl:value-of select="$fixedStream//paymentFrequency"/>
				</fixedPaymentFrequency>
				<fixedCalculationFrequency>
					<xsl:variable name="freq">
						<xsl:value-of select="$floatingStream//calculationPeriodFrequency/(periodMultiplier|period)"/>
					</xsl:variable>
					<xsl:value-of select="translate($freq, ' ', '')"/>
				</fixedCalculationFrequency>
				<fixedDayCountFraction>
					<xsl:value-of select="$fixedStream//dayCountFraction"/>
				</fixedDayCountFraction>
			</xsl:if>
			<xsl:variable name="prinex1" select="swapStream[1]/principalExchanges/finalExchange or swapStream[1]/principalExchanges/initialExchange"/>
			<xsl:variable name="prinex2" select="swapStream[2]/principalExchanges/finalExchange or swapStream[2]/principalExchanges/initialExchange"/>
			<xsl:if test="$prinex1">
				<principalExchangeAmount1>
					<xsl:value-of select="swapStream[1]/calculationPeriodAmount/calculation/notionalSchedule/notionalStepSchedule/initialValue"/>
				</principalExchangeAmount1>
				<principalExchangeCurrency1>
					<xsl:value-of select="swapStream[1]/calculationPeriodAmount/calculation/notionalSchedule/notionalStepSchedule/currency"/>
				</principalExchangeCurrency1>
			</xsl:if>
			<xsl:if test="$prinex2">
				<principalExchangeAmount2>
					<xsl:value-of select="swapStream[2]/calculationPeriodAmount/calculation/notionalSchedule/notionalStepSchedule/initialValue"/>
				</principalExchangeAmount2>
				<principalExchangeCurrency2>
					<xsl:value-of select="swapStream[2]/calculationPeriodAmount/calculation/notionalSchedule/notionalStepSchedule/currency"/>
				</principalExchangeCurrency2>
			</xsl:if>

		</interestRateDetails>
	</xsl:template>

	<xsl:template match="fra" mode="asset-class-specific" >
		<interestRateDetails>
			<xsl:choose>
				<xsl:when test="$reportingPartyRef = buyerPartyReference/@href">
					<reportingPartyPaysType>Fixed</reportingPartyPaysType>
					<counterPartyPaysType>Floating</counterPartyPaysType>
				</xsl:when>
				<xsl:otherwise>
					<reportingPartyPaysType>Floating</reportingPartyPaysType>
					<counterPartyPaysType>Fixed</counterPartyPaysType>
				</xsl:otherwise>
			</xsl:choose>
			<floatingPaymentFrequency>1T</floatingPaymentFrequency>
			<floatingCalculationFrequency>1T</floatingCalculationFrequency>
			<floatingResetFrequency>1T</floatingResetFrequency>
			<floatingDayCountFraction><xsl:value-of select="dayCountFraction"/></floatingDayCountFraction>
			<xsl:copy-of select="fixedRate"/>
			<fixedPaymentFrequency>1T</fixedPaymentFrequency>
			<fixedCalculationFrequency>1T</fixedCalculationFrequency>
			<fixedDayCountFraction><xsl:value-of select="dayCountFraction"/></fixedDayCountFraction>

		</interestRateDetails>
	</xsl:template>

	<xsl:template match="creditDefaultSwap" mode="asset-class-specific" >
		<creditDetails>
			<xsl:apply-templates mode="copy-reference" select="generalTerms/buyerPartyReference">
				<xsl:with-param name="tag" select="'protectionBuyerReference'" />
			</xsl:apply-templates>
			<xsl:apply-templates mode="copy-reference" select="generalTerms/sellerPartyReference">
				<xsl:with-param name="tag" select="'protectionSellerReference'" />
			</xsl:apply-templates>
			<!--
			<xsl:apply-templates mode="dereference-party" select="generalTerms/buyerPartyReference">
				<xsl:with-param name="tag" select="'protectionBuyer'" />
			</xsl:apply-templates>
			<xsl:apply-templates mode="dereference-party" select="generalTerms/sellerPartyReference">
				<xsl:with-param name="tag" select="'protectionSeller'" />
			</xsl:apply-templates>
			-->
			<xsl:copy-of select="feeLeg/periodicPayment/fixedAmountCalculation/fixedRate"/>
			<xsl:apply-templates select="feeLeg/periodicPayment/paymentFrequency" mode="payment-frequency"/>
		</creditDetails>
	</xsl:template>

	<xsl:template mode="copy-reference" match="*">
		<xsl:param name="tag" select="local-name(.)"/>
		<xsl:variable name="href" select="@href"/>
		<xsl:element name="{$tag}">
			<xsl:attribute name="href" select="$href"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template mode="dereference-party" match="*">
		<xsl:param name="tag"/>
		<xsl:variable name="href" select="@href"/>
		<xsl:variable name="targ" select="//party[@id=$href]"/>
		<xsl:element name="{concat($tag, 'Id')}">
			<xsl:copy-of select="$targ/partyId/@*"/>
			<xsl:value-of select="$targ/partyId"/>
		</xsl:element>
		<xsl:element name="{concat($tag, 'Name')}">
			<xsl:value-of select="$targ/partyName"/>
		</xsl:element>
	</xsl:template>	


	<xsl:template match="*[.//fixedRateSchedule]" mode="stream-type" >Fixed</xsl:template>
	<xsl:template match="*[.//floatingRateCalculation]" mode="stream-type" >Floating</xsl:template>
	<xsl:template match="*[.//inflationRateCalculation]" mode="stream-type" >Floating</xsl:template>
	<xsl:template match="*[.//knownAmountSchedule]" mode="stream-type" >Fixed</xsl:template>
	<xsl:template match="*" mode="stream-type" >Unknown</xsl:template>



	<xsl:template match="fxSingleLeg" mode="asset-class-specific" >
		<fxDetails>
			<xsl:copy-of select="valueDate"/>
			<xsl:copy-of select="exchangeRate"/>
		</fxDetails>
	</xsl:template>

	<xsl:template match="fxOption" mode="asset-class-specific" >
		<fxDetails>
			<xsl:copy-of select=".//valueDate"/>
			<exchangeRate>
				<quotedCurrencyPair>
					<currency1><xsl:value-of select="putCurrencyAmount/currency"/></currency1>
					<currency2><xsl:value-of select="callCurrencyAmount/currency"/></currency2>
					<quoteBasis>Currency2PerCurrency1</quoteBasis>
				</quotedCurrencyPair>
				<xsl:copy-of select="strike/rate"/>
			</exchangeRate>
		</fxDetails>
	</xsl:template>


	<xsl:template match="commoditySwap" mode="asset-class-specific" >
		<commodityDetails>
			<xsl:copy-of select=".//commodity"/>
			<xsl:if test="not(.//commodity)">
				<xsl:apply-templates mode="get-commodity" select="."/>
			</xsl:if>

			<xsl:copy-of select=".//deliveryPoint"/>
			<xsl:apply-templates mode="copy-field" select=".//pipeline/pipelineName">
				<xsl:with-param name="tag" select="'deliveryPoint'"/>
				<xsl:with-param name="scheme" select="'deliveryPointScheme'"/>
			</xsl:apply-templates>
			<xsl:apply-templates mode="copy-field" select=".//pipeline/withdrawalPoint">
				<xsl:with-param name="tag" select="'interconnectionPoint'"/>
				<xsl:with-param name="scheme" select="'interconnectionPointScheme'"/>
			</xsl:apply-templates>

			<xsl:apply-templates select=".//deliveryType" mode="delivery-type"/>

			<xsl:apply-templates mode="copy-field" select=".//startTime/time/hourMinuteType">
				<xsl:with-param name="tag" select="'startTime'"/>
			</xsl:apply-templates>
			<xsl:apply-templates mode="copy-field" select=".//endTime/time/hourMinuteType">
				<xsl:with-param name="tag" select="'endTime'"/>
			</xsl:apply-templates>
			<xsl:apply-templates mode="copy-structure" select="(.//notionalQuantity|.//deliveryQuantity/physicalQuantity)[1]">
				<xsl:with-param name="tag" select="'quantity'"/>
				<xsl:with-param name="skiprefs" select="1"/>
			</xsl:apply-templates>
			<!--
			<xsl:copy-of select="(.//totalNotionalQuantity)[1]"/>
			-->
			<xsl:copy-of select=".//type"/>
			<xsl:copy-of select=".//grade"/>
		</commodityDetails>
	</xsl:template>	

	<xsl:template match="*" mode="delivery-type" priority="-1" />

	<xsl:template match="deliveryType[firm]" mode="delivery-type" >
		<deliveryType>Firm</deliveryType>
	</xsl:template>	

	<xsl:template match="deliveryType[interruptible]" mode="delivery-type" >
		<deliveryType>Interruptible</deliveryType>
	</xsl:template>	

	<xsl:template match="*" mode="get-commodity" priority="-1" />

	<xsl:template match="*[.//gas]" mode="get-commodity">
		<commodity>
			<instrumentId instrumentIdScheme="http://www.fpml.org/commodity-type">
				<xsl:value-of select=".//gas/type"/>
			</instrumentId>
		</commodity>
	</xsl:template>	

	<xsl:template match="*[.//electricity]" mode="get-commodity">
		<commodity>
			<instrumentId instrumentIdScheme="http://www.fpml.org/commodity-type">
				<xsl:value-of select=".//electricity/type"/>
			</instrumentId>
		</commodity>
	</xsl:template>	

	<xsl:template match="*[.//oil]" mode="get-commodity">
		<commodity>
			<instrumentId instrumentIdScheme="http://www.fpml.org/commodity-type">
				<xsl:value-of select=".//oil/type"/>
			</instrumentId>
		</commodity>
	</xsl:template>	


	<xsl:template mode="copy-structure" match="*">
		<xsl:param name="tag"/>
		<xsl:param name="skiprefs" select="0"/>
		<xsl:element name="{$tag}">
			<xsl:copy-of select="@*"/>
			<xsl:choose>
				<xsl:when test="$skiprefs">
					<xsl:copy-of select="*[not(@href)]"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:copy-of select="*"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:element>
	</xsl:template>	

	<xsl:template mode="copy-field" match="*">
		<xsl:param name="tag"/>
		<xsl:param name="scheme" select="''"/>
		<xsl:element name="{$tag}">
			<xsl:choose>
				<xsl:when test="$scheme = ''">
					<xsl:copy-of select="@*"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:copy-of select="@*[not(contains(local-name(.), 'Scheme'))]"/>
					<xsl:attribute name="{$scheme}">
						<xsl:value-of select="@*[contains(local-name(.), 'Scheme')]"/>
					</xsl:attribute>
				</xsl:otherwise>
			</xsl:choose>
			<xsl:copy-of select="text()"/>
		</xsl:element>
	</xsl:template>	

</xsl:stylesheet>
