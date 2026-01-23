<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0" 
	xmlns="http://www.fpml.org/FpML-5/recordkeeping"
	xmlns:record="http://www.fpml.org/FpML-5/recordkeeping"
	xpath-default-namespace="http://www.fpml.org/FpML-5/master"
	xmlns:common="http://exslt.org/common" 
	xmlns:field="http://www.fpml.org/field-definition" 
	xmlns:xsd="http://www.w3.org/2001/XMLSchema"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
	xmlns:saxon="http://saxon.sf.net/"
	exclude-result-prefixes="common xsd saxon"
	>

	<xsl:import href="recordkeeping-utils.xsl"/>

	<xsl:strip-space elements="*"/>
	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes" standalone="yes"/>


	<xsl:param name="xpathdir" select="'../src/recordkeeping-view-examples'"/>
	<xsl:param name="fieldDefs-file" select="concat(translate($xpathdir,'\','/'), '/NonProductFields.xml')"/>
	<xsl:param name="version" select="version"/>

	<xsl:variable name="target.ns" select="'http://www.fpml.org/FpML-5/recordkeeping'"/>

	<xsl:variable name="fieldDefs" select="document($fieldDefs-file)/field:mappings"/>

	<xsl:variable name="full.document" select="."/>



	<xsl:template match="*" mode="strip-id-no-map">
		<xsl:element name="{local-name(.)}" namespace="http://www.fpml.org/FpML-5/master">
			<xsl:apply-templates select="@*|node()" mode="strip-id-no-map"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template match="text()" priority="1" mode="strip-id-no-map">
		<xsl:copy-of select="."/>
	</xsl:template>	

	<xsl:template match="node() | @*" priority="-1" mode="strip-id-no-map">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()" mode="strip-id-no-map"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="@xxxid" priority="100" mode="strip-id-no-map"/>


	<xsl:template match="nonpublicExecutionReport" >
		<!--
		<regulatorReport xmlns="http://www.fpml.org/FpML-5/regreporting" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" fpmlVersion="5-10" xsi:schemaLocation="http://www.fpml.org/FpML-5/regreporting ../fpml-main-5-10.xsd http://www.w3.org/2000/09/xmldsig# ../xmldsig-core-schema.xsd">
		-->

		<regulatoryDisclosure fpmlVersion="{$version}" xsi:schemaLocation="http://www.fpml.org/FpML-5/recordkeeping ../fpml-main-{$version}.xsd http://www.w3.org/2000/09/xmldsig# ../xmldsig-core-schema.xsd">
			<!--
			<xsl:copy-of select="@*"/>
			-->
			<xsl:variable name="doc">
				<xsl:apply-templates />
			</xsl:variable>
			<xsl:apply-templates mode="postprocess" select="$doc"/>
		</regulatoryDisclosure>
	</xsl:template>	

	<xsl:template match="node()" mode="postprocess">
		<xsl:copy>
			<xsl:copy-of select="@*"/>
			<xsl:apply-templates mode="postprocess"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="record:reportingRegime" mode="postprocess"/>
	<xsl:template match="record:category[contains(@categoryScheme, 'dtcc')]" mode="postprocess"/>

	<xsl:template match="record:tradeHeader" priority="10" mode="postprocess">
		<xsl:copy>
			<xsl:apply-templates mode="usi" select="."/>
			<xsl:apply-templates mode="uti" select="."/>
			<xsl:apply-templates mode="postprocess"/>
			<xsl:comment> Trade Header </xsl:comment>
		</xsl:copy>
	</xsl:template>	

	<!--
	<xsl:template match="record:payment" priority="10" mode="postprocess">
		<xsl:comment>Post-process payment to get elements in the rigth order.</xsl:comment>
		<xsl:copy>
			<xsl:copy-of select="record:payerPartyReference"/>
			<xsl:copy-of select="record:receiverPartyReference"/>
			<xsl:copy-of select="record:paymentAmount"/>
			<xsl:apply-templates mode="postprocess-payment" select="record:paymentDate"/>
			<xsl:copy-of select="record:paymentType"/>
			<xsl:copy-of select="record:settlementInformation"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="node()" priority="10" mode="postprocess-payment">
		<xsl:copy>
			<xsl:apply-templates mode="postprocess-payment"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="record:adjustableDate" priority="10" mode="postprocess-payment">
		<xsl:copy-of select="record:unadjustedDate"/>
		<xsl:copy-of select="record:dateAdjustments"/>
		<xsl:copy-of select="record:adjustedDate"/>
	</xsl:template>	
-->

	<xsl:template match="record:tradeHeader" mode="uti">
		<xsl:variable name="uti" select="record:partyTradeIdentifier[issuer and not(contains(record:issuer/@issuerIdScheme, 'cftc') and contains(record:tradeId/@tradeIdScheme, 'unique-transaction-identifier'))]"/>
		<xsl:if test="$uti">
			<uniqueTransactionIdentifier>
				<xsl:copy-of select="$uti/record:issuer"/>
				<xsl:copy-of select="$uti/record:tradeId"/>
			</uniqueTransactionIdentifier>
		</xsl:if>
	</xsl:template>	

	<xsl:template match="record:tradeHeader" mode="usi">
		<xsl:variable name="usi" select="record:partyTradeIdentifier[contains(record:issuer/@issuerIdScheme, 'cftc') and contains(record:tradeId/@tradeIdScheme, 'unique-transaction-identifier')]"/>
		<xsl:if test="$usi">
			<uniqueSwapIdentifier>
				<xsl:copy-of select="$usi/record:issuer"/>
				<xsl:copy-of select="$usi/record:tradeId"/>
			</uniqueSwapIdentifier>
		</xsl:if>
	</xsl:template>	

	<xsl:template match="record:linkId" mode="postprocess">
		<linkedTrade>
			<linkType>LinkedTrade</linkType>
			<linkId>
				<xsl:copy-of select="@linkIdScheme"/>
				<xsl:if test="not(@linkIdScheme)">
					<xsl:copy-of select="tradeId/@tradeIdScheme"/>
				</xsl:if>

				<xsl:apply-templates mode="format-link-id" select="."/>
			</linkId>
		</linkedTrade>
	</xsl:template>	

	<xsl:template match="record:originatingTradeId|record:blockTradeId|record:resultingTradeId|record:allocationTradeId" mode="postprocess">
		<xsl:apply-templates select="." mode="linked-trade"/>
	</xsl:template>	

	<xsl:template match="*" mode="linked-trade">
		<xsl:param name="linkType" select="local-name(.)"/>
		<linkedTrade>
			<linkType><xsl:value-of select="$linkType"/></linkType>
			<linkId>
				<xsl:if test="record:tradeId/@tradeIdScheme">
					<xsl:attribute name="linkIdScheme" select="record:tradeId/@tradeIdScheme"/>
				</xsl:if>
				<xsl:apply-templates mode="format-link-id" select="."/>
			</linkId>
		</linkedTrade>
	</xsl:template>	

	<xsl:template match="*[record:issuer]" mode="format-link-id" priority="10" >
		<xsl:value-of select="concat(record:issuer, record:tradeId)"/>
	</xsl:template>	

	<xsl:template match="*" mode="format-link-id"  >
		<xsl:value-of select="."/>
	</xsl:template>	


	<xsl:template match="@*" priority="9">
		<xsl:copy-of select="."/>
	</xsl:template>	

	<xsl:template match="@fpmlVersion" priority="10">
		<xsl:attribute name="fpmlVersion"><xsl:value-of select="$version"/></xsl:attribute>
	</xsl:template>	

	<xsl:template match="@xsi:schemaLocation" priority="10">
		<xsl:attribute name="xsi:schemaLocation">123</xsl:attribute>
	</xsl:template>	

	<xsl:template match="comment()">
		<xsl:copy-of select="."/>
	</xsl:template>	

	<xsl:template match="node() | @*" priority="-1">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="originatingEvent|reportingPartyReference|counterPartyReference|beneficiaryReference|clearingOrganizationPartyReference|arrangingBrokerReference|allocationAgentReference|deskReference|traderReference|clearingFirmPartyReference|submittingPartyReference|tradeSourceReference|branchReference|salesBrokerReference|tradeRepositoryReference|priorTradeRepositoryReference|executionVenueReference|executingBrokerReference|investmentDecisionMakerReference|tradingDecisionMakerReference" priority="10" />

	<xsl:template match="isCorrection|correlationId|sequenceNumber" priority="10" />

	<xsl:template match="asOfDate|asOfTime" priority="10" />

	<xsl:template match="onBehalfOf" />

	<xsl:template match="quote" />

	<xsl:template match="tradingEvent" />

	<xsl:template match="productComponentIdentifier" />


	<xsl:variable name="templateTF">
		<onBehalfOf>
			<partyReference reference="Submitted For Party"/>
		</onBehalfOf>
		<asOfDate xpath="asofDate"/>
		<asOfTime xpath="asofTime"/>
		<eventId field="Event ID"/>
		<eventType field="Event Type"/>
		<eventTimestamps>
			<agreementDate field="Event Agreement Date"/>
			<entryDateTime field="Event Entry Datetime"/>
			<executionDateTime field="Event Execution Datetime"/>
			<effectiveDate field="Event Effective Date"/>
		</eventTimestamps>

		<reportingPartyReference reference="Reporting Party"/>
		<counterPartyReference reference="Counterparty"/>

		<!--
		<sizeChange copy="//sizeChange" mode="skipelement"/>
		<novationDetails copy="//novationDetails" mode="skipelement"/>
		-->
		<changeInNotional copy="//tradingEvent/changeInNotional" mode="skipelement"/>
		<changeInNumberOfOptions copy="//tradingEvent/changeInNumberOfOptions" mode="skipelement"/>
		<changeInQuantity copy="//tradingEvent/changeInQuantity" mode="skipelement"/>
		<payment copy="//payment" mode="skipelement"/>

		<applicableRegulations>
			<regulatorApplicability>
				<supervisoryBody  xpath="//reportingRegime[.//supervisoryBody='CFTC']" value="CFTC"/>
				<regulation xpath="//reportingRegime[.//supervisoryBody='CFTC' and reportingPurpose='RealTimePublic']" value="CFTC_PART43"/>
				<regulation xpath="//reportingRegime[.//supervisoryBody='CFTC' and reportingPurpose[not(. = 'RealTimePublic')]]" value="CFTC_PART45"/>
			</regulatorApplicability>
			<regulatorApplicability>
				<supervisoryBody  xpath="//reportingRegime[.//supervisoryBody='CA.ON.OSC']" value="CA.ON.OSC"/>
				<regulation xpath="//reportingRegime[.//supervisoryBody='CA.ON.OSC' and reportingPurpose='RealTimePublic']" value="CFTC_PART43"/>
				<regulation xpath="//reportingRegime[.//supervisoryBody='CFTC' and reportingPurpose[not(. = 'RealTimePublic')]]" value="CSA_2013"/>
			</regulatorApplicability>
			<regulatorApplicability>
				<supervisoryBody  xpath="//reportingRegime[.//supervisoryBody='ESMA']" value="ESMA"/>
				<regulation xpath="//reportingRegime[.//supervisoryBody='ESMA' and name='EMIR']" value="EMIR"/>
				<regulation xpath="//reportingRegime[.//supervisoryBody='EMIR' and name='MIFIR']" value="RTS_23"/>
			</regulatorApplicability>
		</applicableRegulations>
		<regulationSpecificInformation>
			<cftc_part45_2012 copywrapper="//reportingRegime[reportingRole and .//supervisoryBody='CFTC']">
				<supervisoryBody xpath="supervisorRegistration/supervisoryBody"/>
				<reportingRole xpath="reportingRole"/>
				<reportingPurpose xpath="reportingPurpose[1]"/>
				<reportingPurpose xpath="reportingPurpose[2]"/>
				<mandatorilyClearable xpath="mandatorilyClearable"/>
				<endUserException xpath="../endUserException"/>
				<endUserExceptionReason xpath="endExceptionReason"/>
				<endUserExceptionDeclaration xpath="endExceptionDeclaration"/>
				<nonStandardTerms xpath="//nonStandardTerms"/>
				<offMarketPrice xpath="//offMarketPrice"/>

				<largeSizeTrade field="Large Notional Off Facility Trade Indicator"/>
				<executionType field="Execution Type"/>
				<executionVenueType field="Execution Venue Type"/>
				<verificationMethod field="Verification Method"/>
				<confirmationMethod field="Confirmation Method"/>

				<reportingPartyOrganizationType field="Reporting Party Org Type"/>
				<counterPartyOrganizationType field="Counterparty Org Type"/>
				<counterPartyIsUSPerson field="Counterparty US Person"/>
			</cftc_part45_2012>
		<esma_emirRts_2015 copywrapper="//reportingRegime[reportingRole and .//name='EMIR']">
			<supervisoryBody xpath="supervisorRegistration/supervisoryBody"/>
			<reportingRole xpath="reportingRole"/>
			<reportingPurpose xpath="reportingPurpose[1]"/>
			<reportingPurpose xpath="reportingPurpose[2]"/>
			<tradePartyRelationshipType xpath="tradePartyRelationshipType"/>
			<mandatorilyClearable xpath="mandatorilyClearable"/>
			<exceedsClearingThreshold xpath="exceedsClearingThreshold"/>
			<!--
			<buyerSeller field="EMIR buyer/seller"/>
			<counterPartyBuyerSeller field="EMIR buyer/seller - Party 2"/>
				<clearingException field="Clearing exemption for specified person"/>
				<clearingExceptionCounterparty xpath="clearingExceptionCounterparty"/>
				<clearingExceptionReason field="Clearing exception or exemption type"/>
				<counterParty>
					<exceedsClearingThreshold field="Clearing Threshold - Party 2"/>
				</counterParty>
			<counterParty>
				<supervisoryBody field="Regulator - Party 2"/>
			</counterParty>
			-->
			<reportingPartyClassification xpath="entityClassification"/>
			<counterPartyClassification field="partyEntityClassification/entityClassification"/>
			<!--
			<reportingPartySector xpath=""/>
			-->
			<counterPartyIsEEA field="Counterparty EEA"/>
			<actionType field="ESMA Action Type"/>
			<!--
			<level field="EMIR Level"/>
			-->
		</esma_emirRts_2015>
		<!--
		<mifir copywrapper="//reportingRegime[reportingRole and .//name='MiFIR']">
			<supervisoryBody xpath="supervisorRegistration/supervisoryBody"/>
			<reportingRole xpath="reportingRole"/>
			<reportingPurpose xpath="reportingPurpose[1]"/>
			<reportingPurpose xpath="reportingPurpose[2]"/>
			<clearing>
				<mandatorilyClearable xpath="mandatorilyClearable"/>
				<exceedsClearingThreshold xpath="exceedsClearingThreshold"/>
				<clearingException field="Clearing exemption for specified person"/>
				<clearingExceptionCounterparty xpath="clearingExceptionCounterparty"/>
				<clearingExceptionReason field="Clearing exception or exemption type"/>
				<counterParty>
					<exceedsClearingThreshold field="Clearing Threshold - Party 2"/>
				</counterParty>

			</clearing>
			<reportingPartyIsFinancial field="Reporting Party ESMA financial"/>
			<reportingPartySector field="Reporting Party ESMA Sector"/>
			<counterPartyIsFinancial field="Counterparty ESMA financial"/>
			<counterPartyIsEEA field="Counterparty EEA"/>
			<intragroup field="ESMA intragroup"/>
			<actionType field="ESMA Action Type"/>
			<counterPartyActionType field="ESMA Action Type - Party 2"/>
			<level field="EMIR Level"/>
			<transmissionOfOrder field="Transmission of Order Indicator"/>
			<isSecuritiesFinancing field="Is Securities Financing"/>
			<shortSale field="MiFIR Short Sale"/>
		</mifir>
			-->
		</regulationSpecificInformation>

		<trade copy="//trade" mode="skipelement"/>
		<quote copy="//quote" mode="skipelement"/>
		<party copy="//party" mode="skipelement"/>
	</xsl:variable>

	<xsl:variable name="template" select="common:node-set($templateTF)"/>


	<xsl:template match="trade"  >
		<xsl:variable name="defs" select="$fieldDefs"/>
		<xsl:variable name="doc" select=".."/>
		<xsl:comment>Defs has <xsl:value-of select="count($fieldDefs/*)"/></xsl:comment>
		<xsl:apply-templates mode="fill-template" select="$template/*">
			<xsl:with-param name="doc" select="$doc"/>
			<xsl:with-param name="defs" select="$defs"/>
		</xsl:apply-templates>
	</xsl:template>


	<xsl:template match="*[@field]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc" />
		<!--
		<xsl:variable name="doc" select="$full.document"/>
		-->
		<xsl:variable name="fieldname" select="@field"/>
		<xsl:variable name="elem" select="local-name(.)"/>
		<xsl:apply-templates mode="get-xpath-value" select="$doc">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
		</xsl:apply-templates>
	</xsl:template>


	<xsl:template match="*" mode="evaluate-xpath" >
		<xsl:param name="xpath"/>
		<!--<xsl:message>#### Evaluate xpath <xsl:value-of select="$xpath"/>####</xsl:message>-->
		<xsl:variable name="xp2" select="replace($xpath, 'rec:nonpublicExecutionReport/', '')"/>
		<xsl:variable name="xp21" select="replace($xp2, 'rec:trade/rec:tradeHeader', 'rec:transactionIdentification')"/> <!-- for trade date -->
		<xsl:variable name="xp3" select="replace($xp21, 'rec:trade/', 'rec:productInformation/')"/>
		<xsl:variable name="xp4">
			<xsl:call-template name="map-xpath-variables">
				<xsl:with-param name="xpath" select="$xp3"/>
			</xsl:call-template>
		</xsl:variable>
		<xsl:if test="contains($xpath, 'tradeHeader')">
			<!--<xsl:message>********* got <xsl:value-of select="$xp3"/>, <xsl:value-of select="$xp4"/> ****** <xsl:value-of select="saxon:evaluate($xp4)"/> </xsl:message>-->
		</xsl:if>
		<xsl:comment>eval xpath <xsl:value-of select="$xp4"/></xsl:comment>
		<xsl:choose>
			<xsl:when test="contains($xp4, 'if(')">
				<!--<xsl:message>**** evaluating <xsl:value-of select="$xp4"/> ****</xsl:message>-->
				<xsl:apply-templates mode="evaluate-if-xpath" select=".">
					<xsl:with-param name="xpath" select="$xp4"/>
				</xsl:apply-templates>
			</xsl:when>
			<xsl:otherwise>
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

	<xsl:template match="*[@reference]" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:param name="docWIds" select="$docWithIds"/>
		<xsl:variable name="content">
			<xsl:apply-templates mode="fill-template" select="*">
				<xsl:with-param name="doc" select="$doc"/>
				<xsl:with-param name="defs" select="$defs"/>
				<xsl:with-param name="docWIds" select="$docWIds"/>
			</xsl:apply-templates>
		</xsl:variable>
		<xsl:if test="$content/*">
			<xsl:copy>
				<xsl:copy-of select="$content"/>
			</xsl:copy>
		</xsl:if>
	</xsl:template>

	<xsl:template match="*[@reference]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:param name="docWIds" select="$docWithIds"/>
		<xsl:variable name="fieldname" select="@reference"/>
		<!--<xsl:message>[[[[[[ Working on <xsl:value-of select="$fieldname"/> ]]]]</xsl:message>-->
		<xsl:variable name="elem" select="local-name(.)"/>

		<xsl:apply-templates mode="get-xpath-reference" select="$doc">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="*" mode="get-xpath-reference" >
		<xsl:param name="defs"/>
		<xsl:param name="fieldname"/>
		<xsl:param name="elem"/>
		<xsl:variable name="xpath" select="$defs/field:field[@name=$fieldname]/@xpath"/>
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


	<xsl:template match="*[@structure]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:param name="nowrap" select="@nowrap"/>
		<xsl:param name="docWIds" select="$docWithIds"/>
		<xsl:variable name="fieldname" select="@structure"/>
		<!--<xsl:message>Working on <xsl:value-of select="$fieldname"/></xsl:message>-->
		<xsl:variable name="elem" select="local-name(.)"/>

		<xsl:apply-templates mode="get-xpath-copy-structure" select="$docWIds">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
			<xsl:with-param name="wrap" select="not($nowrap)"/>
			<xsl:with-param name="docWIds" select="$docWIds"/>
		</xsl:apply-templates>
	</xsl:template>


	<xsl:template match="*" mode="get-xpath-copy-structure" >
		<xsl:param name="defs"/>
		<xsl:param name="fieldname"/>
		<xsl:param name="elem"/>
		<xsl:param name="wrap"/>
		<xsl:param name="skipelement" select="false"/>
		<xsl:param name="docWIds" select="$docWithIds"/>
		<xsl:param name="xpath" select="$defs/field:field[@name=$fieldname]/@xpath"/>
		<!--<xsl:message>%%%% Working on <xsl:value-of select="$fieldname"/>, xpath=<xsl:value-of select="$xpath"/>, wrap=<xsl:value-of select="$wrap"/> %%%%</xsl:message>-->
		<!--
		<xsl:comment> Processing <xsl:value-of select="$fieldname"/> for <xsl:value-of select="$elem"/>, xpath = <xsl:value-of select="$xpath"/></xsl:comment>
		-->
		<xsl:apply-templates mode="xpath-copy-structure" select=".">
			<xsl:with-param name="elem" select="$elem"/>
			<xsl:with-param name="xpath" select="$xpath"/>
			<xsl:with-param name="wrap" select="$wrap"/>
			<xsl:with-param name="skipelement" select="$skipelement"/>
			<xsl:with-param name="docWIds" select="$docWIds"/>
		</xsl:apply-templates>

	</xsl:template>




	<xsl:template match="*" mode="get-xpath-copy-wrapper" >
		<xsl:param name="defs"/>
		<xsl:param name="fieldname"/>
		<xsl:param name="elem"/>
		<xsl:param name="wrap"/>
		<xsl:param name="template"/>
		<xsl:param name="docWIds" select="$docWithIds"/>
		<xsl:param name="xpath" select="$defs/field:field[@name=$fieldname]/@xpath"/>
		<!--<xsl:message>%%%% Working on <xsl:value-of select="$fieldname"/>, xpath=<xsl:value-of select="$xpath"/>, wrap=<xsl:value-of select="$wrap"/> %%%%</xsl:message>-->
		<!--
		<xsl:comment> Copy wrapper <xsl:value-of select="$fieldname"/> for <xsl:value-of select="$elem"/>, xpath = <xsl:value-of select="$xpath"/></xsl:comment>
		-->

		<xsl:if test="string-length($xpath) &gt; 1">
			<xsl:variable name="idval">
				<xsl:apply-templates select="." mode="evaluate-xpath">
					<xsl:with-param name="xpath" select="concat($xpath,'/@xxxid')"/>
				</xsl:apply-templates>
			</xsl:variable>
			<!--
			<xsl:variable name="xp2" select="replace($xpath, 'rec:nonpublicExecutionReport/', '')"/>
			<xsl:variable name="xp3" select="replace($xp2, 'rec:trade', 'rec:productInformation')"/>
			<xsl:variable name="xp4" select="concat($xp3, '/@xxxid')"/>
			<xsl:message>xp4=<xsl:value-of select="$xp4"/></xsl:message>
			<xsl:variable name="idval" select="saxon:evaluate($xp4)"/>
			-->
			<!--<xsl:message>%%%%%%% idval=<xsl:value-of select="$idval"/> %%%%%%%%%%%%</xsl:message>-->
			<!--
			<xsl:comment>For <xsl:value-of select="$fieldname"/>, xpath is <xsl:value-of select="$xpath"/>, tweaked = <xsl:value-of select="$xp4"/>, id val is <xsl:value-of select="$idval"/></xsl:comment>
			<xsl:comment>For <xsl:value-of select="$fieldname"/>, xpath is <xsl:value-of select="$xpath"/>, tweaked = <xsl:value-of select="$xp3"/></xsl:comment>
			<xsl:comment>Current node is <xsl:value-of select="local-name(.)"/></xsl:comment>
			<xsl:comment>Child node is <xsl:value-of select="local-name(*[1])"/></xsl:comment>
			<xsl:comment>value= <xsl:value-of select="$idval"/></xsl:comment>
			<xsl:comment>element name= <xsl:value-of select="$elem"/></xsl:comment>
			-->
			<xsl:variable name="docbitId">
				<xsl:copy-of select ="$docWIds//*[@xxxid = $idval]"/>
			</xsl:variable>
			<xsl:variable name="docbit">
				<xsl:apply-templates mode="strip-id-no-map" select ="$docWIds//*[@xxxid = $idval]"/>
			</xsl:variable>
			<!--
			<xsl:comment>doc bit value = <xsl:value-of select="$docbitId"/></xsl:comment>
			<xsl:comment>docbit content= <xsl:value-of select="$docbit"/></xsl:comment>
			<xsl:comment>template first= <xsl:value-of select="local-name($template/*[1])"/></xsl:comment>
			-->

			<xsl:if test="string-length($idval) &gt; 0">
				<xsl:element name="{$elem}">
					<xsl:apply-templates mode="fill-template" select ="$template/*">
						<xsl:with-param name="doc" select="$docbit"/>
						<xsl:with-param name="defs" select="$defs"/>
						<xsl:with-param name="docWIds" select="$docbitId/*"/>
					</xsl:apply-templates>
				</xsl:element>
			</xsl:if>
		</xsl:if>
	</xsl:template>




	<xsl:template match="trade" mode="reporting-regime">
		<xsl:apply-templates mode="reporting-regime" select="tradeHeader/partyTradeInformation[1]/reportingRegime"/>
	</xsl:template>	

	<xsl:template match="reportingRegime" mode="reporting-regime">
		<xsl:copy>
			<xsl:copy-of select="@*"/>
			<xsl:copy-of select="name"/>
			<xsl:copy-of select=".//supervisoryBody"/>
			<xsl:copy-of select=".//reportingRole"/>
			<xsl:copy-of select=".//reportingPurpose"/>
			<xsl:apply-templates select="." mode="regime-clearing"/>
			<xsl:apply-templates select="." mode="regime-specific"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template mode="regime-clearing" match="reportingRegime">
		<clearing>
			<xsl:copy-of select="mandatorilyClearable"/>
			<xsl:copy-of select="exceedsClearingThreshold"/>
			<xsl:apply-templates mode="copy-field" select="..//endUserException">
				<xsl:with-param name="tag" select="'clearingException'" />
			</xsl:apply-templates>
			<xsl:apply-templates mode="copy-field" select="..//endUserExceptionReason">
				<xsl:with-param name="tag" select="'clearingExceptionReason'" />
			</xsl:apply-templates>
		</clearing>
	</xsl:template>	

	<xsl:template mode="regime-specific" match="reportingRegime[.//supervisoryBody='CFTC']">
		<xsl:apply-templates mode="cftc-specific" select="."/>
	</xsl:template>	

	<xsl:template mode="regime-specific" match="reportingRegime[name='EMIR']">
		<xsl:apply-templates mode="emir-specific" select="."/>
	</xsl:template>	

	<xsl:template mode="regime-specific" match="reportingRegime[name='MiFIR']">
		<xsl:apply-templates mode="mifir-specific" select="."/>
	</xsl:template>	

	<xsl:template mode="regime-specific" match="*" priority="-1"/>



	<xsl:template match="trade" mode="regulator-specific">
		<xsl:comment> Regulator Specific fields </xsl:comment>
		<xsl:variable name="pti" select="tradeHeader/partyTradeInformation[partyReference/@href = //reportingPartyReference/@href]"/>
		<xsl:apply-templates mode="cftc-specific" select= "$pti/reportingRegime[.//supervisoryBody='CFTC']"/>
		<xsl:apply-templates mode="mifir-specific" select="$pti/reportingRegime[name='MiFIR']"/>
		<xsl:apply-templates mode="emir-specific" select="$pti/reportingRegime[name='EMIR']"/>
	</xsl:template>	


	<xsl:template match="reportingRegime" mode="cftc-specific">
		<cftcDoddFrankInformation>
			<xsl:copy-of select="../largeSizeTrade"/>
			<xsl:variable name="rpt.party" select="//party[@id = //reportingPartyReference/@href]"/>
			<reportingPartyOrganizationType>
				<xsl:value-of select="$rpt.party/organizationType"/>
			</reportingPartyOrganizationType>
			<xsl:variable name="counter.party" select="//party[@id = //counterPartyReference/@href]"/>
			<counterPartyOrganizationType>
				<xsl:value-of select="$counter.party/organizationType"/>
			</counterPartyOrganizationType>
			<counterPartyIsUSPerson>
				<xsl:value-of select="$counter.party/country = 'US'"/>
			</counterPartyIsUSPerson>
		</cftcDoddFrankInformation>
	</xsl:template>	

	<xsl:template match="*" mode="emir-specific"/>

	<xsl:template match="reportingRegime" mode="emir-specific">
		<emirInformation>
			<reportingPartyIsFinancial>
				<xsl:value-of select="//partyEntityClassification[partyReference/@href = //reportingPartyReference/@href]/entityClassification = 'Financial'"/>
			</reportingPartyIsFinancial>
			<xsl:variable name="rpt.party" select="//party[@id = //reportingPartyReference/@href]"/>
			<reportingPartySector>
				<xsl:copy-of select="$rpt.party/classification/@*"/>
				<xsl:copy-of select="$rpt.party/classification/*"/>
			</reportingPartySector>
			<counterPartyIsFinancial>
				<xsl:value-of select="//partyEntityClassification[partyReference/@href = //counterPartyReference/@href]/entityClassification = 'Financial'"/>
			</counterPartyIsFinancial>

			<xsl:variable name="counter.party" select="//party[@id = //counterParty/@href]"/>
			<counterPartyIsEEA>
				<xsl:value-of select="$counter.party/region = 'EEA'"/>
			</counterPartyIsEEA>
			<intragroup>
				<xsl:value-of select="tradePartyRelationshipType = 'Intragroup'"/>
			</intragroup>
		</emirInformation>
	</xsl:template>	


	<xsl:template match="*" mode="mifir-specific"/>

	<xsl:template match="reportingRegime" mode="mifir-specific">
		<mifirInformation>
			<reportingPartyIsFinancial>
				<xsl:value-of select="//partyEntityClassification[partyReference/@href = //reportingPartyReference/@href]/entityClassification = 'Financial'"/>
			</reportingPartyIsFinancial>
			<xsl:variable name="rpt.party" select="//party[@id = //reportingPartyReference/@href]"/>
			<reportingPartySector>
				<xsl:copy-of select="$rpt.party/classification/@*"/>
				<xsl:copy-of select="$rpt.party/classification/*"/>
			</reportingPartySector>
			<counterPartyIsFinancial>
				<xsl:value-of select="//partyEntityClassification[partyReference/@href = //counterPartyReference/@href]/entityClassification = 'Financial'"/>
			</counterPartyIsFinancial>

			<xsl:variable name="counter.party" select="//party[@id = //counterParty/@href]"/>
			<counterPartyIsEEA>
				<xsl:value-of select="$counter.party/region = 'EEA'"/>
			</counterPartyIsEEA>
			<intragroup>
				<xsl:value-of select="tradePartyRelationshipType = 'Intragroup'"/>
			</intragroup>
			<xsl:copy-of select="transmissionOfOrder"/>
			<xsl:copy-of select="../isSecuritiesFinancing"/>
			<xsl:copy-of select="../shortSale"/>
		</mifirInformation>
	</xsl:template>	

	<xsl:template match="reportingRegime" mode="product-information" priority="5"/>


	<xsl:template match="trade" mode="product-information" priority="5">
		<productInformation>
			<xsl:copy-of select="*[2]"/>
			<xsl:copy-of select="tradeHeader/partyTradeInformation/nonStandardTerms"/>
		</productInformation>
	</xsl:template>	


	<xsl:template match="trade[reportingProduct]" mode="product-information" priority="10">
		<productInformation>
			<xsl:apply-templates mode="product-classification" select="*[2]"/>
			<xsl:copy-of select="tradeHeader/partyTradeInformation/nonStandardTerms"/>
			<xsl:apply-templates mode="key-product-fields" select="*[2]"/>
			<xsl:copy-of select="optionData"/>
			<xsl:apply-templates mode="asset-class-specific" select="."/>
		</productInformation>
	</xsl:template>	

	<xsl:template match="*" mode="asset-class-specific" priority="-1" >
		<xsl:copy-of select="interestRateDetails|creditDetails|fxDetails|equityDetails|commodityDetails"/>
	</xsl:template>	


	<xsl:template match="*" mode="product-classification" >
		<xsl:copy-of select="primaryAssetClass|productType|productId"/>
	</xsl:template>	

	<xsl:template match="*" mode="key-product-fields" >
		<xsl:copy-of select="effectiveDate|terminationDate"/>
		<xsl:copy-of select="notional1Amount|notional1Currency"/>
		<xsl:copy-of select="notional2Amount|notional2Currency"/>
	</xsl:template>	


	<xsl:template match="partyTradeIdentifier" mode="transaction-identification" priority="-1" />

	<xsl:template match="partyTradeIdentifier[contains(issuer, 'cftc')]" mode="transaction-identification" >
		<xsl:apply-templates mode="copy-structure" select=".">
			<xsl:with-param name="tag" select="uniqueSwapIdentifier"/>
		</xsl:apply-templates>
	</xsl:template>	

	<xsl:template match="partyTradeIdentifier[not(contains(issuer, 'cftc'))]" mode="transaction-identification" >
		<xsl:apply-templates mode="copy-structure" select=".">
			<xsl:with-param name="tag" select="'uniqueTransactionIdentifier'"/>
		</xsl:apply-templates>
	</xsl:template>	

	<!-- valuation information -->

	<xsl:template match="trade" mode="valuation-information" priority="10">
		<valuationInformation>
			<xsl:copy-of select="tradeHeader/partyTradeInformation/offMarketPrice"/>
			<xsl:variable name="context" select="tradeHeader/partyTradeInformation/pricingContext"/>
			<xsl:if test="$context">
				<offMarketPriceReason><xsl:value-of select="$context"/></offMarketPriceReason>
			</xsl:if>
			<xsl:variable name="addl" select="swap/additionalPayment/paymentAmount"/> <!-- enhance for other products -->
			<xsl:if test="$addl">
				<upfrontPayment>
					<xsl:copy-of select="$addl/*"/>
				</upfrontPayment>
			</xsl:if>
		</valuationInformation>
	</xsl:template>	
	<!-- utility templates -->

	<xsl:template mode="copy-field" match="*">
		<xsl:param name="tag"/>
		<xsl:element name="{$tag}">
			<xsl:copy-of select="@*"/>
			<xsl:copy-of select="text()"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template mode="copy-reference" match="*">
		<xsl:param name="tag" select="local-name(.)"/>
		<xsl:variable name="href" select="*/@href"/>
		<xsl:element name="{$tag}">
			<xsl:attribute name="href" select="$href"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template mode="dereference-field" match="relatedParty">
		<xsl:param name="tag" select="local-name(.)"/>
		<xsl:variable name="href" select="partyReference/@href"/>
		<xsl:variable name="targ" select="//party[@id=$href]"/>
		<xsl:element name="{$tag}">
			<xsl:copy-of select="$targ/partyId/@*"/>
			<xsl:value-of select="$targ/partyId"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template mode="dereference-field" match="relatedBusinessUnit">
		<xsl:param name="tag"/>
		<xsl:variable name="href" select="businessUnitReference/@href"/>
		<xsl:variable name="targ" select="//party/businessUnit[@id=$href][1]"/>
		<xsl:element name="{$tag}">
			<xsl:copy-of select="$targ/name/@*"/>
			<xsl:value-of select="$targ/name"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template mode="dereference-field" match="relatedPerson">
		<xsl:param name="tag"/>
		<xsl:variable name="href" select="personReference/@href"/>
		<xsl:variable name="targ" select="//party/person[@id=$href]"/>
		<xsl:element name="{$tag}">
			<xsl:copy-of select="$targ/personId/@*"/>
			<xsl:value-of select="$targ/personId"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template mode="dereference-party" match="*">
		<xsl:param name="tag"/>
		<xsl:param name="xmlid" select="0"/>
		<xsl:variable name="href" select="@href"/>
		<xsl:variable name="targ" select="//party[@id=$href]"/>
		<xsl:element name="{$tag}">
			<xsl:copy-of select="$targ/partyId/@*"/>
			<xsl:if test="$xmlid">
				<xsl:attribute name="id"><xsl:value-of select="$href"/></xsl:attribute>
			</xsl:if>
			<xsl:value-of select="$targ/partyId"/>
		</xsl:element>
	</xsl:template>	


	<xsl:template mode="copy-structure" match="*">
		<xsl:param name="tag"/>
		<xsl:element name="{$tag}">
			<xsl:copy-of select="@*"/>
			<xsl:copy-of select="*[not(contains(local-name(.), 'TradeId'))]"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template match="header" >
		<xsl:apply-templates mode="strip-id" select="."/>
	</xsl:template>	

	<xsl:template match="party|account" >
		<xsl:apply-templates mode="strip-id" select="."/>
	</xsl:template>	

</xsl:stylesheet>
