<?xml version="1.0" ?>
<!-- Author: Brian Lynn, GEM LLC, for ISDA -->
<!-- Jan 2018 -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0"
	xmlns:common="http://exslt.org/common" 
	xpath-default-namespace="http://www.fpml.org/FpML-5/recordkeeping"
	xmlns:prod="http://fpml.org/product-definition" 
	xmlns:field="http://www.fpml.org/field-definition" 
	xmlns:mapp="http://www.fpml.org/dtccmapping"
	>

	<xsl:variable name="docWithIdsTF">
		<xsl:apply-templates mode="add-id" select="."/>
	</xsl:variable>
	<xsl:variable name="docWithIds" select="common:node-set($docWithIdsTF)"/>
	<xsl:variable name="full.document" select="."/>
	<xsl:variable name="target.ns" select="'http://www.fpml.org/FpML-5/recordkeeping'"/>

	<!-- id processing -->

	<xsl:template match="node() | @*" priority="-1" mode="add-id">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()" mode="add-id"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="*" priority="2" mode="add-id">
		<xsl:copy>
			<xsl:copy-of select="@*"/>
			<xsl:attribute name="xxxid" select="generate-id(.)"/>
			<xsl:apply-templates select="node()" mode="add-id"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="*" mode="strip-id">
		<xsl:element name="{local-name(.)}" namespace="{$target.ns}">
			<xsl:apply-templates select="@*|node()" mode="strip-id"/>
		</xsl:element>
	</xsl:template>	

	<xsl:template match="text()|@*" priority="10" mode="strip-id">
		<xsl:copy-of select="."/>
	</xsl:template>	

	<xsl:template match="node()" priority="-1" mode="strip-id">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()" mode="strip-id"/>
		</xsl:copy>
	</xsl:template>	

	<xsl:template match="@xxxid" priority="100" mode="strip-id"/>
	<xsl:template match="blockTradeId|originatingTradeId|linkId|allocationTradeId" mode="strip-id"/>

<!-- end of id processing -->

	<xsl:template match="*" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:param name="docWIds" select="$docWithIds"/>
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
	</xsl:template>

	<xsl:template match="text()" mode="fill-template" >
		<xsl:copy-of select="."/>
	</xsl:template>

	<xsl:template match="*[@field]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc" select="$full.document"/>
		<xsl:variable name="fieldname" select="@field"/>
		<xsl:variable name="elem" select="local-name(.)"/>
		<xsl:apply-templates mode="get-xpath-value" select="$doc">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="*[@xpath and @value]" mode="fill-template" priority="100" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:variable name="fieldname" select="@field"/>
		<xsl:variable name="elem" select="local-name(.)"/>
		<xsl:variable name="xpath" select="@xpath"/>
		<xsl:variable name="res" >
			<xsl:apply-templates mode="get-xpath-value" select="$doc">
				<xsl:with-param name="fieldname" select="$fieldname"/>
				<xsl:with-param name="defs" select="$defs"/>
				<xsl:with-param name="elem" select="$elem"/>
				<xsl:with-param name="xpath" select="$xpath"/>
			</xsl:apply-templates>
		</xsl:variable>
		<xsl:if test="$res">
			<xsl:copy>
				<xsl:value-of select="@value"/>
			</xsl:copy>
		</xsl:if>
	</xsl:template>

	<xsl:template match="*[@xpath]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:variable name="fieldname" select="@field"/>
		<xsl:variable name="elem" select="local-name(.)"/>
		<xsl:variable name="xpath" select="@xpath"/>
		<xsl:apply-templates mode="get-xpath-value" select="$doc">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
			<xsl:with-param name="xpath" select="$xpath"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="*[@copy]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:param name="docWIds" select="$docWithIds"/>
		<xsl:variable name="mode" select="@mode"/>
		<xsl:variable name="wrap" select="@wrap and not(@nowrap='true')"/>
		<xsl:variable name="copyatts" select="@copyatts='true'"/>
		<xsl:variable name="fieldname" select="@copy"/>
		<xsl:variable name="children" select="@children"/>
		<!--<xsl:message>Working on <xsl:value-of select="$fieldname"/>, mode = <xsl:value-of select="$mode"/>, wrap=<xsl:value-of select="$wrap"/></xsl:message>-->
		<!--
		<xsl:comment>copy <xsl:value-of select="$fieldname"/>, doc has <xsl:value-of select="$docWIds"/>, wrap=<xsl:value-of select="$wrap"/></xsl:comment>
		-->
		<!--    -->
		<xsl:variable name="elem" select="local-name(.)"/>
		<xsl:variable name="xpath" select="@copy"/>

		<xsl:apply-templates mode="get-xpath-copy-structure" select="$docWIds">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
			<xsl:with-param name="skipelement" select="$mode='skipelement'"/>
			<xsl:with-param name="xpath" select="$xpath"/>
			<xsl:with-param name="docWIds" select="$docWIds"/>
			<xsl:with-param name="wrap" select="$wrap"/>
			<xsl:with-param name="copyatts" select="$copyatts"/>
			<xsl:with-param name="children" select="$children"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="*[@copywrapper]" mode="fill-template" >
		<xsl:param name="defs" />
		<xsl:param name="doc"/>
		<xsl:param name="nowrap" select="false"/>
		<xsl:variable name="fieldname" select="@copywrapper"/>
		<xsl:variable name="elem" select="local-name(.)"/>
		<xsl:variable name="xpath" select="@copywrapper"/>
		<xsl:variable name="template" select="."/>
		<!--<xsl:message>Working on <xsl:value-of select="$xpath"/></xsl:message>-->

		<xsl:apply-templates mode="get-xpath-copy-wrapper" select="$docWithIds">
			<xsl:with-param name="fieldname" select="$fieldname"/>
			<xsl:with-param name="defs" select="$defs"/>
			<xsl:with-param name="elem" select="$elem"/>
			<xsl:with-param name="wrap" select="not($nowrap)"/>
			<xsl:with-param name="xpath" select="$xpath"/>
			<xsl:with-param name="template" select="$template"/>
		</xsl:apply-templates>
	</xsl:template>

	<xsl:template match="*" mode="xpath-copy-structure" >
		<xsl:param name="elem"/>
		<xsl:param name="wrap"/>
		<xsl:param name="skipelement" />
		<xsl:param name="docWIds" />
		<xsl:param name="xpath" />
		<xsl:param name="debug" />
		<xsl:param name="children" select="false" />
		<xsl:param name="copyatts" select="1" />

		<xsl:if test="string-length($xpath) &gt; 1">
			<xsl:variable name="idval">
				<xsl:apply-templates select="." mode="evaluate-xpath">
					<xsl:with-param name="xpath" select="concat($xpath,'/@xxxid')"/>
				</xsl:apply-templates>
			</xsl:variable>
			<!--
			<xsl:variable name="xp2" select="replace($xpath, 'rechttps://m.facebook.com/events/2455048641387005/:nonpublicExecutionReport/', '')"/>
			<xsl:variable name="xp3" select="replace($xp2, 'rec:trade', 'rec:productInformation')"/>
			<xsl:variable name="xp4" select="concat($xp3, '/@xxxid')"/>
			<xsl:message>xp4=<xsl:value-of select="$xp4"/></xsl:message>
			<xsl:variable name="idval" select="saxon:evaluate($xp4)"/>
			-->
			<!--<xsl:message>%%%%%%% idval=<xsl:value-of select="$idval"/> %%%%%%%%%%%%</xsl:message>-->
			<xsl:if test="number($debug)&gt;1">
				<!--<xsl:message>xpath= <xsl:value-of select="$xpath"/>, idval=<xsl:value-of select="$idval"/>, wrap=<xsl:value-of select="$wrap"/> children=<xsl:value-of select="$children"/>, skipelem=<xsl:value-of select="$skipelement"/>, elem=<xsl:value-of select="$elem"/> </xsl:message>-->
				<xsl:comment>xpath= <xsl:value-of select="$xpath"/>, idval=<xsl:value-of select="$idval"/>, wrap=<xsl:value-of select="$wrap"/> children=<xsl:value-of select="$children"/> </xsl:comment>
			</xsl:if>
			<!--
			<xsl:comment>For <xsl:value-of select="$fieldname"/>, xpath is <xsl:value-of select="$xpath"/>, tweaked = <xsl:value-of select="$xp4"/>, id val is <xsl:value-of select="$idval"/></xsl:comment>
			<xsl:comment>For <xsl:value-of select="$fieldname"/>, xpath is <xsl:value-of select="$xpath"/>, tweaked = <xsl:value-of select="$xp3"/></xsl:comment>
			<xsl:comment>Current node is <xsl:value-of select="local-name(.)"/></xsl:comment>
			<xsl:comment>Child node is <xsl:value-of select="local-name(*[1])"/></xsl:comment>
			<xsl:comment>value= <xsl:value-of select="$val"/></xsl:comment>
			<xsl:comment>element name= <xsl:value-of select="$elem"/></xsl:comment>
			-->
			<xsl:if test="string-length($idval) &gt; 0">
				<xsl:choose>
					<xsl:when test="$skipelement">
						<xsl:if test="$copyatts">
							<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]/@*"/>
						</xsl:if>
						<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]"/>
					</xsl:when>
					<xsl:when test="$children">
						<!--<xsl:message>doing children for <xsl:value-of select="$elem"/></xsl:message>-->
						<xsl:element name="{$elem}" namespace="{$target.ns}">
							<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]/*"/>
							<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]/text()"/>
						</xsl:element>
					</xsl:when>
					<xsl:when test="$wrap or $wrap = true">
						<xsl:element name="{$elem}" namespace="{$target.ns}">
							<xsl:if test="$copyatts">
								<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]/@*"/>
							</xsl:if>
							<!-- if only generating children, add /* to the xpath -->
							<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]"/>
						</xsl:element>
					</xsl:when>
					<xsl:when test="not($wrap) or $wrap = false">
						<!--
						<xsl:if test="$copyatts">
							<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]/@*"/>
						</xsl:if>
						-->
						<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]"/>
					</xsl:when>
					<xsl:otherwise>
						<xsl:element name="{$elem}" namespace="{$target.ns}">
							<xsl:if test="$copyatts">
								<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]/@*"/>
							</xsl:if>
							<xsl:apply-templates mode="strip-id" select ="$docWIds//*[@xxxid = $idval]/node()"/>
						</xsl:element>
					</xsl:otherwise>
				</xsl:choose>
			</xsl:if>
		</xsl:if>
	</xsl:template>

	<xsl:template match="*" mode="get-xpath-value" >
		<xsl:param name="defs"/>
		<xsl:param name="fieldname"/>
		<xsl:param name="elem"/>
		<xsl:param name="debug" select="0"/>
		<xsl:param name="xpath" select="(@xpath|$defs/field:field[@name=$fieldname]/@xpath|$defs/prod:section/prod:field[@name=$fieldname])[1]"/>
		<xsl:if test="($defs/prod:section/prod:field[@name=$fieldname])[2]">
			<!--<xsl:message> Duplicate def for <xsl:value-of select="$fieldname"/></xsl:message>-->
		</xsl:if>
		<!--
		<xsl:comment> Processing <xsl:value-of select="$fieldname"/> for <xsl:value-of select="$elem"/>, xpath = <xsl:value-of select="$xpath"/></xsl:comment>
		-->
		<xsl:if test="$debug &gt; 0">
			<!--<xsl:message> <xsl:value-of select="$fieldname"/> Xpath is <xsl:value-of select="$xpath"/></xsl:message>-->
			<xsl:comment> Xpath is <xsl:value-of select="$xpath"/></xsl:comment>
		</xsl:if>

		<xsl:if test="string-length($xpath) &gt; 1">
			<!--
			<xsl:variable name="xp2" select="replace($xpath, 'rec:nonpublicExecutionReport/', '')"/>
			<xsl:variable name="xp3" select="replace($xp2, 'rec:trade', 'rec:productInformation')"/>
			<xsl:variable name="val" select="saxon:evaluate($xp3)"/>
			-->
			<xsl:variable name="xp2" select="replace($xpath, '`', '')"/>
			<xsl:variable name="val">
				<xsl:apply-templates select="." mode="evaluate-xpath">
					<xsl:with-param name="xpath" select="$xp2"/>
				</xsl:apply-templates>
			</xsl:variable>
			<xsl:if test="$debug =2">
			<!--<xsl:message>For <xsl:value-of select="$fieldname"/>, xpath is <xsl:value-of select="$xp2"/></xsl:message>-->
			<!--<xsl:message>Current node is <xsl:value-of select="local-name(.)"/></xsl:message>-->
			<!--<xsl:message>Child node is <xsl:value-of select="local-name(*[1])"/></xsl:message>-->
			<!--<xsl:message>value= <xsl:value-of select="$val"/></xsl:message>-->
			<!--<xsl:message>element name= <xsl:value-of select="$elem"/></xsl:message>-->
			</xsl:if>
			<xsl:if test="string-length($val) &gt; 0">
				<xsl:element name="{$elem}" namespace="{$target.ns}"><xsl:value-of select="$val"/></xsl:element>
			</xsl:if>
		</xsl:if>
	</xsl:template>

<!-- template processing -->

</xsl:stylesheet>
