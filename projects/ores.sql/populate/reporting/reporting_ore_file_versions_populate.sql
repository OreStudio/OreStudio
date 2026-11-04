/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * ORE File Versions Population Script
 *
 * GENERATED FILE - DO NOT EDIT. Regenerate with
 * projects/ores.sql/utility/generate_file_types_populate.py.
 *
 * Seeds the contents of the 24 XML schemas ORE 1.8.17.0 ships, from
 * external/ore/xsd, checked against the digest its manifest states. A later engine
 * version adds rows beside these. This script is idempotent.
 */

\echo '--- ORE File Versions ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
    r record;
begin
    for r in
        select * from (values
        ('501ebb91-f017-5fd6-a4ed-2b1e9912a1b6'::uuid, 'master_input_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="ore" name="ORE"/>

  <xs:complexType name="ore">
    <xs:all>
      <xs:element type="parameterListType" name="Setup"/>
      <xs:element type="parameterListType" name="Logging" minOccurs="0" />
      <xs:element type="parameterListType" name="Markets" minOccurs="0" />
      <xs:element type="analyticsType" name="Analytics"/>
      <xs:element type="parameterListType" name="Restore" minOccurs="0" />
    </xs:all>
  </xs:complexType>

  <xs:complexType name="analyticsType">
    <xs:sequence>
      <xs:element name="Analytic" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:complexContent>
            <xs:extension base="parameterListType">
              <xs:attribute type="xs:string" name="type"/>
            </xs:extension>
          </xs:complexContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="parameterListType">
    <xs:sequence>
      <xs:element name="Parameter" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

</xs:schema>
$xsd$, '0fce060b5d37de30375ac94f46081bbcf53230590262f468acf8fe4db2a9edbe', 1484),
        ('a249259b-ca52-5e27-bdd7-c6ddd6d9cb0a'::uuid, 'todays_market_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="todaysmarket" name="TodaysMarket"/>

  <xs:complexType name="todaysmarket">
    <xs:sequence>
      <xs:element type="configurationType" name="Configuration" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="yieldCurvesType"  name= "YieldCurves" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="discountCurvesType" name="DiscountingCurves" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="indexForwardingCurvesType" name="IndexForwardingCurves" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="swapIndexCurvesType" name="SwapIndexCurves" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="zeroInflationIndexCurvesType" name="ZeroInflationIndexCurves" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="yyInflationIndexCurvesType" name="YYInflationIndexCurves" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="fxSpotsType" name="FxSpots" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="fxVolatilitiesType" name="FxVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="swaptionVolatilitiesType" name="SwaptionVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="yieldVolatilitiesType" name="YieldVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="capFloorVolatilitiesType" name="CapFloorVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="cdsVolatilitiesType" name="CDSVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="defaultCurvesType" name="DefaultCurves" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="yyInflationCapFloorVolatilitiesType" name="YYInflationCapFloorVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="zeroInflationCapFloorVolatilitiesType" name="ZeroInflationCapFloorVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="equityCurvesType" name="EquityCurves" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="equityVolatilitiesType" name="EquityVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="securitiesType" name="Securities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="baseCorrelationsType" name="BaseCorrelations" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="commodityCurvesType" name="CommodityCurves" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="commodityVolatilitiesType" name="CommodityVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="correlationsType" name="Correlations" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="bondFutureVolatilitiesType" name="BondFutureVolatilities" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="intradayPowerPriceCurvesType" name="IntradayPowerPriceCurves" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="configurationType">
    <xs:all>
      <xs:element type="xs:string" name="YieldCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="DiscountingCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="IndexForwardingCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="SwapIndexCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="ZeroInflationIndexCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="ZeroInflationCapFloorVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="YYInflationIndexCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="FxSpotsId" minOccurs="0"/>
      <xs:element type="xs:string" name="BaseCorrelationsId" minOccurs="0"/>
      <xs:element type="xs:string" name="FxVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="SwaptionVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="YieldVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="CapFloorVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="CDSVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="DefaultCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="YYInflationCapFloorVolatilitiesId" minOccurs="0"/>
      <!--<xs:element type="xs:string" name="ZeroInflationCapFloorVolatilitiesId" minOccurs="0"/>-->
      <xs:element type="xs:string" name="EquityCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="EquityVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="SecuritiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="CommodityCurvesId" minOccurs="0"/>
      <xs:element type="xs:string" name="CommodityVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="CorrelationsId" minOccurs="0"/>
      <xs:element type="xs:string" name="BondFutureVolatilitiesId" minOccurs="0"/>
      <xs:element type="xs:string" name="IntradayPowerPriceCurvesId" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:string" name="id" use="required"/>
  </xs:complexType>

  <xs:complexType name="yieldCurvesType">
    <xs:sequence>
      <xs:element name="YieldCurve" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
              <!-- FIXME: should be currencyCode -->
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="discountCurvesType">
    <xs:sequence>
      <xs:element name="DiscountingCurve" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="currency" use="required"/>
              <!-- FIXME: should be currencyCode -->
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="indexForwardingCurvesType">
    <xs:sequence>
      <xs:element name="Index" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="indexNameType" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="swapIndexCurvesType">
    <xs:sequence>
      <xs:element name="SwapIndex" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="indexNameType" name="Discounting"/>
          </xs:all>
          <xs:attribute type="indexNameType" name="name" use="required"/>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="zeroInflationIndexCurvesType">
    <xs:sequence>
      <xs:element name="ZeroInflationIndexCurve" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="yyInflationIndexCurvesType">
    <xs:sequence>
      <xs:element name="YYInflationIndexCurve" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="fxSpotsType">
    <xs:sequence>
      <xs:element name="FxSpot" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="currencyPair" name="pair" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="fxVolatilitiesType">
    <xs:sequence>
      <xs:element name="FxVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="currencyPair" name="pair" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="swaptionVolatilitiesType">
    <xs:sequence>
      <xs:element name="SwaptionVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="key"/>
              <xs:attribute type="currencyCode" name="currency"/> <!-- deprecated -->
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="yieldVolatilitiesType">
    <xs:sequence>
      <xs:element name="YieldVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="capFloorVolatilitiesType">
    <xs:sequence>
      <xs:element name="CapFloorVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="key"/>
              <xs:attribute type="currencyCode" name="currency"/> <!-- deprecated -->
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="cdsVolatilitiesType">
    <xs:sequence>
      <xs:element name="CDSVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="defaultCurvesType">
    <xs:sequence>
      <xs:element name="DefaultCurve" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="yyInflationCapFloorVolatilitiesType">
    <xs:sequence>
      <xs:element name="YYInflationCapFloorVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="zeroInflationCapFloorVolatilitiesType">
    <xs:sequence>
      <xs:element name="ZeroInflationCapFloorVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="equityCurvesType">
    <xs:sequence>
      <xs:element name="EquityCurve" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="equityVolatilitiesType">
    <xs:sequence>
      <xs:element name="EquityVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="securitiesType">
    <xs:sequence>
      <xs:element name="Security" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="baseCorrelationsType">
    <xs:sequence>
      <xs:element name="BaseCorrelation" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="commodityCurvesType">
    <xs:sequence>
      <xs:element name="CommodityCurve" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="commodityVolatilitiesType">
    <xs:sequence>
      <xs:element name="CommodityVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="correlationsType">
    <xs:sequence>
      <xs:element name="Correlation" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="bondFutureVolatilitiesType">
    <xs:sequence>
      <xs:element name="BondFutureVolatility" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

  <xs:complexType name="intradayPowerPriceCurvesType">
    <xs:sequence>
      <xs:element name="IntradayPowerPriceCurve" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute type="xs:string" name="name" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>

</xs:schema>
$xsd$, 'df0c699d2005a884f6d1e00dfe5b2db7a476261bb6267b4b73842d258787999c', 17660),
        ('bc840011-1cbd-59b1-85a0-7162383188c2'::uuid, 'pricing_engines_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="pricingengines" name="PricingEngines"/>

  <xs:complexType name="pricingengines">
    <xs:sequence>
      <xs:element type="product" name="Product" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="globalParameters" name="GlobalParameters" minOccurs="0"/>
      <!-- FIXME: poor name -->
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="product">
    <xs:all>
      <xs:element type="xs:string" name="Model"/>
      <xs:element name="ModelParameters">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="parameter" name="Parameter" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:string" name="Engine"/>
      <xs:element name="EngineParameters">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="parameter" name="Parameter" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
    <xs:attribute type="xs:string" name="type" use="required"/>
  </xs:complexType>

  <xs:complexType name="globalParameters">
    <xs:sequence>
      <xs:element type="parameter" name="Parameter" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="parameter">
    <!-- parameter with attribute -->
    <xs:simpleContent>
      <xs:extension base="xs:string">
        <xs:attribute type="xs:string" name="name" use="required"/>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

</xs:schema>
$xsd$, 'add0534a6004b88b7bf70c48cf4c90a7c262124ad653c99aa2732670977fab7c', 1721),
        ('d8cce8f4-ef1e-5611-90c0-74b9350970d1'::uuid, 'simulation_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="simulation" name="Simulation"/>

  <xs:element type="crossAssetModel" name="CrossAssetModel"/>

  <xs:complexType name="simulation">
    <xs:all>
      <xs:element type="parameters" name="Parameters" minOccurs="0" maxOccurs="1"/>
      <xs:element type="crossAssetModel" name="CrossAssetModel" minOccurs="0" maxOccurs="1"/>
      <xs:element type="market" name="Market" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="parameters">
    <xs:all>
      <xs:element type="xs:string" name="Grid"/>
      <xs:element type="xs:string" name="Calendar"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0"/>
      <!-- can be multiple so we don't use calendar type -->
      <xs:element type="sequenceType" name="Sequence"/>
      <!-- does not exist anymore, keep for schema backwards compatibility -->
      <xs:element type="xs:string" name="Scenario" minOccurs="0"/>
      <!-- deprecated, this moved to crossAssetModel below -->
      <xs:element type="discretizationType" name="Discretization" minOccurs="0"/>
      <!-- FIXME: remove this -->
      <xs:element type="xs:integer" name="Seed"/>
      <xs:element type="xs:positiveInteger" name="Samples"/>
      <xs:element type="SobolBrownianGeneratorOrdering" name="Ordering" minOccurs="0"/>
      <xs:element type="SobolRsgDirectionIntegers" name="DirectionIntegers" minOccurs="0"/>
      <xs:element type="xs:string" name="CloseOutLag" minOccurs="0"/>
      <xs:element type="mporMode" name="MporMode" minOccurs="0"/>
      <xs:element type="xs:integer" name="TimeStepsPerYear" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="mporMode">
    <xs:restriction base="xs:string">
      <xs:enumeration value="ActualDate"/>
      <xs:enumeration value="StickyDate"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="discretizationType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Exact"/>
      <xs:enumeration value="Euler"/>
      <xs:enumeration value="BestMarginalDiscretization"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="SobolBrownianGeneratorOrdering">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Factors"/>
      <xs:enumeration value="Steps"/>
      <xs:enumeration value="Diagonal"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="SobolRsgDirectionIntegers">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Unit"/>
      <xs:enumeration value="Jaeckel"/>
      <xs:enumeration value="SobolLevitan"/>
      <xs:enumeration value="SobolLevitanLemieux"/>
      <xs:enumeration value="JoeKuoD5"/>
      <xs:enumeration value="JoeKuoD6"/>
      <xs:enumeration value="JoeKuoD7"/>
      <xs:enumeration value="Kuo"/>
      <xs:enumeration value="Kuo2"/>
      <xs:enumeration value="Kuo3"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="salvagingAlgoType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="None"/>
      <xs:enumeration value="Spectral"/>
      <xs:enumeration value="Hypersphere"/>
      <xs:enumeration value="LowerDiagonal"/>
      <xs:enumeration value="Higham"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="sequenceType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="MersenneTwister"/>
      <xs:enumeration value="MersenneTwisterAntithetic"/>
      <xs:enumeration value="Sobol"/>
      <xs:enumeration value="SobolBrownianBridge"/>
      <xs:enumeration value="Burley2020SobolBrownianBridge"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="crossAssetModel">
    <xs:all>
      <xs:element type="currencyCode" name="DomesticCcy"/>
      <!-- FIXME: Currency not Ccy -->
      <xs:element name="Currencies" maxOccurs="1" minOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="currencyCode" name="Currency" maxOccurs="unbounded" minOccurs="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Equities" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="Equity" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="InflationIndices" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="InflationIndex" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="CreditNames" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="CreditName" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Commodities" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="Commodity" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:float" name="BootstrapTolerance" maxOccurs="1" />
      <xs:element type="measureType" name="Measure" minOccurs="0" maxOccurs="1" />
      <xs:element type="discretizationType" name="Discretization" minOccurs="0"/>
      <xs:element type="salvagingAlgoType" name="SalvagingAlgorithm" minOccurs="0"/>
      <xs:element type="xs:string" name="IntegrationPolicy" minOccurs="0"/>
      <xs:element type="bool" name="PiecewiseIntegration" minOccurs="0"/>
      <xs:element name="InterestRateModels" maxOccurs="1" >
        <xs:complexType>
          <xs:sequence>
            <xs:choice>
              <xs:element type="lgm" name="LGM" maxOccurs="unbounded" minOccurs="0"/>
              <xs:element type="hw" name="HWModel" maxOccurs="unbounded" minOccurs="0"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="ForeignExchangeModels" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:choice>
              <xs:element type="crossCurrencyLGM" name="CrossCcyLGM" maxOccurs="unbounded" minOccurs="0"/>
              <xs:element type="localVol" name="LocalVol" maxOccurs="unbounded" minOccurs="0"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="EquityModels" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="crossAssetLGM" name="CrossAssetLGM" maxOccurs="unbounded" minOccurs="0"/>
            <!-- FIXME: poor name -->
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="InflationIndexModels" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:choice maxOccurs="unbounded" minOccurs="0">
              <xs:element type="lgm" name="LGM"/>
              <xs:element type="jarrowYildrim" name="JarrowYildirim"/>
              <xs:element type="dodgsonKainth" name="DodgsonKainth"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="CreditModels" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:choice maxOccurs="unbounded" minOccurs="0">
              <xs:element type="crlgm" name="LGM"/>
              <xs:element type="cir" name="CIR"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="CommodityModels" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="commoditySchwartz" name="CommoditySchwartz" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="CreditStates" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:integer" name="NumberOfFactors"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="InstantaneousCorrelations" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Correlation" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="correlationValue">
                    <xs:attribute type="xs:string" name="factor1" use="required"/>
                    <xs:attribute type="xs:string" name="factor2" use="required"/>
                    <xs:attribute type="xs:string" name="index1"/>
                    <xs:attribute type="xs:string" name="index2"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="correlationValue">
    <xs:restriction base="xs:double">
      <xs:minInclusive value="-1.0"/>
      <xs:maxInclusive value="1.0"/>
    </xs:restriction>
  </xs:simpleType>
  
  <xs:complexType name="calibrationCpiCapFloor">
    <xs:sequence>
      <xs:element type="capFloor" name="Type"/>
      <xs:element type="xs:string" name="Maturity"/>
      <xs:element type="xs:string" name="Strike"/>
    </xs:sequence>
  </xs:complexType>
  
  <xs:complexType name="calibrationYoYCapFloor">
    <xs:sequence>
      <xs:element type="capFloor" name="Type"/>
      <xs:element type="xs:string" name="Tenor"/>
      <xs:element type="xs:string" name="Strike"/>
    </xs:sequence>
  </xs:complexType>
  
  <xs:complexType name="calibrationYoYSwap">
    <xs:sequence>
      <xs:element type="xs:string" name="Tenor"/>
    </xs:sequence>
  </xs:complexType>
  
  <xs:complexType name="calibrationBasket">
    <xs:choice>
      <xs:element type="calibrationCpiCapFloor" name="CpiCapFloor" minOccurs="1" maxOccurs="unbounded"/>
      <xs:element type="calibrationYoYCapFloor" name="YoYCapFloor" minOccurs="1" maxOccurs="unbounded"/>
      <xs:element type="calibrationYoYSwap" name="YoYSwap" minOccurs="1" maxOccurs="unbounded"/>
    </xs:choice>
    <xs:attribute type="xs:string" name="parameter" />
  </xs:complexType>
  
  <xs:complexType name="lgm">
    <xs:all>
      <xs:element type="currencyCode" name="Currency" maxOccurs="1" minOccurs="0"/>
      <xs:element type="calibrationTypeType" name="CalibrationType"/>
      <xs:element name="Volatility">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="volatilityTypeType" name="VolatilityType"/>
            <xs:element type="paramTypeType" name="ParamType"/>
            <xs:element type="xs:string" name="TimeGrid"/>
            <!-- FIXME: comma delimited list -->
            <xs:element type="xs:string" name="InitialValue"/>
            <!-- FIXME: comma delimited list -->
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="Reversion">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="reversionTypeType" name="ReversionType"/>
            <xs:element type="paramTypeType" name="ParamType"/>
            <xs:element type="xs:string" name="TimeGrid"/>
            <xs:element type="xs:string" name="InitialValue"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CalibrationSwaptions" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Expiries"/>
            <xs:element type="xs:string" name="Terms"/>
            <xs:element type="xs:string" name="Strikes" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CalibrationCapFloors" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="CapFloor"/>
            <xs:element type="xs:string" name="Expiries"/>
            <!-- FIXME: comma delimited list -->
            <xs:element type="xs:string" name="Strikes"/>
            <!-- FIXME: comma delimited list -->
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CalibrationBaskets" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="calibrationBasket" name="CalibrationBasket" minOccurs="1" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="ParameterTransformation">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:float" name="ShiftHorizon"/>
            <xs:element type="xs:float" name="Scaling"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element type="floatSpreadMappingType" name="FloatSpreadMapping" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="currencyCodeWithDefault" name="ccy"/>
    <!-- FIXME: currency not ccy -->
    <xs:attribute type="xs:string" name="index" />
    <!-- FIXME: currency not ccy -->
    <xs:attribute type="xs:string" name="key" />
  </xs:complexType>

  <xs:simpleType name="floatSpreadMappingType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="NextCoupon"/>
      <xs:enumeration value="ProRata"/>
      <xs:enumeration value="Simple"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="hw">
  <xs:all>
    <xs:element type="currencyCode" name="Currency" maxOccurs="1" minOccurs="0"/>
    <xs:element type="calibrationTypeType" name="CalibrationType"/>
    <xs:element name="Volatility" minOccurs="0">
      <xs:complexType>
        <xs:all>
          <xs:element type="bool" name="Calibrate"/>
		  <xs:element type="volatilityTypeType" name="VolatilityType" minOccurs="0"/>
          <xs:element type="paramTypeType" name="ParamType"/>
          <xs:element type="xs:string" name="TimeGrid"/>
          <!-- FIXME: comma delimited list -->
          <xs:element name="InitialValue">
            <xs:complexType>
              <xs:sequence>
                <xs:element name="Sigma" maxOccurs="unbounded">
                  <xs:complexType>
                    <xs:sequence>
                      <xs:element type="xs:string" name="Row" maxOccurs="unbounded" minOccurs="1"/>
                    </xs:sequence>
                  </xs:complexType>
                </xs:element>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
          <!-- FIXME: comma delimited list -->
        </xs:all>
      </xs:complexType>
    </xs:element>
    <xs:element name="Reversion">
      <xs:complexType>
        <xs:all>
          <xs:element type="bool" name="Calibrate"/>
		  <xs:element type="reversionTypeType" name="ReversionType" minOccurs="0"/>
          <xs:element type="paramTypeType" name="ParamType"/>
          <xs:element type="xs:string" name="TimeGrid"/>
          <xs:element name="InitialValue" maxOccurs="1" minOccurs="1">
            <xs:complexType>
              <xs:sequence>
                <xs:element type="xs:string" name="Kappa" maxOccurs="unbounded" minOccurs="1"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </xs:all>
      </xs:complexType>
    </xs:element>
    <xs:element name="PCALoadings" minOccurs="0">
      <xs:complexType>
        <xs:sequence>
          <xs:element type="xs:string" name="Loadings" maxOccurs="unbounded" minOccurs="1"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <!-- FIXME: comma delimited list -->
    <xs:element type="volatilityParameter" name="PCASigma0" minOccurs="0"/>
    <xs:element type="xs:string" name="PCASigmaRatios" minOccurs="0"/>
    <!-- FIXME: comma delimited list -->
    <xs:element name="CalibrationSwaptions" maxOccurs="1" minOccurs="0">
      <xs:complexType>
        <xs:all>
          <xs:element type="xs:string" name="Expiries"/>
          <!-- FIXME: comma delimited list -->
          <xs:element type="xs:string" name="Terms"/>
          <!-- FIXME: comma delimited list -->
          <xs:element type="xs:string" name="Strikes"/>
          <!-- FIXME: comma delimited list -->
        </xs:all>
      </xs:complexType>
    </xs:element>
  </xs:all>
  <xs:attribute type="currencyCodeWithDefault" name="ccy"/>
  <!-- FIXME: currency not ccy -->
  <xs:attribute type="xs:string" name="index"/>
  <xs:attribute type="xs:string" name="key"/>
  </xs:complexType>
  
  <xs:complexType name="volatilityParameter">
    <xs:all>
      <xs:element type="volatilityTypeType" name="VolatilityType" minOccurs="0"/>
      <xs:element type="bool" name="Calibrate"/>
      <xs:element type="paramTypeType" name="ParamType"/>
      <xs:element type="xs:string" name="TimeGrid" minOccurs="0"/>
      <xs:element type="xs:string" name="InitialValue"/>
    </xs:all>
  </xs:complexType>
  
  <xs:complexType name="reversionParameter">
    <xs:all>
      <xs:element type="reversionTypeType" name="ReversionType"/>
      <xs:element type="bool" name="Calibrate"/>
      <xs:element type="paramTypeType" name="ParamType"/>
      <xs:element type="xs:string" name="TimeGrid" minOccurs="0"/>
      <xs:element type="xs:string" name="InitialValue"/>
    </xs:all>
  </xs:complexType>
  
  <xs:complexType name="lgmReversionTransformation">
    <xs:sequence>
      <xs:element type="xs:float" name="ShiftHorizon"/>
      <xs:element type="xs:float" name="Scaling"/>
    </xs:sequence>
  </xs:complexType>
  
  <xs:complexType name="boundaryConstraint">
    <xs:sequence>
      <xs:element type="xs:float" name="LowerBound"/>
      <xs:element type="xs:float" name="UpperBound"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="parameter" use="required"/>
  </xs:complexType>
  
  <xs:complexType name="calibrationConfiguration">
    <xs:sequence>
      <xs:element type="positiveDecimal" name="RmseTolerance" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:positiveInteger" name="MaxIterations" maxOccurs="1" minOccurs="0"/>
      <xs:element name="Constraints" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="boundaryConstraint" name="BoundaryConstraint" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>
  
  <xs:complexType name="jarrowYildrim">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="calibrationTypeType" name="CalibrationType"/>
      <xs:element name="CalibrationBaskets" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="calibrationBasket" name="CalibrationBasket" minOccurs="1" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="RealRate">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="volatilityParameter" name="Volatility"/>
            <xs:element type="reversionParameter" name="Reversion"/>
            <xs:element type="lgmReversionTransformation" name="ParameterTransformation"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Index">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="volatilityParameter" name="Volatility"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="calibrationConfiguration" name="CalibrationConfiguration" maxOccurs="1" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:string" name="index" />
  </xs:complexType>

  <xs:complexType name="dodgsonKainth">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="calibrationTypeType" name="CalibrationType"/>
      <xs:element name="CalibrationBaskets" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="calibrationBasket" name="CalibrationBasket" minOccurs="1" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Reversion">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="reversionTypeType" name="ReversionType"/>
            <xs:element type="paramTypeType" name="ParamType"/>
            <xs:element type="xs:string" name="TimeGrid"/>
            <xs:element type="xs:string" name="InitialValue"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="Volatility">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="volatilityTypeType" name="VolatilityType"/>
            <xs:element type="paramTypeType" name="ParamType"/>
            <xs:element type="xs:string" name="TimeGrid"/>
            <!-- FIXME: comma delimited list -->
            <xs:element type="xs:string" name="InitialValue"/>
            <!-- FIXME: comma delimited list -->
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="ParameterTransformation">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:float" name="ShiftHorizon"/>
            <xs:element type="xs:float" name="Scaling"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element type="calibrationConfiguration" name="CalibrationConfiguration" maxOccurs="1" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:string" name="index" />
  </xs:complexType>

  <xs:complexType name="cir">
    <xs:sequence>
      <xs:element type="currencyCode" name="Currency" maxOccurs="1" minOccurs="0"/>
      <xs:element type="calibrationTypeType" name="CalibrationType"/>
      <xs:element type="xs:string" name="CalibrationStrategy"/>
      <xs:element type="xs:float" name="StartValue"/>
      <xs:element type="xs:float" name="ReversionValue"/>
      <xs:element type="xs:float" name="LongTermValue"/>
      <xs:element type="xs:float" name="Volatility"/>
      <xs:element type="bool" name="RelaxedFeller"/>
      <xs:element type="xs:float" name="FellerFactor"/>
      <xs:element type="xs:float" name="Tolerance"/>
      <xs:element name="CalibrationCdsOptions">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Expiries"/>
            <xs:element type="xs:string" name="Terms"/>
            <xs:element type="xs:string" name="Strikes"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="crlgm">
    <xs:all>
      <xs:element type="currencyCode" name="Currency" maxOccurs="1" minOccurs="0"/>
      <xs:element type="calibrationTypeType" name="CalibrationType"/>
      <xs:element name="Volatility">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="volatilityTypeType" name="VolatilityType"/>
            <xs:element type="paramTypeType" name="ParamType"/>
            <xs:element type="xs:string" name="TimeGrid"/>
            <xs:element type="xs:string" name="InitialValue"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="Reversion">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="reversionTypeType" name="ReversionType"/>
            <xs:element type="paramTypeType" name="ParamType"/>
            <xs:element type="xs:string" name="TimeGrid"/>
            <xs:element type="xs:string" name="InitialValue"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CalibrationCdsOptions" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Expiries"/>
            <xs:element type="xs:string" name="Terms"/>
            <xs:element type="xs:string" name="Strikes"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="ParameterTransformation" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:float" name="ShiftHorizon"/>
            <xs:element type="xs:float" name="Scaling"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element type="floatSpreadMappingType" name="FloatSpreadMapping" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:string" name="name"/>
  </xs:complexType>

  <xs:simpleType name="calibrationTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Bootstrap"/>
      <xs:enumeration value="BestFit"/>
      <xs:enumeration value="None"/>
      <xs:enumeration value="StatisticalWithRiskNeutralVolatility"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="volatilityTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Hagan"/>
      <xs:enumeration value="HullWhite"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="paramTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Constant"/>
      <xs:enumeration value="Piecewise"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="reversionTypeType">
    <!-- just use volatilityTypeType ?-->
    <xs:restriction base="xs:string">
      <xs:enumeration value="Hagan"/>
      <xs:enumeration value="HullWhite"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="measureType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="LGM"/>
      <xs:enumeration value="BA"/>
      <xs:enumeration value=""/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="crossAssetLGM">
    <xs:all>
      <!-- FIXME: Why is this here every time?? -->
      <xs:element type="currencyCode" name="Currency" />
      <!-- FIXME: Currency not Ccy -->
      <xs:element type="xs:string" name="CalibrationType"/>
      <xs:element name="Sigma">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="xs:string" name="ParamType"/>
            <xs:element type="xs:string" name="TimeGrid"/>
            <!-- FIXME: comma delimited lists -->
            <xs:element type="xs:string" name="InitialValue"/>
            <!-- FIXME: comma delimited lists -->
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CalibrationOptions" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Expiries"/>
            <!-- FIXME: comma delimited lists -->
            <xs:element type="xs:string" name="Strikes"/>
            <!-- FIXME: comma delimited lists -->
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="crossCurrencyLGM">
    <xs:all>
      <xs:element type="currencyCode" name="DomesticCcy"/>
      <xs:element type="xs:string" name="CalibrationType"/>
      <xs:element name="Sigma">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="xs:string" name="ParamType"/>
            <xs:element type="xs:string" name="TimeGrid"/>
            <!-- FIXME: comma delimited lists -->
            <xs:element type="xs:string" name="InitialValue"/>
            <!-- FIXME: comma delimited lists -->
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CalibrationOptions" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Expiries" minOccurs="0"/>
            <!-- FIXME: comma delimited lists -->
            <xs:element type="xs:string" name="Strikes"  minOccurs="0"/>
            <!-- FIXME: comma delimited lists -->
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
    <xs:attribute type="currencyCodeWithDefault" name="foreignCcy" use="required"/>
  </xs:complexType>

  <xs:complexType name="localVolSimpleMcParameters">
    <xs:all>
      <xs:element type="xs:integer" name="TimeStepsPerYear" minOccurs="0" />
      <xs:element type="xs:double" name="CalibrationMoneynessMin" minOccurs="0" />
      <xs:element type="xs:double" name="CalibrationMoneynessMax" minOccurs="0" />
      <xs:element type="xs:integer" name="nStrikes" minOccurs="0" />
      <xs:element type="xs:integer" name="Samples" minOccurs="0" />
      <xs:element type="xs:double" name="d2CdK2Threshold" minOccurs="0" />
      <xs:element type="xs:integer" name="nPasses" minOccurs="0" />
    </xs:all>
  </xs:complexType>

  <xs:complexType name="localVol">
    <xs:all>
      <xs:element type="currencyCode" name="DomesticCcy" />
      <xs:element type="xs:string" name="Model"/>
      <xs:element type="xs:string" name="StochasticRatesCorrection"/>
      <xs:element type="xs:string" name="CalibrationMoneyness"/>
      <xs:element type="xs:string" name="CalibrationGrid"/>
      <xs:element type="localVolSimpleMcParameters" name="SimpleMcParameters" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="currencyCodeWithDefault" name="foreignCcy" use="required"/>
  </xs:complexType>

  <xs:complexType name="commoditySchwartz">
    <xs:all>
      <xs:element type="currencyCode" name="Currency" />
      <xs:element type="xs:string" name="CalibrationType"/>
      <xs:element name="Sigma">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="xs:string" name="InitialValue"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="Kappa">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate"/>
            <xs:element type="xs:string" name="InitialValue"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="Seasonality" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Calibrate" minOccurs="0"/>
            <xs:element type="paramTypeType" name="ParamType" minOccurs="0"/>
            <xs:element type="xs:string" name="TimeGrid" minOccurs="0"/>
            <xs:element type="xs:string" name="InitialValue" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CalibrationOptions" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Expiries"/>
            <xs:element type="xs:string" name="Strikes"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:boolean" name="DriftFreeState" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:simpleType name="default">
    <xs:restriction base="xs:string">
      <xs:enumeration value="default"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="currencyCodeWithDefault">
    <xs:union memberTypes="currencyCode default"/>
  </xs:simpleType>

  <xs:complexType name="market">
    <xs:all>
      <xs:element type="currencyCode" name="BaseCurrency"/>
      <xs:element name="Currencies">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="currencyCode" name="Currency" maxOccurs="unbounded" minOccurs="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="YieldCurves">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Configuration" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="Tenors"/>
                  <xs:element type="ycInterpolation" name="Interpolation" minOccurs="0"/>
                  <xs:element type="ycExtrapolation" name="Extrapolation" minOccurs="0"/>
                  <xs:element type="ycRollDown" name="RollDown" minOccurs="0"/>
                  <xs:element type="dayCounter" name="DayCounter" minOccurs="0"/>
                </xs:all>
                <xs:attribute name="curve" type="xs:string" use="optional" />
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="FxRates" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element name="CurrencyPairs" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="currencyPair" name="CurrencyPair" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="Indices" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="indexNameType" name="Index" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="SwapIndices" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="SwapIndex" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="Name"/>
                  <xs:element type="indexNameType" name="ForwardingIndex" minOccurs="0"/>
                  <xs:element type="indexNameType" name="DiscountingIndex"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="DefaultCurves" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:all>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Tenors"/>
            <xs:element type="xs:boolean" name="SimulateSurvivalProbabilities" minOccurs="0"/>
            <xs:element type="xs:boolean" name="SimulateRecoveryRates" minOccurs="0"/>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="Calendars" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Calendar" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
	    <xs:element type="defaultCurveExtrapolation" name="Extrapolation" minOccurs="0"/>
	    <xs:element type="ycRollDown" name="RollDown" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="Equities" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="SimulateEquityForecastCurve" maxOccurs="1" minOccurs="0"/>
            <xs:element type="bool" name="SimulateDividendYield" maxOccurs="1" minOccurs="0"/>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="DividendTenors"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="SwaptionVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Keys" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Key" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
	    <!-- deprecated -->
            <xs:element name="Currencies" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="currencyCode" name="Currency" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="Expiries">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string" use="optional"/>
                    <xs:attribute name="ccy" type="xs:string" use="optional"/> <!-- deprecated -->
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element name="Terms">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string" use="optional"/>
                    <xs:attribute name="ccy" type="xs:string" use="optional"/> <!-- deprecated -->
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:boolean" name="SimulateATMOnly" maxOccurs="1" minOccurs="0"/>
            <xs:element name="StrikeSpreads" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string" use="optional"/>
                    <xs:attribute name="ccy" type="xs:string" use="optional"/> <!-- deprecated -->
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
			  <xs:attribute name="key" type="xs:string" use="optional"/>
			  <xs:attribute name="ccy" type="xs:string" use="optional"/> <!-- deprecated -->
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="YieldVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Expiries"/>
            <xs:element type="xs:string" name="Terms"/>
            <xs:element type="xs:boolean" name="SimulateATMOnly" maxOccurs="1" minOccurs="0"/>
            <xs:element name="Cube" minOccurs="0">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="StrikeSpreads" maxOccurs="1" minOccurs="0"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="ccy" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="CapFloorVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Keys" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Key" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
	    <!-- deprecated -->
            <xs:element name="Currencies" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="currencyCode" name="Currency" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="Expiries" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string" use="optional"/>
                    <xs:attribute name="ccy" type="xs:string" use="optional"/> <!-- deprecated -->
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element name="Strikes" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string" use="optional"/>
                    <xs:attribute name="ccy" type="xs:string" use="optional"/> <!-- deprecated -->
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="ccy" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="bool" name="AdjustOptionletPillars" maxOccurs="1" minOccurs="0"/>
            <xs:element type="bool" name="UseCapAtm" maxOccurs="1" minOccurs="0"/>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element name="SmileForwardInteraction" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="CDSVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Expiries"/>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="FxVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate"  maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay" maxOccurs="1" minOccurs="0"/>
            <xs:element name="CurrencyPairs" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="currencyPair" name="CurrencyPair" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="Expiries" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="ccyPair" type="xs:string" use="optional"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:boolean" name="SimulateATMOnly" maxOccurs="1" minOccurs="0"/>
            <xs:element name="Surface" minOccurs="0">
              <xs:complexType>
                <xs:choice>
                  <xs:element name="Moneyness" maxOccurs="unbounded" minOccurs="0">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="ccyPair" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                  <xs:element name="StandardDeviations" maxOccurs="unbounded" minOccurs="0">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="ccyPair" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:choice>
              </xs:complexType>
            </xs:element>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="ccyPair" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="EquityVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate"  maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="Expiries" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="name" type="xs:string" use="optional"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:boolean" name="SimulateATMOnly" maxOccurs="1" minOccurs="0"/>
            <xs:element name="Surface" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Moneyness" maxOccurs="unbounded" minOccurs="0">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                  <xs:element name="StandardDeviations" maxOccurs="unbounded" minOccurs="0">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="BenchmarkCurves" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="BenchmarkCurve" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:all>
                  <xs:element type="currencyCode" name="Currency" maxOccurs="1"/>
                  <xs:element type="xs:string" name="Name" maxOccurs="1"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Securities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element name="Names" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CPRs" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element name="Names" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CpiIndices" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="Index" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="ZeroInflationIndexCurves" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Tenors"/>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="YYInflationIndexCurves" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Tenors"/>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CPICapFloorVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Expiries"/>
            <xs:element type="xs:string" name="Strikes"/>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="YYCapFloorVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Expiries"/>
            <xs:element type="xs:string" name="Strikes"/>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Commodities" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element type="priceRollDown" name="RollDown" minOccurs="0"/>
            <xs:element name="Names" maxOccurs="1" minOccurs="1">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="Interpolation" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="name" type="xs:string" use="optional"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element name="Tenors" maxOccurs="unbounded" minOccurs="1">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="name" type="xs:string" use="optional"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="IntradayPowerCurves" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="1" minOccurs="0"/>
            <xs:element name="Names" maxOccurs="1" minOccurs="1">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Name" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="Interpolation" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="name" type="xs:string" use="optional"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element name="Tenors" maxOccurs="unbounded" minOccurs="1">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="name" type="xs:string" use="optional"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="CommodityVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate"  maxOccurs="1" minOccurs="0"/>
            <xs:element type="xs:boolean" name="SimulateATMOnly" maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Name" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:all>
                        <xs:element type="xs:string" name="Expiries"/>
                        <xs:element type="xs:string" name="Moneyness" maxOccurs="1" minOccurs="0"/>
                      </xs:all>
                      <xs:attribute name="id" type="xs:string" />
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="DayCounter" maxOccurs="1" minOccurs="0"/>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="BondFutureVolatilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate"  maxOccurs="1" minOccurs="0"/>
            <xs:element type="xs:boolean" name="SimulateATMOnly" maxOccurs="1" minOccurs="0"/>
            <xs:element type="timeDecayType" name="ReactionToTimeDecay"/>
            <xs:element name="Names">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Name" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:all>
                        <xs:element type="xs:string" name="Expiries"/>
                        <xs:element type="xs:string" name="Moneyness" maxOccurs="1" minOccurs="0"/>
                      </xs:all>
                      <xs:attribute name="id" type="xs:string" />
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="DayCounter" maxOccurs="1" minOccurs="0"/>
            <xs:element name="SmileDynamics" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="key" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="AggregationScenarioDataCurrencies" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="currencyCode" name="Currency" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="AggregationScenarioDataIndices" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="indexNameType" name="Index" maxOccurs="unbounded" minOccurs="0"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="AggregationScenarioDataCreditStates" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:integer" name="NumberOfFactors"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="AggregationScenarioDataSurvivalWeights" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="Name" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="BaseCorrelations"  minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="bool" name="Simulate" maxOccurs="unbounded" minOccurs="0"/>
            <xs:element name="IndexNames">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="IndexName" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Terms" maxOccurs="unbounded" minOccurs="0"/>
            <xs:element type="xs:string" name="DetachmentPoints" maxOccurs="unbounded" minOccurs="0"/>
            <xs:element name="DayCounters" maxOccurs="1" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="DayCounter" maxOccurs="unbounded" minOccurs="1">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string" use="optional"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Correlations" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:all>
            <xs:element type="bool" name="Simulate"  maxOccurs="1" minOccurs="0"/>
            <xs:element name="Pairs">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Pair" maxOccurs="unbounded" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:string" name="Expiries"/>
          </xs:all>
        </xs:complexType>
      </xs:element> 
      <xs:element name="CreditStates" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="NumberOfFactors"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="curveAlgebra" name="CurveAlgebra" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="curveAlgebra">
    <xs:sequence>
      <xs:element type="curveAlgebraCurve" name="Curve" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="curveAlgebraCurve">
    <xs:all>
      <xs:element type="xs:string" name="Key"/>
      <xs:element type="curveAlgebraCurveOperation" name="Operation"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="curveAlgebraCurveOperation">
    <xs:all>
      <xs:element type="xs:string" name="Type"/>
      <xs:element name="Arguments" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="Argument" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="ycInterpolation">
    <xs:restriction base="xs:string">
      <xs:enumeration value="LogLinear"/>
      <xs:enumeration value="LinearZero"/>
      <xs:enumeration value="LogCubic"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="ycExtrapolation">
    <xs:restriction base="xs:string">
      <xs:enumeration value="FlatFwd"/>
      <xs:enumeration value="FlatZero"/>
      <!-- deprecated, for backwards compatibility only -->
      <xs:enumeration value="Y"/>
      <xs:enumeration value="YES"/>
      <xs:enumeration value="TRUE"/>
      <xs:enumeration value="True"/>
      <xs:enumeration value="true"/>
      <xs:enumeration value="1"/>
      <xs:enumeration value="N"/>
      <xs:enumeration value="NO"/>
      <xs:enumeration value="FALSE"/>
      <xs:enumeration value="False"/>
      <xs:enumeration value="false"/>
      <xs:enumeration value="0"/>
      <xs:enumeration value=""/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="ycRollDown">
    <xs:restriction base="xs:string">
      <xs:enumeration value="ConstantDiscounts"/>
      <xs:enumeration value="ForwardForward"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="priceRollDown">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Spot"/>
      <xs:enumeration value="Forward"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="defaultCurveExtrapolation">
    <xs:restriction base="xs:string">
      <xs:enumeration value="FlatFwd"/>
      <xs:enumeration value="FlatZero"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="timeDecayType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="ForwardVariance"/>
      <xs:enumeration value="ConstantVariance"/>
    </xs:restriction>
  </xs:simpleType>

</xs:schema>
$xsd$, 'ae7dc01af9789d437e9f12b02d12fe446f9ca2fdbec2c5b68626692d53648bf0', 74203),
        ('7855cdf4-edf3-53f8-bcb0-e15e75fe0f4a'::uuid, 'sensitivity_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="sensitivityanalysis" name="SensitivityAnalysis"/>

  <xs:complexType name="sensitivityanalysis">
    <xs:all>
      <xs:element type="parExcludes" name="ParConversionExcludes" minOccurs="0"/>
      <xs:element type="xs:string" name="ParSensiRemoveFixing" minOccurs="0"/>
      <xs:element type="parConversionMatrixRegularisation" name="ParConversionMatrixRegularisation" minOccurs="0"/>
      <xs:element type="xs:boolean" name="ParConversion" minOccurs="0"/>
      <xs:element type="discountcurves" name="DiscountCurves" minOccurs="0"/>
      <xs:element type="indexcurves" name="IndexCurves" minOccurs="0"/>
      <xs:element type="yieldcurves" name="YieldCurves" minOccurs="0"/>
      <xs:element type="fxspots" name="FxSpots" minOccurs="0"/>
      <xs:element type="fxvolatilities" name="FxVolatilities" minOccurs="0"/>
      <xs:element type="swaptionvolatilities" name="SwaptionVolatilities" minOccurs="0"/>
      <xs:element type="yieldvolatilities" name="YieldVolatilities" minOccurs="0"/>
      <xs:element type="capfloorvolatilities" name="CapFloorVolatilities" minOccurs="0"/>
      <xs:element type="cdsvolatilities" name="CDSVolatilities" minOccurs="0"/>
      <xs:element type="creditcurves" name="CreditCurves" minOccurs="0"/>
      <xs:element type="equityspots" name="EquitySpots" minOccurs="0"/>
      <xs:element type="equityvolatilities" name="EquityVolatilities" minOccurs="0"/>
      <xs:element type="zeroinflationindexcurves" name="ZeroInflationIndexCurves" minOccurs="0"/>
      <xs:element type="yyinflationindexcurves" name="YYInflationIndexCurves" minOccurs="0"/>
      <xs:element type="cpicapfloorvolatilities" name="CPICapFloorVolatilities" minOccurs="0"/>
      <xs:element type="yycapfloorvolatilities" name="YYCapFloorVolatilities" minOccurs="0"/>
      <xs:element type="dividendyields" name="DividendYieldCurves" minOccurs="0"/>
      <xs:element type="basecorrelations" name="BaseCorrelations"  minOccurs="0"/>
      <xs:element type="securityspreads" name="SecuritySpreads"  minOccurs="0"/>
      <xs:element type="commodityCurves" name="CommodityCurves"  minOccurs="0"/>
      <xs:element type="intradaypowercurves" name="IntradayPowerCurves" minOccurs="0"/>
      <xs:element type="commodityvolatilities" name="CommodityVolatilities" minOccurs="0"/>
      <xs:element type="correlationcurves" name="Correlations" minOccurs="0"/>
      <xs:element type="sensiBondFutureVolatilities" name="BondFutureVolatilities" minOccurs="0"/>
      <xs:element type="crossgammafilter" name="CrossGammaFilter" minOccurs="0"/>
      <xs:element type="bool" name="ComputeGamma" minOccurs="0"/>
      <xs:element type="period" name="ThetaPeriod" minOccurs="0"/>
      <xs:element type="bool" name="UseSpreadedTermStructures" minOccurs="0"/>
      <xs:element type="setRiskFactorKeyTypes" name="TwoSidedDeltaKeyTypes" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="parconversion">
    <xs:sequence>
      <xs:element type="xs:string" name="Instruments" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="xs:boolean" name="SingleCurve" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="xs:string" name="DiscountCurve" maxOccurs="1" minOccurs="0"/>
      <xs:element type="currencyCode" name="OtherCurrency" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:string" name="RateComputationPeriod" maxOccurs="1" minOccurs="0"/>
      <xs:element name="Conventions" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Convention" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="id" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="parExcludes">
    <xs:sequence>
      <xs:element name="Type" maxOccurs="unbounded" minOccurs="0">
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="discountcurves">
    <xs:sequence>
      <xs:element type="discountcurve" name="DiscountCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="survivalprobabilities">
    <xs:sequence>
      <xs:element type="survivalprobability" name="SurvivalProbability" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="indexcurves">
    <xs:sequence>
      <xs:element type="indexcurve" name="IndexCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="yieldcurves">
    <xs:sequence>
      <xs:element type="yieldcurve" name="YieldCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="fxspots">
    <xs:sequence>
      <xs:element type="fxspot" name="FxSpot" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="recoveryrates">
    <xs:sequence>
      <xs:element type="recoveryrate" name="RecoverRate" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="fxvolatilities">
    <xs:sequence>
      <xs:element type="fxvolatility" name="FxVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="swaptionvolatilities">
    <xs:sequence>
      <xs:element type="swaptionvolatility" name="SwaptionVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="yieldvolatilities">
    <xs:sequence>
      <xs:element type="yieldvolatility" name="YieldVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="capfloorvolatilities">
    <xs:sequence>
      <xs:element type="capfloorvolatility" name="CapFloorVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="cdsvolatilities">
    <xs:sequence>
      <xs:element type="cdsvolatility" name="CDSVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="creditcurves">
    <xs:sequence>
      <xs:element type="creditcurve" name="CreditCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="equityspots">
    <xs:sequence>
      <xs:element type="equityspot" name="EquitySpot" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="equityvolatilities">
    <xs:sequence>
      <xs:element type="equityvolatility" name="EquityVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="zeroinflationindexcurves">
    <xs:sequence>
      <xs:element type="zeroinflationindexcurve" name="ZeroInflationIndexCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="yyinflationindexcurves">
    <xs:sequence>
      <xs:element type="yyinflationindexcurve" name="YYInflationIndexCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="yycapfloorvolatilities">
    <xs:sequence>
      <xs:element type="yycapfloorvolatility" name="YYCapFloorVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="cpicapfloorvolatilities">
    <xs:sequence>
      <xs:element type="cpicapfloorvolatility" name="CPICapFloorVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="basecorrelations">
    <xs:sequence>
      <xs:element type="basecorrelation" name="BaseCorrelation" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="securityspreads">
    <xs:sequence>
      <xs:element type="securityspread" name="SecuritySpread" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="dividendyields">
    <xs:sequence>
      <xs:element type="dividendyield" name="DividendYieldCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="commodityCurves">
    <xs:sequence>
      <xs:element type="commodityCurve" name="CommodityCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="commodityvolatilities">
    <xs:sequence>
      <xs:element type="commodityvolatility" name="CommodityVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="correlationcurves">
    <xs:sequence>
      <xs:element type="correlationcurve" name="Correlation" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="sensiBondFutureVolatilities">
    <xs:sequence>
      <xs:element type="sensiBondFutureVolatility" name="BondFutureVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="crossgammafilter">
    <xs:sequence>
      <xs:element type="xs:string" name="Pair" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="shiftTypeEntry">
    <xs:simpleContent>
      <xs:extension base="shiftType">
        <xs:attribute type="xs:string" name="key"/>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="shiftSizeEntry">
    <xs:simpleContent>
      <xs:extension base="xs:float">
        <xs:attribute type="xs:string" name="key"/>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="shiftSchemeEntry">
    <xs:simpleContent>
      <xs:extension base="shiftScheme">
        <xs:attribute type="xs:string" name="key"/>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="survivalprobability">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="discountcurve">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="currencyCode" name="ccy" use="required"/>
  </xs:complexType>

  <xs:complexType name="indexcurve">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="indexNameType" name="index" use="required"/>
  </xs:complexType>

  <xs:complexType name="yieldcurve">
    <xs:sequence>
      <xs:element type="xs:string" name="CurveType" minOccurs="0"/>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="fxspot">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
    <xs:attribute type="currencyPair" name="ccypair" use="required"/>
  </xs:complexType>

  <xs:complexType name="recoveryrate">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="fxvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="currencyPair" name="ccypair" use="required"/>
  </xs:complexType>

  <xs:complexType name="swaptionvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element name="Shifts">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="Shift" maxOccurs="unbounded" minOccurs="0">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute type="xs:string" name="expiry"/>
                      <xs:attribute type="xs:string" name="term"/>
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
      <xs:element type="xs:string" name="ShiftTerms"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="key"/>
    <xs:attribute type="currencyCode" name="ccy"/> <!-- deprecated -->
  </xs:complexType>

  <xs:complexType name="yieldvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element name="Shifts">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="Shift" maxOccurs="unbounded" minOccurs="0">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute type="xs:string" name="expiry"/>
                      <xs:attribute type="xs:string" name="term"/>
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftTerms"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="capfloorvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
      <xs:element type="indexNameType" name="Index" minOccurs="0"/>
      <xs:element type="xs:boolean" name="IsRelative" minOccurs="0"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="key"/>
    <xs:attribute type="currencyCode" name="ccy"/> <!-- deprecated -->
  </xs:complexType>

  <xs:complexType name="cdsvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="creditcurve">
    <xs:sequence>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="equityspot">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="equity" use="required"/>
  </xs:complexType>

  <xs:complexType name="equityvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="equity" use="required"/>
  </xs:complexType>

  <xs:complexType name="zeroinflationindexcurve">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="index" use="required"/>
  </xs:complexType>

  <xs:complexType name="yyinflationindexcurve">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="index" use="required"/>
  </xs:complexType>

  <xs:complexType name="cpicapfloorvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="index" use="required"/>
  </xs:complexType>

  <xs:complexType name="yycapfloorvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="index" use="required"/>
  </xs:complexType>

  <xs:complexType name="basecorrelation">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftLossLevels"/>
      <xs:element type="xs:string" name="ShiftTerms"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="indexName" use="required"/>
  </xs:complexType>

  <xs:complexType name="securityspread">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="security" use="required"/>
  </xs:complexType>

  <xs:complexType name="dividendyield">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="equity" use="required"/>
  </xs:complexType>

  <xs:complexType name="commodityCurve">
    <xs:sequence>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="intradaypowercurves">
    <xs:sequence>
      <xs:element type="intradaypowercurve" name="IntradayPowerCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="intradaypowercurve">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
      <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="commodityvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="correlationcurve">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="index1" use="required"/>
    <xs:attribute type="xs:string" name="index2" use="required"/>
  </xs:complexType>

  <xs:complexType name="sensiBondFutureVolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
        <xs:element type="xs:string" name="Shifts"/>
      </xs:choice>
      <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="setRiskFactorKeyTypes">
    <xs:sequence>
      <xs:element type="riskFactorKeyType" name="RiskFactorKeyType"/>
    </xs:sequence>
  </xs:complexType>

  <xs:simpleType name="parConversionMatrixRegularisation">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Silent"/>
      <xs:enumeration value="Warning"/>
      <xs:enumeration value="Disable"/>
    </xs:restriction>
  </xs:simpleType>

</xs:schema>
$xsd$, 'b59b19d23736cfce33e84e839afbe8e746ea4baa99c5dbb1142ef92d6742dc59', 29088),
        ('880baac2-d745-5923-9e77-caf670ed41d0'::uuid, 'stress_test_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="stresstesting" name="StressTesting"/>

  <xs:complexType name="stresstesting">
    <xs:sequence>
      <xs:element type="bool" name="UseSpreadedTermStructures" minOccurs="0"/>
      <xs:element type="stresstest" name="StressTest" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

<xs:complexType name="stresstestparshifts">
  <xs:all>
    <xs:element type="bool" name="IRCurves" minOccurs="0"/>
    <xs:element type="bool" name="CapFloorVolatilities" minOccurs="0"/>
    <xs:element type="bool" name="SurvivalProbability" minOccurs="0"/>
  </xs:all>
</xs:complexType>

<xs:complexType name="stresscapfloorvolatility">
  <xs:sequence>
    <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
    <xs:element name="Shifts">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="Shift" maxOccurs="unbounded" minOccurs="0">
            <xs:complexType>
              <xs:simpleContent>
                <xs:extension base="xs:string">
                  <xs:attribute type="xs:string" name="tenor"/>
                </xs:extension>
              </xs:simpleContent>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:element type="xs:string" name="ShiftExpiries"/>
    <xs:element type="xs:string" name="ShiftStrikes" minOccurs="0"/>
    <xs:element type="indexNameType" name="Index" minOccurs="0"/>
    <xs:element type="xs:boolean" name="IsRelative" minOccurs="0"/>
  </xs:sequence>
  <xs:attribute type="xs:string" name="key"/>
  <xs:attribute type="currencyCode" name="ccy"/> <!-- deprecated -->
</xs:complexType>

<xs:complexType name="stresscapfloorvolatilities">
  <xs:sequence>
    <xs:element type="stresscapfloorvolatility" name="CapFloorVolatility" maxOccurs="unbounded" minOccurs="0"/>
  </xs:sequence>
</xs:complexType>

  <xs:complexType name="stresstest">
    <xs:all>
      <xs:element type="xs:string" name="Date" minOccurs="0"/>
      <xs:element type="stresstestparshifts" name="ParShifts" minOccurs="0"/>
      <xs:element type="stressdiscountcurves" name="DiscountCurves" minOccurs="0"/>
      <xs:element type="stressindexcurves" name="IndexCurves" minOccurs="0"/>
      <xs:element type="stressyieldcurves" name="YieldCurves" minOccurs="0"/>
      <xs:element type="fxspots" name="FxSpots" minOccurs="0"/>
      <xs:element type="stressfxvolatilities" name="FxVolatilities" minOccurs="0"/>
      <xs:element type="stressswaptionvolatilities" name="SwaptionVolatilities" minOccurs="0"/>
      <xs:element type="stresscapfloorvolatilities" name="CapFloorVolatilities" minOccurs="0"/>
      <xs:element type="equityspots" name="EquitySpots" minOccurs="0"/>
      <xs:element type="equityvolatilities" name="EquityVolatilities" minOccurs="0"/>
      <xs:element type="stresscommoditycurves" name="CommodityCurves" minOccurs="0"/>
      <xs:element type="stressintradaypowercurves" name="IntradayPowerCurves" minOccurs="0"/>
      <xs:element type="stresscommodityvolatilities" name="CommodityVolatilities" minOccurs="0"/>
      <xs:element type="securityspreads" name="SecuritySpreads" minOccurs="0"/>
      <xs:element type="recoveryrates" name="RecoveryRates" minOccurs="0"/>
      <xs:element type="survivalprobabilities" name="SurvivalProbabilities" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:string" name="id" use="required"/>
  </xs:complexType>

  <xs:complexType name="stressfxvolatilities">
    <xs:sequence>
      <xs:element type="stressfxvolatility" name="FxVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stressfxvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:choice>
        <xs:sequence>
          <xs:element type="xs:string" name="Shifts"/>
          <xs:element type="xs:string" name="ShiftExpiries"/>
        </xs:sequence>

        <xs:sequence>
          <xs:element name="WeightedShifts">
            <xs:complexType>
              <xs:all>
                <xs:element type="xs:string" name="WeightingSchema"/>
                <xs:element type="xs:string" name="Shift"/>
                <xs:element type="xs:string" name="Tenor"/>
                <xs:element type="xs:string" name="ShiftWeights" minOccurs="0"/>
                <xs:element type="xs:string" name="WeightTenors" minOccurs="0"/>
              </xs:all>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:choice>
    </xs:sequence>
    <xs:attribute type="currencyPair" name="ccypair" use="required"/>
  </xs:complexType>

  <xs:complexType name="stressdiscountcurves">
    <xs:sequence>
      <xs:element type="stressdiscountcurve" name="DiscountCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
	
  <xs:complexType name="stressdiscountcurve">
	  <xs:sequence>
		  <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
		  <xs:element type="xs:boolean" name="ShiftingZeros" minOccurs="0"/>
		  <xs:choice>
			  <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
			  <xs:element type="xs:string" name="Shifts"/>
		  </xs:choice>
		  <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
		  <xs:element type="xs:string" name="ShiftTenors"/>
		  <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
	  </xs:sequence>
	  <xs:attribute type="currencyCode" name="ccy" use="required"/>
  </xs:complexType>

  <xs:complexType name="stressindexcurves">
    <xs:sequence>
      <xs:element type="stressindexcurve" name="IndexCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
	
  <xs:complexType name="stressindexcurve">
	  <xs:sequence>
		  <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
		  <xs:element type="xs:boolean" name="ShiftingZeros" minOccurs="0"/>
		  <xs:choice>
			  <xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
			  <xs:element type="xs:string" name="Shifts"/>
		  </xs:choice>
		  <xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
		  <xs:element type="xs:string" name="ShiftTenors"/>
		  <xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
	  </xs:sequence>
	  <xs:attribute type="indexNameType" name="index" use="required"/>
  </xs:complexType>


	<xs:complexType name="stressyieldcurves">
		<xs:sequence>
			<xs:element type="stressyieldcurve" name="YieldCurve" maxOccurs="unbounded" minOccurs="0"/>
		</xs:sequence>
	</xs:complexType>

	<xs:complexType name="stressyieldcurve">
		<xs:sequence>
			<xs:element type="xs:string" name="CurveType" minOccurs="0"/>
			<xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
			<xs:element type="xs:boolean" name="ShiftingZeros" minOccurs="0"/>
			<xs:choice>
				<xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
				<xs:element type="xs:string" name="Shifts"/>
			</xs:choice>
			<xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
			<xs:element type="xs:string" name="ShiftTenors"/>
			<xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
		</xs:sequence>
		<xs:attribute type="xs:string" name="name" use="required"/>
	</xs:complexType>

	<xs:complexType name="stresssurvivalprobabilities">
		<xs:sequence>
			<xs:element type="stresssurvivalprobability" name="SurvivalProbability" maxOccurs="unbounded" minOccurs="0"/>
		</xs:sequence>
	</xs:complexType>

	<xs:complexType name="stresssurvivalprobability">
		<xs:sequence>
			<xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
			<xs:element type="xs:boolean" name="ShiftingZeros" minOccurs="0"/>
			<xs:choice>
				<xs:element type="shiftSizeEntry" name="ShiftSize" maxOccurs="unbounded"/>
				<xs:element type="xs:string" name="Shifts"/>
			</xs:choice>
			<xs:element type="shiftSchemeEntry" name="ShiftScheme" minOccurs="0" maxOccurs="unbounded"/>
			<xs:element type="xs:string" name="ShiftTenors"/>
			<xs:element type="parconversion" name="ParConversion" minOccurs="0"/>
		</xs:sequence>
		<xs:attribute type="xs:string" name="name" use="required"/>
	</xs:complexType>

  <xs:complexType name="stressswaptionvolatilities">
    <xs:sequence>
      <xs:element type="stressswaptionvolatility" name="SwaptionVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stressswaptionvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType"/>
      <xs:element name="Shifts">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Shift" maxOccurs="unbounded" minOccurs="0">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute type="xs:string" name="expiry"/>
                    <xs:attribute type="xs:string" name="term"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftTerms"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="key"/>
    <xs:attribute type="xs:string" name="ccy"/> <!-- deprecated -->
  </xs:complexType>

  <xs:complexType name="stresscommoditycurves">
    <xs:sequence>
      <xs:element type="stresscommoditycurve" name="CommodityCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stressintradaypowercurves">
    <xs:sequence>
      <xs:element type="stressintradaypowercurve" name="IntradayPowerCurve" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stresscommodityvolatilities">
    <xs:sequence>
      <xs:element type="stresscommodityvolatility" name="CommodityVolatility" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stresscommoditycurve">
    <xs:sequence>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="Shifts"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="commodity" use="required"/>
  </xs:complexType>

  <xs:complexType name="stressintradaypowercurve">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="Shifts"/>
      <xs:element type="xs:string" name="ShiftTenors"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>

  <xs:complexType name="stresscommodityvolatility">
    <xs:sequence>
      <xs:element type="shiftTypeEntry" name="ShiftType" maxOccurs="unbounded"/>
      <xs:element type="xs:string" name="Shifts"/>
      <xs:element type="xs:string" name="ShiftExpiries"/>
      <xs:element type="xs:string" name="ShiftMoneyness"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="commodity" use="required"/>
  </xs:complexType>

</xs:schema>
$xsd$, 'ec8faf7654b630f603021032163322d8f9a9051ed63c876124491cf2db3a899c', 11574),
        ('82362946-655e-5963-b973-66ddacd1ff6b'::uuid, 'calendar_adjustment_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="calendaradjustment" name="CalendarAdjustments"/>

  <xs:complexType name="calendaradjustment">
    <xs:sequence>
      <xs:element type="newcalendar" name="Calendar" maxOccurs="unbounded" minOccurs="0">
      </xs:element>
    </xs:sequence>
  </xs:complexType>  
  
  <xs:complexType name="newcalendar">
    <xs:all>
      <xs:element type="calendar" name="BaseCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="Dates" name="AdditionalHolidays" minOccurs="0"/>
      <xs:element type="Dates" name="AdditionalBusinessDays" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:string" name="name" use="required"/>
  </xs:complexType>
  
  <xs:complexType name="Dates">
    <xs:sequence>
     <xs:element type="date" name="Date" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType> 
  
</xs:schema>
$xsd$, '82a3e4ddef6eabf0040112ebb7e38c5e26fbcd5158dc27516302a2b3dd6105cb', 981),
        ('2642624a-6735-5bbf-8739-9d8fb67b1b4d'::uuid, 'curve_configuration_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:include schemaLocation="ore_types.xsd"/>
  <xs:element type="curveconfiguration" name="CurveConfiguration"/>
  <xs:complexType name="curveconfiguration">
    <xs:all>
      <xs:element type="globalReportConfiguration" name="ReportConfiguration" minOccurs="0"/>
      <xs:element type="fxSpots" name="FXSpots" minOccurs="0"/>
      <xs:element type="fxVolatilities" name="FXVolatilities" minOccurs="0"/>
      <xs:element type="swaptionVolatilities" name="SwaptionVolatilities" minOccurs="0"/>
      <xs:element type="yieldVolatilities" name="YieldVolatilities" minOccurs="0"/>
      <xs:element type="capFloorVolatilities" name="CapFloorVolatilities" minOccurs="0"/>
      <xs:element type="cdsVolatilities" name="CDSVolatilities" minOccurs="0"/>
      <xs:element type="defaultCurves" name="DefaultCurves" minOccurs="0"/>
      <xs:element type="yieldCurves" name="YieldCurves" minOccurs="0"/>
      <xs:element type="inflationCurves" name="InflationCurves" minOccurs="0"/>
      <xs:element type="inflationCapFloorVolatlities" name="InflationCapFloorVolatilities" minOccurs="0"/>
      <xs:element type="equityCurves" name="EquityCurves" minOccurs="0"/>
      <xs:element type="equityVolatilities" name="EquityVolatilities" minOccurs="0"/>
      <xs:element type="securities" name="Securities" minOccurs="0"/>
      <xs:element type="baseCorrelations" name="BaseCorrelations" minOccurs="0"/>
      <xs:element type="simCommodityCurves" name="CommodityCurves" minOccurs="0"/>
      <xs:element type="commodityVolatilities" name="CommodityVolatilities" minOccurs="0"/>
      <xs:element type="correlations" name="Correlations" minOccurs="0"/>
      <xs:element type="bondFutureVolatilities" name="BondFutureVolatilities" minOccurs="0"/>
      <xs:element type="intradayPowerCurves" name="IntradayPowerCurves" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <!-- Volatility configurations -->
  <xs:complexType name="constantVolatilityConfig">
    <xs:all>
      <xs:element type="xs:string" name="QuoteType" minOccurs="0"/>
      <xs:element type="xs:string" name="VolatilityType" minOccurs="0"/>
      <xs:element type="xs:string" name="ExerciseType" minOccurs="0"/>
      <xs:element type="xs:string" name="Quote"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
    </xs:all>
    <xs:attribute type="xs:nonNegativeInteger" name="priority"/>
  </xs:complexType>
  <xs:complexType name="volatilityCurveConfig">
    <xs:all>
      <xs:element type="xs:string" name="QuoteType" minOccurs="0"/>
      <xs:element type="xs:string" name="VolatilityType" minOccurs="0"/>
      <xs:element type="xs:string" name="ExerciseType" minOccurs="0"/>
      <xs:element type="quoteType" name="Quotes"/>
      <xs:element type="interpolationMethodType" name="Interpolation"/>
      <xs:element type="extrapolationType" name="Extrapolation"/>
      <xs:element type="xs:boolean" name="EnforceMontoneVariance" minOccurs="0"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
    </xs:all>
    <xs:attribute type="xs:nonNegativeInteger" name="priority"/>
  </xs:complexType>
  <xs:complexType name="volatilityStrikeSurfaceConfig">
    <xs:all>
      <xs:element type="xs:string" name="QuoteType" minOccurs="0"/>
      <xs:element type="xs:string" name="VolatilityType" minOccurs="0"/>
      <xs:element type="xs:string" name="ExerciseType" minOccurs="0"/>
      <xs:element type="xs:string" name="Strikes"/>
      <xs:element type="xs:string" name="Expiries"/>
      <xs:element type="volatilityInterpolationType" name="TimeInterpolation"/>
      <xs:element type="volatilityInterpolationType" name="StrikeInterpolation"/>
      <xs:element type="bool" name="Extrapolation"/>
      <xs:element type="extrapolationType" name="TimeExtrapolation"/>
      <xs:element type="bool" name="TimeExtrapolationVariance" minOccurs="0" maxOccurs="1"/>
      <xs:element type="extrapolationType" name="StrikeExtrapolation"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="parametricSmileConfig" name="ParametricSmileConfiguration" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:nonNegativeInteger" name="priority"/>
  </xs:complexType>
  <xs:complexType name="volatilityDeltaSurfaceConfig">
    <xs:all>
      <xs:element type="xs:string" name="QuoteType" minOccurs="0"/>
      <xs:element type="xs:string" name="VolatilityType" minOccurs="0"/>
      <xs:element type="xs:string" name="ExerciseType" minOccurs="0"/>
      <xs:element type="strikeDeltaType" name="DeltaType"/>
      <xs:element type="strikeAtmType" name="AtmType"/>
      <xs:element type="strikeDeltaType" name="AtmDeltaType" minOccurs="0"/>
      <xs:element type="xs:string" name="PutDeltas"/>
      <xs:element type="xs:string" name="CallDeltas"/>
      <xs:element type="xs:string" name="Expiries"/>
      <xs:element type="volatilityInterpolationType" name="TimeInterpolation"/>
      <xs:element type="volatilityInterpolationType" name="StrikeInterpolation"/>
      <xs:element type="bool" name="Extrapolation"/>
      <xs:element type="extrapolationType" name="TimeExtrapolation"/>
      <xs:element type="bool" name="TimeExtrapolationVariance" minOccurs="0" maxOccurs="1"/>
      <xs:element type="extrapolationType" name="StrikeExtrapolation"/>
      <xs:element type="bool" name="FuturePriceCorrection" minOccurs="0"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="parametricSmileConfig" name="ParametricSmileConfiguration" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:nonNegativeInteger" name="priority"/>
  </xs:complexType>
  <xs:complexType name="volatilityMoneynessSurfaceConfig">
    <xs:all>
      <xs:element type="xs:string" name="QuoteType" minOccurs="0"/>
      <xs:element type="xs:string" name="VolatilityType" minOccurs="0"/>
      <xs:element type="xs:string" name="ExerciseType" minOccurs="0"/>
      <xs:element type="strikeMoneynessType" name="MoneynessType"/>
      <xs:element type="xs:string" name="MoneynessLevels"/>
      <xs:element type="xs:string" name="Expiries"/>
      <xs:element type="volatilityInterpolationType" name="TimeInterpolation"/>
      <xs:element type="volatilityInterpolationType" name="StrikeInterpolation"/>
      <xs:element type="bool" name="Extrapolation"/>
      <xs:element type="extrapolationType" name="TimeExtrapolation"/>
      <xs:element type="bool" name="TimeExtrapolationVariance" minOccurs="0" maxOccurs="1"/>
      <xs:element type="extrapolationType" name="StrikeExtrapolation"/>
      <xs:element type="bool" name="FuturePriceCorrection" minOccurs="0"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="parametricSmileConfig" name="ParametricSmileConfiguration" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:nonNegativeInteger" name="priority"/>
  </xs:complexType>
  <xs:complexType name="proxySurface">
    <xs:all>
      <xs:element type="xs:string" name="ProxyVolatilityCurve"/>
      <xs:element type="xs:string" name="FXVolatilityCurve" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="CorrelationCurve" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="CDSVolatilityCurve" minOccurs="0" maxOccurs="1"/>
    </xs:all>
    <xs:attribute type="xs:nonNegativeInteger" name="priority"/>
  </xs:complexType>
  <xs:complexType name="parametricSmileConfigParameter">
    <xs:all>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="xs:string" name="InitialValue" minOccurs="0" maxOccurs="1"/>
      <xs:element type="parametricVolatilityParameterCalibration" name="Calibration"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="parametricSmileConfigParameters">
    <xs:sequence>
      <xs:element type="parametricSmileConfigParameter" name="Parameter" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="parametricSmileConfigCalibration">
    <xs:all>
      <xs:element type="xs:integer" name="MaxCalibrationAttempts"/>
      <xs:element type="xs:float" name="ExitEarlyErrorThreshold"/>
      <xs:element type="xs:float" name="MaxAcceptableError"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="parametricSmileConfigResidualCorrection">
    <xs:all>
      <xs:element name="Dimension">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="AbsoluteStrike"/>
            <xs:enumeration value="StrikeMinusForward"/>
            <xs:enumeration value="StrikeOverForward"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="parametricSmileConfig">
    <xs:all>
      <xs:element type="parametricSmileConfigParameters" name="Parameters"/>
      <xs:element type="parametricSmileConfigCalibration" name="Calibration"/>
      <xs:element type="parametricSmileConfigResidualCorrection" name="ResidualCorrection" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="fxSpots">
    <xs:sequence>
      <xs:element type="fxSpot" name="FXSpot" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="fxSpot">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="fxVolatilities">
    <xs:sequence>
      <xs:element type="fxVolatility" name="FXVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="fxVolatility">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="dimensionType" name="Dimension"/>
      <xs:element type="smileType" name="SmileType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="fxVolInterpolation" name="SmileInterpolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="parametricSmileConfig" name="ParametricSmileConfiguration" minOccurs="0"/>
      <xs:element type="xs:string" name="Deltas" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="SmileDelta" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Expiries" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FXSpotID" minOccurs="0"/>
      <xs:element type="xs:string" name="FXForeignCurveID" minOccurs="0"/>
      <xs:element type="xs:string" name="FXDomesticCurveID" minOccurs="0"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FXIndexTag" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="BaseVolatility1" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="BaseVolatility2" minOccurs="0" maxOccurs="1"/>
      <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
      <xs:element type="extrapolationType" name="SmileExtrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="TimeInterpolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="TimeWeighting" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="ButterflyErrorTolerance" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="equityVolatilities">
    <xs:sequence>
      <xs:element type="equityVolatility" name="EquityVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="equityVolatility">
    <xs:sequence>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="xs:string" name="EquityId" minOccurs="0" maxOccurs="1"/>
      <xs:element type="extendedCurrencyCode" name="Currency"/>
      <xs:element type="dimensionType" name="Dimension" minOccurs="0"/>
      <xs:element type="xs:string" name="Expiries" minOccurs="0"/>
      <xs:element type="xs:string" name="Strikes" minOccurs="0"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="extrapolationType" name="TimeExtrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="extrapolationType" name="StrikeExtrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:choice>
        <xs:element type="volatilityConfig" name="VolatilityConfig" minOccurs="0" maxOccurs="1"/>
        <xs:choice minOccurs="0">
          <xs:element type="constantVolatilityConfig" name="Constant"/>
          <xs:element type="volatilityCurveConfig" name="Curve"/>
          <xs:element type="volatilityStrikeSurfaceConfig" name="StrikeSurface"/>
          <xs:element type="volatilityMoneynessSurfaceConfig" name="MoneynessSurface"/>
          <xs:element type="volatilityDeltaSurfaceConfig" name="DeltaSurface"/>
          <xs:element type="proxySurface" name="ProxySurface"/>
        </xs:choice>
      </xs:choice>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="oneDimSolverConfigType" name="OneDimSolverConfig" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="PreferOutOfTheMoney" minOccurs="0" maxOccurs="1"/>
      <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="volatilityConfig">
    <xs:all>
      <xs:element type="constantVolatilityConfig" name="Constant" minOccurs="0"/>
      <xs:element type="volatilityCurveConfig" name="Curve" minOccurs="0"/>
      <xs:element type="volatilityStrikeSurfaceConfig" name="StrikeSurface" minOccurs="0"/>
      <xs:element type="volatilityMoneynessSurfaceConfig" name="MoneynessSurface" minOccurs="0"/>
      <xs:element type="volatilityDeltaSurfaceConfig" name="DeltaSurface" minOccurs="0"/>
      <xs:element type="volatilityApoFutureSurfaceConfig" name="ApoFutureSurface" minOccurs="0"/>
      <xs:element type="proxySurface" name="ProxySurface" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="swaptionVolatilities">
    <xs:sequence>
      <xs:element type="swaptionVolatility" name="SwaptionVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="swaptionVolatility">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element name="ProxyConfig" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element name="Source">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="CurveId"/>
                  <xs:element type="xs:string" name="ShortSwapIndexBase"/>
                  <xs:element type="xs:string" name="SwapIndexBase"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
            <xs:element name="Target">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="ShortSwapIndexBase"/>
                  <xs:element type="xs:string" name="SwapIndexBase"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element type="dimensionType" name="Dimension" minOccurs="0"/>
      <xs:element type="volatilityType" name="VolatilityType" minOccurs="0"/>
      <xs:element type="xs:string" name="Interpolation" minOccurs="0"/>
      <xs:element type="parametricSmileConfig" name="ParametricSmileConfiguration" minOccurs="0"/>
      <xs:element type="xs:string" name="Extrapolation" minOccurs="0"/>
      <xs:element type="xs:string" name="OutputVolatilityType" minOccurs="0"/>
      <xs:element type="xs:string" name="ModelShift" minOccurs="0"/>
      <xs:element type="xs:string" name="OutputShift" minOccurs="0"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="0"/>
      <xs:element type="xs:string" name="OptionTenors" minOccurs="0"/>
      <xs:element type="xs:string" name="SwapTenors" minOccurs="0"/>
      <xs:element type="xs:string" name="ShortSwapIndexBase" minOccurs="0"/>
      <xs:element type="xs:string" name="SwapIndexBase" minOccurs="0"/>
      <xs:element type="xs:string" name="SmileOptionTenors" minOccurs="0"/>
      <xs:element type="xs:string" name="SmileSwapTenors" minOccurs="0"/>
      <xs:element type="xs:string" name="SmileSpreads" minOccurs="0"/>
      <xs:element type="xs:string" name="QuoteTag" minOccurs="0"/>
      <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="yieldVolatilities">
    <xs:sequence>
      <xs:element type="yieldVolatility" name="YieldVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="yieldVolatility">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="xs:string" name="Qualifier"/>
      <xs:element type="dimensionType" name="Dimension" minOccurs="0"/>
      <xs:element type="volatilityType" name="VolatilityType"/>
      <xs:element type="extrapolationType" name="Extrapolation"/>
      <xs:element type="dayCounter" name="DayCounter"/>
      <xs:element type="calendar" name="Calendar"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention"/>
      <xs:element type="xs:string" name="OptionTenors"/>
      <xs:element type="xs:string" name="BondTenors"/>
      <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="indexFactorsType">
    <xs:sequence>
      <xs:element type="non-negative-decimal" name="IndexFactor"/>
      <xs:element type="non-negative-decimal" name="IndexFactorStrike"/>
      <xs:element type="non-negative-decimal" name="RealisedFep"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="priceInfoType">
    <xs:sequence>
      <xs:element type="xs:string" name="CdsConventions"/>
      <xs:element type="positiveDecimal" name="RunningCoupon"/>
      <xs:element type="indexFactorsType" name="IndexFactors" minOccurs="0"/>
      <xs:element name="EngineOverride" minOccurs="0">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="NumericalIntegrationEngine"/>
            <xs:enumeration value="BlackIndexCdsOptionEngine"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element name="QuoteDimension" minOccurs="0">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="BpsPerOptionNtl"/>
            <xs:enumeration value="BpsPerOutstandingNtl"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element type="oneDimSolverConfigType" name="OneDimSolverConfig" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="cdsVolatilities">
    <xs:sequence>
      <xs:element type="cdsVolatility" name="CDSVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="cdsVolatility">
    <xs:sequence>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element name="Terms" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Term" maxOccurs="unbounded">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="Label"/>
                  <xs:element type="xs:string" name="Curve"/>
                  <xs:element type="xs:string" name="Maturity" minOccurs="0"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:choice>
        <xs:element type="xs:string" name="Expiries"/>
        <xs:element type="constantVolatilityConfig" name="Constant"/>
        <xs:element type="volatilityCurveConfig" name="Curve"/>
        <xs:element type="volatilityStrikeSurfaceConfig" name="StrikeSurface"/>
        <xs:element type="proxySurface" name="ProxySurface"/>
      </xs:choice>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="StrikeType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="QuoteName" minOccurs="0" maxOccurs="1"/>
      <xs:element type="positiveDecimal" name="StrikeFactor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="priceInfoType" name="PriceInfo" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="capFloorVolatilities">
    <xs:sequence>
      <xs:element type="capFloorVolatility" name="CapFloorVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="capFloorVolatility">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element name="ProxyConfig" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element name="Source">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="CurveId"/>
                  <xs:element type="xs:string" name="Index"/>
                  <xs:element type="xs:string" name="RateComputationPeriod" minOccurs="0"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
            <xs:element name="Target">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="Index"/>
                  <xs:element type="xs:string" name="RateComputationPeriod" minOccurs="0"/>
                  <xs:element type="xs:integer" name="ONCapSettlementDays" minOccurs="0"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:double" name="ScalingFactor" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element type="volatilityType" name="VolatilityType" minOccurs="0"/>
      <xs:element type="volatilityType" name="OutputVolatilityType" minOccurs="0"/>
      <xs:element type="xs:float" name="ModelShift" minOccurs="0"/>
      <xs:element type="xs:float" name="OutputShift" minOccurs="0"/>
      <xs:element type="extrapolationType" name="Extrapolation" minOccurs="0"/>
      <xs:element name="InterpolationMethod" minOccurs="0">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="Bilinear"/>
            <xs:enumeration value="BicubicSpline"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element type="bool" name="IncludeAtm" minOccurs="0"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="0"/>
      <xs:element type="xs:string" name="Tenors" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Strikes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="OptionalQuotes" minOccurs="0" maxOccurs="1"/>
      <!-- deprecated -->
      <xs:element type="indexNameType" name="IborIndex" minOccurs="0" maxOccurs="1"/>
      <xs:element type="indexNameType" name="Index" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="RateComputationPeriod" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="ONCapSettlementDays" minOccurs="0"/>
      <xs:element type="xs:string" name="DiscountCurve" minOccurs="0"/>
      <xs:element type="xs:string" name="AtmTenors" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="SettlementDays" minOccurs="0" maxOccurs="1"/>
      <xs:element name="InterpolateOn" minOccurs="0" maxOccurs="1">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="TermVolatilities"/>
            <xs:enumeration value="OptionletVolatilities"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element name="TimeInterpolation" minOccurs="0" maxOccurs="1">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="Linear"/>
            <xs:enumeration value="LinearFlat"/>
            <xs:enumeration value="BackwardFlat"/>
            <xs:enumeration value="Cubic"/>
            <xs:enumeration value="CubicFlat"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element name="StrikeInterpolation" minOccurs="0" maxOccurs="1">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="Linear"/>
            <xs:enumeration value="LinearFlat"/>
            <xs:enumeration value="Cubic"/>
            <xs:enumeration value="Hagan2002Lognormal"/>
            <xs:enumeration value="Hagan2002Normal"/>
            <xs:enumeration value="Hagan2002NormalZeroBeta"/>
            <xs:enumeration value="Antonov2015FreeBoundaryNormal"/>
            <xs:enumeration value="KienitzLawsonSwaynePde"/>
            <xs:enumeration value="FlochKennedy"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element type="parametricSmileConfig" name="ParametricSmileConfiguration" minOccurs="0"/>
      <xs:element name="InputType" minOccurs="0" maxOccurs="1">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="TermVolatilities"/>
            <xs:enumeration value="OptionletVolatilities"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element type="bool" name="QuoteIncludesIndexName" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="FlatFirstPeriod" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="UseEffeciveVolatility" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bootstrapConfigType" name="BootstrapConfig" minOccurs="0" maxOccurs="1"/>
      <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
      <xs:element type="bool" name="UseEffectiveVolatility" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="equityCurves">
    <xs:sequence>
      <xs:element type="equityCurve" name="EquityCurve" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="equityCurve">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="extendedCurrencyCode" name="Currency"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="ForecastingCurve"/>
      <xs:element type="equityType" name="Type"/>
      <xs:element type="exerciseStyle" name="ExerciseStyle" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="SpotQuote"/>
      <xs:element type="quoteType" name="Quotes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dividendInterpolation" name="DividendInterpolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="DividendExtrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="Extrapolation" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="defaultCurves">
    <xs:sequence>
      <xs:element type="defaultCurve" name="DefaultCurve" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="defaultCurve">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element name="Configurations" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Configuration" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:all>
                  <xs:element type="defaultCurveType" name="Type"/>
                  <xs:element type="xs:string" name="DiscountCurve" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="dayCounter" name="DayCounter"/>
                  <xs:element type="xs:string" name="RecoveryRate" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="date" name="StartDate" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="quoteType" name="Quotes" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="xs:string" name="BenchmarkCurve" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="xs:string" name="ReinterpretedYieldCurve" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="xs:string" name="SourceCurve" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="xs:string" name="Pillars" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="xs:integer" name="SpotLag" minOccurs="0" maxOccurs="1"/>
                  <xs:element name="SourceCurves" minOccurs="0">
                    <xs:complexType>
                      <xs:sequence>
                        <xs:element type="xs:string" name="SourceCurve" minOccurs="0" maxOccurs="unbounded"/>
                      </xs:sequence>
                    </xs:complexType>
                  </xs:element>
                  <xs:element name="SwitchDates" minOccurs="0">
                    <xs:complexType>
                      <xs:sequence>
                        <xs:element type="xs:string" name="SwitchDate" minOccurs="0" maxOccurs="unbounded"/>
                      </xs:sequence>
                    </xs:complexType>
                  </xs:element>
                  <xs:element type="xs:string" name="InitialState" minOccurs="0"/>
                  <xs:element type="xs:string" name="States" minOccurs="0"/>
                  <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="bool" name="Extrapolation" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="xs:decimal" name="RunningSpread" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="xs:string" name="IndexTerm" minOccurs="0"/>
                  <xs:element type="bool" name="ImplyDefaultFromMarket" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="bootstrapConfigType" name="BootstrapConfig" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="bool" name="AllowNegativeRates" minOccurs="0" maxOccurs="1"/>
                  <xs:element type="bool" name="PriceIsUpfront" minOccurs="0" maxOccurs="1"/>
                </xs:all>
                <xs:attribute type="xs:nonNegativeInteger" name="priority"/>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="defaultCurveType" name="Type" minOccurs="0"/>
      <xs:element type="xs:string" name="DiscountCurve" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0"/>
      <xs:element type="xs:string" name="RecoveryRate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="date" name="StartDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="quoteType" name="Quotes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="BenchmarkCurve" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="SourceCurve" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Pillars" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpotLag" minOccurs="0" maxOccurs="1"/>
      <xs:element name="SourceCurves" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="SourceCurve" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="SwitchDates" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="SwitchDate" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="Extrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:decimal" name="RunningSpread" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="IndexTerm" minOccurs="0"/>
      <xs:element type="bool" name="ImplyDefaultFromMarket" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bootstrapConfigType" name="BootstrapConfig" minOccurs="0" maxOccurs="1"/>
      <!-- AllowNegativeRates removed by JW, but restored here -->
      <xs:element type="bool" name="AllowNegativeRates" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="InitialState" minOccurs="0"/>
      <xs:element type="xs:string" name="States" minOccurs="0"/>
      <xs:element type="bool" name="PriceIsUpfront" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:simpleType name="defaultCurveType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="SpreadCDS"/>
      <xs:enumeration value="ConvSpreadCDS"/>
      <xs:enumeration value="HazardRate"/>
      <xs:enumeration value="Benchmark"/>
      <xs:enumeration value="Price"/>
      <xs:enumeration value="TransitionMatrix"/>
      <xs:enumeration value="YieldCurve"/>
      <xs:enumeration value="Null"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="inflationCurves">
    <xs:sequence>
      <xs:element type="inflationCurve" name="InflationCurve" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="inflationCurve">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="xs:string" name="NominalTermStructure" minOccurs="1" maxOccurs="1"/>
      <xs:element type="inflationType" name="Type" minOccurs="1" maxOccurs="1"/>
      <!-- Legacy block: Quotes/Conventions OR new Segments block -->
      <xs:element type="quoteType" name="Quotes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
      <!-- New Segments block use instead of Quotes/Conventions-->
      <xs:element type="inflSegmentsType" name="Segments" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="Extrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="calendar" name="Calendar"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Lag" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="Frequency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="BaseRate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="Tolerance" minOccurs="0" maxOccurs="1"/>
      <xs:element type="seasonalityType" name="Seasonality" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="UseLastFixingDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="InterpolationVariable" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="InterpolationMethod" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="inflSegmentsType">
    <xs:sequence>
      <xs:element type="inlfSegmentType" name="Segment" minOccurs="1" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="inlfSegmentType">
    <xs:all>
      <xs:element type="xs:string" name="Conventions" minOccurs="1" maxOccurs="1"/>
      <xs:element type="quoteType" name="Quotes" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="seasonalityType">
    <xs:all>
      <xs:element type="date" name="BaseDate" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="Frequency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="factorType" name="Factors"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="inflationCapFloorVolatlities">
    <xs:sequence>
      <xs:element type="inflationCapFloorVolatility" name="InflationCapFloorVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="inflationCapFloorVolatility">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="inflationType" name="Type" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="QuoteType"/>
      <xs:element type="volatilityType" name="VolatilityType"/>
      <xs:element type="bool" name="Extrapolation"/>
      <xs:element type="xs:string" name="Tenors" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="SettlementDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="CapStrikes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FloorStrikes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Strikes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="calendar" name="Calendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="IndexCurve" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="IndexInterpolated" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="ObservationLag" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="YieldTermStructure" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="QuoteIndex" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
      <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
      <xs:element type="bootstrapConfigType" name="BootstrapConfig" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="yieldCurves">
    <xs:sequence>
      <xs:element type="yieldCurve" name="YieldCurve" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="yieldCurve">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:string" name="DiscountCurve"/>
      <xs:element type="segmentsType" name="Segments"/>
      <xs:element type="interpolationVariableType" name="InterpolationVariable" minOccurs="0" maxOccurs="1"/>
      <xs:element type="interpolationMethodType" name="InterpolationMethod" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="MixedInterpolationCutoff" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayCounter" name="YieldCurveDayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="Tolerance" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="Extrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:element name="ExtrapolationMethod" minOccurs="0" maxOccurs="1">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="ContinuousForward"/>
            <xs:enumeration value="DiscreteForward"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element type="bool" name="ExcludeT0FromInterpolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bootstrapConfigType" name="BootstrapConfig" minOccurs="0" maxOccurs="1"/>
      <xs:element type="yieldCurveReport" name="Report" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="securities">
    <xs:sequence>
      <xs:element type="security" name="Security" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="security">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="xs:string" name="SpreadQuote" minOccurs="0"/>
      <xs:element type="xs:string" name="RecoveryRateQuote" minOccurs="0"/>
      <xs:element type="xs:string" name="CPRQuote" minOccurs="0"/>
      <xs:element type="xs:string" name="PriceQuote" minOccurs="0"/>
      <xs:element type="xs:string" name="ConversionFactor" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="baseCorrelations">
    <xs:sequence>
      <xs:element type="baseCorrelation" name="BaseCorrelation" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="baseCorrelation">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="xs:string" name="Terms"/>
      <xs:element type="xs:string" name="DetachmentPoints"/>
      <xs:element type="xs:float" name="SettlementDays"/>
      <xs:element type="calendar" name="Calendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="Extrapolate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="QuoteName" minOccurs="0" maxOccurs="1"/>
      <xs:element type="date" name="StartDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dateRule" name="Rule" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="AdjustForLosses" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="IndexTerm" minOccurs="0"/>
      <xs:element type="xs:string" name="IndexSpread" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Currency" minOccurs="0"/>
      <xs:element type="bool" name="CalibrateConstituentsToIndexSpread" minOccurs="0"/>
      <xs:element type="bool" name="UseAssumedRecovery" minOccurs="0"/>
      <xs:element name="RecoveryGrid" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Grid" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="seniority" type="xs:string" use="required"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="RecoveryProbabilities" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Probabilities" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="seniority" type="xs:string" use="required"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="QuoteTypes" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="QuoteType" type="xs:string" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="simCommodityCurves">
    <xs:sequence>
      <xs:element type="simCommodityCurve" name="CommodityCurve" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:simpleType name="commodityInterpolationType">
    <xs:restriction base="interpolationMethodType">
      <xs:enumeration value="Linear"/>
      <xs:enumeration value="LogLinear"/>
      <xs:enumeration value="Cubic"/>
      <xs:enumeration value="Hermite"/>
      <xs:enumeration value="LinearFlat"/>
      <xs:enumeration value="LogLinearFlat"/>
      <xs:enumeration value="CubicFlat"/>
      <xs:enumeration value="HermiteFlat"/>
      <xs:enumeration value="BackwardFlat"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="commodityBasisConfig">
    <xs:all>
      <xs:element type="xs:string" name="BasePriceCurve"/>
      <xs:element type="xs:string" name="BasePriceConventions"/>
      <xs:element type="quoteType" name="BasisQuotes"/>
      <xs:element type="xs:string" name="BasisConventions"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="commodityInterpolationType" name="InterpolationMethod" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="AddBasis" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="MonthOffset" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="AverageBase" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="PriceAsHistoricalFixing" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="offPeakDailyType">
    <xs:all>
      <xs:element type="quoteType" name="OffPeakQuotes" minOccurs="1" maxOccurs="1"/>
      <xs:element type="quoteType" name="PeakQuotes" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="priceSegmentType">
    <xs:all>
      <xs:element type="priceSegmentTypeType" name="Type" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="1" maxOccurs="1"/>
      <xs:element type="quoteType" name="Quotes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="PeakPriceCurveId" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="PeakPriceCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="offPeakDailyType" name="OffPeakDaily" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="priceSegmentsType">
    <xs:sequence>
      <xs:element type="priceSegmentType" name="PriceSegment" minOccurs="1" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="simCommodityCurve">
    <xs:sequence>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:choice>
        <xs:sequence>
          <xs:element type="xs:string" name="BasePriceCurve"/>
          <xs:element type="xs:string" name="BaseYieldCurve"/>
          <xs:element type="xs:string" name="YieldCurve"/>
        </xs:sequence>
        <xs:sequence>
          <xs:element type="xs:string" name="SpotQuote" minOccurs="0" maxOccurs="1"/>
          <xs:element type="quoteType" name="Quotes"/>
          <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
          <xs:element type="commodityInterpolationType" name="InterpolationMethod" minOccurs="0" maxOccurs="1"/>
          <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
        </xs:sequence>
        <xs:element type="commodityBasisConfig" name="BasisConfiguration"/>
        <xs:sequence>
          <xs:element type="priceSegmentsType" name="PriceSegments"/>
          <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
          <xs:element type="commodityInterpolationType" name="InterpolationMethod" minOccurs="0" maxOccurs="1"/>
        </xs:sequence>
      </xs:choice>
      <xs:element type="bool" name="Extrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bootstrapConfigType" name="BootstrapConfig" minOccurs="0" maxOccurs="1"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="commodityVolatilities">
    <xs:sequence>
      <xs:element type="commodityVolatility" name="CommodityVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="volatilityApoFutureSurfaceConfig">
    <xs:all>
      <xs:element type="xs:string" name="QuoteType" minOccurs="0"/>
      <xs:element type="xs:string" name="VolatilityType" minOccurs="0"/>
      <xs:element type="xs:string" name="MoneynessLevels"/>
      <xs:element type="xs:string" name="VolatilityId"/>
      <xs:element type="xs:string" name="PriceCurveId"/>
      <xs:element type="xs:string" name="FutureConventions"/>
      <xs:element type="interpolationMethodType" name="TimeInterpolation"/>
      <xs:element type="interpolationMethodType" name="StrikeInterpolation"/>
      <xs:element type="bool" name="Extrapolation"/>
      <xs:element type="extrapolationType" name="TimeExtrapolation"/>
      <xs:element type="bool" name="TimeExtrapolationVariance" minOccurs="0" maxOccurs="1"/>
      <xs:element type="extrapolationType" name="StrikeExtrapolation"/>
      <xs:element type="xs:string" name="MaxTenor" minOccurs="0"/>
      <xs:element type="non-negative-decimal" name="Beta" minOccurs="0"/>
    </xs:all>
    <xs:attribute type="xs:nonNegativeInteger" name="priority"/>
  </xs:complexType>
  <xs:simpleType name="commVolQuoteSuffix">
    <xs:restriction base="xs:string">
      <xs:enumeration value="C"/>
      <xs:enumeration value="P"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="commodityVolatility">
    <xs:sequence>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:string" name="InstrumentType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="CalendarSpreadOffset" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="CalendarSpreadUnderlyingName" minOccurs="0" maxOccurs="1"/>
      <xs:choice>
        <xs:element type="volatilityConfig" name="VolatilityConfig" minOccurs="0" maxOccurs="1"/>
        <xs:choice minOccurs="0">
          <xs:element type="constantVolatilityConfig" name="Constant"/>
          <xs:element type="volatilityCurveConfig" name="Curve"/>
          <xs:element type="volatilityStrikeSurfaceConfig" name="StrikeSurface"/>
          <xs:element type="volatilityMoneynessSurfaceConfig" name="MoneynessSurface"/>
          <xs:element type="volatilityDeltaSurfaceConfig" name="DeltaSurface"/>
          <xs:element type="volatilityApoFutureSurfaceConfig" name="ApoFutureSurface"/>
          <xs:element type="proxySurface" name="ProxySurface"/>
        </xs:choice>
      </xs:choice>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FutureConventions" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="OptionExpiryRollDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="PriceCurveId" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="YieldCurveId" minOccurs="0" maxOccurs="1"/>
      <xs:element type="commVolQuoteSuffix" name="QuoteSuffix" minOccurs="0" maxOccurs="1"/>
      <xs:element type="oneDimSolverConfigType" name="OneDimSolverConfig" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="PreferOutOfTheMoney" minOccurs="0" maxOccurs="1"/>
      <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="correlations">
    <xs:sequence>
      <xs:element type="correlation" name="Correlation" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="correlation">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="correlationType" name="CorrelationType"/>
      <xs:element type="xs:string" name="Index1" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index2" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="SwaptionVolatility" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="DiscountCurve" minOccurs="0" maxOccurs="1"/>
      <xs:element type="currencyCode" name="Currency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dimensionType" name="Dimension" minOccurs="0" maxOccurs="1"/>
      <xs:element type="correlationQuoteType" name="QuoteType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="Extrapolation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="OptionTenors" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondFutureVolatilities">
    <xs:sequence>
      <xs:element type="bondFutureVolatility" name="BondFutureVolatility" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="bondFutureVolatility">
    <xs:sequence>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="xs:string" name="ContractName"/>
      <xs:choice>
        <xs:element type="volatilityConfig" name="VolatilityConfig" minOccurs="0" maxOccurs="1"/>
        <xs:choice minOccurs="0">
          <xs:element type="volatilityStrikeSurfaceConfig" name="StrikeSurface"/>
        </xs:choice>
      </xs:choice>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="YieldCurveId" minOccurs="0" maxOccurs="1"/>
      <xs:element type="positiveDecimal" name="StrikeFactor" minOccurs="0" maxOccurs="1"/>
      <xs:element name="UseOnlyPutCall" minOccurs="0" maxOccurs="1">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="C"/>
            <xs:enumeration value="P"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element type="oneDimSolverConfigType" name="OneDimSolverConfig" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="PreferOutOfTheMoney" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="TreatAsEuropean" minOccurs="0" maxOccurs="1"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="intradayPowerCurves">
    <xs:sequence>
      <xs:element type="intradayPowerCurve" name="IntradayPowerCurve" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="intradayPowerCurve">
    <xs:all>
      <xs:element type="xs:string" name="CurveId"/>
      <xs:element type="xs:string" name="CurveDescription"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:string" name="DailyAveragePriceCurve"/>
      <xs:element type="xs:string" name="ShapeQuoteName"/>
      <xs:element type="xs:string" name="Convention"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="segmentsType">
    <xs:choice minOccurs="1" maxOccurs="unbounded">
      <xs:element type="directSegmentType" name="Direct"/>
      <xs:element type="simpleSegmentType" name="Simple"/>
      <xs:element type="aoisSegmentType" name="AverageOIS"/>
      <xs:element type="tenorBasisSegmentType" name="TenorBasis"/>
      <xs:element type="crossCurrencySegmentType" name="CrossCurrency"/>
      <xs:element type="zeroSpreadType" name="ZeroSpread"/>
      <xs:element type="discountRatioType" name="DiscountRatio"/>
      <xs:element type="fittedBondType" name="FittedBond"/>
      <!-- weightedAverageType and yieldPlusDefaultType removed by JW, but restored here -->
      <xs:element type="BondYieldShiftedType" name="BondYieldShifted"/>
      <xs:element type="weightedAverageType" name="WeightedAverage"/>
      <xs:element type="yieldPlusDefaultType" name="YieldPlusDefault"/>
      <xs:element type="iborFallbackType" name="IborFallback"/>
    </xs:choice>
  </xs:complexType>
  <xs:complexType name="directSegmentType">
    <xs:all>
      <xs:element type="directSegmentTypeType" name="Type"/>
      <xs:element type="quoteType" name="Quotes"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:simpleType name="directSegmentTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Zero"/>
      <xs:enumeration value="Discount"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="simpleSegmentType">
    <xs:all>
      <xs:element type="simpleSegmentTypeType" name="Type"/>
      <xs:element type="quoteType" name="Quotes"/>
      <xs:element type="xs:string" name="Conventions"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
      <xs:element type="xs:string" name="ProjectionCurve" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:simpleType name="simpleSegmentTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Deposit"/>
      <xs:enumeration value="FRA"/>
      <xs:enumeration value="Future"/>
      <xs:enumeration value="OIS"/>
      <xs:enumeration value="Swap"/>
      <xs:enumeration value="BMA Basis Swap"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="aoisSegmentType">
    <xs:all>
      <xs:element type="xs:string" name="Type" fixed="Average OIS"/>
      <xs:element type="compositeQuoteType" name="Quotes"/>
      <xs:element type="xs:string" name="Conventions"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
      <xs:element type="xs:string" name="ProjectionCurve" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="tenorBasisSegmentType">
    <xs:all>
      <xs:element type="tenorBasisSegmentTypeType" name="Type"/>
      <xs:element type="quoteType" name="Quotes"/>
      <xs:element type="xs:string" name="Conventions"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
      <xs:element type="xs:string" name="ProjectionCurvePay" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="ProjectionCurveReceive" minOccurs="0" maxOccurs="1"/>
      <!-- deprecated fields -->
      <xs:element type="xs:string" name="ProjectionCurveLong" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="ProjectionCurveShort" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:simpleType name="tenorBasisSegmentTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Tenor Basis Swap"/>
      <xs:enumeration value="Tenor Basis Two Swaps"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="crossCurrencySegmentType">
    <xs:all>
      <xs:element type="crossCurrencySegmentTypeType" name="Type"/>
      <xs:element type="quoteType" name="Quotes"/>
      <xs:element type="xs:string" name="Conventions"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
      <xs:element type="xs:string" name="DiscountCurve"/>
      <xs:element type="xs:string" name="SpotRate"/>
      <xs:element type="xs:string" name="ProjectionCurveDomestic" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="ProjectionCurveForeign" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:simpleType name="crossCurrencySegmentTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Cross Currency Basis Swap"/>
      <xs:enumeration value="Cross Currency Fix Float Swap"/>
      <xs:enumeration value="FX Forward"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="zeroSpreadType">
    <xs:all>
      <xs:element type="zeroSpreadSegmentTypeType" name="Type"/>
      <xs:element type="quoteType" name="Quotes"/>
      <xs:element type="xs:string" name="Conventions"/>
      <xs:element type="xs:string" name="ReferenceCurve"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:simpleType name="zeroSpreadSegmentTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Zero Spread"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="discountRatioType">
    <xs:all>
      <xs:element type="discountRatioTypeType" name="Type"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="0" maxOccurs="1"/>
      <xs:element type="discountRatioCurveElement" name="BaseCurve"/>
      <xs:element type="discountRatioCurveElement" name="NumeratorCurve"/>
      <xs:element type="discountRatioCurveElement" name="DenominatorCurve"/>
    </xs:all>
  </xs:complexType>
  <xs:simpleType name="discountRatioTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Discount Ratio"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:complexType name="discountRatioCurveElement">
    <xs:simpleContent>
      <xs:extension base="xs:string">
        <xs:attribute name="currency" type="xs:string" use="required"/>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>
  <xs:complexType name="fittedBondType">
    <xs:all>
      <xs:element type="xs:string" name="Type" fixed="FittedBond"/>
      <xs:element type="quoteType" name="Quotes"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
      <xs:element name="IndexCurves" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="IndexCurve" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="Index" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <!-- IborIndexCurves is deprecated, kept for backwards compatibility; use IndexCurves instead -->
      <xs:element name="IborIndexCurves" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="IborIndexCurve" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="iborIndex" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="InflationIndexCurves" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="InflationIndexCurve" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="inflationIndex" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:boolean" name="ExtrapolateFlat" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="BondYieldShiftedType">
    <xs:all>
      <xs:element type="xs:string" name="Type" fixed="Bond Yield Shifted"/>
      <xs:element type="xs:string" name="ReferenceCurve"/>
      <xs:element type="quoteType" name="Quotes"/>
      <xs:element name="IndexCurves" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="IndexCurve" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="Index" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <!-- IborIndexCurves is deprecated, kept for backwards compatibility; use IndexCurves instead -->
      <xs:element name="IborIndexCurves" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="IborIndexCurve" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="iborIndex" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:string" name="Conventions"/>
      <xs:element type="xs:boolean" name="ExtrapolateFlat" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <!-- yieldPlusDefaultType removed by JW, but restored here -->
  <xs:complexType name="yieldPlusDefaultType">
    <xs:sequence>
      <xs:element type="xs:string" name="Type" fixed="Yield Plus Default"/>
      <xs:element type="xs:string" name="ReferenceCurve"/>
      <xs:element name="DefaultCurves">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="DefaultCurve"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Weights">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:float" name="Weight"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>
  <!-- weightedAverageType removed by JW, but restored here -->
  <xs:complexType name="weightedAverageType">
    <xs:sequence>
      <xs:element type="xs:string" name="Type" fixed="Weighted Average"/>
      <xs:element type="xs:string" name="ReferenceCurve1"/>
      <xs:element type="xs:string" name="ReferenceCurve2"/>
      <xs:element type="xs:float" name="Weight1"/>
      <xs:element type="xs:float" name="Weight2"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="iborFallbackType">
    <xs:all>
      <xs:element type="xs:string" name="Type" fixed="Ibor Fallback"/>
      <xs:element type="indexNameType" name="IborIndex"/>
      <xs:element type="xs:string" name="RfrCurve"/>
      <xs:element type="indexNameType" name="RfrIndex" minOccurs="0"/>
      <xs:element type="xs:float" name="Spread" minOccurs="0"/>
      <xs:element type="xs:string" name="PillarChoice" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="Priority" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="MinDistance" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="quoteType">
    <xs:sequence>
      <xs:element name="Quote" minOccurs="0" maxOccurs="unbounded">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute name="optional" type="xs:string"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="compositeQuoteType">
    <xs:sequence>
      <xs:element name="CompositeQuote" minOccurs="1" maxOccurs="unbounded">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="SpreadQuote"/>
            <xs:element type="xs:string" name="RateQuote"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="factorType">
    <xs:sequence>
      <xs:element type="xs:string" name="Factor" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="dividendInterpolation">
    <xs:all>
      <xs:element type="interpolationVariableType" name="InterpolationVariable" minOccurs="0" maxOccurs="1"/>
      <xs:element type="interpolationMethodType" name="InterpolationMethod" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="bootstrapConfigType">
    <xs:all>
      <xs:element type="xs:double" name="Accuracy" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="GlobalAccuracy" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:boolean" name="DontThrow" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:positiveInteger" name="MaxAttempts" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="MaxFactor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="MinFactor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:positiveInteger" name="DontThrowSteps" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:boolean" name="Global" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="SmoothnessLambda" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="minMaxType">
    <xs:all>
      <xs:element type="xs:double" name="Min"/>
      <xs:element type="xs:double" name="Max"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="oneDimSolverConfigType">
    <xs:sequence>
      <xs:element type="xs:positiveInteger" name="MaxEvaluations"/>
      <xs:element type="xs:double" name="InitialGuess"/>
      <xs:element type="xs:double" name="Accuracy"/>
      <xs:choice>
        <xs:element type="minMaxType" name="MinMax"/>
        <xs:element type="xs:double" name="Step"/>
      </xs:choice>
      <xs:element type="xs:double" name="LowerBound" minOccurs="0"/>
      <xs:element type="xs:double" name="UpperBound" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="globalReportConfiguration">
    <xs:all>
      <xs:element name="FXVolatilities" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="EquityVolatilities" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CommodityVolatilities" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="IRSwaptionVolatilities" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="IRCapFloorVolatilities" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="YieldCurves" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="yieldCurveReport" name="Report" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="InflationCapFloorVolatilities" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="DefaultCurves" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="reportConfiguration" name="Report" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
    </xs:element>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="reportConfiguration">
    <xs:all>
      <xs:element type="bool" name="ReportOnDeltaGrid" minOccurs="0"/>
      <xs:element type="bool" name="ReportOnMoneynessGrid" minOccurs="0"/>
      <xs:element type="bool" name="ReportOnStrikeGrid" minOccurs="0"/>
      <xs:element type="bool" name="ReportOnStrikeSpreadGrid" minOccurs="0"/>
      <xs:element type="xs:string" name="Deltas" minOccurs="0"/>
      <xs:element type="xs:string" name="Moneyness" minOccurs="0"/>
      <xs:element type="xs:string" name="Strikes" minOccurs="0"/>
      <xs:element type="xs:string" name="StrikeSpreads" minOccurs="0"/>
      <xs:element type="xs:string" name="Expiries" minOccurs="0"/>
      <xs:element type="xs:string" name="PillarDates" minOccurs="0"/>
      <xs:element type="xs:string" name="UnderlyingTenors" minOccurs="0"/>
      <xs:element type="xs:string" name="ContinuationExpiry" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="yieldCurveReport">
    <xs:all>
      <xs:element type="xs:string" name="PillarDates" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
</xs:schema>
$xsd$, 'b84d95feddc31b7986dffe349c1f06b85e29eaf99f6655b929d394b586acd1a9', 76389),
        ('66000d61-75da-524f-83c9-afa34c318a25'::uuid, 'reference_data_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element name="ReferenceData">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="ReferenceDatum" maxOccurs="unbounded" minOccurs="0">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:string" name="Type" minOccurs="1" maxOccurs="1" />
              <xs:group ref="referenceDataTypes"/>
            </xs:sequence>
            <xs:attribute type="xs:string" name="id" use="required"/>
            <xs:attribute type="xs:string" name="validFrom"/>
          </xs:complexType>
        </xs:element>
      </xs:sequence>
    </xs:complexType>
  </xs:element>

  <xs:group name="referenceDataTypes">
    <xs:choice>
      <xs:element type="currencyHedgedIndexReferenceDatum" name="CurrencyHedgedEquityIndexReferenceData"/>
      <xs:element type="indexReferenceDatum" name="EquityIndexReferenceData"/>
      <xs:element type="indexReferenceDatum" name="CommodityIndexReferenceData"/>
      <xs:element type="creditIndexReferenceDatum" name="CreditIndexReferenceData"/>
      <xs:element type="bondReferenceDatum" name="BondReferenceData"/>
      <xs:element type="cboReferenceDatum" name="CboReferenceData"/>
      <xs:element type="bondFutureReferenceDatum" name="BondFutureReferenceData"/>
      <xs:element type="convertibleBondReferenceDatum" name="ConvertibleBondReferenceData"/>
      <xs:element type="callableBondReferenceDatum" name="CallableBondReferenceData"/>
      <xs:element type="bondBasketData" name="BondBasketData"/>
      <xs:element type="referenceDatum" name="CreditReferenceData"/>
      <xs:element type="equityReferenceDatum" name="EquityReferenceData"/>
      <xs:element type="portfolioBasketReferenceDatum" name="PortfolioBasketReferenceData"/>
    </xs:choice>
  </xs:group>

  <xs:complexType name="indexReferenceDatum">
    <xs:sequence>
      <xs:element name="Underlying" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Name" minOccurs="1" maxOccurs="1"/>
            <xs:element type="xs:float" name="Weight" minOccurs="1" maxOccurs="1"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="equityReferenceDatum">
    <xs:sequence>
      <xs:element name="EquityId" minOccurs="0" maxOccurs="1"/>
      <xs:element name="EquityName" minOccurs="0" maxOccurs="1" />
      <xs:element name="Currency" minOccurs="1" maxOccurs="1" />
      <xs:element name="ScalingFactor"  minOccurs="0" maxOccurs="1"/>
      <xs:element name="ExchangeCode" minOccurs="1" maxOccurs="1"/>
      <xs:element name="IsIndex" minOccurs="1" maxOccurs="1"/>
      <xs:element name="EquityStartDate" minOccurs="1" maxOccurs="1"/>
      <xs:element name="ProxyIdentifier" minOccurs="1" maxOccurs="1"/>
      <xs:element name="SimmBucket" minOccurs="1" maxOccurs="1"/>
      <xs:element name="CrifQualifier" minOccurs="1" maxOccurs="1"/>
      <xs:element name="ProxyVolatilityId" minOccurs="1" maxOccurs="1"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="creditIndexReferenceDatum">
    <xs:sequence>
      <xs:element name="IndexFamily" type="xs:string" minOccurs="0" maxOccurs="1"/>
      <xs:element name="IndexSubFamily" type="xs:string" minOccurs="0" maxOccurs="1" />
      <xs:element name="IndexVersion" type="xs:positiveInteger" minOccurs="0" />
      <xs:element name="IndexFactor" minOccurs="0">
        <xs:simpleType>
          <xs:restriction base="xs:decimal">
            <xs:minInclusive value="0"/>
            <xs:maxInclusive value="1"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element name="Underlying" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Name" minOccurs="1" maxOccurs="1"/>
            <xs:element type="weightType" name="Weight" minOccurs="1" maxOccurs="1"/>
            <xs:element type="weightType" name="PriorWeight" minOccurs="0" maxOccurs="1"/>
            <xs:element type="recoveryRate" name="RecoveryRate" minOccurs="0" maxOccurs="1"/>
            <xs:element type="date" name="AuctionDate" minOccurs="0" maxOccurs="1"/>
            <xs:element type="date" name="AuctionSettlementDate" minOccurs="0" maxOccurs="1"/>
            <xs:element type="date" name="DefaultDate" minOccurs="0" maxOccurs="1"/>
            <xs:element type="date" name="EventDeterminationDate" minOccurs="0" maxOccurs="1"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="bondReferenceDatum">
    <xs:sequence>
      <xs:element type="xs:string" name="IssuerId"/>
      <xs:element type="xs:string" name="CreditCurveId" minOccurs="0"/>
      <xs:element type="xs:string" name="CreditGroup" minOccurs="0"/>
      <xs:element type="xs:string" name="ReferenceCurveId" minOccurs="0"/>
      <xs:element type="xs:string" name="IncomeCurveId" minOccurs="0"/>
      <xs:element type="xs:string" name="VolatilityCurveId" minOccurs="0"/>
      <xs:element type="xs:string" name="SettlementDays"/>
      <xs:element type="xs:string" name="Calendar"/>
      <xs:element type="xs:string" name="IssueDate"/>
      <xs:element type="xs:string" name="PriceQuoteMethod" minOccurs="0"/>
      <xs:element type="xs:string" name="PriceQuoteBaseValue" minOccurs="0"/>
      <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="xs:string" name="SubType" minOccurs="0"/>
      <xs:element type="bondPriceType" name="PriceType" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="cboReferenceDatum">
    <xs:all>
      <xs:element type="xs:string" name="DayCounter"/>
      <xs:element type="xs:string" name="PaymentConvention"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:string" name="ReinvestmentEndDate" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:float" name="SeniorFee"/>
      <xs:element type="xs:string" name="FeeDayCounter"/>
      <xs:element type="xs:float" name="SubordinatedFee"/>
      <xs:element type="xs:float" name="EquityKicker"/>
      <xs:element type="cboBondBasketData" name="BondBasketData" maxOccurs="1" minOccurs="1"/>
      <xs:element type="cbotranches" name="CBOTranches" maxOccurs="1" minOccurs="1"/>
      <xs:element type="scheduleData" name="ScheduleData"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondFutureReferenceDatum">
    <xs:all>
      <xs:element type="currencyCode" name="Currency" minOccurs="0"/>
      <xs:element type="deliveryBasket" name="DeliveryBasket" minOccurs="0"/>
      <xs:element type="xs:string" name="DeliverableGrade" minOccurs="0"/>
      <xs:element type="xs:string" name="LastTradingDate" minOccurs="0"/>
      <xs:element type="xs:string" name="LastDeliveryDate" minOccurs="0"/>
      <xs:element type="xs:string" name="Settlement" minOccurs="0"/>
      <xs:element type="xs:string" name="DirtyQuotation" minOccurs="0"/>
      <xs:element type="xs:string" name="ContractMonth" minOccurs="0"/>
      <xs:element type="xs:string" name="RootDate" minOccurs="0"/>
      <xs:element type="xs:string" name="ExpiryBasis" minOccurs="0"/>
      <xs:element type="xs:string" name="SettlementBasis" minOccurs="0"/>
      <xs:element type="xs:string" name="ExpiryLag" minOccurs="0"/>
      <xs:element type="xs:string" name="SettlementLag" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="convertibleBondReferenceDatum">
    <xs:all>
      <xs:element type="bondReferenceDatum" name="BondData"/>
      <xs:element type="cbCallData" name="CallData" minOccurs="0"/>
      <xs:element type="cbCallData" name="PutData" minOccurs="0"/>
      <xs:element type="cbConversionData" name="ConversionData"/>
      <xs:element type="cbDividendProtectionData" name="DividendProtectionData" minOccurs="0"/>
      <xs:element type="bool" name="Detachable" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="callableBondReferenceDatum">
    <xs:all>
      <xs:element type="bondReferenceDatum" name="BondData"/>
      <xs:element type="callableBondCallData" name="CallData" minOccurs="0"/>
      <xs:element type="callableBondCallData" name="PutData" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="referenceDatum">
    <xs:sequence>
      <xs:any processContents="lax" minOccurs="0" maxOccurs="unbounded" />
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="currencyHedgedIndexReferenceDatum">
    <xs:sequence>
    <xs:element type="xs:string" name="UnderlyingIndex" minOccurs="1"/>
    <xs:element type="currencyCode" name="HedgeCurrency" minOccurs="1"/>
    <xs:element type="CurrencyHedgedIndexRebalancingStrategy" name="RebalancingStrategy" minOccurs="1"/>
    <xs:element type="xs:string" name="ReferenceDateOffset" minOccurs="1"/>
    <xs:element type="CurrencyHedgedIndexRehedgingStrategy" name="HedgeAdjustment" minOccurs="1"/>
    <xs:element type="xs:string" name="HedgeCalendar" minOccurs="1"/>
    <xs:element name="FxIndexes" maxOccurs="unbounded" minOccurs="1">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="FxIndex" maxOccurs="unbounded" minOccurs="0">
            <xs:complexType>
              <xs:all>
                <xs:element type="xs:string" name="Currency" minOccurs="1" maxOccurs="1"/>
                <xs:element type="xs:string" name="IndexName" minOccurs="1" maxOccurs="1"/>
              </xs:all>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    <xs:element name="IndexWeightsAtLastRebalancingDate" maxOccurs="unbounded" minOccurs="0">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="Underlying" maxOccurs="unbounded" minOccurs="0">
            <xs:complexType>
              <xs:all>
                <xs:element type="xs:string" name="Name" minOccurs="1" maxOccurs="1"/>
                <xs:element type="xs:float" name="Weight" minOccurs="1" maxOccurs="1"/>
              </xs:all>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
    </xs:sequence>
  </xs:complexType>
  
  <xs:complexType name="portfolioBasketReferenceDatum">
    <xs:sequence>
      <xs:element name="Components">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Trade" maxOccurs="unbounded">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="oreTradeType" name="TradeType"/>
                  <xs:element type="envelope" name="Envelope" minOccurs="0"/>
                  <xs:group ref="oreTradeData"/>
                </xs:sequence>
                <xs:attribute type="xs:string" name="id"/>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

</xs:schema>
$xsd$, 'c7624404b4fcf9d7a5149f8293a3c82d52c425cb517d631fe7c39a181d40d495', 11185),
        ('1ed4a030-748a-5fcb-9d7b-2bcb2de1e8f9'::uuid, 'ibor_fallback_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element name="IborFallbackConfig">
    <xs:complexType>
      <xs:all>
        <xs:element type="iborFallbackGlobalSettings" name="GlobalSettings" minOccurs="0"/>
        <xs:element type="iborFallbackFallbacks" name="Fallbacks" minOccurs="0"/>
      </xs:all>
    </xs:complexType>
  </xs:element>

  <xs:complexType name="iborFallbackGlobalSettings">
    <xs:all>
      <xs:element type="bool" name="EnableIborFallbacks"/>
      <xs:element type="bool" name="UseRfrCurveInTodaysMarket"/>
      <xs:element type="bool" name="UseRfrCurveInSimulationMarket"/>
    </xs:all>
  </xs:complexType>  
  
  <xs:complexType name="iborFallbackFallbacks">
    <xs:sequence>
      <xs:element type="iborFallbackFallback" name="Fallback" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  
  <xs:complexType name="iborFallbackFallback">
    <xs:all>
      <xs:element type="indexNameType" name="IborIndex"/>
      <xs:element type="indexNameType" name="RfrIndex"/>
      <xs:element type="xs:float" name="Spread"/>
      <xs:element type="date" name="SwitchDate"/>
    </xs:all>
  </xs:complexType>

</xs:schema>
$xsd$, 'aa93ecd38ad9c052f852c3f6790caa8c072928dd7f006975ab7bbcd7ec56edca', 1258),
        ('4895a33d-75b2-5a76-9614-6d49715ce3f4'::uuid, 'simm_calibration_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element name="SIMMCalibrationData">
    <xs:complexType>
      <xs:sequence>
        <xs:element type="simmCalibration" name="SIMMCalibration" maxOccurs="unbounded" />
      </xs:sequence>
    </xs:complexType>
  </xs:element>

  <xs:complexType name="simmCalibration">
    <xs:all>
      <xs:element name="VersionNames">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Name" type="xs:string" maxOccurs="unbounded" />
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="AdditionalFields" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:any processContents="lax" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="interestRate" name="InterestRate" />
      <xs:element type="creditQualifying" name="CreditQualifying" />
      <xs:element type="creditNonQualifying" name="CreditNonQualifying" />
      <xs:element type="equity" name="Equity" />
      <xs:element type="commodity" name="Commodity" />
      <xs:element type="fx" name="FX" />
      <xs:element name="RiskClassCorrelations">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Correlation" maxOccurs="unbounded" >
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal">
                    <xs:attribute name="label1" type="xs:string"/>
                    <xs:attribute name="label2" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
    <xs:attribute name="id" type="xs:string" />
  </xs:complexType>

  <!-- InterestRate SIMM calibration -->
  <xs:complexType name="interestRate">
    <xs:sequence>

      <!-- InterestRate risk weights -->
      <xs:element name="RiskWeights">
        <xs:complexType>
          <xs:choice maxOccurs="unbounded">
            <!-- InterestRate delta risk weights -->
            <xs:element name="Delta">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Weight" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:integer" use="required" />
                          <xs:attribute name="label1" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- InterestRate vega risk weight -->
            <xs:element name="Vega">
              <xs:complexType>
                <xs:all>
                  <xs:element name="Weight" type="xs:decimal" />
                </xs:all>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- InterestRate historical volatility ratio -->
            <xs:element name="HistoricalVolatilityRatio" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

            <!-- InterestRate Inflation risk weight -->
            <xs:element name="Inflation">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

            <!-- InterestRate XCcyBasis risk weight -->
            <xs:element name="XCcyBasis">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

            <!-- InterestRate Currency groups -->
            <xs:element name="CurrencyLists" type="currencyLists" />

          </xs:choice>
        </xs:complexType>
      </xs:element>
     
      <!-- InterestRate correlations -->
      <xs:element name="Correlations">
        <xs:complexType>
          <xs:all>
            <!-- InterestRate intra-bucket correlations -->
            <xs:element name="IntraBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="label1" type="xs:string" use="required" />
                          <xs:attribute name="label2" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

            <!-- InterestRate sub-curves correlation -->
            <xs:element name="SubCurves" type="xs:decimal" />

            <!-- InterestRate Inflation correlation -->
            <xs:element name="Inflation" type="xs:decimal" />

            <!-- InterestRate cross-currency basis correlation -->
            <xs:element name="XCcyBasis" type="xs:decimal" />

            <!-- InterestRate cross-currency basis correlation -->
            <xs:element name="Outer" type="xs:decimal" />
          </xs:all>
        </xs:complexType>
      </xs:element>

      <!-- InterestRate concentration thresholds -->
      <xs:element name="ConcentrationThresholds" type="irfxConcentrationThresholds" />

    </xs:sequence>
  </xs:complexType>

  <!-- CreditQualifying SIMM calibration -->
  <xs:complexType name="creditQualifying">
    <xs:sequence>

      <!-- CreditQualifying risk weights -->
      <xs:element name="RiskWeights">
        <xs:complexType>
          <xs:choice maxOccurs="unbounded">
            <!-- CreditQualifying delta risk weights -->
            <xs:element name="Delta">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Weight" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- CreditQualifying vega risk weights -->
            <xs:element name="Vega">
              <xs:complexType>
                <xs:all>
                  <xs:element name="Weight" type="xs:decimal" />
                </xs:all>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- CreditQualifying base correlation -->
            <xs:element name="BaseCorrelation" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

          </xs:choice>
        </xs:complexType>
      </xs:element>

      <!-- CreditQualifying correlations -->
      <xs:element name="Correlations">
        <xs:complexType>
          <xs:all>
            <!-- CreditQualifying intra-bucket correlations -->
            <xs:element name="IntraBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="label1" type="xs:string" use="required" />
                          <xs:attribute name="label2" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

            <!-- CreditQualifying inter-bucket correlations -->
            <xs:element name="InterBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="label1" type="xs:integer" use="required" />
                          <xs:attribute name="label2" type="xs:integer" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

            <!-- CreditQualifying base correlation correlation -->
            <xs:element name="BaseCorrelation" type="xs:decimal" />
          </xs:all>
        </xs:complexType>
      </xs:element>

      <!-- CreditQualifying concentration thresholds -->
      <xs:element name="ConcentrationThresholds" type="concentrationThresholds" />
    </xs:sequence>
  </xs:complexType>


  <!-- CreditNonQualifying SIMM calibration -->
  <xs:complexType name="creditNonQualifying">
    <xs:sequence>

      <!-- CreditNonQualifying risk weights -->
      <xs:element name="RiskWeights">
        <xs:complexType>
          <xs:choice maxOccurs="unbounded">
            <xs:element name="Delta">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Weight" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <xs:element name="Vega">
              <xs:complexType>
                <xs:all>
                  <xs:element name="Weight" type="xs:decimal" />
                </xs:all>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

          </xs:choice>
        </xs:complexType>
      </xs:element>

      <!-- CreditNonQualifying correlations -->
      <xs:element name="Correlations">
        <xs:complexType>
          <xs:all>
            <!-- CreditNonQualifying intra-bucket correlations -->
            <xs:element name="IntraBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="label1" type="xs:string" use="required" />
                          <xs:attribute name="label2" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

            <!-- CreditNonQualifying inter-bucket correlations -->
            <xs:element name="InterBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="label1" type="xs:integer" use="required" />
                          <xs:attribute name="label2" type="xs:integer" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>

      <!-- CreditNonQualifying concentration thresholds -->
      <xs:element name="ConcentrationThresholds" type="concentrationThresholds" />

    </xs:sequence>
  </xs:complexType>

  <!-- Equity SIMM calibration -->
  <xs:complexType name="equity">
    <xs:sequence>

      <!-- Equity risk weights -->
      <xs:element name="RiskWeights">
        <xs:complexType>
          <xs:choice maxOccurs="unbounded">
            <!-- Equity delta risk weights -->
            <xs:element name="Delta">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Weight" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- Equity vega risk weights -->
            <xs:element name="Vega">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Weight" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- Equity historical volatility ratio -->
            <xs:element name="HistoricalVolatilityRatio" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

          </xs:choice>
        </xs:complexType>
      </xs:element>

      <!-- Equity correlations -->
      <xs:element name="Correlations">
        <xs:complexType>
          <xs:all>
            <!-- Equity intra-bucket correlations -->
            <xs:element name="IntraBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

            <!-- Equity inter-bucket correlations -->
            <xs:element name="InterBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="label1" type="xs:integer" use="required" />
                          <xs:attribute name="label2" type="xs:integer" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

          </xs:all>
        </xs:complexType>
      </xs:element>

      <!-- Equity concentration thresholds -->
      <xs:element name="ConcentrationThresholds" type="concentrationThresholds" />

    </xs:sequence>
  </xs:complexType>

  <!-- Commodity SIMM calibration -->
  <xs:complexType name="commodity">
    <xs:sequence>

      <!-- Commodity risk weights -->
      <xs:element name="RiskWeights">
        <xs:complexType>
          <xs:choice maxOccurs="unbounded">
            <!-- Commodity delta risk weights -->
            <xs:element name="Delta">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Weight" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- Commodity vega risk weights -->
            <xs:element name="Vega">
              <xs:complexType>
                <xs:all>
                  <xs:element name="Weight" type="xs:decimal" />
                </xs:all>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- Commodity historical volatility ratio -->
            <xs:element name="HistoricalVolatilityRatio">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

          </xs:choice>
        </xs:complexType>
      </xs:element>

      <!-- Commodity correlations -->
      <xs:element name="Correlations">
        <xs:complexType>
          <xs:all>
            <!-- Commodity intra-bucket correlations -->
            <xs:element name="IntraBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:string" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

            <!-- Commodity inter-bucket correlations -->
            <xs:element name="InterBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="label1" type="xs:integer" use="required" />
                          <xs:attribute name="label2" type="xs:integer" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

          </xs:all>
        </xs:complexType>
      </xs:element>

      <!-- Commodity concentration thresholds -->
      <xs:element name="ConcentrationThresholds" type="concentrationThresholds" />

    </xs:sequence>
  </xs:complexType>

  <!-- FX SIMM calibration -->
  <xs:complexType name="fx">
    <xs:sequence>

      <!-- FX risk weights -->
      <xs:element name="RiskWeights">
        <xs:complexType>
          <xs:choice maxOccurs="unbounded">
            <!-- FX delta risk weights -->
            <xs:element name="Delta">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Weight" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="label1" type="xs:integer" use="required" />
                          <xs:attribute name="label2" type="xs:integer" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- FX vega risk weight -->
            <xs:element name="Vega">
              <xs:complexType>
                <xs:all>
                  <xs:element name="Weight" type="xs:decimal" />
                </xs:all>
                <xs:attribute name="mporDays" type="xs:integer" />
              </xs:complexType>
            </xs:element>

            <!-- FX historical volatility ratio -->
            <xs:element name="HistoricalVolatilityRatio" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

            <!-- FX Inflation risk weight -->
            <xs:element name="Inflation">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

            <!-- FX XCcyBasis risk weight -->
            <xs:element name="XCcyBasis">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal" >
                    <xs:attribute name="mporDays" type="xs:integer" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>

            <!-- FX Currency groups -->
            <xs:element name="CurrencyLists" type="currencyLists" />

          </xs:choice>
        </xs:complexType>
      </xs:element>

      <!-- FX correlations -->
      <xs:element name="Correlations">
        <xs:complexType>
          <xs:all>
            <!-- FX intra-bucket correlations -->
            <xs:element name="IntraBucket">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Correlation" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:decimal">
                          <xs:attribute name="bucket" type="xs:integer" use="required" />
                          <xs:attribute name="label1" type="xs:integer" use="required" />
                          <xs:attribute name="label2" type="xs:integer" use="required" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>

            <!-- FX Volatility correlation -->
            <xs:element name="Volatility" type="xs:decimal" />

          </xs:all>
        </xs:complexType>
      </xs:element>

      <!-- FX concentration thresholds -->
      <xs:element name="ConcentrationThresholds" type="irfxConcentrationThresholds" />

    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="currencyLists">
    <xs:sequence>
      <xs:element name="Currency" maxOccurs="unbounded">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute name="bucket" type="xs:integer" use="required" />
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="concentrationThresholds">
    <xs:sequence>
      <xs:element name="Delta">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Threshold" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal">
                    <xs:attribute name="bucket" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Vega">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Threshold" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:decimal">
                    <xs:attribute name="bucket" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="irfxConcentrationThresholds">
    <xs:complexContent>
      <xs:extension base="concentrationThresholds">
        <xs:sequence>
          <xs:element name="CurrencyLists" type="currencyLists" />
        </xs:sequence>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

</xs:schema>
$xsd$, '3e89db5b5f2065955b0ce1dc07bbdccd99bc8f2bea45fb625b20c57b4642dd56', 27766),
        ('2451c622-1cb5-5f26-a0f2-3c9be3f327c1'::uuid, 'conventions_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:include schemaLocation="intradaypowerloaddata.xsd"/>

  <xs:element type="conventions" name="Conventions"/>

  <xs:complexType name="conventions">
    <xs:choice minOccurs="0" maxOccurs="unbounded">
      <xs:element type="zeroType" name="Zero"/>
      <xs:element type="cdsConventionsType" name="CDS"/>
      <xs:element type="depositType" name="Deposit"/>
      <xs:element type="futureType" name="Future"/>
      <xs:element type="fraType" name="FRA"/>
      <xs:element type="oisType" name="OIS"/>
      <xs:element type="swapType" name="Swap"/>
      <xs:element type="averageOISType" name="AverageOIS"/>
      <xs:element type="tenorBasisSwapType" name="TenorBasisSwap"/>
      <xs:element type="tenorBasisTwoSwapType" name="TenorBasisTwoSwap"/>
      <xs:element type="bmaBasisSwapType" name="BMABasisSwap"/>
      <xs:element type="fxType" name="FX"/>
      <xs:element type="crossCurrencyBasisType" name="CrossCurrencyBasis"/>
      <xs:element type="crossCurrencyFixFloatType" name="CrossCurrencyFixFloat"/>
      <xs:element type="iborIndexType" name="IborIndex"/>
      <xs:element type="overnightIndexType" name="OvernightIndex"/>
      <xs:element type="swapIndexType" name="SwapIndex"/>
      <xs:element type="inflationswapType" name="InflationSwap"/>
      <xs:element type="cmsSpreadOptionType" name="CmsSpreadOption"/>
      <xs:element type="commodityForwardType" name="CommodityForward"/>
      <xs:element type="commodityFutureType" name="CommodityFuture"/>
      <xs:element type="fxOption" name="FxOption"/>
      <xs:element type="fxOptionTimeWeighting" name="FxOptionTimeWeighting"/>
      <xs:element type="zeroInflationIndexType" name="ZeroInflationIndex"/>
      <xs:element type="bondYield" name="BondYield"/>
      <xs:element type="intradayPowerLoad" name="IntradayPowerLoad"/>
    </xs:choice>
  </xs:complexType>

  <xs:complexType name="zeroType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="TenorBased" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="compounding" name="Compounding" minOccurs="0" maxOccurs="1"/>
      <xs:element type="frequencyType" name="CompoundingFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="TenorCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpotLag" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="SpotCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="RollConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="EOM" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cdsConventionsType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SettlementDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="frequencyType" name="Frequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="PaymentConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dateRule" name="Rule" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="UpfrontSettlementDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="SettlesAccrual" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="PaysAtDefaultTime" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="LastPeriodDayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="UsesReferenceData" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="depositType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="IndexBased" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="Convention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="EOM" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:unsignedLong" name="SettlementDays" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="futureType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
      <xs:element type="futureDateGenerationRule" name="DateGenerationRule" minOccurs="0" maxOccurs="1"/>
      <xs:element type="overnightIndexFutureNettingType" name="OvernightIndexFutureNettingType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="OvernightIndexTenor" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fraType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="oisType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpotLag" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="FixedDayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixedCalendar" minOccurs="0"/>
      <xs:element type="xs:integer" name="PaymentLag" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="EOM" minOccurs="0" maxOccurs="1"/>
      <xs:element type="frequencyType" name="FixedFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="FixedConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="FixedPaymentConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dateRule" name="Rule" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="PaymentCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="RateCutoff" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="swapType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixedCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="frequencyType" name="FixedFrequency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="FixedConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayCounter" name="FixedDayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="FloatFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="subPeriodsCouponType" name="SubPeriodsCouponType" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="averageOISType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpotLag" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixedTenor" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="FixedDayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixedCalendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="FixedConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="FixedPaymentConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="FixedFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="OnTenor" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="RateCutoff" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="tenorBasisSwapType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="PayIndex" minOccurs="0" maxOccurs="1"/>
      <xs:element type="period" name="PayFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="ReceiveIndex" minOccurs="0" maxOccurs="1"/>
      <xs:element type="period" name="ReceiveFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="SpreadOnRec" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="IncludeSpread" minOccurs="0" maxOccurs="1"/>
      <xs:element type="subPeriodsCouponType" name="SubPeriodsCouponType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="PayIsAveraged" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="RecIsAveraged" minOccurs="0" maxOccurs="1"/>
      <!-- deprecated fields -->
      <xs:element type="xs:string" name="LongIndex" minOccurs="0" maxOccurs="1"/>
      <xs:element type="period" name="LongPayTenor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="ShortIndex" minOccurs="0" maxOccurs="1"/>
      <xs:element type="period" name="ShortPayTenor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="SpreadOnShort" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="tenorBasisTwoSwapType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Calendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="LongFixedFrequency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="LongFixedConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="LongFixedDayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="LongIndex" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="ShortFixedFrequency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="ShortFixedConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="ShortFixedDayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="ShortIndex" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="LongMinusShort" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bmaBasisSwapType">
    <xs:all>
      <xs:element type="xs:string" name="Id"/>
      <xs:element type="xs:string" name="Index"/>
      <xs:element type="xs:string" name="BMAIndex"/>
      <xs:element type="xs:string" name="BMAPaymentCalendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="BMAPaymentConvention" minOccurs="0"/>
      <xs:element type="xs:integer" name="BMAPaymentLag" minOccurs="0"/>
      <xs:element type="xs:string" name="IndexPaymentCalendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="IndexPaymentConvention" minOccurs="0"/>
      <xs:element type="xs:integer" name="IndexPaymentLag" minOccurs="0"/>
      <xs:element type="xs:integer" name="IndexSettlementDays" minOccurs="0"/>
      <xs:element type="period" name="IndexPaymentPeriod" minOccurs="0"/>
      <xs:element type="xs:integer" name="OvernightLockoutDays" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpotDays" minOccurs="1" maxOccurs="1"/>
      <xs:element type="currencyCode" name="SourceCurrency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="currencyCode" name="TargetCurrency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:decimal" name="PointsFactor" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="AdvanceCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="SpotRelative" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="EOM" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="Convention" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="crossCurrencyBasisType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SettlementDays" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="SettlementCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="RollConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FlatIndex" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="SpreadIndex" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="EOM" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="IsResettable" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="FlatIndexIsResettable" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FlatTenor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="SpreadTenor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpreadPaymentLag" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="FlatPaymentLag" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="SpreadIncludeSpread" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="SpreadLookback" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpreadFixingDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpreadRateCutoff" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="SpreadIsAveraged" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="SpreadObservationShift" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="FlatIncludeSpread" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FlatLookback" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="FlatFixingDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="FlatRateCutoff" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="FlatIsAveraged" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="FlatObservationShift" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="crossCurrencyFixFloatType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SettlementDays" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="SettlementCalendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="SettlementConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="currencyCode" name="FixedCurrency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="FixedFrequency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="FixedConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="FixedDayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="EOM" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="IsResettable" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="FloatIndexIsResettable" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="IncludeSpread" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Lookback" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="RateCutoff" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="IsAveraged" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="ObservationShift" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="iborIndexType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixingCalendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SettlementDays" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="EndOfMonth" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="overnightIndexType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixingCalendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SettlementDays" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="swapIndexType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixingCalendar" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="inflationswapType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixCalendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="FixConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="Interpolated" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="ObservationLag" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="AdjustInflationObservationDates" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="InflationCalendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="InflationConvention" minOccurs="1" maxOccurs="1"/>
      <xs:element type="publicationRoll" name="PublicationRoll" minOccurs="0" maxOccurs="1"/>
      <xs:element type="scheduleData" name="PublicationSchedule" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="StartDelay" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="StartDelayConvention" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cmsSpreadOptionType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="ForwardStart" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="SpotDays" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="SwapTenor" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="FixingDays" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Calendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="1" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="RollConvention" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="commodityForwardType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="SpotDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:decimal" name="PointsFactor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="AdvanceCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="SpotRelative" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="DeliveryLocation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="Outright" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="nthWeekdayType">
    <xs:all>
      <xs:element name="Nth" minOccurs="1" maxOccurs="1">
        <xs:simpleType>
          <xs:restriction base="xs:integer">
            <xs:minInclusive value="1"/>
            <xs:maxInclusive value="4"/>
          </xs:restriction> 
        </xs:simpleType>
      </xs:element>
      <xs:element type="weekdayType" name="Weekday" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  
  <xs:simpleType name="prohibitedExpiriesBdcType">
    <xs:restriction base="businessDayConvention">
      <xs:enumeration value="F"/>
      <xs:enumeration value="Following"/>
      <xs:enumeration value="FOLLOWING"/>
      <xs:enumeration value="MF"/>
      <xs:enumeration value="ModifiedFollowing"/>
      <xs:enumeration value="Modified Following"/>
      <xs:enumeration value="MODIFIEDF"/>
      <xs:enumeration value="MODFOLLOWING"/>
      <xs:enumeration value="P"/>
      <xs:enumeration value="Preceding"/>
      <xs:enumeration value="PRECEDING"/>
      <xs:enumeration value="MP"/>
      <xs:enumeration value="ModifiedPreceding"/>
      <xs:enumeration value="Modified Preceding"/>
      <xs:enumeration value="MODIFIEDP"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="prohibitedExpiriesType">
    <xs:all>
      <xs:element name="Dates">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Date" minOccurs="1" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="date">
                    <xs:attribute name="forFuture" type="bool" />
                    <xs:attribute name="convention" type="prohibitedExpiriesBdcType" />
                    <xs:attribute name="forOption" type="bool" />
                    <xs:attribute name="optionConvention" type="prohibitedExpiriesBdcType" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="continuationMappingType">
    <xs:sequence>
      <xs:element type="xs:positiveInteger" name="From"/>
      <xs:element type="xs:positiveInteger" name="To"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="continuationMappingsType">
    <xs:sequence>
      <xs:element type="continuationMappingType" name="ContinuationMapping" minOccurs="1" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  
  <xs:complexType name="averagingDataType">
    <xs:all>
      <xs:element type="xs:string" name="CommodityName" minOccurs="1" maxOccurs="1"/>
      <xs:element type="averagingDataPeriodType" name="Period" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="PricingCalendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Conventions" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="UseBusinessDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="DeliveryRollDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="FutureMonthOffset" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="DailyExpiryOffset" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>
  
  <xs:complexType name="offPeakPowerIndexDataType">
    <xs:all>
      <xs:element type="xs:string" name="OffPeakIndex" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="PeakIndex" minOccurs="1" maxOccurs="1"/>
      <xs:element name="OffPeakHours" minOccurs="1" maxOccurs="1">
        <xs:simpleType>
          <xs:restriction base="xs:decimal">
            <xs:minExclusive value="0"/>
            <xs:maxExclusive value="24"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element type="xs:string" name="PeakCalendar" minOccurs="1" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="commodityFutureType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element name="AnchorDay" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:choice>
            <xs:element type="nthWeekdayType" name="NthWeekday"/>
            <xs:element type="dayOfMonth" name="DayOfMonth"/>
            <xs:element type="xs:nonNegativeInteger" name="CalendarDaysBefore"/>
            <xs:element type="weekdayType" name="LastWeekday"/>
            <xs:element type="weekdayType" name="WeeklyDayOfTheWeek"/>
            <xs:element type="xs:integer" name="BusinessDaysAfter"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
      <xs:element type="frequencyType" name="ContractFrequency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Calendar" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="ExpiryCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="ExpiryMonthLag" minOccurs="0" maxOccurs="1"/>
      <xs:element type="monthType" name="OneContractMonth" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="OffsetDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="BusinessDayConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="AdjustBeforeOffset" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="IsAveraging" minOccurs="0" maxOccurs="1"/>
      <xs:element type="prohibitedExpiriesType" name="ProhibitedExpiries" minOccurs="0" maxOccurs="1"/>
      <xs:element name="ValidContractMonths" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="monthType" name="Month" minOccurs="1" maxOccurs="12"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:integer" name="OptionExpiryMonthLag" minOccurs="0" maxOccurs="1"/>
      <xs:element type="frequencyType" name="OptionContractFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="OptionExpiryOffset" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="OptionCalendarDaysBefore" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="OptionMinBusinessDaysBefore" minOccurs="0" maxOccurs="1"/>
      <xs:element type="dayOfMonth" name="OptionExpiryDay" minOccurs="0" maxOccurs="1"/>
      <xs:element type="nthWeekdayType" name="OptionNthWeekday" minOccurs="0" maxOccurs="1"/>
      <xs:element type="weekdayType" name="OptionExpiryLastWeekdayOfMonth" minOccurs="0" maxOccurs="1"/>
      <xs:element type="weekdayType" name="OptionExpiryWeeklyDayOfTheWeek" minOccurs="0" maxOccurs="1"/>
      <xs:element type="businessDayConvention" name="OptionBusinessDayConvention" minOccurs="0" maxOccurs="1"/>
      <xs:element type="continuationMappingsType" name="FutureContinuationMappings" minOccurs="0" maxOccurs="1"/>
      <xs:element type="continuationMappingsType" name="OptionContinuationMappings" minOccurs="0" maxOccurs="1"/>
      <xs:element type="averagingDataType" name="AveragingData" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="HoursPerDay" minOccurs="0" maxOccurs="1"/>
      <xs:element type="offPeakPowerIndexDataType" name="OffPeakPowerIndexData" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="IndexName" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="SavingsTime" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="DeliveryLocation" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="BalanceOfTheMonth" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="BalanceOfTheMonthPricingCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="OptionUnderlyingFutureConvention" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxOption">
    <xs:all>
      <xs:element type="xs:string" name="Id"/>
      <xs:element type="xs:string" name="FXConventionID" minOccurs="0"/>
      <xs:element type="xs:string" name="AtmType"/>
      <xs:element type="xs:string" name="DeltaType"/>
      <xs:element type="xs:string" name="SwitchTenor" minOccurs="0"/>
      <xs:element type="xs:string" name="LongTermAtmType" minOccurs="0"/>
      <xs:element type="xs:string" name="LongTermDeltaType" minOccurs="0"/>
      <xs:element type="xs:string" name="RiskReversalInFavorOf" minOccurs="0"/>
      <xs:element type="xs:string" name="ButterflyStyle" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxOptionTimeWeighting">
    <xs:all>
      <xs:element type="xs:string" name="Id"/>
      <xs:element name="WeekdayWeights">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:float" name="Monday"/>
            <xs:element type="xs:float" name="Tuesday"/>
            <xs:element type="xs:float" name="Wednesday"/>
            <xs:element type="xs:float" name="Thursday"/>
            <xs:element type="xs:float" name="Friday"/>
            <xs:element type="xs:float" name="Saturday"/>
            <xs:element type="xs:float" name="Sunday"/>
         </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="TradingCenters" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="TradingCenter">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="Name" />
                  <xs:element type="xs:string" name="Calendar" />
                  <xs:element type="xs:float" name="Weight" />
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Events" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Event">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="Description" />
                  <xs:element type="date" name="Date" />
                  <xs:element type="xs:float" name="Weight" />
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="zeroInflationIndexType">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="RegionName" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="RegionCode" minOccurs="1" maxOccurs="1"/>
      <xs:element type="bool" name="Revised" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="Frequency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="AvailabilityLag" minOccurs="1" maxOccurs="1"/>
      <xs:element type="currencyCode" name="Currency" minOccurs="1" maxOccurs="1"/>
      <xs:element name="RebasingEvents" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Event" minOccurs="1" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:double">
                    <xs:attribute name="date" type="date" use="required"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondYield">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="Compounding" minOccurs="1" maxOccurs="1"/>
      <xs:element type="frequencyType" name="Frequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="PriceType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="Accuracy" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="MaxEvaluations" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="Guess" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

</xs:schema>
$xsd$, '0bd66a01885d9bf3bf0040ab850ff0d54283f9bce8c16ebc533ed33e34c3acb1', 33513),
        ('1a4ecbf3-c15e-5e5f-a257-0d6ffcb87d33'::uuid, 'currency_configuration_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="currencyConfig" name="CurrencyConfig"/>

  <xs:element type="currencyDefinition" name="Currency"/>

  <xs:complexType name="currencyConfig">
    <xs:sequence>
      <xs:element type="currencyDefinition" name="Currency" maxOccurs="unbounded" minOccurs="0">
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="currencyDefinition">
    <xs:all>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="xs:string" name="ISOCode"/>
      <xs:element type="xs:string" name="MinorUnitCodes" minOccurs="0"/>
      <xs:element type="xs:integer" name="NumericCode" minOccurs="0"/>
      <xs:element type="xs:string" name="Symbol"/>
      <xs:element type="xs:string" name="FractionSymbol"/>
      <xs:element type="xs:integer" name="FractionsPerUnit"/>
      <xs:element type="roundingType" name="RoundingType"/>
      <xs:element type="xs:integer" name="RoundingPrecision"/>
      <xs:element type="xs:string" name="CurrencyType" minOccurs="0"/>
    </xs:all>
  </xs:complexType>
  
</xs:schema>
$xsd$, 'adea97847d67e5cee5cbc2f303a206952103d2eb168d3054d066bb0af7f159b6', 1178),
        ('42d4c8c6-e32a-58f9-bf7a-6af254070151'::uuid, 'credit_simulation_schema', '1.8.17.0', $xsd$<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" attributeFormDefault="unqualified" elementFormDefault="qualified">
  <xs:element type="creditsimulation" name="CreditSimulation"/>
  <xs:complexType name="creditsimulation">
    <xs:all>
      <xs:element type="transitionmatrices" name="TransitionMatrices"/>
      <xs:element type="entities" name="Entities"/>
      <xs:element type="xs:string" name="NettingSetIds"/>
      <xs:element type="risk" name="Risk"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="transitionmatrices">
    <xs:sequence>
      <xs:element type="transitionmatrix" name="TransitionMatrix" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="transitionmatrix">
    <xs:sequence>
      <xs:element type="xs:string" name="Name"/>
      <xs:element name="Data">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:string">
              <xs:attribute name="t0" type="xs:string"/>
              <xs:attribute name="t1" type="xs:string"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="entities">
    <xs:sequence>
      <xs:element type="entity" name="Entity" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>
  <xs:complexType name="entity">
    <xs:all>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="xs:string" name="FactorLoadings"/>
      <xs:element type="xs:string" name="TransitionMatrix"/>
      <xs:element type="xs:integer" name="InitialState"/>
    </xs:all>
  </xs:complexType>
  <xs:complexType name="risk">
    <xs:all>
      <xs:element type="bool" name="Market"/>
      <xs:element type="bool" name="Credit"/>
      <xs:element type="bool" name="ZeroMarketPnl"/>
      <xs:element type="xs:string" name="Evaluation"/>
      <xs:element type="bool" name="DoubleDefault"/>
      <xs:element type="xs:integer" name="Seed"/>
      <xs:element type="xs:integer" name="Paths"/>
      <xs:element type="xs:string" name="CreditMode"/>
      <xs:element type="xs:string" name="LoanExposureMode"/>
    </xs:all>
  </xs:complexType>
</xs:schema>
$xsd$, '0ec08e579108cb4110f34390ca26e66f3b754c15c73a7886c07c61173ea680c1', 2242),
        ('85d9e578-e2d1-5498-b40d-356c27c132c8'::uuid, 'script_library_schema', '1.8.17.0', $xsd$<?xml version="1.0"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" attributeFormDefault="unqualified" elementFormDefault="qualified">
  <xs:complexType name="ore_script">
    <xs:all>
      <xs:element type="xs:string" name="Code"/>
      <xs:element type="xs:string" name="NPV"/>
      <xs:element name="Results" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Result" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="rename" type="xs:string" use="optional"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="PricingEngineConfigOverwrite" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element name="ModelParameters" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Parameter" minOccurs="0">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="EngineParameters" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Parameter" minOccurs="0" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:string">
                          <xs:attribute name="name" type="xs:string"/>
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="CalibrationSpec" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Calibration" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Index"/>
                  <xs:element name="Strikes">
                    <xs:complexType>
                      <xs:sequence>
                        <xs:element type="xs:string" name="Strike" maxOccurs="unbounded"/>
                      </xs:sequence>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="ScheduleCoarsening" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="EligibleSchedule" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="NewSchedules" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="NewSchedule" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="Name"/>
                  <xs:element type="xs:string" name="Operation"/>
                  <xs:element name="Schedules">
                    <xs:complexType>
                      <xs:sequence>
                        <xs:element type="xs:string" name="Schedule" maxOccurs="unbounded"/>
                      </xs:sequence>
                    </xs:complexType>
                  </xs:element>
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="StickyCloseOutStates" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="StickyCloseOutState" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="ConditionalExpectation" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element name="ModelStates" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="ModelState" minOccurs="0" maxOccurs="unbounded"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="AmcCg" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element name="Components" minOccurs="0">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:string" name="Component" minOccurs="0"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="Target" minOccurs="0">
              <xs:complexType>
                <xs:all>
                  <xs:element type="xs:string" name="Value"/>
                  <xs:element type="xs:string" name="Derivative"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
    <xs:attribute type="xs:string" name="purpose" use="optional"/>
  </xs:complexType>
  <xs:element name="ScriptLibrary">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="Script" minOccurs="0" maxOccurs="unbounded">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:string" name="Name"/>
              <xs:element type="xs:string" name="ProductTag" minOccurs="0"/>
              <xs:element type="ore_script" name="Script" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>
$xsd$, '472c4a5d3f928c1d6984d6f1b88ff130f374a87c26b2df5059b316e3ba44e732', 6369),
        ('af8cce1d-d10e-5f54-bbcb-7fc433931c72'::uuid, 'netting_set_definitions_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="collateralBalances" name="CollateralBalances"/>

  <xs:complexType name="collateralBalances">
    <xs:sequence>
      <xs:element name="CollateralBalance" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element ref="nettingSetGroup"/>
            <xs:element type="currencyCode" name="Currency" minOccurs="0"/>
            <xs:element type="xs:decimal" name="InitialMargin" minOccurs="0"/>
            <xs:element type="xs:decimal" name="VariationMargin" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>
  
  <xs:element type="nettingsetdefinitions" name="NettingSetDefinitions"/>

  <xs:complexType name="nettingsetdefinitions">
    <xs:sequence>
      <xs:element name="NettingSet" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element ref="nettingSetGroup"/>
            <xs:element type="xs:boolean" name="ActiveCSAFlag" minOccurs="0"/>
            <xs:element name="CSADetails" minOccurs="0">
              <xs:complexType mixed="true">
                <xs:all>
                  <xs:element type="csaType" name="Bilateral" minOccurs="0"/>
                  <xs:element type="currencyCode" name="CSACurrency" minOccurs="0"/>
                  <xs:element type="xs:string" name="Index" minOccurs="0"/>
                  <xs:element type="non-negative-decimal" name="ThresholdPay" minOccurs="0"/>
                  <xs:element type="non-negative-decimal" name="ThresholdReceive" minOccurs="0"/>
                  <xs:element type="non-negative-decimal" name="MinimumTransferAmountPay" minOccurs="0"/>
                  <xs:element type="non-negative-decimal" name="MinimumTransferAmountReceive" minOccurs="0"/>
                  <xs:element name="IndependentAmount" minOccurs="0">
                    <xs:complexType>
                      <xs:all>
                        <xs:element type="xs:decimal" name="IndependentAmountHeld"/>
                        <xs:element type="independentAmountType" name="IndependentAmountType"/>
                      </xs:all>
                    </xs:complexType>
                  </xs:element>
                  <xs:element name="MarginingFrequency" minOccurs="0">
                    <xs:complexType>
                      <xs:all>
                        <xs:element type="xs:string" name="CallFrequency"/>
                        <xs:element type="xs:string" name="PostFrequency"/>
                      </xs:all>
                    </xs:complexType>
                  </xs:element>
                  <xs:element type="xs:string" name="MarginPeriodOfRisk" minOccurs="0"/>
                  <xs:element type="xs:decimal" name="CollateralCompoundingSpreadReceive" minOccurs="0"/>
                  <xs:element type="xs:decimal" name="CollateralCompoundingSpreadPay" minOccurs="0"/>
                  <xs:element name="EligibleCollaterals" minOccurs="0">
                    <xs:complexType>
                      <xs:all>
                        <xs:element name="Currencies">
                          <xs:complexType>
                            <xs:sequence>
                              <xs:element type="currencyCode" name="Currency" maxOccurs="unbounded" minOccurs="0"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </xs:all>
                    </xs:complexType>
                  </xs:element>
                  <xs:element type="xs:boolean" name="ApplyInitialMargin" minOccurs="0"/>
                  <xs:element type="csaType" name="InitialMarginType" minOccurs="0"/>
                  <xs:element type="xs:boolean" name="CalculateIMAmount" minOccurs="0"/>
                  <xs:element type="xs:boolean" name="CalculateVMAmount" minOccurs="0"/>
                  <xs:element type="xs:string" name="NonExemptIMRegulations" minOccurs="0"/>
                </xs:all>
              </xs:complexType>
            </xs:element>
            <xs:element type="xs:decimal" name="RiskWeight" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:simpleType name="csaType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Bilateral"/>
      <xs:enumeration value="CallOnly"/>
      <xs:enumeration value="PostOnly"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="independentAmountType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="FIXED"/>
    </xs:restriction>
  </xs:simpleType>

</xs:schema>
$xsd$, 'ba21cab7bfed2a6f0e22876c63c7a7ef36f5b1418b7b61b070f15b310c04b03e', 4791),
        ('2e7a96bf-5d8b-51ee-9324-c32e2cb68853'::uuid, 'counterparty_information_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="counterpartyInformation" name="CounterpartyInformation"/>

  <xs:complexType name="counterpartyInformation">
    <xs:all>
      <xs:element type="counterparties" name="Counterparties" minOccurs="0" />
      <xs:element type="counterPartyCorrelations" name="Correlations" minOccurs="0" />
    </xs:all>
  </xs:complexType>

  <xs:complexType name="counterparties">
    <xs:sequence>
      <xs:element type="counterparty" name="Counterparty" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="counterparty">
    <xs:all>
      <xs:element type="xs:string" name="CounterpartyId"/>
      <xs:element type="xs:boolean" name="ClearingCounterparty" minOccurs="0"/>
      <xs:element type="creditQualityType" name="CreditQuality" minOccurs="0"/>
      <xs:element type="xs:decimal" name="BaCvaRiskWeight" minOccurs="0"/>
      <xs:element type="xs:decimal" name="SaCcrRiskWeight" minOccurs="0"/>
      <xs:element type="xs:decimal" name="SaCvaRiskBucket" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="counterPartyCorrelations">
    <xs:sequence>
      <xs:element name="Correlation" minOccurs="0" maxOccurs="unbounded">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="correlationValue">
              <xs:attribute type="xs:string" name="cpty1" use="required"/>
              <xs:attribute type="xs:string" name="cpty2" use="required"/>
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:simpleType name="creditQualityType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="HY"/>
      <xs:enumeration value="IG"/>
      <xs:enumeration value="NR"/>
    </xs:restriction>
  </xs:simpleType>

</xs:schema>
$xsd$, '67b62693eaae5e7c425b906de3a25a511c57f48e25aca8da444892bc1d135103', 1975),
        ('8167f995-6c53-5adb-8ed7-5f70ad9127ff'::uuid, 'collateral_balances_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:element type="collateralBalances" name="CollateralBalances"/>

  <xs:complexType name="collateralBalances">
    <xs:sequence>
      <xs:element name="CollateralBalance" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element ref="nettingSetGroup"/>
            <xs:element type="currencyCode" name="Currency" minOccurs="0"/>
            <xs:element type="xs:decimal" name="InitialMargin" minOccurs="0"/>
            <xs:element type="xs:decimal" name="VariationMargin" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

</xs:schema>
$xsd$, '03d18b12c028aa349f4317ee6cc6772b4e5b2409c8dd7eb09f703809a019051f', 782),
        ('3fd22896-8dc0-5a6a-b361-3a1dd95c5bd5'::uuid, 'trade_data_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:include schemaLocation="ore_types.xsd"/>
  <xs:include schemaLocation="instruments.xsd"/>
  <xs:include schemaLocation="referencedata.xsd"/>
  <xs:include schemaLocation="simulation.xsd"/>
  <xs:include schemaLocation="creditsimulation.xsd"/>
  <xs:include schemaLocation="curveconfig.xsd"/>
  <xs:include schemaLocation="conventions.xsd"/>
  <xs:include schemaLocation="nettingsetdefinitions.xsd"/>
  <xs:include schemaLocation="pricingengines.xsd"/>
  <xs:include schemaLocation="todaysmarket.xsd"/>
  <xs:include schemaLocation="sensitivity.xsd"/>
  <xs:include schemaLocation="stress.xsd"/>
  <xs:include schemaLocation="ore.xsd"/>
  <xs:include schemaLocation="calendaradjustment.xsd"/>
  <xs:include schemaLocation="currencyconfig.xsd"/>
  <xs:include schemaLocation="iborfallbackconfig.xsd"/>
  <xs:include schemaLocation="baselTrafficLightconfig.xsd"/>
  <xs:include schemaLocation="scriptlibrary.xsd"/>
  <xs:include schemaLocation="simmcalibration.xsd"/>
  <xs:include schemaLocation="counterparty.xsd"/>
  <xs:include schemaLocation="historicalreturnconfig.xsd" />
  <!--<xs:include schemaLocation="collateralbalance.xsd"/>-->

  <xs:element type="portfolio" name="Portfolio"/>

<xs:element name="subTradeGroup" abstract="true"/>
<xs:element name="Trade" type="componentTrade" substitutionGroup="subTradeGroup"/>
<xs:element name="SubTrade" type="componentSubTrade" substitutionGroup="subTradeGroup"/>


  <xs:complexType name="componentTrade">
    <xs:sequence>
      <xs:element type="oreTradeType" name="TradeType"/>
      <xs:element type="envelope" name="Envelope" minOccurs="0"/>
      <xs:element type="tradeActions" name="TradeActions" minOccurs="0"/>
      <xs:group ref="oreTradeData" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>
  <xs:complexType name="componentSubTrade">
    <xs:sequence>
      <xs:element type="oreTradeType" name="SubTradeType"/>
      <xs:element type="envelope" name="Envelope" minOccurs="0"/>
      <xs:element type="tradeActions" name="TradeActions" minOccurs="0"/>
      <xs:group ref="oreTradeData" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id"/>
  </xs:complexType>


  <xs:complexType name="portfolio">
    <xs:sequence>
      <xs:element type="trade" name="Trade" maxOccurs="unbounded" minOccurs="0">
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="trade">
    <xs:sequence>
      <xs:element type="oreTradeType" name="TradeType"/>
      <xs:element type="envelope" name="Envelope" minOccurs="0"/>
      <xs:element type="tradeActions" name="TradeActions" minOccurs="0"/>
      <xs:group ref="oreTradeData" minOccurs="0"/>
    </xs:sequence>
    <xs:attribute type="xs:string" name="id" use="required"/>
  </xs:complexType>
  <!-- <xs:complexType name="subTrade"> -->
    <!-- <xs:sequence> -->
      <!-- <xs:element type="oreTradeType" name="SubTradeType"/> -->
      <!-- <xs:element type="envelope" name="Envelope" minOccurs="0"/> -->
      <!-- <xs:element type="tradeActions" name="TradeActions" minOccurs="0"/> -->
      <!-- <xs:group ref="oreTradeData" minOccurs="0"/> -->
    <!-- </xs:sequence> -->
    <!-- <xs:attribute type="xs:string" name="id"/> -->
  <!-- </xs:complexType> -->

</xs:schema>
$xsd$, 'c33267a0c1a78366a02df0f62bf664c670cc48777bc3451f8014cc907a353f09', 3417),
        ('a0929ad3-09de-5ca8-a18d-4e6578cb1bbf'::uuid, 'historical_return_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:include schemaLocation="ore_types.xsd"/>  
    <!-- Enum for Return Type -->
    <xs:simpleType name="ReturnEnum">
        <xs:restriction base="xs:string">
            <xs:enumeration value="Log" />
            <xs:enumeration value="Absolute" />
            <xs:enumeration value="Relative" />
        </xs:restriction>
    </xs:simpleType>

    <!-- Reusable type for a Return -->
    <xs:complexType name="ReturnType">
        <xs:sequence>
            <xs:element name="Type" type="ReturnEnum" />
            <xs:element name="Displacement" type="xs:decimal" />
        </xs:sequence>
         <xs:attribute name="key" type="xs:string" use="required" />
    </xs:complexType>

    <!-- Root element -->
    <xs:element name="ReturnConfiguration">
        <xs:complexType>
            <xs:sequence>
                <xs:element name="Return" type="ReturnType" minOccurs="1" maxOccurs="unbounded"/>
            </xs:sequence>
        </xs:complexType>
    </xs:element>
</xs:schema>$xsd$, '78880dcd6f35ed5426f9c5973fa00e65cfe576ad2027bd8de041a08a2ce82204', 1112),
        ('93440f80-74b9-502c-9e51-dce9c2c20ddc'::uuid, 'basel_traffic_light_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">
    <xs:element name="BaselTrafficLightConfig">
        <xs:complexType>
            <xs:sequence>
                <xs:element name="Configuration" maxOccurs="unbounded">
                    <xs:complexType>
                        <xs:sequence>
                            <xs:element type="xs:string" name="mporDays" maxOccurs="1"/>
                            <xs:element name="ObservationThresholds">
                                <xs:complexType>
                                    <xs:sequence>
                                        <xs:element type="xs:string" name="ObservationCount" maxOccurs="1"/>
                                        <xs:element type="xs:string" name="AmberLimit" maxOccurs="1"/>
                                        <xs:element type="xs:string" name="RedLimit" maxOccurs="1"/>
                                    </xs:sequence>
                                </xs:complexType>
                            </xs:element>
                        </xs:sequence>
                    </xs:complexType>
                </xs:element>
            </xs:sequence>
        </xs:complexType>
    </xs:element>
</xs:schema>
$xsd$, '2f488aa30f99570e29ded10442b1a94f65b0e301768a93edf555c564ca05c3e5', 1273),
        ('adaf51a9-f885-5146-94c9-e60df6359d4b'::uuid, 'instruments_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified"
  xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:include schemaLocation="ore_types.xsd"/>
  <xs:include schemaLocation="intradaypowerloaddata.xsd"/>

  <xs:group name="oreTradeData">
    <xs:choice>
      <xs:element type="swapData" name="CrossCurrencySwapData"/>
      <xs:element type="swapData" name="InflationSwapData"/>
      <xs:element type="swapData" name="SwapData"/>
      <xs:element type="swapData" name="EquitySwapData"/>
      <xs:element type="callableSwapData" name="CallableSwapData"/>
      <xs:element type="arcOptionData" name="ArcOptionData"/>
      <xs:element type="swaptionData" name="SwaptionData"/>
      <xs:element type="swaptionStraddleData" name="SwaptionStraddleData"/>
      <xs:element type="varianceSwapData" name="VarianceSwapData"/>
      <xs:element type="varianceSwapData" name="EquityVarianceSwapData"/>
      <xs:element type="varianceSwapData" name="FxVarianceSwapData"/>
      <xs:element type="varianceSwapData" name="CommodityVarianceSwapData"/>
      <xs:element type="forwardRateAgreementData" name="ForwardRateAgreementData"/>
      <xs:element type="fxForwardData" name="FxForwardData"/>
      <xs:element type="fxAverageForwardData" name="FxAverageForwardData"/>
      <xs:element type="fxOptionData" name="FxOptionData"/>
      <xs:element type="fxBarrierOptionData" name="FxBarrierOptionData"/>
      <xs:element type="fxBarrierOptionData" name="FxDoubleBarrierOptionData"/>
      <xs:element type="fxDigitalOptionData" name="FxDigitalOptionData"/>
      <xs:element type="fxBarrierOptionData" name="FxEuropeanBarrierOptionData"/>
      <xs:element type="fxKIKOBarrierOptionData" name="FxKIKOBarrierOptionData"/>
      <xs:element type="fxDigitalBarrierOptionData" name="FxDigitalBarrierOptionData"/>
      <xs:element type="fxTouchOptionData" name="FxTouchOptionData"/>
      <xs:element type="fxTouchOptionData" name="FxDoubleTouchOptionData"/>
      <xs:element type="fxSwapData" name="FxSwapData"/>
      <xs:element type="capFloorData" name="CapFloorData"/>
      <xs:element type="rateDigitalOptionData" name="RateDigitalOptionData"/>
      <xs:element type="equityFutureOptionData" name="EquityFutureOptionData"/>
      <xs:element type="equityOptionData" name="EquityOptionData"/>
      <xs:element type="equityAutoDeltaHedgedOptionData" name="EquityAutoDeltaHedgedOptionData"/>
      <xs:element type="eqBarrierOptionData" name="EquityBarrierOptionData"/>
      <xs:element type="eqBarrierOptionData" name="EquityDoubleBarrierOptionData"/>
      <xs:element type="equityForwardData" name="EquityForwardData"/>
      <xs:element type="eqBarrierOptionData" name="EquityEuropeanBarrierOptionData"/>
      <xs:element type="eqDigitalOptionData" name="EquityDigitalOptionData"/>
      <xs:element type="eqTouchOptionData" name="EquityDoubleTouchOptionData"/>
      <xs:element type="eqTouchOptionData" name="EquityTouchOptionData"/>
      <xs:element type="cliquetOptionData" name="EquityCliquetOptionData"/>
      <xs:element type="bondData" name="BondData"/>
      <xs:element type="forwardBondData" name="ForwardBondData"/>
      <xs:element type="bondFutureData" name="BondFutureData"/>
      <xs:element type="bondFutureOptionData" name="BondFutureOptionData"/>
      <xs:element type="creditDefaultSwapData" name="CreditDefaultSwapData"/>
      <xs:element type="creditDefaultSwapOptionData" name="CreditDefaultSwapOptionData"/>
      <xs:element type="commodityForwardData" name="CommodityForwardData"/>
      <xs:element type="intradayPowerForwardData" name="IntradayPowerForwardData"/>
      <xs:element type="commodityOptionData" name="CommodityOptionData"/>
      <xs:element type="commodityDigitalAveragePriceOptionData" name="CommodityDigitalAveragePriceOptionData"/>
      <xs:element type="commodityDigitalOptionData" name="CommodityDigitalOptionData"/>
      <xs:element type="commoditySpreadOptionData" name="CommoditySpreadOptionData"/>
      <xs:element type="commoditySwapData" name="CommoditySwapData"/>
      <xs:element type="commoditySwaptionData" name="CommoditySwaptionData"/>
      <xs:element type="commodityAveragePriceOptionData" name="CommodityAveragePriceOptionData"/>
      <xs:element type="commodityOptionStripData" name="CommodityOptionStripData"/>
      <xs:element type="commodityPositionData" name="CommodityPositionData"/>
      <xs:element type="singleUnderlyingAsianOptionData" name="EquityAsianOptionData" />
      <xs:element type="singleUnderlyingAsianOptionData" name="FxAsianOptionData" />
      <xs:element type="singleUnderlyingAsianOptionData" name="CommodityAsianOptionData" />
      <xs:element type="bondOptionData" name="BondOptionData"/>
      <xs:element type="bondRepoData" name="BondRepoData"/>
      <xs:element type="bondTRSData" name="BondTRSData"/>
      <xs:element type="cdoData" name="CdoData"/>
      <xs:element type="creditLinkedSwapData" name="CreditLinkedSwapData"/>
      <xs:element type="indexCreditDefaultSwapData" name="IndexCreditDefaultSwapData"/>
      <xs:element type="indexCreditDefaultSwapOptionData" name="IndexCreditDefaultSwapOptionData"/>
      <xs:element type="ascotData" name="AscotData"/>
      <xs:element type="convertibleBondData" name="ConvertibleBondData"/>
      <xs:element type="callableBondData" name="CallableBondData"/>
      <xs:element type="tlockData" name="TreasuryLockData"/>
      <xs:element type="rpaData" name="RiskParticipationAgreementData"/>
      <xs:element type="cbodata" name="CBOData"/>
      <xs:element type="bondBasketData" name="BondBasketData"/>
      <xs:element type="equityPositionData" name="EquityPositionData"/>
      <xs:element type="equityOptionPositionData" name="EquityOptionPositionData"/>
      <xs:element type="totalReturnSwapData" name="TotalReturnSwapData"/>
      <xs:element type="totalReturnSwapData" name="ContractForDifferenceData"/>
      <xs:element type="compositeTradeData" name="CompositeTradeData"/>
      <xs:element type="pairwiseVarianceSwapData1" name="PairwiseVarianceSwapData"/>
      <xs:element type="pairwiseVarianceSwapData2" name="EquityPairwiseVarianceSwapData"/>
      <xs:element type="pairwiseVarianceSwapData2" name="FxPairwiseVarianceSwapData"/>
      <xs:element type="eqOutperformanceOptionData" name="EquityOutperformanceOptionData"/>
      <xs:element type="flexiSwapData" name="FlexiSwapData"/>
      <xs:element type="bgSwapData" name="BalanceGuaranteedSwapData"/>
      <xs:element type="commodityRevenueOptionData" name="CommodityRevenueOptionData"/>
      <!-- scripted trades -->
      <xs:element type="basketVarianceSwapData" name="BasketVarianceSwapData"/>
      <xs:element type="basketVarianceSwapData2" name="EquityBasketVarianceSwapData"/>
      <xs:element type="basketVarianceSwapData2" name="FxBasketVarianceSwapData"/>
      <xs:element type="basketVarianceSwapData2" name="CommodityBasketVarianceSwapData"/>
      <xs:element type="extendedAccumulatorData" name="ExtendedAccumulatorData"/>
      <xs:element type="varianceOptionData" name="VarianceOptionData"/>
      <xs:element type="varianceDispersionSwapData" name="VarianceDispersionSwapData"/>
      <xs:element type="kikoVarianceSwapData" name="KIKOVarianceSwapData"/>
      <xs:element type="corridorVarianceSwapData" name="CorridorVarianceSwapData"/>
      <xs:element type="indexedCorridorVarianceSwapData" name="IndexedCorridorVarianceSwapData"/>
      <xs:element type="kikoCorridorVarianceSwapData" name="KIKOCorridorVarianceSwapData"/>
      <xs:element type="corridorVarianceDispersionSwapData" name="CorridorVarianceDispersionSwapData"/>
      <xs:element type="koCorridorVarianceDispersionSwapData" name="KOCorridorVarianceDispersionSwapData"/>
      <xs:element type="pairwiseGeometricVarianceDispersionSwapData" name="PairwiseGeometricVarianceDispersionSwapData"/>
      <xs:element type="conditionalVarianceSwap01Data" name="ConditionalVarianceSwap01Data"/>
      <xs:element type="conditionalVarianceSwap02Data" name="ConditionalVarianceSwap02Data"/>
      <xs:element type="gammaSwapData" name="GammaSwapData"/>
      <xs:element type="bestEntryOptionData" name="BestEntryOptionData"/>
      <xs:element type="dualEuroBinaryOptionData" name="DualEuroBinaryOptionData"/>
      <xs:element type="dualEuroBinaryOptionDoubleKOData" name="DualEuroBinaryOptionDoubleKOData"/>
      <xs:element type="volBarrierOptionData" name="VolatilityBarrierOptionData"/>
      <xs:element type="forwardVolatilityAgreementData2" name="FxForwardVolatilityAgreementData"/>
      <xs:element type="forwardVolatilityAgreementData2" name="EquityForwardVolatilityAgreementData"/>
      <xs:element type="forwardVolatilityAgreementData2" name="CommodityForwardVolatilityAgreementData"/>
      <xs:element type="tarfData2" name="FxTaRFData"/>
      <xs:element type="tarfData2" name="EquityTaRFData"/>
      <xs:element type="tarfData2" name="CommodityTaRFData"/>
      <xs:element type="accumulatorData" name="FxAccumulatorData"/>
      <xs:element type="accumulatorData" name="EquityAccumulatorData"/>
      <xs:element type="accumulatorData" name="CommodityAccumulatorData"/>
      <xs:element type="callableRangeAccrualData" name="CallableRangeAccrualData"/>
      <xs:element type="windowBarrierOptionData2" name="FxWindowBarrierOptionData"/>
      <xs:element type="windowBarrierOptionData2" name="EquityWindowBarrierOptionData"/>
      <xs:element type="windowBarrierOptionData2" name="CommodityWindowBarierOptionData"/>
      <xs:element type="basketOptionData" name="EquityBasketOptionData"/>
      <xs:element type="basketOptionData" name="FxBasketOptionData"/>
      <xs:element type="basketOptionData" name="CommodityBasketOptionData"/>
      <xs:element type="genericBarrierOptionData" name="FxGenericBarrierOptionData"/>
      <xs:element type="genericBarrierOptionData" name="EquityGenericBarrierOptionData"/>
      <xs:element type="genericBarrierOptionData" name="CommodityGenericBarrierOptionData"/>
      <xs:element type="rainbowOptionData" name="EquityRainbowOptionData"/>
      <xs:element type="rainbowOptionData" name="FxRainbowOptionData"/>
      <xs:element type="rainbowOptionData" name="CommodityRainbowOptionData"/>
      <xs:element type="autocallable01Data" name="Autocallable01Data"/>
      <xs:element type="doubleDigitalOptionData" name="DoubleDigitalOptionData"/>
      <xs:element type="performanceOption01Data" name="PerformanceOption01Data"/>
      <xs:element type="scriptedTradeData" name="ScriptedTradeData"/>
      <xs:element type="vanillaBasketOptionData" name="VanillaBasketOptionData"/>
      <xs:element type="asianBasketOptionData" name="AsianBasketOptionData"/>
      <xs:element type="averageStrikeBasketOptionData" name="AverageStrikeBasketOptionData"/>
      <xs:element type="lookbackCallBasketOptionData" name="LookbackCallBasketOptionData"/>
      <xs:element type="lookbackPutBasketOptionData" name="LookbackPutBasketOptionData"/>
      <xs:element type="bestOfAirbagData" name="BestOfAirbagData"/>
      <xs:element type="worstOfBasketSwapData" name="WorstOfBasketSwapData"/>
      <xs:element type="worstOfBasketSwapData2" name="FxWorstOfBasketSwapData"/>
      <xs:element type="worstOfBasketSwapData2" name="EquityWorstOfBasketSwapData"/>
      <xs:element type="worstOfBasketSwapData2" name="CommodityWorstOfBasketSwapData"/>
      <xs:element type="worstPerformanceRainbowOption01Data" name="WorstPerformanceRainbowOption01Data"/>
      <xs:element type="worstPerformanceRainbowOption02Data" name="WorstPerformanceRainbowOption02Data"/>
      <xs:element type="worstPerformanceRainbowOption03Data" name="WorstPerformanceRainbowOption03Data"/>
      <xs:element type="worstPerformanceRainbowOption04Data" name="WorstPerformanceRainbowOption04Data"/>
      <xs:element type="worstPerformanceRainbowOption05Data" name="WorstPerformanceRainbowOption05Data"/>
      <xs:element type="worstPerformanceRainbowOption06Data" name="WorstPerformanceRainbowOption06Data"/>
      <xs:element type="worstPerformanceRainbowOption07Data" name="WorstPerformanceRainbowOption07Data"/>
      <xs:element type="worstPerformanceRainbowOption08Data" name="WorstPerformanceRainbowOption08Data"/>
      <xs:element type="worstPerformanceRainbowOption09Data" name="WorstPerformanceRainbowOption09Data"/>
      <xs:element type="bestOfAssetOrCashRainbowOptionData" name="BestOfAssetOrCashRainbowOptionData"/>
      <xs:element type="worstOfAssetOrCashRainbowOptionData" name="WorstOfAssetOrCashRainbowOptionData"/>
      <xs:element type="minRainbowOptionData" name="MinRainbowOptionData"/>
      <xs:element type="maxRainbowOptionData" name="MaxRainbowOptionData"/>
      <xs:element type="windowBarrierOptionData" name="WindowBarrierOptionData"/>
      <xs:element type="accumulator01Data" name="Accumulator01Data"/>
      <xs:element type="accumulator02Data" name="Accumulator02Data"/>
      <xs:element type="bestEntryOptionData2" name="EquityBestEntryOptionData"/>
      <xs:element type="bestEntryOptionData2" name="FxBestEntryOptionData"/>
      <xs:element type="bestEntryOptionData2" name="CommodityBestEntryOptionData"/>
      <xs:element type="tarfData" name="TaRFData"/>
      <xs:element type="europeanRainbowCallSpreadOptionData" name="EuropeanRainbowCallSpreadOptionData"/>
      <xs:element type="rainbowCallSpreadBarrierOptionData" name="RainbowCallSpreadBarrierOptionData"/>
      <xs:element type="asianRainbowCallSpreadOptionData" name="AsianRainbowCallSpreadOptionData"/>
      <xs:element type="asianIrCapFloorData" name="AsianIrCapFloorData"/>
      <xs:element type="forwardVolatilityAgreementData" name="ForwardVolatilityAgreementData"/>
      <xs:element type="correlationSwapData" name="CorrelationSwapData"/>
      <xs:element type="assetLinkedCliquetOptionData" name="AssetLinkedCliquetOptionData"/>
      <xs:element type="constantMaturityVolatilitySwapData" name="ConstantMaturityVolatilitySwapData"/>
      <xs:element type="cmsCapFloorBarrierData" name="CMSCapFloorBarrierData"/>
      <xs:element type="fixedStrikeForwardStartingOptionData" name="FixedStrikeForwardStartingOptionData"/>
      <xs:element type="floatingStrikeForwardStartingOptionData" name="FloatingStrikeForwardStartingOptionData"/>
      <xs:element type="forwardStartingSwaptionData" name="ForwardStartingSwaptionData"/>
      <xs:element type="flooredAverageCPIZCIISData" name="FlooredAverageCPIZCIISData"/>
      <xs:element type="genericBarrierOptionDataRaw" name="GenericBarrierOptionData"/>
      <xs:element type="movingMaxYYIISData" name="MovingMaxYYIISData"/>
      <xs:element type="irregularYYIISData" name="IrregularYYIISData"/>
      <xs:element type="europeanOptionBarrierData" name="EuropeanOptionBarrierData"/>
      <xs:element type="ladderLockInOptionData" name="LadderLockInOptionData"/>
      <xs:element type="lapseHedgeSwapData" name="LapseHedgeSwapData"/>
      <xs:element type="knockOutSwapData" name="KnockOutSwapData"/>
      <xs:element type="LPISwapData" name="LPISwapData"/>
      <xs:element type="cashPositionData" name="CashPositionData"/>
      <xs:element type="strikeResettableOptionData" name="StrikeResettableOptionData"/>
      <xs:element type="strikeResettableOptionData2" name="EquityStrikeResettableOptionData"/>
      <xs:element type="strikeResettableOptionData2" name="FxStrikeResettableOptionData"/>
      <xs:element type="strikeResettableOptionData2" name="CommodityStrikeResettableOptionData"/>
    </xs:choice>
  </xs:group>

  <xs:complexType name="envelope">
    <xs:all>
      <xs:element type="xs:string" name="CounterParty" minOccurs="0"/>
      <xs:element ref="nettingSetGroup" minOccurs="0"/>
      <xs:element name="PortfolioIds" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:string" name="PortfolioId" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="AdditionalFields" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:any processContents="lax" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="nettingSetDetails">
    <xs:all>
      <xs:element type="xs:string" name="NettingSetId"/>
      <xs:element type="xs:string" minOccurs="0" name="AgreementType"/>
      <xs:element type="xs:string" minOccurs="0" name="CallType"/>
      <xs:element type="xs:string" minOccurs="0" name="InitialMarginType"/>
      <xs:element type="xs:string" minOccurs="0" name="LegalEntityId"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="strikePriceData">
    <xs:all>
      <xs:element type="extendedCurrencyCode" name="Currency" minOccurs="0"/>
      <xs:element type="xs:float" name="Value"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="strikeYieldData">
    <xs:all>
      <xs:element type="compounding" name="Compounding" minOccurs="0"/>
      <xs:element type="xs:float" name="Yield"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="levelData">
    <xs:all>
      <xs:element type="xs:string" name="Currency"/>
      <xs:element type="xs:string" name="Value"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="tradeActions">
    <xs:sequence>
      <xs:element type="tradeAction" name="TradeAction" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="tradeAction">
    <xs:all>
      <xs:element type="tradeActionType" name="Type"/>
      <xs:element type="tradeActionOwner" name="Owner"/>
      <xs:element type="scheduleData" name="Schedule"/>
    </xs:all>
  </xs:complexType>


  <xs:complexType name="swapData">
    <xs:sequence>
      <xs:element type="xs:boolean" name="RoundNettedFloatingLegs" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="NettingPrecision" minOccurs="0" maxOccurs="1"/>
      <xs:element type="settlementType" name="Settlement" maxOccurs="1" minOccurs="0"/>
      <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="1"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="swaptionData">
    <xs:choice>
      <xs:sequence>
        <xs:element type="optionData" name="OptionData"/>
        <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="1"/>
      </xs:sequence>
      <xs:sequence>
        <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="1"/>
        <xs:element type="optionData" name="OptionData"/>
      </xs:sequence>
    </xs:choice>
  </xs:complexType>

  <xs:complexType name="swaptionStraddleData">
    <xs:choice>
      <xs:sequence>
        <xs:element type="xs:string" name="LongShort" maxOccurs="unbounded" minOccurs="0"/>
        <xs:element type="optionData" name="OptionData"/>
        <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="1"/>
      </xs:sequence>
      <xs:sequence>
        <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="1"/>
        <xs:element type="optionData" name="OptionData"/>
        <xs:element type="xs:string" name="LongShort" maxOccurs="unbounded" minOccurs="0"/>
      </xs:sequence>
    </xs:choice>
  </xs:complexType>

  <xs:complexType name="optionPaymentData">
    <xs:choice>
      <xs:element name="Dates">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="date" name="Date" minOccurs="1" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Rules">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:nonNegativeInteger" name="Lag"/>
            <xs:element type="calendar" name="Calendar"/>
            <xs:element type="businessDayConvention" name="Convention"/>
            <xs:element type="optionPayRelativeTo" name="RelativeTo" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:choice>
  </xs:complexType>

  <xs:complexType name="optionExerciseData">
    <xs:all>
      <xs:element type="date" name="Date"/>
      <xs:element type="xs:decimal" name="Price" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="optionData">
    <xs:all>
      <xs:element type="xs:string" name="LongShort"/>
      <xs:element type="xs:string" name="OptionType" minOccurs="0"/>
      <xs:element type="xs:string" name="PayoffType" minOccurs="0"/>
      <xs:element type="xs:string" name="PayoffType2" minOccurs="0"/>
      <xs:element type="xs:string" name="Style" minOccurs="0"/>
      <xs:element type="xs:string" name="NoticePeriod" minOccurs="0"/>
      <xs:element type="xs:string" name="NoticeCalendar" minOccurs="0"/>
      <xs:element type="xs:string" name="NoticeConvention" minOccurs="0"/>
      <xs:element type="xs:string" name="MidCouponExercise" minOccurs="0"/>
      <xs:element type="settlementType" name="Settlement" minOccurs="0"/>
      <xs:element type="settlementMethod" name="SettlementMethod" minOccurs="0"/>
      <xs:element type="xs:string" name="PayOffAtExpiry" minOccurs="0"/>
      <xs:element type="xs:string" name="PremiumAmount" minOccurs="0"/>
      <xs:element type="premiumCurrencyCode" name="PremiumCurrency" minOccurs="0"/>
      <xs:element type="xs:string" name="PremiumPayDate" minOccurs="0"/>
      <xs:element type="premiumData" name="Premiums" minOccurs="0"/>
      <xs:element type="xs:string" name="ExercisePrices" minOccurs="0"/>
      <xs:element name="ExerciseFees" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="ExerciseFee" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="type" type="xs:string" />
                    <xs:attribute name="startDate" type="xs:string" />
                    <xs:attribute name="currency" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:string" name="ExerciseFeeSettlementPeriod" minOccurs="0"/>
      <xs:element type="xs:string" name="ExerciseFeeSettlementCalendar" minOccurs="0"/>
      <xs:element type="xs:string" name="ExerciseFeeSettlementConvention" minOccurs="0"/>
      <xs:element ref="exerciseDatesGroup" minOccurs="0" />
      <xs:element type="bool" name="AutomaticExercise" minOccurs="0"/>
      <xs:element type="optionExerciseData" name="ExerciseData" minOccurs="0"/>
      <xs:element type="optionPaymentData" name="PaymentData" minOccurs="0"/>
      <xs:element name="SettlementData" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="currencyCode" name="PayCurrency"/>
            <xs:element type="xs:string" name="FXIndex"/>
            <xs:element type="xs:string" name="FixingDate" minOccurs="0" maxOccurs="1"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondData">
    <xs:sequence>
      <xs:element type="xs:string" name="IssuerId" minOccurs="0"/>
      <xs:element type="xs:string" name="CreditCurveId" minOccurs="0"/>
      <xs:element type="xs:string" name="CreditGroup" minOccurs="0"/>
      <xs:element type="xs:string" name="SecurityId"/>
      <xs:element type="xs:string" name="ReferenceCurveId" minOccurs="0"/>
      <xs:element type="xs:string" name="IncomeCurveId" minOccurs="0"/>
      <xs:element type="xs:string" name="VolatilityCurveId" minOccurs="0"/>
      <xs:element type="xs:string" name="SettlementDays" minOccurs="0"/>
      <xs:element type="xs:string" name="Calendar" minOccurs="0"/>
      <xs:element type="xs:string" name="IssueDate" minOccurs="0"/>
      <xs:element type="xs:string" name="PriceQuoteMethod" minOccurs="0"/>
      <xs:element type="xs:string" name="PriceQuoteBaseValue" minOccurs="0"/>
      <xs:element type="xs:string" name="BondNotional" minOccurs="0"/>
      <xs:element type="bondPriceType" name="PriceType" minOccurs="0"/>
      <xs:element type="xs:string" name="Payer" minOccurs="0"/>
      <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="0"/>
      <xs:element type="bool" name="CreditRisk" minOccurs="0"/>
      <xs:element type="xs:string" name="SubType" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="forwardBondData">
    <xs:all>
      <xs:element type="bondData" name="BondData"/>
      <xs:element type="settlementData" name="SettlementData"/>
      <xs:element name="PremiumData" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Amount"/>
            <xs:element type="xs:string" name="Date"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:string" name="LongInForward"/>
      <xs:element type="xs:boolean" name="KnockOut" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondFutureData">
    <xs:all>
      <xs:element type="xs:string" name="ContractName"/>
      <xs:element type="xs:string" name="ContractNotional"/>
      <xs:element type="xs:string" name="LongShort"/>
      <xs:element type="xs:boolean" name="ApplyConversionFactor" minOccurs="0"/>
      <xs:element type="xs:boolean" name="UseFuturePrice" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondFutureOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:string" name="ContractName"/>
      <xs:element type="xs:float" name="ContractNotional"/>
      <xs:element ref="strikeGroup"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="deliveryBasket">
    <xs:sequence>
      <xs:element type="xs:string" name="Id" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="varianceSwapData">
    <xs:all>
      <xs:element type="date" name="StartDate"/>
      <xs:element type="date" name="EndDate"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="xs:string" name="LongShort"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element type="calendar" name="Calendar"/>
      <xs:element type="momentType" name="MomentType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:boolean" name="AddPastDividends" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cliquetOptionData">
    <xs:all>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element type="longShort" name="LongShort"/>
      <xs:element type="optionType" name="OptionType"/>
      <xs:element type="xs:float" name="Moneyness"/>
      <xs:element type="xs:float" name="LocalCap" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="LocalFloor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="GlobalCap" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="GlobalFloor" minOccurs="0" maxOccurs="1"/>
      <xs:element type="scheduleData" name="ScheduleData"/>
      <xs:element type="xs:integer" name="SettlementDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="Premium" minOccurs="0" maxOccurs="1"/>
      <xs:element type="date" name="PremiumPaymentDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="currencyCode" name="PremiumCurrency" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="settlementData">
    <xs:all>
      <xs:element type="xs:string" name="ForwardMaturityDate"/>
      <xs:element type="xs:string" name="ForwardSettlementDate" minOccurs="0"/>
      <xs:element type="xs:string" name="Settlement" minOccurs="0"/>
      <xs:element type="xs:float" name="Amount" minOccurs="0"/>
      <xs:element type="xs:float" name="LockRate" minOccurs="0"/>
      <xs:element type="xs:float" name="dv01" minOccurs="0"/>
      <xs:element type="xs:string" name="LockRateDayCounter" minOccurs="0"/>
      <xs:element type="xs:string" name="SettlementDirty" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:element name="exerciseDatesGroup" abstract="true" />
  <xs:element name="ExerciseDates" substitutionGroup="exerciseDatesGroup">
    <xs:complexType>
      <xs:sequence>
        <xs:element type="date" name="ExerciseDate" minOccurs="0" maxOccurs="unbounded"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
  <xs:element name="ExerciseSchedule" type="scheduleData" substitutionGroup="exerciseDatesGroup" />

  <xs:element name="legDataType" abstract="true"/>
  <xs:element name="CashflowData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element name="Cashflow">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="Amount" maxOccurs="unbounded">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute name="date" type="xs:string" />
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="FixedLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element name="Rates">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="Rate" minOccurs="0" maxOccurs="unbounded">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute name="startDate" type="xs:string" />
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="FloatingLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="indexNameType" name="Index"/>
        <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="LastRecentPeriod" minOccurs="0"/>
        <xs:element type="calendar" name="LastRecentPeriodCalendar" minOccurs="0"/>
        <xs:element type="xs:nonNegativeInteger" name="FixingDays" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="Lookback" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="RateCutoff" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IsAveraged" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="HasSubPeriods" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IncludeSpread" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IsNotResettingXCCY" minOccurs="0" maxOccurs="1"/>
        <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
        <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
        <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
        <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="LocalCapFloor" minOccurs="0"/>
        <xs:element type="scheduleData" name="FixingSchedule" minOccurs="0"/>
        <xs:element type="scheduleData" name="ResetSchedule" minOccurs="0"/>
        <xs:element type="tradeLevelFixings" name="HistoricalFixings" minOccurs="0"/>
        <xs:element type="stubInterpolation" name="FrontStubInterpolation" minOccurs="0" maxOccurs="1"/>
        <xs:element type="stubInterpolation" name="BackStubInterpolation" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="StubUseOriginalCurve" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="ObservationShift" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="RoundingPrecision" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="RangeAccrualLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:sequence>
        <xs:element ref="FloatingLegData" minOccurs="0" maxOccurs="1"/>
        <xs:element name="Coupons" minOccurs="0" maxOccurs="1">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="Coupon" minOccurs="0" maxOccurs="unbounded">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute name="startDate" type="xs:string" />
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="UpperBounds" minOccurs="0" maxOccurs="1">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="UpperBound" maxOccurs="unbounded">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute name="startDate" type="xs:string" />
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="LowerBounds" minOccurs="0" maxOccurs="1">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="LowerBound" maxOccurs="unbounded">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute name="startDate" type="xs:string" />
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
  <xs:element name="CPILegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:choice maxOccurs="unbounded" minOccurs="0">
        <xs:element type="xs:string" name="Index" minOccurs="1" maxOccurs="1"/>
        <xs:element name="Rates">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="Rate" maxOccurs="unbounded">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute name="startDate" type="xs:string" />
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element type="xs:float" name="BaseCPI" minOccurs="0" maxOccurs="1"/>
        <xs:element type="date" name="StartDate" minOccurs="0"/>
        <xs:element type="xs:string" name="ObservationLag" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="Interpolation" minOccurs="0" maxOccurs="1"/>
        <xs:element type="bool" name="Interpolated" minOccurs="0" maxOccurs="1"/>
        <xs:element type="bool" name="SubtractInflationNotional" maxOccurs="1"/>
        <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
        <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:float" name="FinalFlowCap" minOccurs="0"/>
        <xs:element type="xs:float" name="FinalFlowFloor" minOccurs="0"/>
        <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="SubtractInflationNotionalAllCoupons" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="BaseCPIBase" minOccurs="0" maxOccurs="1"/>
      </xs:choice>
    </xs:complexType>
  </xs:element>
  <xs:element name="YYLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="xs:string" name="Index"/>
        <xs:element type="xs:integer" name="FixingDays" maxOccurs="1"/>
        <xs:element type="xs:string" name="ObservationLag" maxOccurs="1"/>
        <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
        <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
        <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
        <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="AddInflationNotional" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IrregularYoY" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="CMSLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="indexNameType" name="Index"/>
        <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1"/>
        <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
        <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
        <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
        <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="CMBLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="xs:string" name="Index"/>
        <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="FixingDays"/>
        <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
        <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
        <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
        <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="CreditRisk" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="DigitalCMSLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:choice maxOccurs="unbounded" minOccurs="0">
        <xs:element name="CMSLegData">
          <xs:complexType>
            <xs:all>
              <xs:element type="indexNameType" name="Index"/>
              <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
              <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1"/>
              <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
              <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
              <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
              <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
              <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
            </xs:all>
          </xs:complexType>
        </xs:element>
        <xs:element type="xs:string" name="CallPosition"/>
        <xs:element type="xs:boolean" name="IsCallATMIncluded"/>
        <xs:element name="CallStrikes">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:float" name="Strike" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="CallPayoffs">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:float" name="Payoff" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element type="xs:string" name="PutPosition"/>
        <xs:element type="xs:boolean" name="IsPutATMIncluded"/>
        <xs:element name="PutStrikes">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:float" name="Strike" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="PutPayoffs">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:float" name="Payoff" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:choice>
    </xs:complexType>
  </xs:element>
  <xs:element name="DurationAdjustedCMSLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="indexNameType" name="Index"/>
        <xs:element type="xs:integer" name="Duration" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1"/>
        <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
        <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
        <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
        <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="CMSSpreadLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="indexNameType" name="Index1"/>
        <xs:element type="indexNameType" name="Index2"/>
        <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1"/>
        <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
        <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
        <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
        <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="DigitalCMSSpreadLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:choice maxOccurs="unbounded" minOccurs="0">
        <xs:element name="CMSSpreadLegData">
          <xs:complexType>
            <xs:all>
              <xs:element type="indexNameType" name="Index1"/>
              <xs:element type="indexNameType" name="Index2"/>
              <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
              <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1"/>
              <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
              <xs:element type="caps" name="Caps" minOccurs="0" maxOccurs="1"/>
              <xs:element type="floors" name="Floors" minOccurs="0" maxOccurs="1"/>
              <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
              <xs:element type="xs:boolean" name="NakedOption" minOccurs="0" maxOccurs="1"/>
            </xs:all>
          </xs:complexType>
        </xs:element>
        <xs:element type="xs:string" name="CallPosition"/>
        <xs:element type="xs:boolean" name="IsCallATMIncluded"/>
        <xs:element name="CallStrikes">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:float" name="Strike" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="CallPayoffs">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:float" name="Payoff" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element type="xs:string" name="PutPosition"/>
        <xs:element type="xs:boolean" name="IsPutATMIncluded"/>
        <xs:element name="PutStrikes">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:float" name="Strike" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element name="PutPayoffs">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:float" name="Payoff" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:choice>
    </xs:complexType>
  </xs:element>
  <xs:element name="EquityLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="xs:float" name="Quantity" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="ReturnType" minOccurs="1" maxOccurs="1"/>
        <xs:element ref="underlyingTypes"/>
        <xs:element type="xs:float" name="InitialPrice" minOccurs="0" maxOccurs="1"/>
        <xs:element type="extendedCurrencyCode" name="InitialPriceCurrency" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:float" name="DividendFactor" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="NotionalReset" minOccurs="0" maxOccurs="1"/>
        <xs:element type="scheduleData" name="ValuationSchedule" minOccurs="0" maxOccurs="1" />
        <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1" />
        <xs:element name="FXTerms" minOccurs="0" maxOccurs="1">
          <xs:complexType>
            <xs:all>
              <xs:element type="extendedCurrencyCode" name="EquityCurrency" minOccurs="0" maxOccurs="1"/>
              <xs:element type="xs:string" name="FXIndex" minOccurs="0" maxOccurs="1"/>
              <xs:element type="xs:integer" name="FXIndexFixingDays" minOccurs="0" maxOccurs="1"/>
              <xs:element type="xs:string" name="FXIndexCalendar" minOccurs="0" maxOccurs="1"/>
            </xs:all>
          </xs:complexType>
        </xs:element>
      </xs:all>
    </xs:complexType>
  </xs:element>
  <xs:element name="ZeroCouponFixedLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:choice maxOccurs="unbounded" minOccurs="0">
        <xs:element name="Rates">
          <xs:complexType>
            <xs:sequence>
              <xs:element name="Rate" maxOccurs="unbounded">
                <xs:complexType>
                  <xs:simpleContent>
                    <xs:extension base="xs:float">
                      <xs:attribute name="startDate" type="xs:string" />
                    </xs:extension>
                  </xs:simpleContent>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element type="xs:string" name="Compounding" maxOccurs="1"/>
        <xs:element type="xs:string" name="SubtractNotional" maxOccurs="1"/>
      </xs:choice>
    </xs:complexType>
  </xs:element>
  <xs:complexType name="amortizationData">
    <xs:all>
      <xs:element type="amortizationType" name="Type"/>
      <xs:element type="xs:float" name="Value" minOccurs="0"/>
      <xs:element type="xs:string" name="StartDate" minOccurs="0"/>
      <xs:element type="xs:string" name="EndDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Frequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:boolean" name="Underflow" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="indexingData">
    <xs:all>
      <xs:element type="xs:float" name="Quantity" minOccurs="0"/>
      <xs:element type="xs:string" name="Index" minOccurs="0"/>
      <xs:element type="xs:integer" name="IndexFixingDays" minOccurs="0"/>
      <xs:element type="xs:string" name="IndexFixingCalendar" minOccurs="0"/>
      <xs:element type="xs:boolean" name="Dirty" minOccurs="0"/>
      <xs:element type="xs:boolean" name="Relative" minOccurs="0"/>
      <xs:element type="xs:boolean" name="ConditionalOnSurvival" minOccurs="0"/>
      <xs:element type="xs:float" name="InitialFixing" minOccurs="0"/>
      <xs:element type="xs:float" name="InitialNotionalFixing" minOccurs="0"/>
      <xs:element type="scheduleData" name="ValuationSchedule" minOccurs="0"/>
      <xs:element type="xs:integer" name="FixingDays" minOccurs="0"/>
      <xs:element type="xs:string" name="FixingCalendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="FixingConvention" minOccurs="0"/>
      <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="legData">
    <xs:all>
      <xs:element type="xs:boolean" name="Payer"/>
      <xs:element type="legType" name="LegType"/>
      <xs:element type="extendedCurrencyCode" name="Currency" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="PaymentConvention" minOccurs="0"/>
      <xs:element type="paymentLag" name="PaymentLag" minOccurs="0"/>
      <xs:element type="dateDeltaUnit" name="PaymentLagUnit" minOccurs="0"/>
      <xs:element type="dateDeltaAnchor" name="PaymentLagAnchor" minOccurs="0"/>
      <xs:element type="xs:integer" name="NotionalPaymentLag" minOccurs="0"/>
      <xs:element type="dateDeltaUnit" name="NotionalPaymentLagUnit" minOccurs="0"/>
      <xs:element type="dateDeltaAnchor" name="NotionalPaymentLagAnchor" minOccurs="0"/>
      <xs:element type="xs:string" name="PaymentCalendar" minOccurs="0"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0"/>
      <xs:element name="Amortizations" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="amortizationData" name="AmortizationData" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Notionals" minOccurs="0">
        <xs:complexType>
          <xs:choice minOccurs="0" maxOccurs="unbounded">
            <xs:element name="Notional" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:element type="fxreset" name="FXReset" minOccurs="0"/>
            <xs:element type="exchanges" name="Exchanges" minOccurs="0"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
      <xs:element type="scheduleData" name="ScheduleData" minOccurs="0"/>
      <xs:element name="PaymentDates" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="date" name="PaymentDate" minOccurs="1" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Indexings" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:boolean" name="FromAssetLeg" minOccurs="0"/>
            <xs:element type="indexingData" name="Indexing" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="dayCounter" name="LastPeriodDayCounter" minOccurs="0"/>
      <xs:element ref="legDataType" minOccurs="0"/>
      <xs:element type="xs:boolean" name="StrictNotionalDates" minOccurs="0"/>
      <xs:element type="scheduleData" name="PaymentSchedule" minOccurs="0"/>
	  <xs:element name="SettlementData" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="FXIndex"/>
            <xs:element type="xs:string" name="FixingDate" minOccurs="0" maxOccurs="1"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="legData_capfloor">
    <xs:all>
      <xs:element type="xs:boolean" name="Payer" minOccurs="0"/>
      <xs:element type="legType" name="LegType"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="dayCounter" name="DayCounter"/>
      <xs:element type="businessDayConvention" name="PaymentConvention" minOccurs="0"/>
      <xs:element type="paymentLag" name="PaymentLag" minOccurs="0"/>
      <xs:element type="dateDeltaUnit" name="PaymentLagUnit" minOccurs="0"/>
      <xs:element type="dateDeltaAnchor" name="PaymentLagAnchor" minOccurs="0"/>
      <xs:element type="xs:string" name="PaymentCalendar" minOccurs="0"/>
      <xs:element name="Notionals">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Notional" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
            <xs:choice minOccurs="0" maxOccurs="unbounded">
            <xs:element type="fxreset" name="FXReset" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element type="exchanges" name="Exchanges" minOccurs="0" maxOccurs="1"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="scheduleData" name="ScheduleData">
      </xs:element>
      <xs:element name="PaymentDates" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="date" name="PaymentDate" minOccurs="1" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element ref="legDataType"/>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="priceType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Spot"/>
      <xs:enumeration value="FutureSettlement"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="quantitiesType">
    <xs:sequence>
      <xs:element name="Quantity" minOccurs="1" maxOccurs="unbounded">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:float">
              <xs:attribute name="startDate" type="xs:string" />
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="pricesType">
    <xs:sequence>
      <xs:element name="Price" minOccurs="1" maxOccurs="unbounded">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:float">
              <xs:attribute name="startDate" type="xs:string" />
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:simpleType name="commodityQuantityFrequencyType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="PerCalculationPeriod"/>
      <xs:enumeration value="PerCalendarDay"/>
      <xs:enumeration value="PerPricingDay"/>
      <xs:enumeration value="PerHour"/>
      <xs:enumeration value="PerHourAndCalendarDay"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="commodityPayRelativeToType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="CalculationPeriodStartDate"/>
      <xs:enumeration value="CalculationPeriodEndDate"/>
      <xs:enumeration value="FutureExpiryDate"/>
      <xs:enumeration value="TerminationDate"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="pricingDateRuleType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="FutureExpiryDate"/>
      <xs:enumeration value="None"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:element name="EquityMarginLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
            <xs:element type="xs:float" name="InitialMarginFactor" minOccurs="1" maxOccurs="1"/>
            <xs:element type="xs:float" name="Multiplier" minOccurs="1" maxOccurs="1"/>
            <xs:element name="Rates">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Rate" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:float">
                          <xs:attribute name="startDate" type="xs:string" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          <xs:element name="EquityLegData">
            <xs:complexType>
              <xs:all>
                <xs:element type="xs:float" name="Quantity" minOccurs="0" maxOccurs="1"/>
                <xs:element type="xs:string" name="ReturnType" minOccurs="1" maxOccurs="1"/>
                <xs:element ref="underlyingTypes"/>
                <xs:element type="xs:float" name="InitialPrice" minOccurs="0" maxOccurs="1"/>
                <xs:element type="extendedCurrencyCode" name="InitialPriceCurrency" minOccurs="0" maxOccurs="1"/>
                <xs:element type="xs:float" name="DividendFactor" minOccurs="0" maxOccurs="1"/>
                <xs:element type="xs:boolean" name="NotionalReset" minOccurs="0" maxOccurs="1"/>
                <xs:element type="scheduleData" name="ValuationSchedule" minOccurs="0" maxOccurs="1" />
                <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1" />
                <xs:element name="FXTerms" minOccurs="0" maxOccurs="1">
                  <xs:complexType>
                    <xs:all>
                      <xs:element type="extendedCurrencyCode" name="EquityCurrency" minOccurs="0" maxOccurs="1"/>
                      <xs:element type="xs:string" name="FXIndex" minOccurs="0" maxOccurs="1"/>
                      <xs:element type="xs:integer" name="FXIndexFixingDays" minOccurs="0" maxOccurs="1"/>
                      <xs:element type="xs:string" name="FXIndexCalendar" minOccurs="0" maxOccurs="1"/>
                    </xs:all>
                  </xs:complexType>
                </xs:element>
              </xs:all>
            </xs:complexType>
          </xs:element>
      </xs:all>
    </xs:complexType>
  </xs:element>

  <xs:element name="CommodityFixedLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="quantitiesType" name="Quantities" minOccurs="0"/>
        <xs:element type="pricesType" name="Prices"/>
        <xs:element type="commodityPayRelativeToType" name="CommodityPayRelativeTo" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="Tag" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>

  <xs:element name="CommodityFloatingLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="xs:string" name="Name"/>
        <xs:element type="priceType" name="PriceType"/>
        <xs:element type="quantitiesType" name="Quantities"/>
        <xs:element type="commodityQuantityFrequencyType" name="CommodityQuantityFrequency" minOccurs="0" maxOccurs="1"/>
        <xs:element type="commodityPayRelativeToType" name="CommodityPayRelativeTo" minOccurs="0" maxOccurs="1"/>
        <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
        <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
        <xs:element type="pricingDateRuleType" name="PricingDateRule" minOccurs="0" maxOccurs="1"/>
        <xs:element type="calendar" name="PricingCalendar" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="PricingLag" minOccurs="0" maxOccurs="1"/>
        <xs:element name="PricingDates" minOccurs="0" maxOccurs="1">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="date" name="PricingDate" minOccurs="1" maxOccurs="unbounded"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element type="xs:boolean" name="IsAveraged" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="FutureMonthOffset" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:nonNegativeInteger" name="DeliveryRollDays" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:nonNegativeInteger" name="DailyExpiryOffset" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IncludePeriodEnd" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="ExcludePeriodStart" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:nonNegativeInteger" name="HoursPerDay" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="UseBusinessDays" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="UnrealisedQuantity" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:positiveInteger" name="LastNDays" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="Tag" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="FXIndex" minOccurs="0"/>
        <xs:element type="xs:nonNegativeInteger" name="AvgPricePrecision" minOccurs="0"/>
      </xs:all>
    </xs:complexType>
  </xs:element>

  <xs:element name="IntradayPowerFloatingLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="xs:string" name="Name"/>
        <xs:element type="quantitiesType" name="Quantities"/>
        <xs:element type="spreads" name="Spreads" minOccurs="0" maxOccurs="1"/>
        <xs:element type="gearings" name="Gearings" minOccurs="0" maxOccurs="1"/>
        <xs:element type="calendar" name="PricingCalendar" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IncludePeriodStart" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="IncludePeriodEnd" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:boolean" name="BusinessDays" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="PowerLoadProfileReference" minOccurs="0" maxOccurs="1"/>
        <xs:element type="powerLoadProfileDataType" name="PowerLoadProfileData" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="FXIndex" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:nonNegativeInteger" name="AvgPricePrecision" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="Tag" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:string" name="QuantityMode" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>

  <xs:complexType name="commoditySwapData">
    <xs:sequence>
      <xs:element type="xs:boolean" name="RoundNettedFloatingLegs" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="NettingPrecision" minOccurs="0" maxOccurs="1"/>
      <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="2"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="commoditySwaptionData">
    <xs:sequence>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="legData" name="LegData" maxOccurs="2" minOccurs="2"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="commoditySpreadOptionStripPaymentData">
    <xs:all>
      <xs:element type="scheduleData" name="OptionStripDefinition"/>
      <xs:element type="xs:integer" name="PaymentLag"/>
      <xs:element type="businessDayConvention" name="PaymentConvention"/>
      <xs:element type="calendar" name="PaymentCalendar"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="commoditySpreadOptionData">
    <xs:sequence>
      <xs:element type="legData" name="LegData" minOccurs="2" maxOccurs="2"/>
      <xs:element type="optionData" name="OptionData" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:double" name="SpreadStrike" minOccurs="1" maxOccurs="1"/>
      <xs:element type="commoditySpreadOptionStripPaymentData" name="OptionStripPaymentDates" minOccurs="0" maxOccurs="1"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="commodityAveragePriceOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="barrierData" name="BarrierData" minOccurs="0"/>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="priceType" name="PriceType"/>
      <xs:element type="date" name="StartDate"/>
      <xs:element type="date" name="EndDate"/>
      <xs:element type="calendar" name="PaymentCalendar"/>
      <xs:element type="paymentLag" name="PaymentLag"/>
      <xs:element type="businessDayConvention" name="PaymentConvention"/>
      <xs:element type="calendar" name="PricingCalendar"/>
      <xs:element type="date" name="PaymentDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="positiveDecimal" name="Gearing" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="Spread" minOccurs="0" maxOccurs="1"/>
      <xs:element type="commodityQuantityFrequencyType" name="CommodityQuantityFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="commodityPayRelativeToType" name="CommodityPayRelativeTo" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="FutureMonthOffset" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="DeliveryRollDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:boolean" name="IncludePeriodEnd" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FXIndex" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

    <xs:complexType name="commodityDigitalAveragePriceOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="barrierData" name="BarrierData" minOccurs="0"/>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="DigitalCashPayoff"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="priceType" name="PriceType"/>
      <xs:element type="date" name="StartDate"/>
      <xs:element type="date" name="EndDate"/>
      <xs:element type="calendar" name="PaymentCalendar"/>
      <xs:element type="paymentLag" name="PaymentLag"/>
      <xs:element type="businessDayConvention" name="PaymentConvention"/>
      <xs:element type="calendar" name="PricingCalendar"/>
      <xs:element type="date" name="PaymentDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="positiveDecimal" name="Gearing" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="Spread" minOccurs="0" maxOccurs="1"/>
      <xs:element type="commodityQuantityFrequencyType" name="CommodityQuantityFrequency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="commodityPayRelativeToType" name="CommodityPayRelativeTo" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="FutureMonthOffset" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:nonNegativeInteger" name="DeliveryRollDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:boolean" name="IncludePeriodEnd" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FXIndex" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="longShortsType">
    <xs:sequence>
      <xs:element type="longShort" name="LongShort" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="strikes">
    <xs:sequence>
      <xs:element type="xs:float" name="Strike" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="callsPutsType">
    <xs:all>
      <xs:element type="longShortsType" name="LongShorts" minOccurs="0" maxOccurs="1"/>
      <xs:element type="strikes" name="Strikes" minOccurs="0" maxOccurs="1"/>
      <xs:element type="barrierData" name="BarrierData" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="commodityOptionStripData">
    <xs:all>
      <xs:element type="legData" name="LegData" maxOccurs="1" minOccurs="1"/>
      <xs:element type="callsPutsType" name="Calls" maxOccurs="1" minOccurs="0"/>
      <xs:element type="callsPutsType" name="Puts" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:string" name="PremiumAmount" minOccurs="0"/>
      <xs:element type="premiumCurrencyCode" name="PremiumCurrency" minOccurs="0"/>
      <xs:element type="xs:string" name="PremiumPayDate" minOccurs="0"/>
      <xs:element type="premiumData" name="Premiums" minOccurs="0"/>
      <xs:element type="xs:string" name="Style" minOccurs="0"/>
      <xs:element type="settlementType" name="Settlement" minOccurs="0"/>
      <xs:element type="xs:boolean" name="IsDigital" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="PayoffPerUnit" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="oreTradeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="CompositeTrade"/>
      <xs:enumeration value="ConvertibleBond"/>
      <xs:enumeration value="CallableBond"/>
      <xs:enumeration value="Ascot"/>
      <xs:enumeration value="TotalReturnSwap"/>
      <xs:enumeration value="ContractForDifference"/>
      <xs:enumeration value="CBO"/>
      <xs:enumeration value="EquityPosition"/>
      <xs:enumeration value="EquityOptionPosition"/>
      <xs:enumeration value="BondPosition"/>
      <xs:enumeration value="IndexCreditDefaultSwap"/>
      <xs:enumeration value="IndexCreditDefaultSwapOption"/>
      <xs:enumeration value="SyntheticCDO"/>
      <xs:enumeration value="BondOption"/>
      <xs:enumeration value="BondTRS"/>
      <xs:enumeration value="BondRepo"/>
      <xs:enumeration value="CreditLinkedSwap"/>
      <xs:enumeration value="CrossCurrencySwap"/>
      <xs:enumeration value="InflationSwap"/>
      <xs:enumeration value="Swap"/>
      <xs:enumeration value="Swaption"/>
      <xs:enumeration value="SwaptionStraddle"/>
      <xs:enumeration value="ForwardRateAgreement"/>
      <xs:enumeration value="FxAverageForward"/>
      <xs:enumeration value="FxForward"/>
      <xs:enumeration value="FxOption"/>
      <xs:enumeration value="FxAsianOption"/>
      <xs:enumeration value="FxBarrierOption"/>
      <xs:enumeration value="FxDoubleBarrierOption"/>
      <xs:enumeration value="FxSwap"/>
      <xs:enumeration value="FxDigitalOption"/>
      <xs:enumeration value="FxEuropeanBarrierOption"/>
      <xs:enumeration value="FxKIKOBarrierOption"/>
      <xs:enumeration value="FxTouchOption"/>
      <xs:enumeration value="FxDoubleTouchOption"/>
      <xs:enumeration value="FxDigitalBarrierOption"/>
      <xs:enumeration value="FxVarianceSwap"/>
      <xs:enumeration value="CapFloor"/>
      <xs:enumeration value="EquityOption"/>
      <xs:enumeration value="EquityFutureOption"/>
      <xs:enumeration value="EquityAsianOption"/>
      <xs:enumeration value="EquityBarrierOption"/>
      <xs:enumeration value="EquityDoubleBarrierOption"/>
      <xs:enumeration value="EquityEuropeanBarrierOption"/>
      <xs:enumeration value="EquityTouchOption"/>
      <xs:enumeration value="EquityDoubleTouchOption"/>
      <xs:enumeration value="EquityDigitalOption"/>
      <xs:enumeration value="EquityForward"/>
      <xs:enumeration value="EquitySwap"/>
      <xs:enumeration value="EquityVarianceSwap"/>
      <xs:enumeration value="EquityCliquetOption"/>
      <xs:enumeration value="CommodityForward"/>
      <xs:enumeration value="IntradayPowerForward"/>
      <xs:enumeration value="CommodityOption"/>
      <xs:enumeration value="CommodityDigitalAveragePriceOption"/>
      <xs:enumeration value="CommodityDigitalOption"/>
      <xs:enumeration value="CommodityAsianOption"/>
      <xs:enumeration value="CommoditySwap"/>
      <xs:enumeration value="CommoditySwaption"/>
      <xs:enumeration value="CommoditySpreadOption"/>
      <xs:enumeration value="CommodityAveragePriceOption"/>
      <xs:enumeration value="CommodityOptionStrip"/>
      <xs:enumeration value="CommodityPosition"/>
      <xs:enumeration value="CommodityVarianceSwap"/>
      <xs:enumeration value="Bond"/>
      <xs:enumeration value="ForwardBond"/>
      <xs:enumeration value="FxForwardVolatilityAgreement"/>
      <xs:enumeration value="EquityForwardVolatilityAgreement"/>
      <xs:enumeration value="CommodityForwardVolatilityAgreement"/>
      <xs:enumeration value="BondFuture"/>
      <xs:enumeration value="BondFutureOption"/>
      <xs:enumeration value="CreditDefaultSwap"/>
      <xs:enumeration value="CreditDefaultSwapOption"/>
      <xs:enumeration value="Failed"/>
      <xs:enumeration value="FlexiSwap"/>
      <xs:enumeration value="BalanceGuaranteedSwap"/>
      <xs:enumeration value="CallableSwap"/>
      <xs:enumeration value="EquityOutperformanceOption"/>
      <xs:enumeration value="EquityPairwiseVarianceSwap"/>
      <xs:enumeration value="FxPairwiseVarianceSwap"/>
      <xs:enumeration value="CommodityPairwiseVarianceSwap"/>
      <!-- scripted trades -->
      <xs:enumeration value="ScriptedTrade"/>
      <xs:enumeration value="EquityBasketVarianceSwap"/>
      <xs:enumeration value="FxBasketVarianceSwap"/>
      <xs:enumeration value="CommodityBasketVarianceSwap"/>
      <xs:enumeration value="RiskParticipationAgreement"/>
      <xs:enumeration value="Autocallable_01"/>
      <xs:enumeration value="DoubleDigitalOption"/>
      <xs:enumeration value="EuropeanOptionBarrier"/>
      <xs:enumeration value="PerformanceOption_01"/>
      <xs:enumeration value="FxTaRF"/>
      <xs:enumeration value="EquityTaRF"/>
      <xs:enumeration value="CommodityTaRF"/>
      <xs:enumeration value="FxWorstOfBasketSwap"/>
      <xs:enumeration value="EquityWorstOfBasketSwap"/>
      <xs:enumeration value="CommodityWorstOfBasketSwap"/>
      <xs:enumeration value="EquityBestEntryOption"/>
      <xs:enumeration value="FxBestEntryOption"/>
      <xs:enumeration value="CommodityBestEntryOption"/>
      <xs:enumeration value="FxAccumulator"/>
      <xs:enumeration value="EquityAccumulator"/>
      <xs:enumeration value="CommodityAccumulator"/>
      <xs:enumeration value="FxWindowBarrierOption"/>
      <xs:enumeration value="EquityWindowBarrierOption"/>
      <xs:enumeration value="CommodityWindowBarrierOption"/>
      <xs:enumeration value="FxGenericBarrierOption"/>
      <xs:enumeration value="EquityGenericBarrierOption"/>
      <xs:enumeration value="CommodityGenericBarrierOption"/>
      <xs:enumeration value="FxBasketOption"/>
      <xs:enumeration value="EquityBasketOption"/>
      <xs:enumeration value="CommodityBasketOption"/>
      <xs:enumeration value="FxRainbowOption"/>
      <xs:enumeration value="EquityRainbowOption"/>
      <xs:enumeration value="CommodityRainbowOption"/>
      <xs:enumeration value="ForwardVolatilityAgreement"/>
      <xs:enumeration value="KnockOutSwap"/>
      <xs:enumeration value="CashPosition"/>
      <xs:enumeration value="RateDigitalOption"/>
      <xs:enumeration value="CommodityStrikeResettableOption"/>
      <xs:enumeration value="FxStrikeResettableOption"/>
      <xs:enumeration value="EquityStrikeResettableOption"/>
      <xs:enumeration value="EquityAutoDeltaHedgedOption"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="legType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Fixed"/>
      <xs:enumeration value="Floating"/>
      <xs:enumeration value="CPI"/>
      <xs:enumeration value="YY"/>
      <xs:enumeration value="CMS"/>
      <xs:enumeration value="CMB"/>
      <xs:enumeration value="DigitalCMS"/>
      <xs:enumeration value="CMSSpread"/>
      <xs:enumeration value="DigitalCMSSpread"/>
      <xs:enumeration value="Cashflow"/>
      <xs:enumeration value="Equity"/>
      <xs:enumeration value="FormulaBased"/>
      <xs:enumeration value="ZeroCouponFixed"/>
      <xs:enumeration value="IntradayPowerFloating"/>
      <xs:enumeration value="CommodityFixed"/>
      <xs:enumeration value="CommodityFloating"/>
      <xs:enumeration value="EquityMargin"/>
      <xs:enumeration value="DurationAdjustedCMS"/>
      <xs:enumeration value="RangeAccrual"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="forwardRateAgreementData">
    <xs:all>
      <xs:element type="date" name="StartDate"/>
      <xs:element type="date" name="EndDate"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:string" name="Index"/>
      <xs:element type="longShort" name="LongShort"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="Notional"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxForwardData">
    <xs:all>
      <xs:element type="date" name="ValueDate"/>
      <xs:element type="currencyCode" name="BoughtCurrency"/>
      <xs:element type="xs:float" name="BoughtAmount"/>
      <xs:element type="currencyCode" name="SoldCurrency"/>
      <xs:element type="xs:float" name="SoldAmount"/>
      <xs:element type="settlementType" name="Settlement" maxOccurs="1" minOccurs="0"/>
      <xs:element type="fxForwardSettlementData" name="SettlementData" maxOccurs="1" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxAverageForwardData">
    <xs:all>
      <xs:element type="date" name="PaymentDate"/>
      <xs:element type="scheduleData" name="ObservationDates"/>
      <xs:element type="bool" name="FixedPayer"/>
      <xs:element type="xs:float" name="ReferenceNotional"/>
      <xs:element type="currencyCode" name="ReferenceCurrency"/>
      <xs:element type="xs:float" name="SettlementNotional"/>
      <xs:element type="currencyCode" name="SettlementCurrency"/>
      <xs:element type="settlementType" name="Settlement" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndex"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxSwapData">
    <xs:all>
      <xs:element type="date" name="NearDate"/>
      <xs:element type="currencyCode" name="NearBoughtCurrency"/>
      <xs:element type="xs:float" name="NearBoughtAmount"/>
      <xs:element type="currencyCode" name="NearSoldCurrency"/>
      <xs:element type="xs:float" name="NearSoldAmount"/>
      <xs:element type="date" name="FarDate"/>
      <xs:element type="xs:float" name="FarBoughtAmount"/>
      <xs:element type="xs:float" name="FarSoldAmount"/>
      <xs:element type="settlementType" name="Settlement" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="currencyCode" name="BoughtCurrency"/>
      <xs:element type="xs:float" name="BoughtAmount"/>
      <xs:element type="currencyCode" name="SoldCurrency"/>
      <xs:element type="xs:float" name="SoldAmount" minOccurs="0"/>
      <xs:element type="xs:float" name="Delta" minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndex" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="rateDigitalOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData" minOccurs="1"/>
      <xs:element type="indexNameType" name="Index"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="PayoffAmount"/>
      <xs:element type="currencyCode" name="PayoffCurrency"/>
      <xs:element type="date" name="FixingDate"/>
      <xs:element type="date" name="PaymentDate"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxDigitalOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="currencyCode" name="PayoffCurrency" minOccurs="0"/>
      <xs:element type="xs:float" name="PayoffAmount"/>
      <xs:element type="currencyCode" name="ForeignCurrency"/>
      <xs:element type="currencyCode" name="DomesticCurrency"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxKIKOBarrierOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element name="Barriers">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="barrierData" name="BarrierData"  maxOccurs="2" minOccurs="2"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="date" name="StartDate"  minOccurs="0"/>
      <xs:element type="calendar" name="Calendar"  minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndex"  minOccurs="0"/>
      <xs:element type="currencyCode" name="BoughtCurrency"/>
      <xs:element type="xs:float" name="BoughtAmount"/>
      <xs:element type="currencyCode" name="SoldCurrency"/>
      <xs:element type="xs:float" name="SoldAmount"/>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="barrierType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="UpAndOut"/>
      <xs:enumeration value="UpAndIn"/>
      <xs:enumeration value="DownAndOut"/>
      <xs:enumeration value="DownAndIn"/>
      <xs:enumeration value="KnockIn"/>
      <xs:enumeration value="KnockOut"/>
      <xs:enumeration value="CumulatedProfitCap"/>
      <xs:enumeration value="CumulatedProfitCapPoints"/>
      <xs:enumeration value="FixingCap"/>
      <xs:enumeration value="FixingFloor"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="barrierCompare">
    <xs:restriction base="xs:string">
      <xs:enumeration value="0"/>
      <xs:enumeration value="1"/>
      <xs:enumeration value="2"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="barrierStyle">
    <xs:restriction base="xs:string">
      <xs:enumeration value="American"/>
      <xs:enumeration value="European"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="barrierData">
    <xs:all>
      <xs:element type="barrierType" name="Type"/>
	  <xs:element type="barrierCompare" name="StrictComparison" minOccurs="0"/>
      <xs:element type="barrierStyle" name="Style" minOccurs="0"/>
      <xs:element name="Levels">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:float" name="Level" minOccurs="1" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:float" name="Rebate" minOccurs="0"/>
      <xs:element type="currencyCode" name="RebateCurrency" minOccurs="0"/>
      <xs:element name="RebatePayTime" minOccurs="0">
      <xs:simpleType>
        <xs:restriction base="xs:string">
          <xs:enumeration value="atHit"/>
          <xs:enumeration value="atExpiry"/>
        </xs:restriction>
      </xs:simpleType>
      </xs:element>
      <xs:element type="bool" name="OverrideTriggered" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxBarrierOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData" minOccurs="1"/>
      <xs:element type="barrierData" name="BarrierData" minOccurs="1"/>
      <xs:element type="date" name="StartDate"  minOccurs="0"/>
      <xs:element type="calendar" name="Calendar"  minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndex"  minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndexDailyLows"  minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndexDailyHighs"  minOccurs="0"/>
      <xs:element type="currencyCode" name="BoughtCurrency"/>
      <xs:element type="xs:float" name="BoughtAmount"/>
      <xs:element type="currencyCode" name="SoldCurrency"/>
      <xs:element type="xs:float" name="SoldAmount"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxDigitalBarrierOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData" minOccurs="1"/>
      <xs:element type="barrierData" name="BarrierData" minOccurs="1"/>
      <xs:element type="date" name="StartDate"  minOccurs="0"/>
      <xs:element type="calendar" name="Calendar"  minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndex"  minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndexDailyLows"  minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndexDailyHighs"  minOccurs="0"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="PayoffAmount"/>
      <xs:element type="currencyCode" name="PayoffCurrency" minOccurs="0"/>
      <xs:element type="currencyCode" name="ForeignCurrency"/>
      <xs:element type="currencyCode" name="DomesticCurrency"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxTouchOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData" minOccurs="1"/>
      <xs:element type="barrierData" name="BarrierData" minOccurs="1"/>
      <xs:element type="currencyCode" name="ForeignCurrency"/>
      <xs:element type="currencyCode" name="DomesticCurrency"/>
      <xs:element type="currencyCode" name="PayoffCurrency"/>
      <xs:element type="xs:float" name="PayoffAmount"/>
      <xs:element type="date" name="StartDate" minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndex" minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndexDailyLows"  minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndexDailyHighs"  minOccurs="0"/>
      <xs:element type="xs:string" name="Calendar" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxForwardSettlementData">
    <xs:all>
      <xs:element type="currencyCode" name="Currency" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndex" maxOccurs="1" minOccurs="0"/>
      <xs:element type="date" name="Date" maxOccurs="1" minOccurs="0"/>
      <xs:element name="Rules" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="paymentLag" name="PaymentLag" maxOccurs="1" minOccurs="0"/>
            <xs:element type="calendar" name="PaymentCalendar" maxOccurs="1" minOccurs="0"/>
            <xs:element type="businessDayConvention" name="PaymentConvention" maxOccurs="1" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="eqForwardSettlementData">
    <xs:all>
      <xs:element type="xs:string" name="FXIndex" maxOccurs="1" minOccurs="0"/>
      <xs:element type="date" name="Date" maxOccurs="1" minOccurs="0"/>
      <xs:element name="Rules" maxOccurs="1" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="paymentLag" name="PaymentLag" maxOccurs="1" minOccurs="0"/>
            <xs:element type="calendar" name="PaymentCalendar" maxOccurs="1" minOccurs="0"/>
            <xs:element type="businessDayConvention" name="PaymentConvention" maxOccurs="1" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="flexiSwapData">
    <xs:sequence>
      <xs:element name="LowerNotionalBounds" maxOccurs="unbounded" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Notional" maxOccurs="unbounded" minOccurs="1">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
          <xs:attribute name="currency" type="xs:string" />
        </xs:complexType>
      </xs:element>
      <xs:element type="longShort" name="OptionLongShort"/>
      <xs:element type="legData" name="LegData"  maxOccurs="2" minOccurs="2"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="bgSwapData">
    <xs:sequence>
      <xs:element type="xs:string" name="ReferenceSecurity"/>
      <xs:element type="tranches" name="Tranches" maxOccurs="1" minOccurs="1"/>
      <xs:element type="legData" name="LegData"  maxOccurs="2" minOccurs="2"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="capFloorData">
    <xs:sequence>
      <xs:element type="longShort" name="LongShort"/>
      <xs:element type="legData_capfloor" name="LegData"/>
      <xs:element name="Caps" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Cap" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Floors" minOccurs="0" maxOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Floor" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:string" name="PremiumAmount" minOccurs="0"/>
      <xs:element type="premiumCurrencyCode" name="PremiumCurrency" minOccurs="0"/>
      <xs:element type="xs:string" name="PremiumPayDate" minOccurs="0"/>
      <xs:element type="premiumData" name="Premiums" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="equityOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="extendedCurrencyCode" name="Currency"/>
      <xs:element type="extendedCurrencyCode" name="StrikeCurrency" minOccurs="0"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element ref="strikeGroup"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="equityAutoDeltaHedgedUnderlyingData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="extendedCurrencyCode" name="Currency"/>
      <xs:element ref="strikeGroup"/>
      <xs:element type="extendedCurrencyCode" name="StrikeCurrency" minOccurs="0"/>
      <xs:element type="xs:float" name="Quantity"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="equityAutoDeltaHedgedOptionData">
    <xs:all>
      <xs:element name="Volatility" type="xs:float"/>
      <xs:element name="DriftRate" type="xs:float"/>
      <xs:element name="Underlyings">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Underlying" type="equityAutoDeltaHedgedUnderlyingData" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="ObservationStartDate" type="date"/>
      <xs:element name="PaymentDate" type="date"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="eqBarrierOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData" minOccurs="1"/>
      <xs:element type="barrierData" name="BarrierData" minOccurs="1"/>
      <xs:element type="date" name="StartDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="EQIndex" minOccurs="0" maxOccurs="1"/>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element ref="strikeGroup"/>
      <xs:element type="xs:float" name="Quantity"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="equityFutureOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="date" name="FutureExpiryDate" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="eqDigitalOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData" minOccurs="1"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="currencyCode" name="PayoffCurrency" minOccurs="0"/>
      <xs:element type="xs:float" name="PayoffAmount"/>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="xs:float" name="Quantity"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="eqTouchOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData" minOccurs="1"/>
      <xs:element type="barrierData" name="BarrierData" minOccurs="1"/>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="currencyCode" name="PayoffCurrency"/>
      <xs:element type="xs:float" name="PayoffAmount"/>
      <xs:element type="date" name="StartDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="EQIndex" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="commodityOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element ref="strikeGroup"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="bool" name="IsFuturePrice" minOccurs="0"/>
      <xs:element type="date" name="FutureExpiryDate" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="commodityDigitalOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="Payoff"/>
      <xs:element type="bool" name="IsFuturePrice" minOccurs="0"/>
      <xs:element type="date" name="FutureExpiryDate" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="equityForwardData">
    <xs:all>
      <xs:element type="longShort" name="LongShort"/>
      <xs:element type="date" name="Maturity"/>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="extendedCurrencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="extendedCurrencyCode" name="StrikeCurrency" minOccurs="0"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="eqForwardSettlementData" name="SettlementData" maxOccurs="1" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="commodityForwardData">
    <xs:all>
      <xs:element type="longShort" name="Position"/>
      <xs:element type="date" name="Maturity"/>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="bool" name="IsFuturePrice" minOccurs="0"/>
      <xs:element type="date" name="FutureExpiryDate" minOccurs="0"/>
      <xs:element type="xs:string" name="FutureExpiryOffset" minOccurs="0"/>
      <xs:element type="xs:string" name="FutureExpiryOffsetCalendar" minOccurs="0"/>
      <xs:element type="bool" name="PhysicallySettled" minOccurs="0"/>
      <xs:element type="date" name="PaymentDate" minOccurs="0"/>
      <xs:element type="commForwardSettlementData" name="SettlementData" maxOccurs="1" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="commForwardSettlementData">
    <xs:all>
      <xs:element type="currencyCode" name="PayCurrency" maxOccurs="1" minOccurs="1"/>
      <xs:element type="xs:string" name="FXIndex" maxOccurs="1" minOccurs="1"/>
      <xs:element type="date" name="FixingDate" maxOccurs="1" minOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="intradayPowerForwardData">
    <xs:sequence>
      <xs:element type="longShort" name="Position"/>
      <xs:element type="date" name="Maturity"/>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="date" name="DeliveryDate"/>
      <xs:choice>
        <xs:sequence>
          <xs:element type="xs:integer" name="DeliveryStart"/>
          <xs:element type="xs:integer" name="DeliveryEnd"/>
          <xs:element type="xs:boolean" name="IsDstHour" minOccurs="0"/>
        </xs:sequence>
        <xs:element type="powerLoadProfileDataType" name="PowerLoadProfileData"/>
      </xs:choice>
      <xs:element type="bool" name="PhysicallySettled" minOccurs="0"/>
      <xs:element type="date" name="PaymentDate" minOccurs="0"/>
      <xs:element type="commForwardSettlementData" name="SettlementData" maxOccurs="1" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:element name="creditCurveIdType" abstract="true"/>
  <xs:element type="xs:string" name="CreditCurveId" substitutionGroup="creditCurveIdType"/>
  <xs:element name="ReferenceInformation" substitutionGroup="creditCurveIdType">
    <xs:complexType>
      <xs:all>
        <xs:element type="xs:string" name="ReferenceEntityId"/>
        <xs:element type="cdsTierType" name="Tier"/>
        <xs:element type="currencyCode" name="Currency"/>
        <xs:element type="cdsDocClauseType" name="DocClause" minOccurs="0"/>
      </xs:all>
    </xs:complexType>
  </xs:element>

  <xs:complexType name="creditDefaultSwapData">
    <xs:all>
      <xs:element type="xs:string" name="IssuerId" minOccurs="0"/>
      <xs:element ref="creditCurveIdType"/>
      <xs:element type="xs:string" name="ReferenceObligation" minOccurs="0"/>
      <xs:element type="bool" name="SettlesAccrual" minOccurs="0"/>
      <xs:element type="bool" name="RebatesAccrual" minOccurs="0"/>
      <xs:element type="bool" name="PaysAtDefaultTime" minOccurs="0"/>
      <xs:element type="xs:string" name="ProtectionPaymentTime" minOccurs="0"/>
      <xs:element type="date" name="ProtectionStart" minOccurs="0"/>
      <xs:element type="date" name="UpfrontDate" minOccurs="0"/>
      <xs:element type="xs:float" name="UpfrontFee" minOccurs="0"/>
      <xs:element type="xs:float" name="FixedRecoveryRate" minOccurs="0"/>
      <xs:element type="legData" name="LegData" maxOccurs="1" minOccurs="1"/>
      <xs:element type="date" name="TradeDate" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="CashSettlementDays" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="auctionSettlementInformation">
    <xs:all>
      <xs:element type="date" name="AuctionSettlementDate"/>
      <xs:element type="xs:float" name="AuctionFinalPrice"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="creditDefaultSwapOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="creditDefaultSwapData" name="CreditDefaultSwapData"/>
      <xs:element type="xs:float" name="Strike" minOccurs="0"/>
      <xs:element name="StrikeType" minOccurs="0">
        <xs:simpleType>
          <xs:restriction base="xs:string">
            <xs:enumeration value="Spread"/>
          </xs:restriction>
        </xs:simpleType>
      </xs:element>
      <xs:element type="bool" name="KnockOut" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="Term" minOccurs="0" maxOccurs="1"/>
      <xs:element type="auctionSettlementInformation" name="AuctionSettlementInformation" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="singleUnderlyingAsianOptionData">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element ref="strikeGroup"/>
      <xs:element type="underlying" name="Underlying" minOccurs="0" maxOccurs="1"/>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:string" name="Settlement" minOccurs="0"/>
      <xs:element type="scheduleData" name="ObservationDates" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="tradeActionType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Break"/>
      <xs:enumeration value="Termination"/>
      <xs:enumeration value="Conversion"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="tradeActionOwner">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Mutual"/>
      <xs:enumeration value="Sold"/>
      <xs:enumeration value="Bought"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="caps">
    <xs:sequence>
      <xs:element type="floatWithAttribute" name="Cap" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="floors">
    <xs:sequence>
      <xs:element type="floatWithAttribute" name="Floor" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="tradeLevelFixings">
    <xs:sequence>
      <xs:element name="Fixing" minOccurs="0" maxOccurs="unbounded">
        <xs:complexType>
          <xs:simpleContent>
            <xs:extension base="xs:float">
              <xs:attribute name="fixingDate" type="xs:string" />
            </xs:extension>
          </xs:simpleContent>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="spreads">
    <xs:sequence>
      <xs:element type="floatWithAttribute" name="Spread" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="gearings">
    <xs:sequence>
      <xs:element type="floatWithAttribute" name="Gearing" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="floatWithAttribute">
    <xs:simpleContent>
      <xs:extension base="xs:float">
        <xs:attribute name="startDate" type="xs:string">
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="exchanges">
    <xs:all>
      <xs:element type="bool" name="NotionalInitialExchange" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="NotionalFinalExchange" minOccurs="0" maxOccurs="1"/>
      <xs:element type="bool" name="NotionalAmortizingExchange" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxreset">
    <xs:all>
      <xs:element type="currencyCode" name="ForeignCurrency" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:string" name="StartDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="ForeignAmount" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FXIndex" minOccurs="1" maxOccurs="1"/>
      <xs:element type="xs:integer" name="FixingDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FixingCalendar" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="underlying">
    <xs:all>
      <xs:element type="xs:string" name="Type"/>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="xs:string" name="IdentifierType" minOccurs="0"/>
      <xs:element type="extendedCurrencyCode" name="Currency" minOccurs="0"/>
      <xs:element type="xs:string" name="Exchange" minOccurs="0"/>
      <xs:element type="xs:float" name="Weight" minOccurs="0"/>
      <xs:element type="xs:string" name="PriceType" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="FutureMonthOffset" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="DeliveryRollDays" minOccurs="0"/>
      <xs:element type="xs:string" name="DeliveryRollCalendar" minOccurs="0"/>
      <xs:element type="xs:string" name="FutureExpiryDate" minOccurs="0"/>
      <xs:element type="xs:string" name="FutureContractMonth" minOccurs="0"/>
      <xs:element type="xs:string" name="Interpolation" minOccurs="0"/>
      <xs:element type="xs:float" name="BidAskAdjustment" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="underlyings">
    <xs:sequence>
      <xs:element type="underlying" name="Underlying" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:element name="underlyingTypes" abstract="true"/>
  <xs:element type="xs:string" name="Name" substitutionGroup="underlyingTypes"/>
  <xs:element type="underlying" name="Underlying" substitutionGroup="underlyingTypes"/>
  <xs:element type="underlyings" name="Underlyings" substitutionGroup="underlyingTypes"/>

  <xs:element name="nettingSetGroup" abstract="true"/>
  <xs:element type="xs:string" name="NettingSetId" substitutionGroup="nettingSetGroup"/>
  <xs:element type="nettingSetDetails" name="NettingSetDetails" substitutionGroup="nettingSetGroup"/>

  <xs:element name="strikeGroup" abstract="true"/>
  <xs:element type="xs:string" name="Strike" substitutionGroup="strikeGroup"/>
  <xs:element name="StrikeData" substitutionGroup="strikeGroup">
    <xs:complexType>
      <xs:choice>
        <xs:element type="strikePriceData" name="StrikePrice"/>
        <xs:element type="strikeYieldData" name="StrikeYield"/>
        <xs:sequence>
          <xs:element type="xs:float" name="Value"/>
          <xs:element type="extendedCurrencyCode" name="Currency" minOccurs="0"/>
        </xs:sequence>
      </xs:choice>
    </xs:complexType>
  </xs:element>

  <xs:element name="levelGroup" abstract="true"/>
  <xs:element type="xs:float" name="Level" substitutionGroup="levelGroup"/>
  <xs:element type="levelData" name="LevelData" substitutionGroup="levelGroup"/>

  <xs:complexType name="callableSwapData">
    <xs:choice>
      <xs:sequence>
        <xs:element type="optionData" name="OptionData"/>
        <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="1"/>
      </xs:sequence>
      <xs:sequence>
        <xs:element type="legData" name="LegData" maxOccurs="unbounded" minOccurs="1"/>
        <xs:element type="optionData" name="OptionData"/>
      </xs:sequence>
    </xs:choice>
  </xs:complexType>

  <xs:complexType name="nameData">
    <xs:all>
      <xs:element type="xs:string" name="IssuerId"/>
      <xs:element type="xs:string" name="Qualifier" maxOccurs="1" minOccurs="0"/>
      <xs:element ref="creditCurveIdType"/>
      <xs:element type="xs:float" name="Notional" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="Weight" minOccurs="0" maxOccurs="1"/>
      <xs:element type="currencyCode" name="Currency" minOccurs="0" maxOccurs="1"/>
      <xs:element type="non-negative-decimal" name="PriorNotional" minOccurs="0" maxOccurs="1"/>
      <xs:element type="non-negative-decimal" name="PriorWeight" minOccurs="0" maxOccurs="1"/>
      <xs:element type="recoveryRate" name="RecoveryRate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="date" name="AuctionDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="date" name="AuctionSettlementDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="date" name="DefaultDate" minOccurs="0" maxOccurs="1"/>
      <xs:element type="date" name="EventDeterminationDate" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="basketData">
    <xs:sequence>
      <xs:element type="nameData" name="Name" maxOccurs="unbounded" minOccurs="0"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="indexCreditDefaultSwapData">
    <xs:all>
      <xs:element type="xs:string" name="IssuerId" minOccurs="0"/>
      <xs:element type="xs:string" name="CreditCurveId"/>
      <xs:element type="bool" name="SettlesAccrual" minOccurs="0"/>
      <xs:element type="bool" name="RebatesAccrual" minOccurs="0"/>
      <xs:element type="bool" name="PaysAtDefaultTime" minOccurs="0"/>
      <xs:element type="xs:string" name="ProtectionPaymentTime" minOccurs="0"/>
      <xs:element type="date" name="ProtectionStart" minOccurs="0"/>
      <xs:element type="date" name="UpfrontDate" minOccurs="0"/>
      <xs:element type="xs:float" name="UpfrontFee" minOccurs="0"/>
      <xs:element type="legData" name="LegData"/>
      <xs:element type="date" name="TradeDate" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="CashSettlementDays" minOccurs="0"/>
      <xs:element type="basketData" name="BasketData" maxOccurs="1" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="indexCreditDefaultSwapOptionData">
    <xs:all>
      <xs:element type="xs:float" name="Strike" minOccurs="0"/>
    <!-- KnockOut is ignored. Remains in the schema for backward compatibility of the schema -->
      <xs:element type="bool" name="KnockOut" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="IndexTerm" minOccurs="0" maxOccurs="1"/>
      <xs:element type="cdsOptionstrikeType" name="StrikeType" minOccurs="0"/>
      <xs:element type="date" name="TradeDate" minOccurs="0"/>
      <xs:element type="date" name="FrontEndProtectionStartDate" minOccurs="0"/>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="indexCreditDefaultSwapData" name="IndexCreditDefaultSwapData"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="creditLinkedSwapData">
    <xs:all>
      <xs:element type="xs:string" name="CreditCurveId"/>
      <xs:element type="bool" name="SettlesAccrual" minOccurs="0"/>
      <xs:element type="xs:float" name="FixedRecoveryRate" minOccurs="0"/>
      <xs:element type="xs:string" name="DefaultPaymentTime" minOccurs="0"/>
      <xs:element name="IndependentPayments" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="legData" name="LegData" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="ContingentPayments" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="legData" name="LegData" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="DefaultPayments" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="legData" name="LegData" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="RecoveryPayments" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="legData" name="LegData" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="cdsOptionstrikeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Spread"/>
      <xs:enumeration value="Price"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="emptyFloat">
    <xs:union>
      <xs:simpleType>
        <xs:restriction base='xs:string'>
          <xs:length value="0"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType>
        <xs:restriction base='xs:float'>
        </xs:restriction>
      </xs:simpleType>
    </xs:union>
  </xs:simpleType>

  <xs:complexType name="cdoData">
    <xs:all>
      <xs:element type="xs:string" name="Qualifier"/>
      <xs:element type="date" name="ProtectionStart"/>
      <xs:element type="date" name="UpfrontDate" minOccurs="0"/>
      <xs:element type="emptyFloat" name="UpfrontFee" minOccurs="0"/>
      <xs:element type="bool" name="SettlesAccrual" minOccurs="0"/>
      <xs:element type="bool" name="RebatesAccrual" minOccurs="0"/>
      <xs:element type="bool" name="PaysAtDefaultTime" minOccurs="0"/>
      <xs:element type="xs:string" name="ProtectionPaymentTime" minOccurs="0"/>
      <xs:element type="xs:float" name="FixedRecoveryRate" minOccurs="0"/>
      <xs:element type="xs:float" name="AttachmentPoint"/>
      <xs:element type="xs:float" name="DetachmentPoint"/>
      <xs:element type="legData" name="LegData"/>
      <xs:element type="basketData" name="BasketData" maxOccurs="1" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element ref="strikeGroup"/>
      <xs:element type="xs:string" name="Redemption" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:string" name="PriceType" maxOccurs="1" minOccurs="0"/>
      <xs:element type="bool" name="KnocksOut" maxOccurs="1" minOccurs="0"/>
      <xs:element type="bondData" name="BondData" minOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondRepoData">
    <xs:all>
      <xs:element type="bondData" name="BondData"/>
      <xs:element name="RepoData">
        <xs:complexType>
          <xs:all>
            <xs:element type="legData" name="LegData"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fundingData">
    <xs:all>
      <xs:element type="legData" name="LegData" maxOccurs="1" minOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fxTermsData">
    <xs:sequence>
      <xs:element type="xs:string" name="FXIndex" minOccurs="1" maxOccurs="unbounded"/>
      <!-- just kept here for backward compatibility of the schema, these elements are not used any more -->
      <xs:element type="xs:integer" name="FXIndexFixingDays" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="FXIndexCalendar" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:boolean" name="ApplyFXIndexFixingDays" minOccurs="0" maxOccurs="1"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="totalReturnData">
    <xs:all>
      <xs:element type="xs:string" name="Payer" maxOccurs="1" minOccurs="1"/>
      <xs:element type="xs:float" name="InitialPrice" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:string" name="PriceType" maxOccurs="1" minOccurs="1"/>
      <xs:element type="xs:string" name="ObservationLag" maxOccurs="1" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="ObservationConvention" maxOccurs="1" minOccurs="0"/>
      <xs:element type="calendar" name="ObservationCalendar" maxOccurs="1" minOccurs="0"/>
      <xs:element type="paymentLag" name="PaymentLag" maxOccurs="1" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="PaymentConvention" maxOccurs="1" minOccurs="0"/>
      <xs:element type="calendar" name="PaymentCalendar" maxOccurs="1" minOccurs="0"/>
      <xs:element name="PaymentDates" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="date" name="PaymentDate" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="trsFxConversion" name="FXConversion" minOccurs="0" maxOccurs="1"/>
      <xs:element type="fxTermsData" name="FXTerms" minOccurs="0" maxOccurs="1"/>
      <xs:element type="scheduleData" name="ScheduleData" maxOccurs="1" minOccurs="1"/>
      <xs:element type="xs:boolean" name="PayBondCashFlowsImmediately" minOccurs="0"/>
      <xs:element type="dateDeltaUnit" name="PaymentLagUnit" minOccurs="0"/>
      <xs:element type="dateDeltaAnchor" name="PaymentLagAnchor" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bondTRSData">
    <xs:all>
      <xs:element type="bondData" name="BondData" maxOccurs="1" minOccurs="1"/>
      <xs:element type="totalReturnData" name="TotalReturnData" maxOccurs="1" minOccurs="1"/>
      <xs:element type="fundingData" name="FundingData" maxOccurs="1" minOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="convertibleBondData">
    <xs:all>
      <xs:element type="bondData" name="BondData"/>
      <xs:element type="cbCallData" name="CallData" minOccurs="0"/>
      <xs:element type="cbCallData" name="PutData" minOccurs="0"/>
      <xs:element type="cbConversionData" name="ConversionData" minOccurs="0"/>
      <xs:element type="cbDividendProtectionData" name="DividendProtectionData" minOccurs="0"/>
      <xs:element type="bool" name="Detachable" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="ascotData">
    <xs:all>
      <xs:element type="convertibleBondData" name="ConvertibleBondData"/>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="fundingData" name="ReferenceSwapData" maxOccurs="1" minOccurs="1"/>
    </xs:all>
  </xs:complexType>

 <xs:complexType name="callableBondCallData">
    <xs:all>
      <xs:element type="scheduleData" name="ScheduleData"/>
      <xs:element name="Styles">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Style" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Prices">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Price" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="PriceTypes">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="PriceType" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="IncludeAccruals">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="IncludeAccrual"  maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="bool">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="callableBondData">
    <xs:all>
      <xs:element type="bondData" name="BondData"/>
      <xs:element type="callableBondCallData" name="CallData" minOccurs="0"/>
      <xs:element type="callableBondCallData" name="PutData" minOccurs="0"/>
    </xs:all>
  </xs:complexType>


  <xs:complexType name="trsUnderlyingData">
    <xs:sequence>
      <xs:choice maxOccurs="unbounded">
        <xs:element name="Derivative">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:string" name="Id"/>
              <xs:element ref="subTradeGroup"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
        <xs:element ref="subTradeGroup"/>
        <xs:element name="PortfolioIndexTradeData" minOccurs="0">
          <xs:complexType>
            <xs:sequence>
              <xs:element type="xs:string" name="BasketName" minOccurs="1"/>
              <xs:element type="xs:string" name="IndexQuantity" minOccurs="0"/>
              <xs:element type="xs:boolean" name="PriceIsPerUnit" minOccurs="0"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:choice>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="trsReturnData">
    <xs:all>
      <xs:element type="xs:boolean" name="Payer"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="scheduleData" name="ScheduleData"/>
      <xs:element type="xs:string" name="ObservationLag" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="ObservationConvention" minOccurs="0"/>
      <xs:element type="calendar" name="ObservationCalendar" minOccurs="0"/>
      <xs:element type="paymentLag" name="PaymentLag" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="PaymentConvention" minOccurs="0"/>
      <xs:element type="calendar" name="PaymentCalendar" minOccurs="0"/>
      <xs:element name="PaymentDates" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="date" name="PaymentDate" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:float" name="InitialPrice" minOccurs="0"/>
      <xs:element type="extendedCurrencyCode" name="InitialPriceCurrency" minOccurs="0"/>
      <xs:element type="fxTermsData" name="FXTerms" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:boolean" name="PayUnderlyingCashFlowsImmediately" minOccurs="0"/>
      <xs:element type="dateDeltaUnit" name="PaymentLagUnit" minOccurs="0"/>
      <xs:element type="dateDeltaAnchor" name="PaymentLagAnchor" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="trsNotionalType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="PeriodReset"/>
      <xs:enumeration value="DailyReset"/>
      <xs:enumeration value="Fixed"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="trsFxConversion">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Start"/>
      <xs:enumeration value="End"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="trsFundingData">
    <xs:choice minOccurs="0" maxOccurs="unbounded">
      <xs:element type="xs:integer" name="FundingResetGracePeriod"/>
      <xs:element type="trsNotionalType" name="NotionalType"/>
      <xs:element type="legData" name="LegData"/>
    </xs:choice>
  </xs:complexType>

  <xs:complexType name="trsAdditionalCashflowData">
    <xs:sequence>
      <xs:element type="legData" name="LegData"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="totalReturnSwapData">
    <xs:all>
      <xs:element type="trsUnderlyingData" name="UnderlyingData"/>
      <xs:element type="trsReturnData" name="ReturnData"/>
      <xs:element type="trsFundingData" name="FundingData" minOccurs="0"/>
      <xs:element type="trsAdditionalCashflowData" name="AdditionalCashflowData" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="equityPositionData">
    <xs:sequence>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="underlying" name="Underlying" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="commodityPositionData">
    <xs:sequence>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="underlying" name="Underlying" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="equityOptionUnderlyingData">
    <xs:sequence>
      <xs:element type="underlying" name="Underlying"/>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:float" name="Strike"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="equityOptionPositionData">
    <xs:sequence>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="equityOptionUnderlyingData" name="Underlying" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="bondBasketData">
    <xs:sequence>
      <xs:element type="xs:float" name="Quantity" minOccurs="0"/>
      <xs:element type="xs:string" name="Identifier" minOccurs="0"/>
      <xs:element type="underlying" name="Underlying" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="tranches">
    <xs:sequence>
      <xs:element type="tranche" name="Tranche" maxOccurs="unbounded" minOccurs="1"/>
      <xs:element type="scheduleData" name="ScheduleData"  maxOccurs="1" minOccurs="1"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="tranche">
    <xs:all>
      <xs:element type="xs:string" name="Description" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:string" name="SecurityId" maxOccurs="1" minOccurs="1"/>
      <xs:element type="xs:integer" name="Seniority" maxOccurs="1" minOccurs="1"/>
      <xs:element name="Notionals" maxOccurs="1" minOccurs="1">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Notional" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="rpaData">
    <xs:all>
      <xs:element type="xs:float" name="ParticipationRate"/>
      <xs:element type="date" name="ProtectionStart"/>
      <xs:element type="date" name="ProtectionEnd"/>
      <xs:element type="xs:string" name="CreditCurveId"/>
      <xs:element type="xs:string" name="IssuerId" minOccurs="0"/>
      <xs:element type="xs:boolean" name="SettlesAccrual" minOccurs="0"/>
      <xs:element type="xs:float" name="FixedRecoveryRate" minOccurs="0"/>
      <xs:element name="ProtectionFee">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="legData" name="LegData" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Underlying">
        <xs:complexType>
          <xs:choice>
            <xs:sequence>
              <xs:element type="optionData" name="OptionData" minOccurs="0"/>
          <xs:element type="xs:boolean" name="NakedOption" minOccurs="0"/>
              <xs:element type="legData" name="LegData" maxOccurs="unbounded"/>
            </xs:sequence>
            <xs:element type="tlockData" name="TreasuryLockData"/>
          </xs:choice>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbodata">
    <xs:all>
      <xs:element type="cboInvestment" name="CBOInvestment" maxOccurs="1" minOccurs="1"/>
      <xs:element type="cboStructure" name="CBOStructure" maxOccurs="1" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cboInvestment">
    <xs:all>
      <xs:element type="xs:string" name="TrancheName" maxOccurs="1" minOccurs="1"/>
      <xs:element type="xs:float" name="Notional" maxOccurs="1" minOccurs="1"/>
      <xs:element type="xs:string" name="StructureId" maxOccurs="1" minOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cboStructure">
    <xs:all>
      <xs:element type="xs:string" name="DayCounter"/>
      <xs:element type="xs:string" name="PaymentConvention"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:string" name="ReinvestmentEndDate" maxOccurs="1" minOccurs="0"/>
      <xs:element type="xs:float" name="SeniorFee"/>
      <xs:element type="xs:string" name="FeeDayCounter"/>
      <xs:element type="xs:float" name="SubordinatedFee"/>
      <xs:element type="xs:float" name="EquityKicker"/>
      <xs:element type="cboBondBasketData" name="BondBasketData" maxOccurs="1" minOccurs="1"/>
      <xs:element type="cbotranches" name="CBOTranches" maxOccurs="1" minOccurs="1"/>
      <xs:element type="scheduleData" name="ScheduleData"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cboBondBasketData">

	<xs:sequence>
		<xs:element ref="subTradeGroup" maxOccurs="unbounded"/>
      <!-- <xs:element name="Trade" maxOccurs="unbounded"> -->
        <!-- <xs:complexType> -->
          <!-- <xs:all> -->
            <!-- <xs:element type="oreTradeType" name="TradeType"/> -->
            <!-- <xs:element type="envelope" name="Envelope" minOccurs="0"/> -->
            <!-- <xs:element type="bondData" name="BondData" maxOccurs="1" minOccurs="0"/> -->
          <!-- </xs:all> -->
          <!-- <xs:attribute type="xs:string" name="id"/> -->
        <!-- </xs:complexType> -->
      <!-- </xs:element> -->
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="cbotranches">
    <xs:sequence>
      <xs:element type="cbotranche" name="Tranche" maxOccurs="unbounded">
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="cbotranche">
    <xs:all>
      <xs:element type="xs:string" name="Name"/>
      <xs:element type="xs:float" name="ICRatio"/>
      <xs:element type="xs:float" name="OCRatio"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element ref="legDataType" maxOccurs="1" minOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbCallData">
    <xs:all>
      <xs:element type="scheduleData" name="ScheduleData"/>
      <xs:element name="Styles">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Style" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Prices">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Price" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="PriceTypes">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="PriceType" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="IncludeAccruals">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="IncludeAccrual"  maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="bool">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Soft" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Soft" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="bool">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="TriggerRatios" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="TriggerRatio" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="NOfMTriggers" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="NOfMTrigger" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="MakeWhole" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element name="ConversionRatioIncrease">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="xs:float" name="Cap" minOccurs="0"/>
                  <xs:element type="xs:string" name="StockPrices"/>
                  <xs:element name="CrIncreases">
                    <xs:complexType>
                      <xs:sequence>
                        <xs:element name="CrIncrease" maxOccurs="unbounded">
                          <xs:complexType>
                            <xs:simpleContent>
                              <xs:extension base="xs:string">
                                <xs:attribute name="startDate" type="xs:string" />
                              </xs:extension>
                            </xs:simpleContent>
                          </xs:complexType>
                        </xs:element>
                      </xs:sequence>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbContingentConversionData">
    <xs:all>
      <xs:element name="Observations" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Observation" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Barriers" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Barrier" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbPepsData">
    <xs:all>
      <xs:element type="xs:float" name="UpperBarrier"/>
      <xs:element type="xs:float" name="LowerBarrier"/>
      <xs:element type="xs:float" name="UpperConversionRatio"/>
      <xs:element type="xs:float" name="LowerConversionRatio"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbMandatoryConversionData">
    <xs:all>
    <xs:element type="date" name="Date"/>
    <xs:element type="xs:string" name="Type"/>
    <xs:element type="cbPepsData" name="PepsData"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbConversionResetData">
    <xs:all>
      <xs:element type="scheduleData" name="ScheduleData"/>
      <xs:element name="References">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Reference" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Thresholds">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Threshold" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Gearings">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Gearing" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Floors" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Floor" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="GlobalFloors" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="GloobalFloor" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbExchangeableData">
    <xs:all>
      <xs:element type="bool" name="IsExchangeable"/>
      <xs:element type="xs:string" name="EquityCreditCurve" minOccurs="0"/>
      <xs:element type="bool" name="Secured" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbConversionData">
    <xs:all>
      <xs:element type="scheduleData" name="ScheduleData" minOccurs="0"/>
      <xs:element name="Styles" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Style" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="ConversionRatios" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="ConversionRatio" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="FixedAmountConversion" minOccurs="0">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:string" name="Currency"/>
            <xs:element name="Amounts">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="Amount" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:simpleContent>
                        <xs:extension base="xs:float">
                          <xs:attribute name="startDate" type="xs:string" />
                        </xs:extension>
                      </xs:simpleContent>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element type="cbContingentConversionData" name="ContingentConversion" minOccurs="0"/>
      <xs:element type="cbMandatoryConversionData" name="MandatoryConversion" minOccurs="0"/>
      <xs:element type="cbConversionResetData" name="ConversionResets" minOccurs="0"/>
      <xs:element type="underlying" name="Underlying" minOccurs="0"/>
      <xs:element type="xs:string" name="FXIndex" minOccurs="0"/>
      <xs:element type="calendar" name="FXIndexCalendar" minOccurs="0"/>
      <xs:element type="xs:integer" name="FXIndexFixingDays" minOccurs="0"/>
      <xs:element type="cbExchangeableData" name="Exchangeable" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cbDividendProtectionData">
    <xs:all>
      <xs:element type="scheduleData" name="ScheduleData"/>
      <xs:element name="AdjustmentStyles">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="AdjustmentStyle" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="DividendTypes">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="DividendType" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:string">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Thresholds">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Threshold" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string" />
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="tlockData">
    <xs:all>
      <xs:element type="bool" name="Payer"/>
      <xs:element type="bondData" name="BondData"/>
      <xs:element type="xs:float" name="ReferenceRate"/>
      <xs:element type="dayCounter" name="DayCounter" minOccurs="0"/>
      <xs:element type="date" name="TerminationDate"/>
      <xs:element type="xs:integer" name="PaymentGap" minOccurs="0"/>
      <xs:element type="calendar" name="PaymentCalendar"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="compositeTradeComponents">
    <xs:sequence>
		<xs:element ref="subTradeGroup" maxOccurs="unbounded"/>
	  <!-- <xs:element name="Trade" maxOccurs="unbounded"> -->
        <!-- <xs:complexType> -->
          <!-- <xs:sequence> -->
            <!-- <xs:element type="oreTradeType" name="TradeType"/> -->
            <!-- <xs:element type="envelope" name="Envelope" minOccurs="0"/> -->
            <!-- <xs:group ref="oreTradeData"/> -->
          <!-- </xs:sequence> -->
          <!-- <xs:attribute type="xs:string" name="id"/> -->
        <!-- </xs:complexType> -->
      <!-- </xs:element> -->
    </xs:sequence>
  </xs:complexType>

  <xs:simpleType name="notionalCalculation">
    <xs:restriction base="xs:string">
      <xs:enumeration value=""/>
      <xs:enumeration value="Sum"/>
      <xs:enumeration value="Mean"/>
      <xs:enumeration value="Average"/>
      <xs:enumeration value="First"/>
      <xs:enumeration value="Last"/>
      <xs:enumeration value="Min"/>
      <xs:enumeration value="Max"/>
      <xs:enumeration value="Override"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="compositeTradeData">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="notionalCalculation" name="NotionalCalculation" minOccurs="0"/>
      <xs:element type="xs:float" name="NotionalOverride" minOccurs="0"/>
      <xs:element type="xs:boolean" name="PortfolioBasket" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:string" name="BasketName" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:double" name="IndexQuantity" minOccurs="0"/>
      <xs:element type="compositeTradeComponents" name="Components" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="rangeBound">
    <xs:all>
      <xs:element type="xs:float" name="RangeFrom" minOccurs="0"/>
      <xs:element type="xs:float" name="RangeTo" minOccurs="0"/>
      <xs:element type="xs:float" name="Leverage" minOccurs="0"/>
      <xs:element type="xs:float" name="Strike" minOccurs="0"/>
      <xs:element type="xs:float" name="StrikeAdjustment" minOccurs="0"/>
    </xs:all>
    <xs:attribute name="startDate" type="xs:string"/>
  </xs:complexType>

  <xs:complexType name="volBarrierOptionData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="CallNotional"/>
      <xs:element type="stFreeStyleNumber" name="PutNotional"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleBarrierType" name="BarrierType"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleCurrency" name="CallCcy"/>
      <xs:element type="stFreeStyleCurrency" name="PutCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="dualEuroBinaryOptionData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="VolSchedule"/>
      <xs:element type="stFreeStyleNumber" name="VolBarrierLevel"/>
      <xs:element type="stFreeStyleBarrierType" name="VolBarrierType"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleBarrierType" name="BarrierType"/>
      <xs:element type="stFreeStyleEvent" name="BarrierDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleNumber" name="SettlementAmount"/>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="dualEuroBinaryOptionDoubleKOData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleNumber" name="VolBarrierLevel"/>
      <xs:element type="stFreeStyleEvent" name="VolBarrierDate"/>
      <xs:element type="stFreeStyleNumber" name="SpotBarrierLevel"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleNumber" name="SettlementAmount"/>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleBool" name="isTerminalOnly"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="arcOptionData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleNumber" name="StrikeFactor"/>
      <xs:element type="stFreeStyleNumber" name="BarrierFactor"/>
      <xs:element type="stFreeStyleNumber" name="CapRate"/>
      <xs:element type="stFreeStyleNumber" name="Offset"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="extendedAccumulatorData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="FixingAmount"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="ExtensionTrigger"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleCurrency" name="PayCurrency"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationSettlementDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="ConditionalObservationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="ConditionalSettlementDates"/>
      <xs:element type="stFreeStyleEvent" name="ExtensionDecisionDate"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="varianceOptionData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleNumber" name="PremiumAmount"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="VarianceReference"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleBool" name="SquaredPayoff"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="varianceDispersionSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings1"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights1"/>
      <xs:element type="stFreeStyleNumberVector" name="Strikes1"/>
      <xs:element type="stFreeStyleNumberVector" name="Spreads1"/>
      <xs:element type="stFreeStyleNumberVector" name="Notionals1"/>
      <xs:element type="stFreeStyleNumberVector" name="Caps1"/>
      <xs:element type="stFreeStyleNumberVector" name="Floors1"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings2"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights2"/>
      <xs:element type="stFreeStyleNumberVector" name="Strikes2"/>
      <xs:element type="stFreeStyleNumberVector" name="Spreads2"/>
      <xs:element type="stFreeStyleNumberVector" name="Notionals2"/>
      <xs:element type="stFreeStyleNumberVector" name="Caps2"/>
      <xs:element type="stFreeStyleNumberVector" name="Floors2"/>
      <xs:element type="stFreeStyleBool" name="DividendAdjustment"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="kikoVarianceSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleBool" name="SquaredPayoff"/>
      <xs:element type="stFreeStyleBarrierType" name="BarrierType"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="corridorVarianceSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleBool" name="SquaredPayoff"/>
      <xs:element type="stFreeStyleNumber" name="UpperBarrierLevel"/>
      <xs:element type="stFreeStyleNumber" name="LowerBarrierLevel"/>
      <xs:element type="stFreeStyleBool" name="CountBothObservations"/>
      <xs:element type="stFreeStyleBool" name="AccrualAdjustment"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="indexedCorridorVarianceSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleIndex" name="CorridorIndex"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleBool" name="SquaredPayoff"/>
      <xs:element type="stFreeStyleNumber" name="UpperBarrierLevel"/>
      <xs:element type="stFreeStyleNumber" name="LowerBarrierLevel"/>
      <xs:element type="stFreeStyleBool" name="AccrualAdjustment"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="kikoCorridorVarianceSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleNumber" name="CorridorUpperBarrierLevel"/>
      <xs:element type="stFreeStyleNumber" name="CorridorLowerBarrierLevel"/>
      <xs:element type="stFreeStyleBarrierType" name="KIKOBarrierType"/>
      <xs:element type="stFreeStyleNumber" name="KIKOBarrierLevel"/>
      <xs:element type="stFreeStyleBool" name="CountBothObservations"/>
      <xs:element type="stFreeStyleBool" name="AccrualAdjustment"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleEventSchedule" name="SettlementSchedule"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="corridorVarianceDispersionSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings1"/>
      <xs:element type="stFreeStyleNumberVector" name="Strikes1"/>
      <xs:element type="stFreeStyleNumberVector" name="Spreads1"/>
      <xs:element type="stFreeStyleNumberVector" name="Notionals1"/>
      <xs:element type="stFreeStyleNumberVector" name="Caps1"/>
      <xs:element type="stFreeStyleNumberVector" name="Floors1"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings2"/>
      <xs:element type="stFreeStyleNumberVector" name="Strikes2"/>
      <xs:element type="stFreeStyleNumberVector" name="Spreads2"/>
      <xs:element type="stFreeStyleNumberVector" name="Notionals2"/>
      <xs:element type="stFreeStyleNumberVector" name="Caps2"/>
      <xs:element type="stFreeStyleNumberVector" name="Floors2"/>
      <xs:element type="stFreeStyleNumberVector" name="UpperBarrierLevels"/>
      <xs:element type="stFreeStyleNumberVector" name="LowerBarrierLevels"/>
      <xs:element type="stFreeStyleBool" name="CountBothObservations"/>
      <xs:element type="stFreeStyleBool" name="AccrualAdjustment"/>
      <xs:element type="stFreeStyleBool" name="DividendAdjustment"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="koCorridorVarianceDispersionSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings1"/>
      <xs:element type="stFreeStyleNumberVector" name="Strikes1"/>
      <xs:element type="stFreeStyleNumberVector" name="Spreads1"/>
      <xs:element type="stFreeStyleNumberVector" name="Notionals1"/>
      <xs:element type="stFreeStyleNumberVector" name="Caps1"/>
      <xs:element type="stFreeStyleNumberVector" name="Floors1"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings2"/>
      <xs:element type="stFreeStyleNumberVector" name="Strikes2"/>
      <xs:element type="stFreeStyleNumberVector" name="Spreads2"/>
      <xs:element type="stFreeStyleNumberVector" name="Notionals2"/>
      <xs:element type="stFreeStyleNumberVector" name="Caps2"/>
      <xs:element type="stFreeStyleNumberVector" name="Floors2"/>
      <xs:element type="stFreeStyleNumberVector" name="CorridorUpperBarrierLevels"/>
      <xs:element type="stFreeStyleNumberVector" name="CorridorLowerBarrierLevels"/>
      <xs:element type="stFreeStyleNumberVector" name="KOUpperBarrierLevels"/>
      <xs:element type="stFreeStyleNumberVector" name="KOLowerBarrierLevels"/>
      <xs:element type="stFreeStyleBool" name="CountBothObservations"/>
      <xs:element type="stFreeStyleBool" name="AccrualAdjustment"/>
      <xs:element type="stFreeStyleBool" name="DividendAdjustment"/>
      <xs:element type="stFreeStyleEventSchedule" name="KnockOutSchedule"/>
      <xs:element type="stFreeStyleEvent" name="VarianceAccrualStartDate"/>
      <xs:element type="stFreeStyleEventSchedule" name="SettlementSchedule"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="pairwiseGeometricVarianceDispersionSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="PairCount"/>
	  <xs:element type="stFreeStyleNumber" name="BasketCount"/>
      <xs:element type="stFreeStyleNumberVector" name="Notionals"/>
	  <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="StrikesVS"/>
      <xs:element type="stFreeStyleNumberVector" name="StrikesBVS"/>
      <xs:element type="stFreeStyleNumberVector" name="BasketWeights"/>
      <xs:element type="stFreeStyleNumberVector" name="VarianceWeights"/>
      <xs:element type="stFreeStyleNumber" name="Lag"/>
	  <xs:element type="stFreeStyleNumber" name="CapAmountMultiplier"/>
	  <xs:element type="stFreeStyleNumber" name="Cap"/>
	  <xs:element type="stFreeStyleNumber" name="Floor"/>
	  <xs:element type="stFreeStyleBool" name="DividendAdjustment"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="conditionalVarianceSwap01Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleBool" name="SquaredPayoff"/>
      <xs:element type="stFreeStyleBarrierType" name="BarrierType"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleBool" name="CountBothObservations"/>
      <xs:element type="stFreeStyleBool" name="AccrualAdjustment"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="conditionalVarianceSwap02Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="VarianceReference"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleBool" name="SquaredPayoff"/>
      <xs:element type="stFreeStyleBarrierType" name="BarrierType"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleBool" name="CountBothObservations"/>
      <xs:element type="stFreeStyleBool" name="AccrualAdjustment"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="gammaSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="SettlementSchedule" minOccurs="0"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="basketVarianceSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleBool" name="SquaredPayoff"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bestEntryOptionData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="Multiplier"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="TriggerLevel"/>
      <xs:element type="stFreeStyleNumber" name="ResetMinimum"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleEvent" name="ExpiryDate"/>
      <xs:element type="stFreeStyleEvent" name="StrikeDate"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleEventSchedule" name="StrikeObservationDates"/>
      <xs:element type="stFreeStyleCurrency" name="Currency"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="strikeResettableOptionData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
	  <xs:element type="stFreeStyleOptionType" name="OptionType"/>
	  <xs:element type="stFreeStyleCurrency" name="Currency"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
	  <xs:element type="stFreeStyleNumber" name="ResetStrike"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
	  <xs:element type="stFreeStyleNumber" name="TriggerType"/>
	  <xs:element type="stFreeStyleNumber" name="TriggerPrice"/>
	  <xs:element type="stFreeStyleIndex" name="Underlying"/>
	  <xs:element type="stFreeStyleEvent" name="ExpiryDate"/>
	  <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
	  <xs:element type="stFreeStyleEventSchedule" name="ObservationDates"/>
	  <xs:element type="stFreeStyleNumber" name="Premium"/>
	  <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="scriptedTradeData">
    <xs:sequence>
      <xs:sequence>
        <xs:element type="xs:string" name="ScriptName" minOccurs="0"/>
        <xs:element type="xs:string" name="ProductTag" minOccurs="0"/>
        <xs:element type="ore_script" name="Script" minOccurs="0"/>
      </xs:sequence>
      <xs:element name="Data">
        <xs:complexType>
          <xs:sequence>
            <xs:choice minOccurs="0" maxOccurs="unbounded">
              <xs:element name="Number">
                <xs:complexType>
                  <xs:sequence>
                    <xs:element type="xs:string" name="Name"/>
                    <xs:choice>
                      <xs:element name="Value"/>
                      <xs:element name="Values">
                        <xs:complexType>
                          <xs:sequence>
                            <xs:element type="xs:float" name="Value" maxOccurs="unbounded"/>
                          </xs:sequence>
                        </xs:complexType>
                      </xs:element>
                    </xs:choice>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
              <xs:element name="Currency">
                <xs:complexType>
                  <xs:sequence>
                    <xs:element type="xs:string" name="Name"/>
                    <xs:choice>
                      <xs:element name="Value"/>
                      <xs:element name="Values">
                        <xs:complexType>
                          <xs:sequence>
                            <xs:element type="currencyCode" name="Value" maxOccurs="unbounded"/>
                          </xs:sequence>
                        </xs:complexType>
                      </xs:element>
                    </xs:choice>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
              <xs:element name="Index">
                <xs:complexType>
                  <xs:sequence>
                    <xs:element type="xs:string" name="Name"/>
                    <xs:choice>
                      <xs:element name="Value"/>
                      <xs:element name="Values">
                        <xs:complexType>
                          <xs:sequence>
                            <xs:element type="xs:string" name="Value" maxOccurs="unbounded"/>
                          </xs:sequence>
                        </xs:complexType>
                      </xs:element>
                    </xs:choice>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
              <xs:element name="Event">
                <xs:complexType>
                  <xs:sequence>
                    <xs:element type="xs:string" name="Name"/>
                    <xs:choice>
                      <xs:element name="Value"/>
                      <xs:element type="scheduleData" name="ScheduleData"/>
                      <xs:element type="DerivedScheduleType" name="DerivedSchedule"/>
                    </xs:choice>
                    <xs:element type="bool" name="ApplyCoarsening" minOccurs="0"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
              <xs:element name="Daycounter">
                <xs:complexType>
                  <xs:sequence>
                    <xs:element type="xs:string" name="Name"/>
                    <xs:choice>
                      <xs:element name="Value"/>
                      <xs:element name="Values">
                        <xs:complexType>
                          <xs:sequence>
                            <xs:element type="dayCounter" name="Value" maxOccurs="unbounded"/>
                          </xs:sequence>
                        </xs:complexType>
                      </xs:element>
                    </xs:choice>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleEvent">
    <xs:simpleContent>
      <xs:extension base="date">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="event"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleNumber">
    <xs:simpleContent>
      <xs:extension base="xs:float">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="number"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleIndex">
    <xs:simpleContent>
      <xs:extension base="xs:string">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="index"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleCurrency">
    <xs:simpleContent>
      <xs:extension base="currencyCode">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="currency"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleDayCounter">
    <xs:simpleContent>
      <xs:extension base="xs:string">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="dayCounter"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleBool">
    <xs:simpleContent>
      <xs:extension base="xs:boolean">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="bool"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleOptionType">
    <xs:simpleContent>
      <xs:extension base="xs:string">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="optionType"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleLongShort">
    <xs:simpleContent>
      <xs:extension base="xs:string">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="longShort"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleBarrierType">
    <xs:simpleContent>
      <xs:extension base="xs:string">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="barrierType"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleEventScheduleBase">
    <xs:sequence>
      <xs:choice>
        <xs:element type="scheduleData" name="ScheduleData"/>
        <xs:element type="DerivedScheduleType" name="DerivedSchedule"/>
      </xs:choice>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleEventSchedule">
    <xs:complexContent>
      <xs:extension base="stFreeStyleEventScheduleBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="event"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleNumberVectorBase">
    <xs:sequence>
      <xs:element type="xs:float" name="Value" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleNumberVector">
    <xs:complexContent>
      <xs:extension base="stFreeStyleNumberVectorBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="number"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleIndexVectorBase">
    <xs:sequence>
      <xs:element type="xs:string" name="Value" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleIndexVector">
    <xs:complexContent>
      <xs:extension base="stFreeStyleIndexVectorBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="index"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleCurrencyVectorBase">
    <xs:sequence>
      <xs:element type="currencyCode" name="Value" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleCurrencyVector">
    <xs:complexContent>
      <xs:extension base="stFreeStyleCurrencyVectorBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="currency"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleDayCounterVectorBase">
    <xs:sequence>
      <xs:element type="xs:string" name="Value" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleDayCounterVector">
    <xs:complexContent>
      <xs:extension base="stFreeStyleDayCounterVectorBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="dayCounter"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleBoolVectorBase">
    <xs:sequence>
      <xs:element type="xs:boolean" name="Value" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleBoolVector">
    <xs:complexContent>
      <xs:extension base="stFreeStyleDayCounterVectorBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="bool"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleOptionTypeVectorBase">
    <xs:sequence>
      <xs:element type="xs:string" name="Value" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleOptionTypeVector">
    <xs:complexContent>
      <xs:extension base="stFreeStyleOptionTypeVectorBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="optionType"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleLongShortVectorBase">
    <xs:sequence>
      <xs:element type="xs:string" name="Value" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleLongShortVector">
    <xs:complexContent>
      <xs:extension base="stFreeStyleOptionTypeVectorBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="longShort"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="stFreeStyleBarrierTypeVectorBase">
    <xs:sequence>
      <xs:element type="xs:string" name="Value" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="stFreeStyleBarrierTypeVector">
    <xs:complexContent>
      <xs:extension base="stFreeStyleOptionTypeVectorBase">
        <xs:attribute name="type">
          <xs:simpleType>
            <xs:restriction base="xs:string">
              <xs:enumeration value="barrierType"/>
            </xs:restriction>
          </xs:simpleType>
        </xs:attribute>
      </xs:extension>
    </xs:complexContent>
  </xs:complexType>

  <xs:complexType name="asianBasketOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationDates"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="averageStrikeBasketOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationDates"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="lookbackCallBasketOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationDates"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="lookbackPutBasketOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationDates"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="vanillaBasketOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bestOfAirbagData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumberVector" name="StrikePrices"/>
      <xs:element type="stFreeStyleNumber" name="BonusCoupon"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationSchedule"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstOfBasketSwapData">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="InitialFixedRate"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleEventSchedule" name="DeterminationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="SettlementDates"/>
      <xs:element type="stFreeStyleNumberVector" name="KnockOutLevels"/>
      <xs:element type="stFreeStyleNumberVector" name="CouponTriggerLevels"/>
      <xs:element type="stFreeStyleNumber" name="KnockInLevel"/>
      <xs:element type="stFreeStyleNumber" name="CouponRate"/>
      <xs:element type="stFreeStyleBool" name="AccumulatingCoupons"/>
      <xs:element type="stFreeStyleIndex" name="FloatingIndex"/>
      <xs:element type="stFreeStyleNumber" name="FloatingSpread"/>
      <xs:element type="stFreeStyleDayCounter" name="FloatingDayCountFraction"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixingSchedule"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bestOfAssetOrCashRainbowOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstOfAssetOrCashRainbowOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="minRainbowOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="maxRainbowOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption01Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="PayoffMultiplier"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption02Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="PayoffMultiplier"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption03Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="PayoffMultiplier"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleBool" name="BermudanBarrier"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleEventSchedule" name="BarrierSchedule"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption04Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="PayoffMultiplier"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleBool" name="BermudanBarrier"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleEventSchedule" name="BarrierSchedule"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption05Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleBarrierType" name="BarrierType"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption06Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumberVector" name="StrikePrices"/>
      <xs:element type="stFreeStyleNumberVector" name="BarrierLevels"/>
      <xs:element type="stFreeStyleNumberVector" name="KnockInPrices"/>
      <xs:element type="stFreeStyleNumber" name="BonusCoupon"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationSchedule"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption07Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumber" name="FixedRateI"/>
      <xs:element type="stFreeStyleNumber" name="FixedRateII"/>
      <xs:element type="stFreeStyleDayCounter" name="DayCountFraction"/>
      <xs:element type="stFreeStyleNumberVector" name="StrikePrices"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumberVector" name="TriggerLevels"/>
      <xs:element type="stFreeStyleEventSchedule" name="DeterminationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationSchedule"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption08Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleOptionType" name="CallPut"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="StrikePrices"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstPerformanceRainbowOption09Data">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleOptionType" name="CallPut"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="Premium"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="StrikePrices"/>
      <xs:element type="stFreeStyleEvent" name="ObservationDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleEventSchedule" name="BarrierDates"/>
      <xs:element type="stFreeStyleNumberVector" name="KOLevels"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="autocallable01Data">
    <xs:all>
      <xs:element type="xs:float" name="NotionalAmount"/>
      <xs:element type="xs:float" name="DeterminationLevel"/>
      <xs:element type="xs:float" name="TriggerLevel"/>
      <xs:element type="underlying" name="Underlying"/>
      <xs:element type="longShort" name="Position"/>
      <xs:element type="currencyCode" name="PayCcy"/>
      <xs:element name="FixingDates">
        <xs:complexType>
          <xs:all>
            <xs:element type="scheduleData" name="ScheduleData"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="SettlementDates">
        <xs:complexType>
          <xs:all>
            <xs:element type="scheduleData" name="ScheduleData"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="AccumulationFactors">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:float" name="Factor" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:float" name="Cap"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="doubleDigitalOptionData">
    <xs:all>
      <xs:element type="date" name="Expiry"/>
      <xs:element type="date" name="Settlement"/>
      <xs:element type="xs:float" name="BinaryPayout"/>
      <xs:element type="xs:float" name="BinaryLevel1"/>
      <xs:element type="xs:float" name="BinaryLevel2"/>
      <xs:element type="xs:float" name="BinaryLevelUpper1" minOccurs="0"/>
      <xs:element type="xs:float" name="BinaryLevelUpper2" minOccurs="0"/>
      <xs:element type="xs:string" name="Type1"/>
      <xs:element type="xs:string" name="Type2"/>
      <xs:element type="longShort" name="Position"/>
      <xs:element type="underlying" name="Underlying1" minOccurs="0"/>
      <xs:element type="underlying" name="Underlying2" minOccurs="0"/>
      <xs:element type="underlying" name="Underlying3" minOccurs="0"/>
      <xs:element type="underlying" name="Underlying4" minOccurs="0"/>
      <xs:element type="xs:string" name="Name1" minOccurs="0"/>
      <xs:element type="xs:string" name="Name2" minOccurs="0"/>
      <xs:element type="currencyCode" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="performanceOption01Data">
    <xs:all>
      <xs:element type="xs:float" name="NotionalAmount"/>
      <xs:element type="xs:float" name="ParticipationRate"/>
      <xs:element type="date" name="ValuationDate"/>
      <xs:element type="date" name="SettlementDate"/>
      <xs:element type="underlyings" name="Underlyings"/>
      <xs:element name="StrikePrices">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:float" name="StrikePrice" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="bool" name="StrikeIncluded" minOccurs="0"/>
      <xs:element type="longShort" name="Position"/>
      <xs:element type="currencyCode" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="windowBarrierOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleEvent" name="StartDate"/>
      <xs:element type="stFreeStyleEvent" name="EndDate"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleBarrierType" name="BarrierType"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleEvent" name="PremiumPayDate"/>
      <xs:element type="stFreeStyleNumber" name="PremiumAmount"/>
      <xs:element type="stFreeStyleCurrency" name="PremiumCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="accumulator01Data">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="FixingAmount"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleEvent" name="StartDate"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixingDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="SettlementDates"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeUpperBounds"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeLowerBounds"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeLeverages"/>
      <xs:element type="stFreeStyleNumber" name="KnockOutLevel"/>
      <xs:element type="stFreeStyleBarrierType" name="KnockOutType"/>
      <xs:element type="stFreeStyleBool" name="AmericanKO"/>
      <xs:element type="stFreeStyleNumber" name="GuaranteedFixings"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="accumulator02Data">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="FixingAmount"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="KnockOutSettlementDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationPeriodEndDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="SettlementDates"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeUpperBounds"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeLowerBounds"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeLeverages"/>
      <xs:element type="stFreeStyleNumber" name="DefaultRange"/>
      <xs:element type="stFreeStyleNumber" name="KnockOutLevel"/>
      <xs:element type="stFreeStyleBarrierType" name="KnockOutType"/>
      <xs:element type="stFreeStyleEvent" name="GuaranteedPeriodEndDate"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="tarfData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="FixingAmount"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixingDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="SettlementDates"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeUpperBounds"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeLowerBounds"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeLeverages"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeStrikes"/>
      <xs:element type="stFreeStyleNumber" name="KnockOutProfitAmount"/>
      <xs:element type="stFreeStyleNumber" name="KnockOutProfitEvents"/>
      <xs:element type="stFreeStyleNumber" name="TargetAmount"/>
      <xs:element type="stFreeStyleNumber" name="TargetType"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="callableRangeAccrualData">
    <xs:all>
      <xs:element type="stFreeStyleBool" name="IsCallable"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleNumber" name="PremiumAmount"/>
      <xs:element type="stFreeStyleCurrency" name="PremiumCcy"/>
      <xs:element type="stFreeStyleEvent" name="PremiumPayDate"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleEventSchedule" name="ExerciseDates"/>
      <xs:element type="stFreeStyleBool" name="FloatingPayer"/>
      <xs:element type="stFreeStyleNumber" name="FloatingNotional"/>
      <xs:element type="stFreeStyleNumber" name="FloatingSpread"/>
      <xs:element type="stFreeStyleIndex" name="FloatingIndex"/>
      <xs:element type="stFreeStyleDayCounter" name="FloatingDayCounter"/>
      <xs:element type="stFreeStyleEventSchedule" name="FloatingSchedule"/>
      <xs:element type="stFreeStyleIndex" name="RangeAccrualIndex"/>
      <xs:element type="stFreeStyleBool" name="RangeAccrualPayer"/>
      <xs:element type="stFreeStyleNumber" name="RangeAccrualCoupon"/>
      <xs:element type="stFreeStyleNumber" name="RangeAccrualNotional"/>
      <xs:element type="stFreeStyleDayCounter" name="RangeAccrualDayCounter"/>
      <xs:element type="stFreeStyleEventSchedule" name="RangeAccrualSchedule"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeUpperBounds"/>
      <xs:element type="stFreeStyleNumberVector" name="RangeLowerBounds"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="europeanRainbowCallSpreadOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialStrikes"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="rainbowCallSpreadBarrierOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialPrices"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleNumber" name="Gearing"/>
      <xs:element type="stFreeStyleBool" name="BermudanBarrier"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleEventSchedule" name="BarrierSchedule"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="asianRainbowCallSpreadOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="Expiry"/>
      <xs:element type="stFreeStyleEventSchedule" name="AveragingDates"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="InitialStrikes"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="asianIrCapFloorData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="NotionalAmount"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleDayCounter" name="FixingLagDc"/>
      <xs:element type="stFreeStyleNumber" name="MinFixingLag"/>
      <xs:element type="stFreeStyleNumber" name="MaxFixingLag"/>
      <xs:element type="stFreeStyleOptionType" name="OptionType"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Gearing"/>
      <xs:element type="stFreeStyleNumber" name="Spread"/>
      <xs:element type="stFreeStyleDayCounter" name="DayCountFraction"/>
      <xs:element type="stFreeStyleNumber" name="FixedAmount"/>
      <xs:element type="stFreeStyleEvent" name="FixedAmountPayDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleEventSchedule" name="AccrualSchedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixingSchedule"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="constantMaturityVolatilitySwapData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="NotionalAmount"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleDayCounter" name="DayCountFraction"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleEvent" name="Settlement"/>
      <xs:element type="stFreeStyleEventSchedule" name="ResetSchedule"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="forwardVolatilityAgreementData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="FvaDate"/>
      <xs:element type="stFreeStyleEvent" name="OptionExpiry"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="ImpliedVolStrike"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="forwardVolatilityAgreementData2">
        <xs:all>
          <xs:element type="date" name="FvaDate"/>
          <xs:element type="date" name="OptionExpiry"/>
          <xs:element type="date" name="PremiumDate"/>
          <xs:element type="underlying" name="Underlying"/>
          <xs:element type="longShort" name="LongShort"/>
          <xs:element type="xs:float" name="ImpliedVolStrike"/>
          <xs:element type="xs:float" name="Quantity"/>
          <xs:element type="currencyCode" name="PayCcy"/>
          <xs:element type="date" name="SettlementDate"/>
          <xs:element type="xs:float" name="UnderlyingStrike" minOccurs="0"/>
          <xs:element type="dayCounter" name="DayCountFraction" minOccurs="0"/>
          <xs:element type="xs:float" name="DividendYield" minOccurs="0"/>
          <xs:element type="xs:float" name="FixedRate" minOccurs="0"/>
          <xs:element type="xs:string" name="PayoffType" minOccurs="0"/>
        </xs:all>
  </xs:complexType>

  <xs:complexType name="assetLinkedCliquetOptionData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="Nominal"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleCurrency" name="PayCurrency"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="PaymentDates"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleIndexVector" name="FXConversions"/>
      <xs:element type="stFreeStyleNumberVector" name="Weights"/>
      <xs:element type="stFreeStyleIndex" name="LinkedUnderlying"/>
      <xs:element type="stFreeStyleNumber" name="PayStrike"/>
      <xs:element type="stFreeStyleNumber" name="RecStrike"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="correlationSwapData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="Amount"/>
      <xs:element type="stFreeStyleNumber" name="FixedRate"/>
      <xs:element type="stFreeStyleBool" name="FixedRatePayer"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleEventSchedule" name="DeterminationDates"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cmsCapFloorBarrierData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="PremiumAmount"/>
      <xs:element type="stFreeStyleCurrency" name="PremiumCurrency"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleEvent" name="OptionExpiry"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleOptionType" name="OptionType"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="CMSUnderlyings"/>
      <xs:element type="stFreeStyleNumber" name="Gearing"/>
      <xs:element type="stFreeStyleNumber" name="Spread"/>
      <xs:element type="stFreeStyleIndex" name="BarrierUnderlying"/>
      <xs:element type="stFreeStyleNumber" name="BarrierLevel"/>
      <xs:element type="stFreeStyleBarrierType" name="BarrierType"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="SettlementCurrency"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="fixedStrikeForwardStartingOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="ForwardDate"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleEvent" name="OptionExpiry"/>
      <xs:element type="stFreeStyleDayCounter" name="DayCountFraction"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleNumber" name="UnderlyingDrift"/>
      <xs:element type="stFreeStyleNumber" name="DiscountRate"/>
      <xs:element type="stFreeStyleNumber" name="ImpliedVolatility"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="SettlementCurrency"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="floatingStrikeForwardStartingOptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="ForwardDate"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleEvent" name="OptionExpiry"/>
      <xs:element type="stFreeStyleNumber" name="PremiumAmount"/>
      <xs:element type="stFreeStyleCurrency" name="PremiumCurrency"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="SettlementCurrency"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="forwardStartingSwaptionData">
    <xs:all>
      <xs:element type="stFreeStyleEvent" name="DeterminationDate"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleNumber" name="SwaptionType"/>
      <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
      <xs:element type="stFreeStyleNumber" name="PremiumAmount"/>
      <xs:element type="stFreeStyleCurrency" name="PremiumCurrency"/>
      <xs:element type="stFreeStyleEvent" name="OptionExpiry"/>
      <xs:element type="stFreeStyleIndex" name="Underlying"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
      <xs:element type="stFreeStyleDayCounter" name="FixedDayCountFraction"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixedSchedule"/>
      <xs:element type="stFreeStyleDayCounter" name="FloatingDayCountFraction"/>
      <xs:element type="stFreeStyleEventSchedule" name="FloatingSchedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixingSchedule"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="flooredAverageCPIZCIISData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleCurrency" name="PayCurrency"/>
      <xs:element type="stFreeStyleDayCounter" name="FixedDayCounter"/>
      <xs:element type="stFreeStyleNumber" name="FixedRate"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixedLegSchedule"/>
      <xs:element type="stFreeStyleBool" name="PayFixLeg"/>
      <xs:element type="stFreeStyleDayCounter" name="FloatDayCounter"/>
      <xs:element type="stFreeStyleIndex" name="CPIIndex"/>
      <xs:element type="stFreeStyleEventSchedule" name="FloatLegSchedule"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleNumber" name="BaseCPI"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationSchedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixingSchedule"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="movingMaxYYIISData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleCurrency" name="PayCurrency"/>
      <xs:element type="stFreeStyleDayCounter" name="IborLegDayCounter"/>
      <xs:element type="stFreeStyleNumber" name="IborSpread"/>
      <xs:element type="stFreeStyleIndex" name="IborIndex"/>
      <xs:element type="stFreeStyleBool" name="PayIborLeg"/>
      <xs:element type="stFreeStyleEventSchedule" name="IborLegSchedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="IborLegFixingSchedule"/>
      <xs:element type="stFreeStyleDayCounter" name="InflationLeg1_DayCounter"/>
      <xs:element type="stFreeStyleIndex" name="InflationLeg1_CPI"/>
      <xs:element type="stFreeStyleEventSchedule" name="InflationLeg1_Schedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="InflationLeg1_FixingSchedule"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg1_Gearing"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg1_Floor"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg1_InitialCPI"/>
      <xs:element type="stFreeStyleBool" name="InflationLeg1_SubtractNotional"/>
      <xs:element type="stFreeStyleDayCounter" name="InflationLeg2_DayCounter"/>
      <xs:element type="stFreeStyleIndex" name="InflationLeg2_CPI"/>
      <xs:element type="stFreeStyleEventSchedule" name="InflationLeg2_Schedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="InflationLeg2_FixingSchedule"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg2_Gearing"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg2_Floor"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg2_InitialCPI"/>
      <xs:element type="stFreeStyleBool" name="InflationLeg2_SubtractNotional"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="irregularYYIISData">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleCurrency" name="PayCurrency"/>
      <xs:element type="stFreeStyleDayCounter" name="IborLegDayCounter"/>
      <xs:element type="stFreeStyleNumber" name="IborSpread"/>
      <xs:element type="stFreeStyleIndex" name="IborIndex"/>
      <xs:element type="stFreeStyleBool" name="PayIborLeg"/>
      <xs:element type="stFreeStyleEventSchedule" name="IborLegSchedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="IborLegFixingSchedule"/>
      <xs:element type="stFreeStyleDayCounter" name="InflationLeg1_DayCounter"/>
      <xs:element type="stFreeStyleIndex" name="InflationLeg1_CPI"/>
      <xs:element type="stFreeStyleEventSchedule" name="InflationLeg1_Schedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="InflationLeg1_FixingSchedule"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg1_Gearing"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg1_Floor"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg1_InitialCPI"/>
      <xs:element type="stFreeStyleBool" name="InflationLeg1_SubtractNotional"/>
      <xs:element type="stFreeStyleDayCounter" name="InflationLeg2_DayCounter"/>
      <xs:element type="stFreeStyleIndex" name="InflationLeg2_CPI"/>
      <xs:element type="stFreeStyleEventSchedule" name="InflationLeg2_Schedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="InflationLeg2_FixingSchedule"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg2_Gearing"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg2_Floor"/>
      <xs:element type="stFreeStyleNumber" name="InflationLeg2_InitialCPI"/>
      <xs:element type="stFreeStyleBool" name="InflationLeg2_SubtractNotional"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="genericBarrierOptionDataRaw">
    <xs:all>
      <xs:element type="stFreeStyleNumber" name="PayoffType"/>
      <xs:element type="stFreeStyleBarrierTypeVector" name="TransatlanticBarrierType"/>
      <xs:element type="stFreeStyleNumberVector" name="TransatlanticBarrierLevel"/>
      <xs:element type="stFreeStyleNumber" name="TransatlanticBarrierRebate"/>
      <xs:element type="stFreeStyleCurrency" name="TransatlanticBarrierRebateCurrency"/>
      <xs:element type="stFreeStyleNumberVector" name="TransatlanticBarrierStrictComparison"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleNumber" name="Quantity"/>
      <xs:element type="stFreeStyleNumber" name="Strike"/>
      <xs:element type="stFreeStyleNumber" name="Amount"/>
      <xs:element type="stFreeStyleCurrency" name="PayCurrency"/>
      <xs:element type="stFreeStyleEvent" name="ExpiryDate"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleEventSchedule" name="BarrierMonitoringDates"/>
      <xs:element type="stFreeStyleBarrierTypeVector" name="BarrierTypes"/>
      <xs:element type="stFreeStyleNumberVector" name="BarrierLevels"/>
      <xs:element type="stFreeStyleNumberVector" name="BarrierRebates"/>
      <xs:element type="stFreeStyleNumberVector" name="BarrierStrictComparison"/>
      <xs:element type="stFreeStyleCurrencyVector" name="BarrierRebateCurrencies"/>
      <xs:element type="stFreeStyleNumberVector" name="BarrierRebatePayTimes"/>
      <xs:element type="stFreeStyleNumber" name="BarrierRebate"/>
      <xs:element type="stFreeStyleCurrency" name="BarrierRebateCurrency"/>
      <xs:element type="stFreeStyleNumber" name="KikoType"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="ladderLockInOptionData">
    <xs:all>
        <xs:element type="stFreeStyleLongShort" name="LongShort"/>
        <xs:element type="stFreeStyleNumber" name="Quantity"/>
        <xs:element type="stFreeStyleOptionType" name="PutCall"/>
        <xs:element type="stFreeStyleNumber" name="PremiumAmount"/>
        <xs:element type="stFreeStyleEvent" name="PremiumDate"/>
        <xs:element type="stFreeStyleIndex" name="Underlying"/>
        <xs:element type="stFreeStyleNumberVector" name="LockInLevels"/>
        <xs:element type="stFreeStyleEventSchedule" name="ObservationSchedule"/>
        <xs:element type="stFreeStyleNumber" name="Strike"/>
        <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
        <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="europeanOptionBarrierData">
    <xs:all>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="xs:string" name="PutCall"/>
      <xs:element type="longShort" name="LongShort"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="PremiumAmount"/>
      <xs:element type="currencyCode" name="PremiumCurrency"/>
      <xs:element type="date" name="PremiumDate"/>
      <xs:element type="premiumData" name="Premiums" minOccurs="0"/>
      <xs:element type="date" name="OptionExpiry"/>
      <xs:element type="underlying" name="OptionUnderlying"/>
      <xs:element type="underlying" name="BarrierUnderlying"/>
      <xs:element type="xs:float" name="BarrierLevel"/>
      <xs:element type="xs:string" name="BarrierType"/>
      <xs:element type="xs:string" name="BarrierStyle"/>
      <xs:element type="scheduleData" name="BarrierSchedule" minOccurs="0"/>
      <xs:element type="date" name="SettlementDate"/>
      <xs:element type="currencyCode" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="tarfData2">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="FixingAmount"/>
      <xs:element type="xs:float" name="TargetAmount" minOccurs="0"/>
      <xs:element type="xs:float" name="TargetPoints" minOccurs="0"/>
      <xs:element type="xs:float" name="Strike" minOccurs="0"/>
      <xs:element name="Strikes" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="Strike" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:simpleContent>
                  <xs:extension base="xs:float">
                    <xs:attribute name="startDate" type="xs:string"/>
                  </xs:extension>
                </xs:simpleContent>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="underlying" name="Underlying"/>
      <xs:element type="scheduleData" name="ScheduleData"/>
      <xs:element type="xs:string" name="SettlementLag" minOccurs="0"/>
      <xs:element type="calendar" name="SettlementCalendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="SettlementConvention" minOccurs="0"/>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element name="RangeBounds" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="rangeBound" name="RangeBound" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="RangeBoundSet" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="RangeBounds" minOccurs="0" maxOccurs="unbounded">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="rangeBound" name="RangeBound" maxOccurs="unbounded"/>
                </xs:sequence>
                <xs:attribute name="startDate" type="xs:string"/>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Barriers">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="barrierData" name="BarrierData" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="bestEntryOptionData2">
    <xs:all>
      <xs:element type="longShort" name="LongShort"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element type="xs:float" name="Multiplier"/>
      <xs:element type="xs:float" name="Cap"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="TriggerLevel"/>
      <xs:element type="xs:float" name="ResetMinimum"/>
      <xs:element type="xs:float" name="Premium"/>
      <xs:element type="date" name="ExpiryDate"/>
      <xs:element type="date" name="StrikeDate"/>
      <xs:element type="date" name="PremiumDate"/>
      <xs:element type="date" name="SettlementDate"/>
      <xs:element type="scheduleData" name="StrikeObservationDates"/>
      <xs:element type="underlying" name="Underlying"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="strikeResettableOptionData2">
    <xs:all>
      <xs:element type="longShort" name="LongShort"/>
	  <xs:element type="optionType" name="OptionType"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="ResetStrike"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element name="TriggerType">
	    <xs:simpleType>
		  <xs:restriction base="xs:string">
		    <xs:enumeration value="Up"/>
		    <xs:enumeration value="Down"/>
		  </xs:restriction>
	    </xs:simpleType>
	  </xs:element>
      <xs:element type="xs:float" name="TriggerPrice"/>
	  <xs:element type="underlying" name="Underlying"/>
	  <xs:element type="date" name="ExpiryDate"/>
	  <xs:element type="date" name="SettlementDate"/>
      <xs:element type="xs:float" name="Premium"/>
      <xs:element type="date" name="PremiumDate"/>
      <xs:element type="scheduleData" name="ObservationDates"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="worstOfBasketSwapData2">
    <xs:all>
      <xs:element type="longShort" name="LongShort"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Quantity"/>
      <xs:element type="xs:float" name="InitialFixedRate" minOccurs="0"/>
      <xs:element type="date" name="InitialFixedPayDate" minOccurs="0"/>
      <xs:element type="underlyings" name="Underlyings"/>
      <xs:element name="InitialPrices">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:float" name="InitialPrice" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="date" name="KnockInPayDate" minOccurs="0"/>
      <xs:element type="scheduleData" name="FloatingPeriodSchedule"/>
      <xs:element type="scheduleData" name="FloatingFixingSchedule" minOccurs="0"/>
      <xs:element type="scheduleData" name="FixedDeterminationSchedule" minOccurs="0"/>
      <xs:element type="scheduleData" name="FloatingPayDates"/>
      <xs:element type="scheduleData" name="FixedPayDates" minOccurs="0"/>
      <xs:element type="scheduleData" name="KnockOutDeterminationSchedule" minOccurs="0"/>
      <xs:element type="scheduleData" name="FixedAccrualSchedule" minOccurs="0"/>
      <xs:element name="KnockOutLevels">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:float" name="KnockOutLevel" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:boolean" name="BermudanKnockIn" minOccurs="0"/>
      <xs:element name="FixedTriggerLevels">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="xs:float" name="FixedTriggerLevel" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element type="xs:float" name="KnockInLevel" minOccurs="0"/>
      <xs:element type="scheduleData" name="KnockInDeterminationSchedule" minOccurs="0"/>
      <xs:element type="xs:float" name="FixedRate"/>
      <xs:element type="xs:boolean" name="AccumulatingFixedCoupons" minOccurs="0"/>
      <xs:element type="xs:boolean" name="AccruingFixedCoupons" minOccurs="0"/>
      <xs:element type="xs:string" name="FloatingIndex"/>
      <xs:element type="xs:float" name="FloatingSpread" minOccurs="0"/>
      <xs:element type="xs:string" name="FloatingLookback" minOccurs="0"/>
      <xs:element type="xs:string" name="FloatingRateCutoff" minOccurs="0"/>
      <xs:element type="dayCounter" name="FloatingDayCountFraction"/>
      <xs:element type="xs:boolean" name="IsAveraged" minOccurs="0"/>
      <!-- <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0"/> -->
      <xs:element type="xs:boolean" name="IncludeSpread" minOccurs="0"/>
      <xs:element type="xs:float" name="Strike" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="basketVarianceSwapData2">
    <xs:all>
      <xs:element type="longShort" name="LongShort"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element type="underlyings" name="Underlyings"/>
      <xs:element type="scheduleData" name="ValuationSchedule"/>
      <xs:element type="xs:boolean" name="SquaredPayoff"/>
      <xs:element type="xs:float" name="Cap" minOccurs="0"/>
      <xs:element type="xs:float" name="Floor" minOccurs="0"/>
      <xs:element type="date" name="SettlementDate"/>
      <xs:element type="currencyCode" name="Currency"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="accumulatorData">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="FixingAmount"/>
      <xs:element type="xs:boolean" name="DailyFixingAmount" minOccurs="0"/>
      <xs:element type="xs:float" name="Strike" minOccurs="0"/>
      <xs:element type="underlying" name="Underlying"/>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="date" name="StartDate" minOccurs="0"/>
      <xs:element type="scheduleData" name="ObservationDates"/>
      <xs:element type="scheduleData" name="PricingDates" minOccurs="0"/>
      <xs:element type="scheduleData" name="SettlementDates" minOccurs="0"/>
      <xs:element type="xs:string" name="SettlementLag" minOccurs="0"/>
      <xs:element type="calendar" name="SettlementCalendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="SettlementConvention" minOccurs="0"/>
      <xs:element type="xs:boolean" name="NakedOption" minOccurs="0"/>
      <xs:element type="xs:boolean" name="KnockOutSettlementAtPeriodEnd" minOccurs="0"/>
      <xs:element type="xs:boolean" name="KnockOutFixingAtKOSettlement" minOccurs="0" />
      <xs:element type="underlying" name="FxIndex" minOccurs="0" />
      <xs:element name="RangeBounds">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="rangeBound" name="RangeBound" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="Barriers" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="barrierData" name="BarrierData" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="windowBarrierOptionData2">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="FixingAmount"/>
      <xs:element ref="strikeGroup"/>
      <xs:element type="underlying" name="Underlying"/>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="date" name="StartDate"/>
      <xs:element type="date" name="EndDate"/>
      <xs:element type="barrierData" name="BarrierData"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="genericBarrierOptionData">
    <xs:all>
      <xs:element type="currencyCode" name="PayCurrency"/>
      <xs:element ref="underlyingTypes"/>
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="date" name="SettlementDate" minOccurs="0"/>
      <xs:element type="xs:string" name="SettlementLag" minOccurs="0"/>
      <xs:element type="calendar" name="SettlementCalendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="SettlementConvention" minOccurs="0"/>
      <xs:element type="xs:float" name="Quantity" minOccurs="0"/>
      <xs:element type="xs:float" name="Strike" minOccurs="0"/>
      <xs:element type="xs:float" name="Amount" minOccurs="0"/>
      <xs:element name="Barriers">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="scheduleData" name="ScheduleData" minOccurs="0"/>
            <xs:element type="date" name="StartDate" minOccurs="0"/>
            <xs:element type="date" name="EndDate" minOccurs="0"/>
            <xs:element type="barrierData" name="BarrierData" minOccurs="0" maxOccurs="unbounded"/>
            <xs:element name="KikoType" minOccurs="0">
              <xs:simpleType>
                <xs:restriction base="xs:string">
                  <xs:enumeration value="KoAlways"/>
                 <xs:enumeration value="KoBeforeKi"/>
                 <xs:enumeration value="KoAfterKi"/>
                </xs:restriction>
              </xs:simpleType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:element name="TransatlanticBarrier" minOccurs="0">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="barrierData" name="BarrierData" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="basketOptionData">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element type="xs:float" name="Strike" minOccurs="0"/>
      <xs:element type="underlyings" name="Underlyings" />
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:string" name="Settlement" minOccurs="0"/>
      <xs:element type="scheduleData" name="ObservationDates" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="rainbowOptionData">
    <xs:all>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element type="xs:float" name="Strike"/>
      <xs:element type="underlyings" name="Underlyings" />
      <xs:element type="optionData" name="OptionData"/>
      <xs:element type="xs:string" name="Settlement" minOccurs="0"/>
    </xs:all>
  </xs:complexType>


  <xs:complexType name="assetBackedCreditDefaultSwapData">
    <xs:all>
      <xs:element type="bondData" name="BondData"/>
      <xs:element type="xs:string" name="ProtectionSide"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element type="xs:float" name="PremiumRate" minOccurs="0"/>
      <xs:element type="dayCounter" name="PremiumDayCounter" minOccurs="0"/>
      <xs:element name="PremiumData" minOccurs="0">
    <xs:complexType>
      <xs:all>
        <xs:element type="legType" name="LegType"/>
        <xs:element type="dayCounter" name="DayCounter" minOccurs="0"/>
        <xs:element ref="legDataType"/>
      </xs:all>
    </xs:complexType>
      </xs:element>
      <xs:element type="date" name="ProtectionStart" minOccurs="0"/>
      <xs:element type="date" name="ProtectionEnd" minOccurs="0"/>
      <xs:element type="bool" name="SettlesAccrual" minOccurs="0"/>
      <xs:element type="bool" name="PaysAtDefaultTime" minOccurs="0"/>
      <xs:element type="xs:string" name="ProtectionPaymentTime" minOccurs="0"/>
      <xs:element type="date" name="UpfrontDate" minOccurs="0"/>
      <xs:element type="xs:float" name="UpfrontFee" minOccurs="0"/>
      <xs:element type="xs:float" name="FixedRecoveryRate" minOccurs="0"/>
      <xs:element type="date" name="TradeDate" minOccurs="0"/>
      <xs:element type="xs:nonNegativeInteger" name="CashSettlementDays" minOccurs="0"/>
      <xs:element type="bool" name="FullFirstCoupon" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="lapseHedgeSwapData">
    <xs:all>
      <xs:element type="stFreeStyleBool" name="Payer"/>
      <xs:element type="stFreeStyleNumber" name="Notional"/>
      <xs:element type="stFreeStyleNumber" name="LapseHedgePercentage"/>
      <xs:element type="stFreeStyleNumberVector" name="deltaN"/>
      <xs:element type="stFreeStyleCurrency" name="SettlementCurrency"/>
      <xs:element type="stFreeStyleDayCounter" name="Paydaycounter"/>
      <xs:element type="stFreeStyleNumber" name="IRS"/>
      <xs:element type="stFreeStyleNumber" name="IRSSpread"/>
      <xs:element type="stFreeStyleNumber" name="Tau"/>
      <xs:element type="stFreeStyleNumber" name="Fee"/>
      <xs:element type="stFreeStyleNumberVector" name="IMS"/>
      <xs:element type="stFreeStyleNumberVector" name="PHFV"/>
      <xs:element type="stFreeStyleNumberVector" name="Penalty"/>
      <xs:element type="stFreeStyleNumberVector" name="ExitPrice"/>
      <xs:element type="stFreeStyleNumberVector" name="ExitFee"/>
      <xs:element type="stFreeStyleNumber" name="InitialExchangeFee"/>
      <xs:element type="stFreeStyleEventSchedule" name="InitialExchangeDate"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="PaymentDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="ExerciseDates"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="knockOutSwapData">
    <xs:sequence>
      <xs:element type="barrierData" name="BarrierData"/>
      <xs:element type="date" name="BarrierStartDate"/>
      <xs:element type="legData" name="LegData" minOccurs="2" maxOccurs="2"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="LPISwapData">
    <xs:all>
      <xs:element type="stFreeStyleCurrency" name="PayCurrency"/>
      <xs:element type="stFreeStyleBool" name="PayFixLeg"/>
      <xs:element type="stFreeStyleDayCounter" name="FixedDayCounter"/>
      <xs:element type="stFreeStyleNumber" name="ZeroCouponRate"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixedLegSchedule"/>
      <xs:element type="stFreeStyleNumberVector" name="FixedLegNotionals"/>

      <xs:element type="stFreeStyleIndex" name="CPIIndex"/>
      <xs:element type="stFreeStyleEventSchedule" name="FloatLegSchedule"/>
      <xs:element type="stFreeStyleEvent" name="FloatLegFirstPaymentDate"/>
      <xs:element type="stFreeStyleNumberVector" name="FloatLegNotional"/>
      <xs:element type="stFreeStyleNumber" name="Floor"/>
      <xs:element type="stFreeStyleNumber" name="Cap"/>
      <xs:element type="stFreeStyleEventSchedule" name="FixingSchedule"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="cashPositionData">
    <xs:all>
      <xs:element type="extendedCurrencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Amount"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="pairwiseVarianceSwapData2">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="UnderlyingStrikes"/>
      <xs:element type="stFreeStyleNumberVector" name="UnderlyingNotionals"/>
      <xs:element type="stFreeStyleNumber" name="BasketNotional"/>
      <xs:element type="stFreeStyleNumber" name="BasketStrike"/>
      <xs:element type="scheduleData" name="ValuationSchedule"/>
      <xs:element type="scheduleData" name="LaggedValuationSchedule" minOccurs="0"/>
      <xs:element type="stFreeStyleNumber" name="AccrualLag" minOccurs="0"/>
      <xs:element type="stFreeStyleNumber" name="PayoffLimit" minOccurs="0"/>
      <xs:element type="stFreeStyleNumber" name="Cap" minOccurs="0"/>
      <xs:element type="stFreeStyleNumber" name="Floor" minOccurs="0"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="pairwiseVarianceSwapData1">
    <xs:all>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleIndexVector" name="Underlyings"/>
      <xs:element type="stFreeStyleNumberVector" name="UnderlyingStrikes"/>
      <xs:element type="stFreeStyleNumberVector" name="UnderlyingNotionals"/>
      <xs:element type="stFreeStyleNumber" name="BasketNotional"/>
      <xs:element type="stFreeStyleNumber" name="BasketStrike"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationSchedule"/>
      <xs:element type="stFreeStyleEventSchedule" name="LaggedValuationSchedule" minOccurs="0"/>
      <xs:element type="stFreeStyleNumber" name="AccrualLag" minOccurs="0"/>
      <xs:element type="stFreeStyleNumber" name="PayoffLimit" minOccurs="0"/>
      <xs:element type="stFreeStyleNumber" name="Cap" minOccurs="0"/>
      <xs:element type="stFreeStyleNumber" name="Floor" minOccurs="0"/>
      <xs:element type="stFreeStyleEvent" name="SettlementDate"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>


  <xs:complexType name="eqOutperformanceOptionData">
    <xs:all>
      <xs:element type="optionData" name="OptionData" minOccurs="1"/>
      <xs:element type="currencyCode" name="Currency"/>
      <xs:element type="xs:float" name="Notional"/>
      <xs:element type="underlying" name="Underlying1"/>
      <xs:element type="underlying" name="Underlying2"/>
      <xs:element type="xs:float" name="InitialPrice1"/>
      <xs:element type="xs:float" name="InitialPrice2"/>
      <xs:element type="xs:float" name="StrikeReturn"/>
      <xs:element type="xs:float" name="KnockInPrice" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:float" name="KnockOutPrice" minOccurs="0" maxOccurs="1"/>
      <xs:element type="currencyCode" name="InitialPriceCurrency1" minOccurs="0" maxOccurs="1"/>
      <xs:element type="fxTermsData" name="InitialPriceFXTerms1" minOccurs="0" maxOccurs="1"/>
      <xs:element type="currencyCode" name="InitialPriceCurrency2" minOccurs="0" maxOccurs="1"/>
      <xs:element type="fxTermsData" name="InitialPriceFXTerms2" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

  <xs:element name="FormulaBasedLegData" substitutionGroup="legDataType">
    <xs:complexType>
      <xs:all>
        <xs:element type="xs:string" name="Index"/>
        <xs:element type="xs:boolean" name="IsInArrears" minOccurs="0" maxOccurs="1"/>
        <xs:element type="xs:integer" name="FixingDays"/>
        <xs:element type="calendar" name="FixingCalendar" minOccurs="0" maxOccurs="1"/>
      </xs:all>
    </xs:complexType>
  </xs:element>

  <xs:complexType name="commodityRevenueOptionData">
    <xs:all>
      <xs:element type="stFreeStyleOptionType" name="PutCall"/>
      <xs:element type="stFreeStyleLongShort" name="LongShort"/>
      <xs:element type="stFreeStyleEventSchedule" name="ObservationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="ValuationDates"/>
      <xs:element type="stFreeStyleEventSchedule" name="SettlementSchedule"/>
      <xs:element type="stFreeStyleNumber" name="TrueUp"/>
      <xs:element type="stFreeStyleEventSchedule" name="MonthlySchedule"/>
      <xs:element type="stFreeStyleNumberVector" name="MonthlyBaseloadCapacity"/>
      <xs:element type="stFreeStyleNumberVector" name="MonthlyDuctFiredCapacity"/>
      <xs:element type="stFreeStyleNumberVector" name="MonthlyBaseloadHeatRate"/>
      <xs:element type="stFreeStyleNumberVector" name="MonthlyDuctFiredHeatRate"/>
      <xs:element type="stFreeStyleNumberVector" name="VOM"/>
      <xs:element type="stFreeStyleNumber" name="HoursPerDay"/>
      <xs:element type="stFreeStyleIndex" name="GasIndex"/>
      <xs:element type="stFreeStyleIndex" name="EnergyIndex"/>
      <xs:element type="stFreeStyleCurrency" name="PayCcy"/>
    </xs:all>
  </xs:complexType>

  <xs:complexType name="stubInterpolation">
    <xs:all>
      <xs:element type="indexNameType" name="ShortIndex" maxOccurs="1" minOccurs="1"/>
      <xs:element type="indexNameType" name="LongIndex" maxOccurs="1" minOccurs="1"/>
      <xs:element type="roundingType" name="RoundingType" minOccurs="0" maxOccurs="1"/>
      <xs:element type="xs:integer" name="RoundingPrecision" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

</xs:schema>
$xsd$, '5961b0f68b66ec5c9497ecfd54505b9a9719b19c45c3e0600041b5b6b30a2abf', 252175),
        ('824b639d-b3fd-542d-9782-8874c1502174'::uuid, 'ore_types_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:simpleType name="date">
    <!-- date can be YYYYMMDD, DD-MM-YYYY, DD/MM/YYYY,
         YYYY/MM/DD, YYYY-MM-DD or an empty string in some cases -->
    <xs:restriction base="xs:string">
      <xs:maxLength value="10"/>
      <!-- This is not super strict, e.g. 2016-17-39 would pass, but it catches
           most errors-->
      <xs:pattern value="([1-2]\d{3}[0-1]\d[0-3]\d|[1-2]\d{3}[-./][0-1]\d[-./][0-3]\d|[0-3]\d[-./][0-1]\d[-./][1-2]\d{3})?"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="isodate">
    <!-- date can only be YYYY-MM-DD -->
    <xs:restriction base="xs:string">
      <xs:length value="10"/>
      <!-- This is not super strict, e.g. 2016-02-31 would pass, but it catches
           most errors-->
      <xs:pattern value="(\d{4}-([0]\d|1[0-2])-([0-2]\d|3[01]))?"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="period">
    <!-- period can be 1D, 5M, 10w, 20Y etc -->
    <xs:restriction base="xs:string">
      <xs:pattern value="(\d*(d|D|w|W|m|M|y|Y))?"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="paymentLag">
    <!-- paymentLag can be either integer or a period 1D, -5M, 10w, 20Y, 1W2D etc -->
    <xs:restriction base="xs:string">
      <xs:pattern value="(-?\d+|-?\d+[dDwWmMyY]|-?\d+[wW]\d+[dD]|-?\d+[yY]\d+[mM])?"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="indexNameType">
    <xs:restriction base="xs:string">
      <xs:minLength value='1'/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="bool">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Y"/>
      <xs:enumeration value="YES"/>
      <xs:enumeration value="TRUE"/>
      <xs:enumeration value="True"/>
      <xs:enumeration value="true"/>
      <xs:enumeration value="1"/>
      <xs:enumeration value="N"/>
      <xs:enumeration value="NO"/>
      <xs:enumeration value="FALSE"/>
      <xs:enumeration value="False"/>
      <xs:enumeration value="false"/>
      <xs:enumeration value="0"/>
      <xs:enumeration value=""/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="currencyCode">
    <xs:restriction base="xs:string">
      <xs:enumeration value="AED"/>
      <xs:enumeration value="AFN"/>
      <xs:enumeration value="ALL"/>
      <xs:enumeration value="AMD"/>
      <xs:enumeration value="ANG"/>
      <xs:enumeration value="AOA"/>
      <xs:enumeration value="ARS"/>
      <xs:enumeration value="AUD"/>
      <xs:enumeration value="AWG"/>
      <xs:enumeration value="AZN"/>
      <xs:enumeration value="BAM"/>
      <xs:enumeration value="BBD"/>
      <xs:enumeration value="BDT"/>
      <xs:enumeration value="BGN"/>
      <xs:enumeration value="BHD"/>
      <xs:enumeration value="BIF"/>
      <xs:enumeration value="BMD"/>
      <xs:enumeration value="BND"/>
      <xs:enumeration value="BOB"/>
      <xs:enumeration value="BOV"/>
      <xs:enumeration value="BRL"/>
      <xs:enumeration value="BSD"/>
      <xs:enumeration value="BTN"/>
      <xs:enumeration value="BWP"/>
      <xs:enumeration value="BYN"/>
      <xs:enumeration value="BZD"/>
      <xs:enumeration value="CAD"/>
      <xs:enumeration value="CDF"/>
      <xs:enumeration value="CHE"/>
      <xs:enumeration value="CHF"/>
      <xs:enumeration value="CHW"/>
      <xs:enumeration value="CLF"/>
      <xs:enumeration value="CLP"/>
      <xs:enumeration value="CNH"/>
      <xs:enumeration value="CNT"/>
      <xs:enumeration value="CNY"/>
      <xs:enumeration value="COP"/>
      <xs:enumeration value="COU"/>
      <xs:enumeration value="CRC"/>
      <xs:enumeration value="CUC"/>
      <xs:enumeration value="CUP"/>
      <xs:enumeration value="CVE"/>
      <xs:enumeration value="CYP"/>
      <xs:enumeration value="CZK"/>
      <xs:enumeration value="DJF"/>
      <xs:enumeration value="DKK"/>
      <xs:enumeration value="DOP"/>
      <xs:enumeration value="DZD"/>
      <xs:enumeration value="EGP"/>
      <xs:enumeration value="ERN"/>
      <xs:enumeration value="ETB"/>
      <xs:enumeration value="EUR"/>
      <xs:enumeration value="FJD"/>
      <xs:enumeration value="FKP"/>
      <xs:enumeration value="GBP"/>
      <xs:enumeration value="GEL"/>
      <xs:enumeration value="GGP"/>
      <xs:enumeration value="GHS"/>
      <xs:enumeration value="GIP"/>
      <xs:enumeration value="GMD"/>
      <xs:enumeration value="GNF"/>
      <xs:enumeration value="GTQ"/>
      <xs:enumeration value="GYD"/>
      <xs:enumeration value="HKD"/>
      <xs:enumeration value="HNL"/>
      <xs:enumeration value="HRK"/>
      <xs:enumeration value="HTG"/>
      <xs:enumeration value="HUF"/>
      <xs:enumeration value="IDR"/>
      <xs:enumeration value="ILS"/>
      <xs:enumeration value="IMP"/>
      <xs:enumeration value="INR"/>
      <xs:enumeration value="IQD"/>
      <xs:enumeration value="IRR"/>
      <xs:enumeration value="ISK"/>
      <xs:enumeration value="JEP"/>
      <xs:enumeration value="JMD"/>
      <xs:enumeration value="JOD"/>
      <xs:enumeration value="JPY"/>
      <xs:enumeration value="KES"/>
      <xs:enumeration value="KGS"/>
      <xs:enumeration value="KHR"/>
      <xs:enumeration value="KID"/>
      <xs:enumeration value="KMF"/>
      <xs:enumeration value="KPW"/>
      <xs:enumeration value="KRW"/>
      <xs:enumeration value="KWD"/>
      <xs:enumeration value="KYD"/>
      <xs:enumeration value="KZT"/>
      <xs:enumeration value="LAK"/>
      <xs:enumeration value="LBP"/>
      <xs:enumeration value="LKR"/>
      <xs:enumeration value="LRD"/>
      <xs:enumeration value="LSL"/>
      <xs:enumeration value="LTL"/>
      <xs:enumeration value="LVL"/>
      <xs:enumeration value="LYD"/>
      <xs:enumeration value="MAD"/>
      <xs:enumeration value="MDL"/>
      <xs:enumeration value="MGA"/>
      <xs:enumeration value="MKD"/>
      <xs:enumeration value="MMK"/>
      <xs:enumeration value="MNT"/>
      <xs:enumeration value="MOP"/>
      <xs:enumeration value="MRU"/>
      <xs:enumeration value="MUR"/>
      <xs:enumeration value="MVR"/>
      <xs:enumeration value="MWK"/>
      <xs:enumeration value="MXN"/>
      <xs:enumeration value="MXV"/>
      <xs:enumeration value="MYR"/>
      <xs:enumeration value="MZN"/>
      <xs:enumeration value="NAD"/>
      <xs:enumeration value="NGN"/>
      <xs:enumeration value="NIO"/>
      <xs:enumeration value="NOK"/>
      <xs:enumeration value="NPR"/>
      <xs:enumeration value="NZD"/>
      <xs:enumeration value="OMR"/>
      <xs:enumeration value="PAB"/>
      <xs:enumeration value="PEN"/>
      <xs:enumeration value="PGK"/>
      <xs:enumeration value="PHP"/>
      <xs:enumeration value="PKR"/>
      <xs:enumeration value="PLN"/>
      <xs:enumeration value="PYG"/>
      <xs:enumeration value="QAR"/>
      <xs:enumeration value="RON"/>
      <xs:enumeration value="RSD"/>
      <xs:enumeration value="RUB"/>
      <xs:enumeration value="RWF"/>
      <xs:enumeration value="SAR"/>
      <xs:enumeration value="SBD"/>
      <xs:enumeration value="SCR"/>
      <xs:enumeration value="SDG"/>
      <xs:enumeration value="SEK"/>
      <xs:enumeration value="SGD"/>
      <xs:enumeration value="SHP"/>
      <xs:enumeration value="SLL"/>
      <xs:enumeration value="SKK"/>
      <xs:enumeration value="SOS"/>
      <xs:enumeration value="SRD"/>
      <xs:enumeration value="SSP"/>
      <xs:enumeration value="STN"/>
      <xs:enumeration value="SVC"/>
      <xs:enumeration value="SYP"/>
      <xs:enumeration value="SZL"/>
      <xs:enumeration value="THB"/>
      <xs:enumeration value="TJS"/>
      <xs:enumeration value="TMT"/>
      <xs:enumeration value="TND"/>
      <xs:enumeration value="TOP"/>
      <xs:enumeration value="TRY"/>
      <xs:enumeration value="TTD"/>
      <xs:enumeration value="TWD"/>
      <xs:enumeration value="TZS"/>
      <xs:enumeration value="UAH"/>
      <xs:enumeration value="UGX"/>
      <xs:enumeration value="USD"/>
      <xs:enumeration value="USN"/>
      <xs:enumeration value="UYI"/>
      <xs:enumeration value="UYU"/>
      <xs:enumeration value="UYW"/>
      <xs:enumeration value="UZS"/>
      <xs:enumeration value="VES"/>
      <xs:enumeration value="VND"/>
      <xs:enumeration value="VUV"/>
      <xs:enumeration value="WST"/>
      <xs:enumeration value="XAF"/>
      <xs:enumeration value="XAG"/>
      <xs:enumeration value="XAU"/>
      <xs:enumeration value="XCD"/>
      <xs:enumeration value="XOF"/>
      <xs:enumeration value="XPD"/>
      <xs:enumeration value="XPF"/>
      <xs:enumeration value="XPT"/>
      <xs:enumeration value="XSU"/>
      <xs:enumeration value="XUA"/>
      <xs:enumeration value="YER"/>
      <xs:enumeration value="ZAR"/>
      <xs:enumeration value="ZMW"/>
      <xs:enumeration value="ZWL"/>
      <xs:enumeration value="XBT"/>
      <xs:enumeration value="BTC"/>
      <xs:enumeration value="ETH"/>
      <xs:enumeration value="ETC"/>
      <xs:enumeration value="BCH"/>
      <xs:enumeration value="XRP"/>
      <xs:enumeration value="LTC"/>
      <xs:enumeration value="ZUR"/>
      <xs:enumeration value="ZUG"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="premiumCurrencyCode">
    <xs:union memberTypes="currencyCode">
      <xs:simpleType>
        <xs:restriction base='xs:string'>
          <xs:length value="0"/>
        </xs:restriction>
      </xs:simpleType>
    </xs:union>
  </xs:simpleType>

  <xs:simpleType name="extendedCurrencyCode">
     <xs:union memberTypes="currencyCode">
                <xs:simpleType>
                        <xs:restriction base="xs:string">
                                <xs:enumeration value="GBp" />
                                <xs:enumeration value="GBX" />
                                <xs:enumeration value="ILa" />
                                <xs:enumeration value="ILA" />
                                <xs:enumeration value="ILX" />
                                <xs:enumeration value="ILs" />
                                <xs:enumeration value="KWf" />
                                <xs:enumeration value="ZAc" />
                                <xs:enumeration value="ZAC" />
                                <xs:enumeration value="ZAX" />
                                <xs:enumeration value="USc" />
                        </xs:restriction>
                </xs:simpleType>
     </xs:union>
  </xs:simpleType>

  <xs:simpleType name="currencyPair">
    <xs:restriction base="xs:string">
      <xs:length value="6"/>
      <!-- FIXME: can we reference above list?-->
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="businessDayConvention">
    <xs:restriction base="xs:string">
      <xs:enumeration value="F"/>
      <xs:enumeration value="Following"/>
      <xs:enumeration value="FOLLOWING"/>
      <xs:enumeration value="MF"/>
      <xs:enumeration value="ModifiedFollowing"/>
      <xs:enumeration value="Modified Following"/>
      <xs:enumeration value="MODIFIEDF"/>
      <xs:enumeration value="MODFOLLOWING"/>
      <xs:enumeration value="P"/>
      <xs:enumeration value="Preceding"/>
      <xs:enumeration value="PRECEDING"/>
      <xs:enumeration value="MP"/>
      <xs:enumeration value="ModifiedPreceding"/>
      <xs:enumeration value="Modified Preceding"/>
      <xs:enumeration value="MODIFIEDP"/>
      <xs:enumeration value="U"/>
      <xs:enumeration value="Unadjusted"/>
      <xs:enumeration value="INDIFF"/>
      <xs:enumeration value="HMMF"/>
      <xs:enumeration value="HalfMonthModifiedFollowing"/>
      <xs:enumeration value="HalfMonthMF"/>
      <xs:enumeration value="Half Month Modified Following"/>
      <xs:enumeration value="HALFMONTHMF"/>
      <xs:enumeration value="NEAREST"/>
      <xs:enumeration value="NONE"/>
      <xs:enumeration value="NotApplicable"/>
      <xs:enumeration value=""/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="calendar">
    <xs:restriction base="xs:string">
      <xs:pattern value="(JoinHolidays\(|JoinBusinessDays\()?((,)?(, )?(^)?(TGT|TARGET|CN-IB|US-FED|US-GOV|US-NERC|US-NYSE|US-SET|US-SOFR|Australia|Australia settlement|Botswana|Brazil|Canada|Denmark|Hungary|Japan|Norway|Switzerland|Sweden|Belgium|Finland|Luxembourg|Spain|Austria|Romania|Poland|New Zealand|Cyprus|Greece|Ireland|FRA|CATO|CHZU|JPTO|GBLO|SEST|TRIS|USNY|EUTA|BEBR|AE|AT|AR|AU|BW|BR|CA|CL|CN|CO|CY|CZ|DK|FI|FR|GR|DE|HK|HU|IE|IS|IN|ID|IL|IT|JE|JP|MX|MY|NL|NO|NZ|PE|PH|PL|RO|RU|SG|ZA|KR|SE|CH|TW|TH|TR|UA|GB|US|BE|LU|ES|AED|AFN|ALL|AMD|ANG|AOA|ARS|AUD|AWG|AZN|BAM|BBD|BDT|BGN|BHD|BIF|BMD|BND|BOB|BOV|BRL|BSD|BTN|BWP|BYN|BZD|CAD|CDF|CHE|CHF|CHW|CLF|CLP|CNH|CNT|CNY|COP|COU|CRC|CUC|CUP|CVE|CYP|CZK|DJF|DKK|DOP|DZD|EGP|ERN|ETB|EUR|FJD|FKP|GBP|GEL|GGP|GHS|GIP|GMD|GNF|GTQ|GYD|HKD|HNL|HRK|HTG|HUF|IDR|ILS|IMP|INR|IQD|IRR|ISK|JEP|JMD|JOD|JPY|KES|KGS|KHR|KID|KMF|KPW|KRW|KWD|KYD|KZT|LAK|LBP|LKR|LRD|LSL|LTL|LVL|LYD|MAD|MDL|MGA|MKD|MMK|MNT|MOP|MRU|MUR|MUS|MU|MVR|MWK|MXN|MXV|MYR|MZN|NAD|NGN|NIO|NOK|NPR|NZD|OMR|PAB|PEN|PGK|PHP|PKR|PLN|PYG|QAR|RON|RSD|RUB|RWF|SAR|SBD|SCR|SDG|SEK|SGD|SHP|SLL|SKK|SOS|SRD|SSP|STN|SVC|SYP|SZL|THB|TJS|TMT|TND|TOP|TRY|TTD|TWD|TZS|UAH|UGX|USD|USN|UYI|UYU|UYW|UZS|VES|VND|VUV|WST|XAF|XAG|XAU|XCD|XOF|XPD|XPF|XPT|XSU|XUA|YER|ZAR|ZMW|ZWL|XBT|BTC|ETH|ETC|BCH|XRP|LTC|ZUR|ZUG|GBp|GBX|ILa|ILA|ILX|ILs|KWf|ZAc|ZAC|ZAX|USc|ARE|ARG|AUS|BWA|BRA|CAN|CHL|CHN|COL|CZE|DNK|FIN|GRC|DEU|HKG|HUN|ISL|IND|IDN|ISR|ITA|JPN|MEX|MYS|NLD|NOR|NZL|PER|PHL|POL|ROU|RUS|SAU|SGP|ZAF|KOR|SWE|TWN|THA|TUR|UKR|GBR|USA|BEL|LUX|ESP|AUT|FRF|IRL|ITL|BEF|LUF|ATS|XASX|BVMF|XTSE|XSHG|XFRA|XDUB|XPAR|XETR|ECAG|EUWA|XJKT|XIDX|XTAE|XMIL|MISX|XKRX|XSWX|XLON|XLME|XNYS|DEN|TSX|CRC|PYG|JMD|Australia exchange|BOVESPA|Euwax|CME Group|Eurex|Xetra|Frankfurt stock exchange|French settlement|German settlement|Paris stock exchange|Telbor|National Stock Exchange of India|London stock exchange|LNB|London metals exchange|Iceland stock exchange|New York stock exchange|Italian settlement|Santiago Stock Exchange|Ukrainian stock exchange|NGL|NYB|SA|SS|SYB|TKB|TRB|UK|UK settlement|Jakarta stock exchange|US settlement|Belgian settlement|US with Libor impact|US government bond market|North American Energy Reliability Council|Federal Reserve Bankwire System|SOFR fixing calendar|WMR|ZUB|ICE_FuturesUS|ICE_FuturesUS_1|ICE_FuturesUS_2|ICE_FuturesEU|ICE_FuturesEU_1|ICE_EndexEnergy|ICE_EndexEquities|ICE_SwapTradeUS|ICE Futures U.S.|ICE Futures U.S. 1|ICE Futures U.S. 2|ICE Futures Europe|ICE Futures Europe 1|ICE Endex Energy|ICE Endex Equities|ICE Swap Trade U.S.|ICE Swap Trade U.K.|ICE Futures Singapore|ICE_SwapTradeUK|ICE_FuturesSingapore|CME|Thailand stock exchange|Turkey|Milan stock exchange|Taiwan stock exchange|South-Korean settlement|South-Korea exchange|South Africa|Bratislava stock exchange|Moscow exchange|Mexican stock exchange|Russian settlement|Spanish settlement|Luxembourgish settlement|Dutch settlement|Austrian settlement|Tadawul|Tel Aviv stock exchange|Bucharest stock exchange|Singapore exchange|Hong Kong stock exchange|Vienna stock exchange|Prague stock exchange|China inter bank market|Shanghai stock exchange|Colombia Stock Exchange|Buenos Aires stock exchange|Philippine stock exchange|Lima stock exchange|SIX Swiss Exchange|Malaysia Stock Exchange|Thomson Reuters WM/Reuters Spot|Israel Telbor Implementation|WeekendsOnly|weekends only|IslamicWeekendsOnly|islamic weekends only|Islamic weekends only|UNMAPPED|Null|NullCalendar|[A-Z]{4}|CUSTOM_.*))*(\))?"/>
    </xs:restriction>
  </xs:simpleType>
  <xs:simpleType name="dayCounter">
    <xs:restriction base="xs:string">
      <xs:enumeration value="A360"/>
      <xs:enumeration value="Actual/360"/>
      <xs:enumeration value="ACT/360"/>
      <xs:enumeration value="Act/360"/>
      <xs:enumeration value="A360 (Incl Last)"/>
      <xs:enumeration value="Actual/360 (Incl Last)"/>
      <xs:enumeration value="ACT/360 (Incl Last)"/>
      <xs:enumeration value="A365"/>
      <xs:enumeration value="A365F"/>
      <xs:enumeration value="Actual/365 (Fixed)"/>
      <xs:enumeration value="Actual/365 (fixed)"/>
      <xs:enumeration value="ACT/365.FIXED"/>
      <xs:enumeration value="ACT/365"/>
      <xs:enumeration value="ACT/365L"/>
      <xs:enumeration value="Act/365"/>
      <xs:enumeration value="Act/365L"/>
      <xs:enumeration value="Act/365 (Canadian Bond)"/>
      <xs:enumeration value="T360"/>
      <xs:enumeration value="30/360"/>
      <xs:enumeration value="30/360 US"/>
      <xs:enumeration value="30/360 (US)"/>
      <xs:enumeration value="30/360 NASD"/>
      <xs:enumeration value="30U/360"/>
      <xs:enumeration value="30US/360"/>
      <xs:enumeration value="30/360 (Bond Basis)"/>
      <xs:enumeration value="ACT/nACT"/>
      <xs:enumeration value="30E/360 (Eurobond Basis)"/>
      <xs:enumeration value="30/360 AIBD (Euro)"/>
      <xs:enumeration value="30E/360.ICMA"/>
      <xs:enumeration value="30E/360 ICMA"/>
      <xs:enumeration value="30E/360"/>
      <xs:enumeration value="30E/360E"/>
      <xs:enumeration value="30E/360.ISDA"/>
      <xs:enumeration value="30E/360 ISDA"/>
      <xs:enumeration value="30/360 German"/>
      <xs:enumeration value="30/360 (German)"/>
      <xs:enumeration value="30/360 Italian"/>
      <xs:enumeration value="30/360 (Italian)"/>
      <xs:enumeration value="ActActISDA"/>
      <xs:enumeration value="ACT/ACT.ISDA"/>
      <xs:enumeration value="Actual/Actual (ISDA)"/>
      <xs:enumeration value="ActualActual (ISDA)"/>
      <xs:enumeration value="ACT/ACT"/>
      <xs:enumeration value="Act/Act"/>
      <xs:enumeration value="ACT29"/>
      <xs:enumeration value="ACT"/>
      <xs:enumeration value="ActActISMA"/>
      <xs:enumeration value="Actual/Actual (ISMA)"/>
      <xs:enumeration value="ActualActual (ISMA)"/>
      <xs:enumeration value="ACT/ACT.ISMA"/>
      <xs:enumeration value="ActActICMA"/>
      <xs:enumeration value="Actual/Actual (ICMA)"/>
      <xs:enumeration value="ActualActual (ICMA)"/>
      <xs:enumeration value="ACT/ACT.ICMA"/>
      <xs:enumeration value="ActActAFB"/>
      <xs:enumeration value="ACT/ACT.AFB"/>
      <xs:enumeration value="Actual/Actual (AFB)"/>
      <xs:enumeration value="1/1"/>
      <xs:enumeration value="BUS/252"/>
      <xs:enumeration value="Business/252"/>
      <xs:enumeration value="Actual/365 (No Leap)"/>
      <xs:enumeration value="Act/365 (NL)"/>
      <xs:enumeration value="NL/365"/>
      <xs:enumeration value="Actual/365 (JGB)"/>
      <xs:enumeration value="Simple"/>
      <xs:enumeration value="Year"/>
      <xs:enumeration value="A364"/>
      <xs:enumeration value="Actual/364"/>
      <xs:enumeration value="Act/364"/>
      <xs:enumeration value="ACT/364"/>
      <xs:enumeration value="Month"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="frequencyType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Z"/>
      <xs:enumeration value="Once"/>
      <xs:enumeration value="A"/>
      <xs:enumeration value="Annual"/>
      <xs:enumeration value="S"/>
      <xs:enumeration value="Semiannual"/>
      <xs:enumeration value="Q"/>
      <xs:enumeration value="Quarterly"/>
      <xs:enumeration value="B"/>
      <xs:enumeration value="Bimonthly"/>
      <xs:enumeration value="M"/>
      <xs:enumeration value="Monthly"/>
      <xs:enumeration value="L"/>
      <xs:enumeration value="Lunarmonth"/>
      <xs:enumeration value="W"/>
      <xs:enumeration value="Weekly"/>
      <xs:enumeration value="D"/>
      <xs:enumeration value="Daily"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="dateRule">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Backward"/>
      <xs:enumeration value="Forward"/>
      <xs:enumeration value="Zero"/>
      <xs:enumeration value="ThirdWednesday"/>
      <xs:enumeration value="Twentieth"/>
      <xs:enumeration value="TwentiethIMM"/>
      <xs:enumeration value="OldCDS"/>
      <xs:enumeration value="CDS"/>
      <xs:enumeration value="CDS2015"/>
      <xs:enumeration value="ThirdThursday"/>
      <xs:enumeration value="ThirdFriday"/>
      <xs:enumeration value="MondayAfterThirdFriday"/>
      <xs:enumeration value="TuesdayAfterThirdFriday"/>
      <xs:enumeration value="LastWednesday"/>
      <xs:enumeration value="EveryThursday"/>
      <xs:enumeration value="EighthBusinessDay"/>
      <xs:enumeration value=""/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="subPeriodsCouponType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Compounding"/>
      <xs:enumeration value="Averaging"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="compounding">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Simple"/>
      <xs:enumeration value="Compounded"/>
      <xs:enumeration value="Continuous"/>
      <xs:enumeration value="SimpleThenCompounded"/>
      <xs:enumeration value=""/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="interpolationVariableType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Zero"/>
      <xs:enumeration value="Discount"/>
      <xs:enumeration value="Forward"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="interpolationMethodType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Linear"/>
      <xs:enumeration value="Flat"/>
      <xs:enumeration value="LogLinear"/>
      <xs:enumeration value="NaturalCubic"/>
      <xs:enumeration value="FinancialCubic"/>
      <xs:enumeration value="Cubic"/>
      <xs:enumeration value="Hermite"/>
      <xs:enumeration value="Quadratic"/>
      <xs:enumeration value="LogQuadratic"/>
      <xs:enumeration value="LogNaturalCubic"/>
      <xs:enumeration value="LogFinancialCubic"/>
      <xs:enumeration value="LogCubicSpline"/>
      <xs:enumeration value="MonotonicLogCubicSpline"/>
      <xs:enumeration value="CubicSpline"/>
      <xs:enumeration value="LinearFlat"/>
      <xs:enumeration value="LogLinearFlat"/>
      <xs:enumeration value="CubicFlat"/>
      <xs:enumeration value="HermiteFlat"/>
      <xs:enumeration value="ConvexMonotone"/>
      <xs:enumeration value="DefaultLogMixedLinearCubic"/>
      <xs:enumeration value="MonotonicLogMixedLinearCubic"/>
      <xs:enumeration value="KrugerLogMixedLinearCubic"/>
      <xs:enumeration value="LogMixedLinearCubicNaturalSpline"/>
      <xs:enumeration value="ExponentialSplines"/>
      <xs:enumeration value="NelsonSiegel"/>
      <xs:enumeration value="Svensson"/>
      <xs:enumeration value="BackwardFlat"/>
      <xs:enumeration value="ForwardFlat"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="sviModelVariantType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Gatheral2004SviRaw"/>
      <xs:enumeration value="Gatheral2004SviNatural"/>
      <xs:enumeration value="Gatheral2004SviJw"/>
      <xs:enumeration value="Gatheral2012SsviHeston"/>
      <xs:enumeration value="Gatheral2012SsviPowerLaw"/>
      <xs:enumeration value="HendriksMartini2017EssviFirstPowerLaw"/>
      <xs:enumeration value="HendriksMartini2017EssviSecondPowerLaw"/>
      <xs:enumeration value="CorbettaEtAl2019Essvi"/>
      <xs:enumeration value="Mingone2022EssviGJ"/>
      <xs:enumeration value="Mingone2022EssviMM"/>
    </xs:restriction>
  </xs:simpleType>

  <!-- Union of standard interpolation methods and SVI parametric model variants.
       Used for TimeInterpolation and StrikeInterpolation on volatility surfaces. -->
  <xs:simpleType name="volatilityInterpolationType">
    <xs:union memberTypes="interpolationMethodType sviModelVariantType"/>
  </xs:simpleType>

  <xs:simpleType name="cdsType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="SpreadCDS"/>
      <xs:enumeration value="HazardRate"/>
      <xs:enumeration value="Yield"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="equityType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="ForwardPrice"/>
      <xs:enumeration value="ForwardDividendPrice"/>
      <xs:enumeration value="OptionPremium"/>
      <xs:enumeration value="DividendYield"/>
      <xs:enumeration value="NoDividends"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="dimensionType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="ATM"/>
      <xs:enumeration value="ATMTriangulated"/>
      <xs:enumeration value="Smile"/>
      <xs:enumeration value="Constant"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="smileType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="VannaVolga"/>
      <xs:enumeration value="Delta"/>
      <xs:enumeration value="BFRR"/>
      <xs:enumeration value="Absolute"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="fxVolInterpolation">
    <xs:union memberTypes="sviModelVariantType">
      <xs:simpleType>
        <xs:restriction base="xs:string">
          <xs:enumeration value=""/>
          <xs:enumeration value="VannaVolga1"/>
          <xs:enumeration value="VannaVolga2"/>
          <xs:enumeration value="Linear"/>
          <xs:enumeration value="Cubic"/>
        </xs:restriction>
      </xs:simpleType>
    </xs:union>
  </xs:simpleType>

  <xs:simpleType name="correlationType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="CMSSpread"/>
      <xs:enumeration value="Generic"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="correlationQuoteType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="RATE"/>
      <xs:enumeration value="PRICE"/>
      <xs:enumeration value="NULL"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="volatilityType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Normal"/>
      <xs:enumeration value="Lognormal"/>
      <xs:enumeration value="ShiftedLognormal"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="extrapolationType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Linear"/>
      <xs:enumeration value="Flat"/>
      <xs:enumeration value="UseInterpolator"/>
      <xs:enumeration value="None"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="longShort">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Long"/>
      <xs:enumeration value="Short"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="inflationType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="ZC"/>
      <xs:enumeration value="YY"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="amortizationType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="FixedAmount"/>
      <xs:enumeration value="RelativeToInitialNotional"/>
      <xs:enumeration value="RelativeToPreviousNotional"/>
      <xs:enumeration value="Annuity"/>
      <xs:enumeration value="LinearToMaturity"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="settlementType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Physical"/>
      <xs:enumeration value="Cash"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="momentType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Variance"/>
      <xs:enumeration value="Volatility"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="settlementMethod">
    <xs:restriction base="xs:string">
      <xs:enumeration value="PhysicalOTC"/>
      <xs:enumeration value="PhysicalCleared"/>
      <xs:enumeration value="CollateralizedCashPrice"/>
      <xs:enumeration value="ParYieldCurve"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="capFloor">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Cap"/>
      <xs:enumeration value="Floor"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="exerciseStyle">
    <xs:restriction base="xs:string">
      <xs:enumeration value="European"/>
      <xs:enumeration value="Bermudan"/>
      <xs:enumeration value="American"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="weekdayType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Mon"/>
      <xs:enumeration value="Tue"/>
      <xs:enumeration value="Wed"/>
      <xs:enumeration value="Thu"/>
      <xs:enumeration value="Fri"/>
      <xs:enumeration value="Sat"/>
      <xs:enumeration value="Sun"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="monthType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Jan"/>
      <xs:enumeration value="Feb"/>
      <xs:enumeration value="Mar"/>
      <xs:enumeration value="Apr"/>
      <xs:enumeration value="May"/>
      <xs:enumeration value="Jun"/>
      <xs:enumeration value="Jul"/>
      <xs:enumeration value="Aug"/>
      <xs:enumeration value="Sep"/>
      <xs:enumeration value="Oct"/>
      <xs:enumeration value="Nov"/>
      <xs:enumeration value="Dec"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="cdsTierType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="SNRFOR"/>
      <xs:enumeration value="SUBLT2"/>
      <xs:enumeration value="SNRLAC"/>
      <xs:enumeration value="SECDOM"/>
      <xs:enumeration value="JRSUBUT2"/>
      <xs:enumeration value="PREFT1"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="cdsDocClauseType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="CR"/>
      <xs:enumeration value="MM"/>
      <xs:enumeration value="MR"/>
      <xs:enumeration value="XR"/>
      <xs:enumeration value="CR14"/>
      <xs:enumeration value="MM14"/>
      <xs:enumeration value="MR14"/>
      <xs:enumeration value="XR14"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="positiveDecimal">
    <xs:restriction base="xs:decimal">
      <xs:minExclusive value="0"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="optionType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Call"/>
      <xs:enumeration value="Put"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="bondPriceType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Clean"/>
      <xs:enumeration value="Dirty"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="strikeDeltaType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Spot"/>
      <xs:enumeration value="Fwd"/>
      <xs:enumeration value="PaSpot"/>
      <xs:enumeration value="PaFwd"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="strikeAtmType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="AtmSpot"/>
      <xs:enumeration value="AtmFwd"/>
      <xs:enumeration value="AtmDeltaNeutral"/>
      <xs:enumeration value="AtmVegaMax"/>
      <xs:enumeration value="AtmGammaMax"/>
      <xs:enumeration value="AtmPutCall50"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="strikeMoneynessType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Spot"/>
      <xs:enumeration value="Fwd"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="non-negative-decimal">
    <xs:annotation>
      <xs:documentation>The non-negative-decimal type specifies a non-negative decimal value.</xs:documentation>
    </xs:annotation>
    <xs:restriction base="xs:decimal">
      <xs:minInclusive value="0"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="optionPayRelativeTo">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Expiry"/>
      <xs:enumeration value="Exercise"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="overnightIndexFutureNettingType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Averaging"/>
      <xs:enumeration value="Compounding"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="futureDateGenerationRule">
    <xs:restriction base="xs:string">
      <xs:enumeration value="IMM"/>
      <xs:enumeration value="FirstDayOfMonth"/>
      <xs:enumeration value="IMMAUD"/>
      <xs:enumeration value="SecondThursday"/><!-- backward-compatible alias for IMMAUD -->
      <xs:enumeration value="IMMNZD"/>
      <xs:enumeration value="IMMCAD"/>
      <xs:enumeration value="IMMEUR"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="dayOfMonth">
    <xs:restriction base="xs:integer">
      <xs:minInclusive value="1"/>
      <xs:maxInclusive value="31"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="recoveryRate">
    <xs:restriction base="xs:decimal">
      <xs:minInclusive value="0"/>
      <xs:maxInclusive value="1"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="weightType">
    <xs:restriction base="xs:decimal">
      <xs:minInclusive value="0"/>
      <xs:maxInclusive value="1"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="riskFactorKeyType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="DiscountCurve"/>
      <xs:enumeration value="YieldCurve"/>
      <xs:enumeration value="IndexCurve"/>
      <xs:enumeration value="SwaptionVolatility"/>
      <xs:enumeration value="YieldVolatility"/>
      <xs:enumeration value="OptionletVolatility"/>
      <xs:enumeration value="FXSpot"/>
      <xs:enumeration value="FXVolatility"/>
      <xs:enumeration value="EquitySpot"/>
      <xs:enumeration value="EquityVolatility"/>
      <xs:enumeration value="DividendYield"/>
      <xs:enumeration value="SurvivalProbability"/>
      <xs:enumeration value="RecoveryRate"/>
      <xs:enumeration value="CDSVolatility"/>
      <xs:enumeration value="BaseCorrelation"/>
      <xs:enumeration value="CPIIndex"/>
      <xs:enumeration value="ZeroInflationCurve"/>
      <xs:enumeration value="YoYInflationCurve"/>
      <xs:enumeration value="YoYInflationCapFloorVolatility"/>
      <xs:enumeration value="ZeroInflationCapFloorVolatility"/>
      <xs:enumeration value="CommodityCurve"/>
      <xs:enumeration value="CommodityVolatility"/>
      <xs:enumeration value="SecuritySpread"/>
      <xs:enumeration value="Correlation"/>
      <xs:enumeration value="CPR"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="scheduleData">
    <xs:choice minOccurs="0" maxOccurs="unbounded">
      <xs:element name="Rules">
        <xs:complexType>
          <xs:all>
            <xs:element type="date" name="StartDate"/>
            <xs:element type="date" name="EndDate" minOccurs="0"/>
            <xs:element type="bool" name="AdjustEndDateToPreviousMonthEnd" minOccurs="0" maxOccurs="1"/>
            <xs:element type="xs:string" name="Tenor"/>
            <xs:element type="calendar" name="Calendar" minOccurs="0"/>
            <xs:element type="businessDayConvention" name="Convention"/>
            <xs:element type="businessDayConvention" name="TermConvention" minOccurs="0"/>
            <xs:element type="dateRule" name="Rule" minOccurs="0"/>
            <xs:element type="bool" name="EndOfMonth" minOccurs="0"/>
            <xs:element type="businessDayConvention" name="EndOfMonthConvention" minOccurs="0"/>
            <xs:element type="date" name="FirstDate" minOccurs="0"/>
            <xs:element type="date" name="LastDate" minOccurs="0"/>
            <xs:element type="xs:boolean" name="RemoveFirstDate" minOccurs="0"/>
            <xs:element type="xs:boolean" name="RemoveLastDate" minOccurs="0"/>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element name="Dates">
        <xs:complexType>
          <xs:all>
            <xs:element type="calendar" name="Calendar" minOccurs="0"/>
            <xs:element type="businessDayConvention" name="Convention" minOccurs="0"/>
            <xs:element type="xs:string" name="Tenor" minOccurs="0"/>
            <xs:element type="bool" name="EndOfMonth" minOccurs="0"/>
            <xs:element type="bool" name="IncludeDuplicateDates" minOccurs="0" />
            <xs:element name="Dates">
              <xs:complexType>
                <xs:sequence>
                  <xs:element type="date" name="Date" maxOccurs="unbounded"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
      <xs:element ref="DerivedScheduleGroup"/>
    </xs:choice>
    <xs:attribute name="name" type="xs:string" use="optional"/>
  </xs:complexType>

  <xs:simpleType name="publicationRoll">
    <xs:restriction base="xs:string">
      <xs:enumeration value="None"/>
      <xs:enumeration value="OnPublicationDate"/>
      <xs:enumeration value="AfterPublicationDate"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="averagingDataPeriodType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="PreviousMonth"/>
      <xs:enumeration value="ExpiryToExpiry"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="priceSegmentTypeType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Future"/>
      <xs:enumeration value="AveragingFuture"/>
      <xs:enumeration value="AveragingSpot"/>
      <xs:enumeration value="AveragingOffPeakPower"/>
      <xs:enumeration value="OffPeakPowerDaily"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:complexType name="premiumData">
    <xs:sequence>
      <xs:element name="Premium" minOccurs="0" maxOccurs="unbounded">
        <xs:complexType>
          <xs:all>
            <xs:element type="xs:float" name="Amount"/>
            <xs:element type="extendedCurrencyCode" name="Currency"/>
            <xs:element type="date" name="PayDate"/>
            <xs:element name="SettlementData" minOccurs="0">
              <xs:complexType>
                <xs:all>
                  <xs:element type="currencyCode" name="PayCurrency"/>
                  <xs:element type="xs:string" name="FXIndex"/>
                  <xs:element type="xs:string" name="FixingDate" minOccurs="0" maxOccurs="1" />
                </xs:all>
              </xs:complexType>
            </xs:element>
          </xs:all>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:simpleType name="dateDeltaUnit">
    <xs:restriction base="xs:string">
        <xs:enumeration value="CalendarDays"/>
        <xs:enumeration value="BusinessDays"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="dateDeltaAnchor">
    <xs:restriction base="xs:string">
        <xs:enumeration value="Adjusted"/>
        <xs:enumeration value="Unadjusted"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:element name="DerivedScheduleGroup" abstract="true"/>
  <xs:element name="DerivedSchedule" type="DerivedScheduleType" substitutionGroup="DerivedScheduleGroup"/>
  <xs:element name="Derived" type="DerivedScheduleType" substitutionGroup="DerivedScheduleGroup"/>

  <xs:complexType name="DerivedScheduleType">
    <xs:all>
      <xs:element type="xs:string" name="BaseSchedule"/>
      <xs:element type="xs:string" name="Shift" minOccurs="0"/>
      <xs:element type="calendar" name="Calendar" minOccurs="0"/>
      <xs:element type="businessDayConvention" name="Convention" minOccurs="0"/>
      <xs:element type="xs:boolean" name="RemoveFirstDate" minOccurs="0"/>
      <xs:element type="xs:boolean" name="RemoveLastDate" minOccurs="0"/>
      <xs:element type="dateDeltaUnit" name="ShiftUnit" minOccurs="0"/>
      <xs:element type="dateDeltaAnchor" name="ShiftAnchor" minOccurs="0"/>
    </xs:all>
  </xs:complexType>

  <xs:simpleType name="CurrencyHedgedIndexRebalancingStrategy">
    <xs:restriction base="xs:string">
      <xs:enumeration value="EndOfMonth"/>
    </xs:restriction>
   </xs:simpleType>

  <xs:simpleType name="CurrencyHedgedIndexRehedgingStrategy">
    <xs:restriction base="xs:string">
      <xs:enumeration value="None"/>
      <xs:enumeration value="Daily"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="mporCashFlowMode">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Unspecified"/>
      <xs:enumeration value="NonePay"/>
      <xs:enumeration value="BothPay"/>
      <xs:enumeration value="WePay"/>
      <xs:enumeration value="TheyPay"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="shiftType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Relative"/>
      <xs:enumeration value="Absolute"/>
	  <xs:enumeration value="EqualTo"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="shiftScheme">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Forward"/>
      <xs:enumeration value="Backward"/>
      <xs:enumeration value="Central"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="parametricVolatilityParameterCalibration">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Fixed"/>
      <xs:enumeration value="Calibrated"/>
      <xs:enumeration value="Implied"/>
    </xs:restriction>
  </xs:simpleType>

  <xs:simpleType name="roundingType">
    <xs:restriction base="xs:string">
      <xs:enumeration value="Up"/>
      <xs:enumeration value="Down"/>
      <xs:enumeration value="Closest"/>
      <xs:enumeration value="Floor"/>
      <xs:enumeration value="Ceiling"/>
    </xs:restriction>
  </xs:simpleType>

</xs:schema>
$xsd$, 'e29cb61c68f77d24e02e0116ed9dc81512e52c73d117da37e4e4fa6c24d85af5', 41374),
        ('6f41333f-9321-5ce6-b55c-342a2b904aca'::uuid, 'intraday_power_load_data_schema', '1.8.17.0', $xsd$<xs:schema attributeFormDefault="unqualified" elementFormDefault="qualified" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xs:complexType name="loadFactorType">
    <xs:simpleContent>
      <xs:extension base="xs:double">
        <xs:attribute type="xs:integer" name="from" use="required"/>
        <xs:attribute type="xs:integer" name="to" use="required"/>
        <xs:attribute type="xs:string" name="unit" use="optional"/>
        <xs:attribute type="bool" name="dst" use="optional"/>
      </xs:extension>
    </xs:simpleContent>
  </xs:complexType>

  <xs:complexType name="loadFactorsType">
    <xs:sequence>
      <xs:element type="loadFactorType" name="LoadFactor" minOccurs="0" maxOccurs="unbounded"/>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="explicitLoadProfileType">
    <xs:sequence>
      <xs:element name="LoadProfileDatum" minOccurs="0" maxOccurs="unbounded">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="date" name="Date" minOccurs="1" maxOccurs="1"/>
            <xs:element type="loadFactorsType" name="LoadFactors" minOccurs="1" maxOccurs="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="ruleBasedLoadProfileType">
    <xs:sequence>
      <xs:element name="LoadProfileBusinessDayRule" minOccurs="0" maxOccurs="unbounded">
        <xs:complexType>
          <xs:sequence>
            <xs:element type="date" name="Date" minOccurs="1" maxOccurs="1"/>
            <xs:element type="xs:string" name="Calendar" minOccurs="1" maxOccurs="1"/>
            <xs:element type="loadFactorsType" name="BusinessDayLoadFactors" minOccurs="0" maxOccurs="1"/>
            <xs:element type="loadFactorsType" name="NonBusinessDayLoadFactors" minOccurs="0" maxOccurs="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    </xs:sequence>
  </xs:complexType>

  <xs:complexType name="powerLoadProfileDataType">
    <xs:choice>
      <xs:element type="explicitLoadProfileType" name="ExplicitDates"/>
      <xs:element type="ruleBasedLoadProfileType" name="BusinessDayRules"/>
    </xs:choice>
  </xs:complexType>

  <xs:complexType name="intradayPowerLoad">
    <xs:all>
      <xs:element type="xs:string" name="Id" minOccurs="1" maxOccurs="1"/>
      <xs:element type="powerLoadProfileDataType" name="PowerLoadProfileData" minOccurs="0" maxOccurs="1"/>
    </xs:all>
  </xs:complexType>

</xs:schema>
$xsd$, '076651b526326218e2ead45cd3d45fb76f887119e6c9f4cbc1e0dc9a6d86517b', 2476)
        ) as t (id, ore_file_code, engine_version, content, sha256, bytes)
    loop
        if not exists (
            select 1 from ores_reporting_ore_file_versions_tbl
            where tenant_id = v_sys_tenant and ore_file_code = r.ore_file_code
              and engine_version = r.engine_version
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_reporting_ore_file_versions_tbl (
                id, ore_file_code, engine_version, content, sha256, bytes,
                tenant_id, version, modified_by, change_reason_code, change_commentary
            ) values (
                r.id, r.ore_file_code, r.engine_version, r.content, r.sha256, r.bytes,
                v_sys_tenant, 0, current_user, 'system.initial_load',
                'Seed ORE file version: ' || r.ore_file_code || '@' || r.engine_version
            );
        end if;
    end loop;
end $$;
