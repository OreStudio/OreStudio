/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.ore.core/domain/curve_configuration_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.refdata.api/domain/base_correlation_config.hpp"
#include "ores.refdata.api/domain/bond_future_volatility_config.hpp"
#include "ores.refdata.api/domain/cap_floor_volatility_config.hpp"
#include "ores.refdata.api/domain/cds_volatility_config.hpp"
#include "ores.refdata.api/domain/cds_volatility_term.hpp"
#include "ores.refdata.api/domain/commodity_curve_config.hpp"
#include "ores.refdata.api/domain/commodity_price_segment.hpp"
#include "ores.refdata.api/domain/commodity_volatility_config.hpp"
#include "ores.refdata.api/domain/curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/curve_configuration.hpp"
#include "ores.refdata.api/domain/curve_configuration_section.hpp"
#include "ores.refdata.api/domain/curve_correlation_config.hpp"
#include "ores.refdata.api/domain/curve_definition.hpp"
#include "ores.refdata.api/domain/curve_global_report.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile_parameter.hpp"
#include "ores.refdata.api/domain/curve_quote.hpp"
#include "ores.refdata.api/domain/curve_report_configuration.hpp"
#include "ores.refdata.api/domain/curve_security_config.hpp"
#include "ores.refdata.api/domain/curve_segment.hpp"
#include "ores.refdata.api/domain/curve_segment_curve.hpp"
#include "ores.refdata.api/domain/curve_volatility_config.hpp"
#include "ores.refdata.api/domain/default_curve_config.hpp"
#include "ores.refdata.api/domain/default_curve_configuration.hpp"
#include "ores.refdata.api/domain/equity_curve_config.hpp"
#include "ores.refdata.api/domain/equity_volatility_config.hpp"
#include "ores.refdata.api/domain/fx_volatility_config.hpp"
#include "ores.refdata.api/domain/inflation_cap_floor_volatility_config.hpp"
#include "ores.refdata.api/domain/inflation_curve_config.hpp"
#include "ores.refdata.api/domain/inflation_seasonality_factor.hpp"
#include "ores.refdata.api/domain/intraday_power_curve_config.hpp"
#include "ores.refdata.api/domain/swaption_volatility_config.hpp"
#include "ores.refdata.api/domain/yield_curve_config.hpp"
#include "ores.refdata.api/domain/yield_volatility_config.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <format>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

namespace curve_configuration_projection_xg = xml::generated::curve_configuration;
using xml::record_reader;
using xml::record_writer;

constexpr std::string_view curve_configuration_projection_audit_modified_by = "ores";
constexpr std::string_view curve_configuration_projection_audit_reason_code =
    "system.external_data_import";
constexpr std::string_view curve_configuration_projection_audit_commentary =
    "Imported from ORE XML";

constexpr std::string_view yield_curves_section = "YieldCurves";
constexpr std::string_view equity_curves_section = "EquityCurves";
constexpr std::string_view inflation_curves_section = "InflationCurves";
constexpr std::string_view default_curves_section = "DefaultCurves";
constexpr std::string_view commodity_curves_section = "CommodityCurves";
constexpr std::string_view fx_volatilities_section = "FXVolatilities";
constexpr std::string_view yield_volatilities_section = "YieldVolatilities";
constexpr std::string_view base_correlations_section = "BaseCorrelations";
constexpr std::string_view correlations_section = "Correlations";
constexpr std::string_view cds_volatilities_section = "CDSVolatilities";
constexpr std::string_view inflation_cap_floor_volatilities_section =
    "InflationCapFloorVolatilities";
constexpr std::string_view strike_surface_kind = "StrikeSurface";
constexpr std::string_view swaption_volatilities_section = "SwaptionVolatilities";
constexpr std::string_view cap_floor_volatilities_section = "CapFloorVolatilities";
constexpr std::string_view equity_volatilities_section = "EquityVolatilities";
constexpr std::string_view commodity_volatilities_section = "CommodityVolatilities";
constexpr std::string_view bond_future_volatilities_section = "BondFutureVolatilities";
constexpr std::string_view constant_kind = "Constant";
constexpr std::string_view curve_kind = "Curve";
constexpr std::string_view delta_surface_kind = "DeltaSurface";
constexpr std::string_view proxy_surface_kind = "ProxySurface";
constexpr std::string_view curve_quotes_list = "Curve";
constexpr std::string_view wrapped_curve_quotes_list = "VolatilityConfig/Curve";

constexpr std::string_view basis_quotes_list = "BasisQuotes";
constexpr std::string_view off_peak_quotes_list = "OffPeakQuotes";
constexpr std::string_view peak_quotes_list = "PeakQuotes";
constexpr std::string_view securities_section = "Securities";
constexpr std::string_view fx_spots_section = "FXSpots";
constexpr std::string_view intraday_power_curves_section = "IntradayPowerCurves";

// The sections whose entries the projection holds. A section outside this set
// is recorded only when it is empty.
bool is_modelled(std::string_view section) {
    return section == yield_curves_section || section == equity_curves_section ||
           section == inflation_curves_section || section == default_curves_section ||
           section == commodity_curves_section || section == fx_volatilities_section ||
           section == yield_volatilities_section || section == base_correlations_section ||
           section == correlations_section || section == cds_volatilities_section ||
           section == inflation_cap_floor_volatilities_section ||
           section == swaption_volatilities_section || section == cap_floor_volatilities_section ||
           section == equity_volatilities_section || section == commodity_volatilities_section ||
           section == bond_future_volatilities_section || section == securities_section ||
           section == fx_spots_section || section == intraday_power_curves_section;
}

boost::uuids::uuid curve_configuration_projection_new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

template <typename T>
void curve_configuration_projection_set_audit(T& r) {
    r.modified_by = std::string(curve_configuration_projection_audit_modified_by);
    r.performed_by = std::string(curve_configuration_projection_audit_modified_by);
    r.change_reason_code = std::string(curve_configuration_projection_audit_reason_code);
    r.change_commentary = std::string(curve_configuration_projection_audit_commentary);
}

std::runtime_error refusal(const std::string& what) {
    return std::runtime_error("curve_configuration_projection: " + what);
}

/// The text of a member, where the schema requires it and absence is a defect.
std::string required_text(const record_reader& v, std::string_view name) {
    return v.text(name).value_or("");
}

/// The spelling of an enumerated member, where the schema requires it.
std::string required_spelling(const record_reader& v, std::string_view name) {
    return v.spelling(name).value_or("");
}

std::optional<int> optional_int(const record_reader& v, std::string_view name) {
    const auto value = v.integer(name);
    if (!value)
        return std::nullopt;
    return static_cast<int>(*value);
}


/*
 * Some schema members are typed by a primitive the table holds as text: an
 * attribute of a numeric type, and an element typed xs:boolean. The table keeps
 * every attribute as text, because an attribute's type is not what a projection
 * reads; these two helpers are where such a member becomes the value it means,
 * and where the writing states it in a spelling the schema allows.
 */
std::optional<int> integer_text(const record_reader& v, std::string_view name) {
    const auto value = v.text(name);
    if (!value)
        return std::nullopt;
    try {
        return std::stoi(*value);
    } catch (const std::exception&) {
        throw refusal("'" + *value + "' is not a number for " + std::string(name));
    }
}

// The schema allows four spellings of a boolean and no others, so anything else
// stops the import rather than reading as false.
std::optional<bool> boolean_text(const record_reader& v, std::string_view name) {
    const auto value = v.text(name);
    if (!value)
        return std::nullopt;
    if (*value == "true" || *value == "1")
        return true;
    if (*value == "false" || *value == "0")
        return false;
    throw refusal("'" + *value + "' is not a boolean for " + std::string(name));
}

void set_integer_text(record_writer w, std::string_view name, const std::optional<int>& value) {
    if (value)
        w.set_text(name, std::to_string(*value));
}

void set_boolean_text(record_writer w, std::string_view name, const std::optional<bool>& value) {
    if (value)
        w.set_text(name, *value ? "true" : "false");
}

// The strike factor is a schema decimal and a column of the reference data
// holds it as a double, so the boundary converts here rather than storing one
// shape in the other.
std::optional<double> curve_configuration_projection_decimal_as_double(const record_reader& v,
                                                                       std::string_view name) {
    const auto value = v.decimal(name);
    if (!value)
        return std::nullopt;
    return value->to_double();
}

void set_decimal_from_double(record_writer w,
                             std::string_view name,
                             const std::optional<double>& value) {
    if (!value)
        return;
    const auto converted = ores::utility::decimal::decimal::from_double(*value);
    if (!converted)
        throw refusal("'" + std::to_string(*value) + "' is not a decimal amount");
    w.set_decimal(name, *converted);
}


int required_int(const record_reader& v, std::string_view name) {
    return static_cast<int>(v.integer(name).value_or(0));
}

double required_real(const record_reader& v, std::string_view name) {
    return v.real(name).value_or(0.0);
}

// One entry per section element of the document, in the order the schema
// declares them. A section is recorded as present whether or not it holds
// entries, and the entry count is what decides whether the projection can take
// it.
struct section_access {
    std::string_view code;
    std::string_view entry;
};

const std::vector<section_access>& sections() {
    static const std::vector<section_access> table = {
        {fx_spots_section, "FXSpot"},
        {fx_volatilities_section, "FXVolatility"},
        {swaption_volatilities_section, "SwaptionVolatility"},
        {yield_volatilities_section, "YieldVolatility"},
        {cap_floor_volatilities_section, "CapFloorVolatility"},
        {cds_volatilities_section, "CDSVolatility"},
        {default_curves_section, "DefaultCurve"},
        {yield_curves_section, "YieldCurve"},
        {inflation_curves_section, "InflationCurve"},
        {inflation_cap_floor_volatilities_section, "InflationCapFloorVolatility"},
        {equity_curves_section, "EquityCurve"},
        {equity_volatilities_section, "EquityVolatility"},
        {securities_section, "Security"},
        {base_correlations_section, "BaseCorrelation"},
        {commodity_curves_section, "CommodityCurve"},
        {commodity_volatilities_section, "CommodityVolatility"},
        {correlations_section, "Correlation"},
        {bond_future_volatilities_section, "BondFutureVolatility"},
        {intraday_power_curves_section, "IntradayPowerCurve"},
    };
    return table;
}

// Each segment type is written under exactly one segment element. The seeded
// curve_segment_type rows say the same, and the vocabulary test ties both to
// the schema.
const std::map<std::string, std::string, std::less<>>& segment_kinds() {
    static const std::map<std::string, std::string, std::less<>> table = {
        {"Zero", "Direct"},
        {"Discount", "Direct"},
        {"Deposit", "Simple"},
        {"FRA", "Simple"},
        {"Future", "Simple"},
        {"OIS", "Simple"},
        {"Swap", "Simple"},
        {"BMA Basis Swap", "Simple"},
        {"Average OIS", "AverageOIS"},
        {"Tenor Basis Swap", "TenorBasis"},
        {"Tenor Basis Two Swaps", "TenorBasis"},
        {"Cross Currency Basis Swap", "CrossCurrency"},
        {"Cross Currency Fix Float Swap", "CrossCurrency"},
        {"FX Forward", "CrossCurrency"},
        {"Zero Spread", "ZeroSpread"},
        {"Discount Ratio", "DiscountRatio"},
        {"FittedBond", "FittedBond"},
        {"Bond Yield Shifted", "BondYieldShifted"},
        {"Weighted Average", "WeightedAverage"},
        {"Yield Plus Default", "YieldPlusDefault"},
        {"Ibor Fallback", "IborFallback"},
    };
    return table;
}

// The quotes an element holds, read as the document writes them.
void import_quotes(ores::refdata::messaging::curve_configuration_document& out,
                   const boost::uuids::uuid& definition_id,
                   const boost::uuids::uuid& segment_id,
                   const std::optional<record_reader>& quotes,
                   const boost::uuids::uuid& configuration_id = boost::uuids::uuid{},
                   std::optional<std::string_view> list = std::nullopt);

// The state one document's projection accumulates, so each segment element's
// reader adds its rows without threading the vectors through every call.
struct import_context {
    ores::refdata::messaging::curve_configuration_document& out;
    boost::uuids::uuid definition_id;

    refdata::domain::curve_segment& segment(const std::string& type, int position) {
        refdata::domain::curve_segment s;
        s.id = curve_configuration_projection_new_uuid();
        s.curve_definition_id = definition_id;
        s.segment_type = type;
        s.position = position;
        curve_configuration_projection_set_audit(s);
        out.segments.push_back(std::move(s));
        return out.segments.back();
    }

    void quotes(const boost::uuids::uuid& segment_id, const std::optional<record_reader>& v) {
        import_quotes(out, definition_id, segment_id, v);
    }

    void composite_quotes(const boost::uuids::uuid& segment_id, const record_reader& v) {
        int position = 0;
        for (const auto& q : v.children("CompositeQuote")) {
            refdata::domain::curve_quote r;
            r.id = curve_configuration_projection_new_uuid();
            r.curve_definition_id = definition_id;
            r.curve_segment_id = segment_id;
            r.rate_quote = q.text("RateQuote");
            r.spread_quote = q.text("SpreadQuote");
            r.position = position++;
            curve_configuration_projection_set_audit(r);
            out.quotes.push_back(std::move(r));
        }
    }

    void curve(const boost::uuids::uuid& segment_id,
               std::string_view role,
               const std::string& curve_id,
               std::optional<std::string> index_name,
               std::optional<double> weight,
               int position) {
        refdata::domain::curve_segment_curve r;
        r.id = curve_configuration_projection_new_uuid();
        r.curve_segment_id = segment_id;
        r.role = std::string(role);
        r.curve = curve_id;
        r.index_name = std::move(index_name);
        r.weight = weight;
        r.position = position;
        curve_configuration_projection_set_audit(r);
        out.segment_curves.push_back(std::move(r));
    }
};

void import_quotes(ores::refdata::messaging::curve_configuration_document& out,
                   const boost::uuids::uuid& definition_id,
                   const boost::uuids::uuid& segment_id,
                   const std::optional<record_reader>& quotes,
                   const boost::uuids::uuid& configuration_id,
                   std::optional<std::string_view> list) {
    if (!quotes)
        return;
    int position = 0;
    for (const auto& q : quotes->children("Quote")) {
        refdata::domain::curve_quote r;
        r.id = curve_configuration_projection_new_uuid();
        r.curve_definition_id = definition_id;
        r.curve_segment_id = segment_id;
        r.default_curve_configuration_id = configuration_id;
        if (list)
            r.quote_list = std::string(*list);
        r.quote_text = q.own_text().value_or("");
        r.optional_flag = q.text("optional");
        r.position = position++;
        curve_configuration_projection_set_audit(r);
        out.quotes.push_back(std::move(r));
    }
}

void common_settings(refdata::domain::curve_segment& s, const record_reader& v) {
    s.pillar_choice = v.text("PillarChoice");
    s.priority = optional_int(v, "Priority");
    s.min_distance = optional_int(v, "MinDistance");
}

void restore_common_settings(record_writer r, const refdata::domain::curve_segment& s) {
    r.set_optional_text("PillarChoice", s.pillar_choice);
    r.set_optional_integer("Priority", s.priority);
    r.set_optional_integer("MinDistance", s.min_distance);
}

void import_segments(import_context& ctx, const record_reader& v) {
    int position = 0;
    for (const auto& g : v.children("Direct")) {
        auto& s = ctx.segment(required_spelling(g, "Type"), position++);
        s.conventions = g.text("Conventions");
        common_settings(s, g);
        ctx.quotes(s.id, g.child("Quotes"));
    }
    position = 0;
    for (const auto& g : v.children("Simple")) {
        auto& s = ctx.segment(required_spelling(g, "Type"), position++);
        s.conventions = required_text(g, "Conventions");
        common_settings(s, g);
        s.projection_curve = g.text("ProjectionCurve");
        ctx.quotes(s.id, g.child("Quotes"));
    }
    position = 0;
    for (const auto& g : v.children("AverageOIS")) {
        auto& s = ctx.segment(required_text(g, "Type"), position++);
        s.conventions = required_text(g, "Conventions");
        common_settings(s, g);
        s.projection_curve = g.text("ProjectionCurve");
        if (const auto quotes = g.child("Quotes"); quotes)
            ctx.composite_quotes(s.id, *quotes);
    }
    position = 0;
    for (const auto& g : v.children("TenorBasis")) {
        auto& s = ctx.segment(required_spelling(g, "Type"), position++);
        s.conventions = required_text(g, "Conventions");
        common_settings(s, g);
        s.projection_curve_pay = g.text("ProjectionCurvePay");
        s.projection_curve_receive = g.text("ProjectionCurveReceive");
        s.projection_curve_long = g.text("ProjectionCurveLong");
        s.projection_curve_short = g.text("ProjectionCurveShort");
        ctx.quotes(s.id, g.child("Quotes"));
    }
    position = 0;
    for (const auto& g : v.children("CrossCurrency")) {
        auto& s = ctx.segment(required_spelling(g, "Type"), position++);
        s.conventions = required_text(g, "Conventions");
        common_settings(s, g);
        s.discount_curve = required_text(g, "DiscountCurve");
        s.spot_rate = required_text(g, "SpotRate");
        s.projection_curve_domestic = g.text("ProjectionCurveDomestic");
        s.projection_curve_foreign = g.text("ProjectionCurveForeign");
        ctx.quotes(s.id, g.child("Quotes"));
    }
    position = 0;
    for (const auto& g : v.children("ZeroSpread")) {
        auto& s = ctx.segment(required_spelling(g, "Type"), position++);
        s.conventions = required_text(g, "Conventions");
        common_settings(s, g);
        s.reference_curve = required_text(g, "ReferenceCurve");
        ctx.quotes(s.id, g.child("Quotes"));
    }
    position = 0;
    for (const auto& g : v.children("DiscountRatio")) {
        auto& s = ctx.segment(required_spelling(g, "Type"), position++);
        s.conventions = g.text("Conventions");
        common_settings(s, g);
        if (const auto base = g.child("BaseCurve"); base) {
            s.base_curve = base->own_text();
            s.base_curve_currency = base->text("currency");
        }
        if (const auto numerator = g.child("NumeratorCurve"); numerator) {
            s.numerator_curve = numerator->own_text();
            s.numerator_curve_currency = numerator->text("currency");
        }
        if (const auto denominator = g.child("DenominatorCurve"); denominator) {
            s.denominator_curve = denominator->own_text();
            s.denominator_curve_currency = denominator->text("currency");
        }
    }
    position = 0;
    for (const auto& g : v.children("FittedBond")) {
        auto& s = ctx.segment(required_text(g, "Type"), position++);
        common_settings(s, g);
        s.extrapolate_flat = boolean_text(g, "ExtrapolateFlat");
        ctx.quotes(s.id, g.child("Quotes"));
        const auto id = s.id;
        int item = 0;
        if (const auto curves = g.child("IndexCurves"); curves)
            for (const auto& c : curves->children("IndexCurve"))
                ctx.curve(id,
                          "IndexCurve",
                          c.own_text().value_or(""),
                          c.text("Index"),
                          std::nullopt,
                          item++);
        item = 0;
        if (const auto curves = g.child("IborIndexCurves"); curves)
            for (const auto& c : curves->children("IborIndexCurve"))
                ctx.curve(id,
                          "IborIndexCurve",
                          c.own_text().value_or(""),
                          c.text("iborIndex"),
                          std::nullopt,
                          item++);
        item = 0;
        if (const auto curves = g.child("InflationIndexCurves"); curves)
            for (const auto& c : curves->children("InflationIndexCurve"))
                ctx.curve(id,
                          "InflationIndexCurve",
                          c.own_text().value_or(""),
                          c.text("inflationIndex"),
                          std::nullopt,
                          item++);
    }
    position = 0;
    for (const auto& g : v.children("BondYieldShifted")) {
        auto& s = ctx.segment(required_text(g, "Type"), position++);
        s.conventions = required_text(g, "Conventions");
        s.reference_curve = required_text(g, "ReferenceCurve");
        s.extrapolate_flat = boolean_text(g, "ExtrapolateFlat");
        ctx.quotes(s.id, g.child("Quotes"));
        const auto id = s.id;
        int item = 0;
        if (const auto curves = g.child("IndexCurves"); curves)
            for (const auto& c : curves->children("IndexCurve"))
                ctx.curve(id,
                          "IndexCurve",
                          c.own_text().value_or(""),
                          c.text("Index"),
                          std::nullopt,
                          item++);
        item = 0;
        if (const auto curves = g.child("IborIndexCurves"); curves)
            for (const auto& c : curves->children("IborIndexCurve"))
                ctx.curve(id,
                          "IborIndexCurve",
                          c.own_text().value_or(""),
                          c.text("iborIndex"),
                          std::nullopt,
                          item++);
    }
    position = 0;
    for (const auto& g : v.children("WeightedAverage")) {
        auto& s = ctx.segment(required_text(g, "Type"), position++);
        s.reference_curve = required_text(g, "ReferenceCurve1");
        s.reference_curve_2 = required_text(g, "ReferenceCurve2");
        s.weight_1 = required_real(g, "Weight1");
        s.weight_2 = required_real(g, "Weight2");
    }
    position = 0;
    for (const auto& g : v.children("YieldPlusDefault")) {
        auto& s = ctx.segment(required_text(g, "Type"), position++);
        s.reference_curve = required_text(g, "ReferenceCurve");
        if (const auto defaults = g.child("DefaultCurves"); defaults)
            ctx.curve(s.id,
                      "DefaultCurve",
                      required_text(*defaults, "DefaultCurve"),
                      std::nullopt,
                      g.child("Weights") ?
                          std::optional<double>(required_real(*g.child("Weights"), "Weight")) :
                          std::nullopt,
                      0);
    }
    position = 0;
    for (const auto& g : v.children("IborFallback")) {
        auto& s = ctx.segment(required_text(g, "Type"), position++);
        common_settings(s, g);
        s.ibor_index = required_text(g, "IborIndex");
        s.rfr_curve = required_text(g, "RfrCurve");
        s.rfr_index = g.text("RfrIndex");
        s.spread = g.decimal("Spread");
    }
}

refdata::domain::curve_definition&
add_definition(ores::refdata::messaging::curve_configuration_document& out,
               std::string_view section,
               const std::string& curve_id,
               const std::string& description,
               int position) {
    refdata::domain::curve_definition d;
    d.id = curve_configuration_projection_new_uuid();
    d.curve_configuration_id = out.config.id;
    d.section_code = std::string(section);
    d.curve_id = curve_id;
    d.description = description;
    d.position = position;
    curve_configuration_projection_set_audit(d);
    out.definitions.push_back(std::move(d));
    return out.definitions.back();
}

void import_bootstrap(ores::refdata::messaging::curve_configuration_document& out,
                      const boost::uuids::uuid& definition_id,
                      const boost::uuids::uuid& configuration_id,
                      const record_reader& b) {
    refdata::domain::curve_bootstrap_config c;
    c.id = curve_configuration_projection_new_uuid();
    c.curve_definition_id = definition_id;
    c.default_curve_configuration_id = configuration_id;
    c.accuracy = b.real("Accuracy");
    c.global_accuracy = b.real("GlobalAccuracy");
    c.dont_throw = boolean_text(b, "DontThrow");
    c.max_attempts = optional_int(b, "MaxAttempts");
    c.max_factor = b.real("MaxFactor");
    c.min_factor = b.real("MinFactor");
    c.dont_throw_steps = optional_int(b, "DontThrowSteps");
    c.global = boolean_text(b, "Global");
    c.smoothness_lambda = b.real("SmoothnessLambda");
    curve_configuration_projection_set_audit(c);
    out.bootstrap_configs.push_back(std::move(c));
}

void import_report(ores::refdata::messaging::curve_configuration_document& out,
                   const boost::uuids::uuid& definition_id,
                   const record_reader& v);

void import_yield_curve(ores::refdata::messaging::curve_configuration_document& out,
                        const record_reader& v,
                        int position) {
    const auto d = add_definition(out,
                                  yield_curves_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::yield_curve_config y;
    y.id = curve_configuration_projection_new_uuid();
    y.curve_definition_id = d.id;
    y.currency = required_spelling(v, "Currency");
    y.discount_curve = required_text(v, "DiscountCurve");
    y.interpolation_variable = v.spelling("InterpolationVariable");
    y.interpolation_method = v.spelling("InterpolationMethod");
    y.mixed_interpolation_cutoff = optional_int(v, "MixedInterpolationCutoff");
    y.day_counter = v.spelling("YieldCurveDayCounter");
    y.tolerance = v.real("Tolerance");
    y.extrapolation = v.spelling("Extrapolation");
    y.extrapolation_method = v.spelling("ExtrapolationMethod");
    y.exclude_t0_from_interpolation = v.spelling("ExcludeT0FromInterpolation");
    y.has_report = v.has("Report");
    if (const auto report = v.child("Report"); report)
        y.report_pillar_dates = report->text("PillarDates");
    curve_configuration_projection_set_audit(y);
    out.yield_curves.push_back(std::move(y));

    if (const auto bootstrap = v.child("BootstrapConfig"); bootstrap)
        import_bootstrap(out, d.id, boost::uuids::uuid{}, *bootstrap);

    import_context ctx{out, d.id};
    if (const auto segments = v.child("Segments"); segments)
        import_segments(ctx, *segments);
}

void import_equity_curve(ores::refdata::messaging::curve_configuration_document& out,
                         const record_reader& v,
                         int position) {
    const auto d = add_definition(out,
                                  equity_curves_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::equity_curve_config e;
    e.id = curve_configuration_projection_new_uuid();
    e.curve_definition_id = d.id;
    e.currency = required_text(v, "Currency");
    e.calendar = v.text("Calendar");
    e.forecasting_curve = required_text(v, "ForecastingCurve");
    e.equity_type = required_spelling(v, "Type");
    e.exercise_style = v.spelling("ExerciseStyle");
    e.spot_quote = required_text(v, "SpotQuote");
    e.has_quotes = v.has("Quotes");
    e.day_counter = v.spelling("DayCounter");
    e.has_dividend_interpolation = v.has("DividendInterpolation");
    if (const auto interpolation = v.child("DividendInterpolation"); interpolation) {
        e.dividend_interpolation_variable = interpolation->spelling("InterpolationVariable");
        e.dividend_interpolation_method = interpolation->spelling("InterpolationMethod");
    }
    e.dividend_extrapolation = v.spelling("DividendExtrapolation");
    e.extrapolation = v.spelling("Extrapolation");
    curve_configuration_projection_set_audit(e);
    out.equity_curves.push_back(std::move(e));

    if (const auto quotes = v.child("Quotes"); quotes) {
        import_context ctx{out, d.id};
        ctx.quotes(boost::uuids::uuid{}, quotes);
    }
}

void import_inflation_curve(ores::refdata::messaging::curve_configuration_document& out,
                            const record_reader& v,
                            int position) {
    const auto d = add_definition(out,
                                  inflation_curves_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);
    if (v.has("Segments"))
        throw refusal("inflation curve " + d.curve_id +
                      " writes Segments, which are not modelled yet");

    refdata::domain::inflation_curve_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.nominal_term_structure = required_text(v, "NominalTermStructure");
    r.inflation_type = required_spelling(v, "Type");
    r.conventions = v.text("Conventions");
    r.has_quotes = v.has("Quotes");
    r.extrapolation = v.spelling("Extrapolation");
    r.calendar = required_text(v, "Calendar");
    r.day_counter = v.spelling("DayCounter");
    r.lag = required_text(v, "Lag");
    r.frequency = required_spelling(v, "Frequency");
    r.base_rate = v.text("BaseRate");
    r.tolerance = v.real("Tolerance");
    r.has_seasonality = v.has("Seasonality");
    if (const auto seasonality = v.child("Seasonality"); seasonality) {
        r.seasonality_base_date = seasonality->text("BaseDate");
        r.seasonality_frequency = seasonality->spelling("Frequency");
        int factor_position = 0;
        if (const auto factors = seasonality->child("Factors"); factors)
            for (const auto& f : factors->values("Factor")) {
                refdata::domain::inflation_seasonality_factor sf;
                sf.id = curve_configuration_projection_new_uuid();
                sf.curve_definition_id = d.id;
                sf.factor = f;
                sf.position = factor_position++;
                curve_configuration_projection_set_audit(sf);
                out.seasonality_factors.push_back(std::move(sf));
            }
    }
    r.use_last_fixing_date = v.spelling("UseLastFixingDate");
    r.interpolation_variable = v.text("InterpolationVariable");
    r.interpolation_method = v.text("InterpolationMethod");
    curve_configuration_projection_set_audit(r);
    out.inflation_curves.push_back(std::move(r));

    if (const auto quotes = v.child("Quotes"); quotes) {
        import_context ctx{out, d.id};
        ctx.quotes(boost::uuids::uuid{}, quotes);
    }
}

// The settings a default curve writes both inline and in a listed
// configuration, under the same element names.
void import_configuration_settings(refdata::domain::default_curve_configuration& r,
                                   const record_reader& v) {
    r.discount_curve = v.text("DiscountCurve");
    r.recovery_rate = v.text("RecoveryRate");
    r.start_date = v.text("StartDate");
    r.has_quotes = v.has("Quotes");
    r.benchmark_curve = v.text("BenchmarkCurve");
    r.source_curve = v.text("SourceCurve");
    r.pillars = v.text("Pillars");
    r.spot_lag = optional_int(v, "SpotLag");
    r.calendar = v.text("Calendar");
    r.conventions = v.text("Conventions");
    r.extrapolation = v.spelling("Extrapolation");
    r.running_spread = v.decimal("RunningSpread");
    r.index_term = v.text("IndexTerm");
    r.imply_default_from_market = v.spelling("ImplyDefaultFromMarket");
    r.allow_negative_rates = v.spelling("AllowNegativeRates");
    r.price_is_upfront = v.spelling("PriceIsUpfront");
    r.initial_state = v.text("InitialState");
    r.states = v.text("States");
}

void import_configuration_lists(ores::refdata::messaging::curve_configuration_document& out,
                                const refdata::domain::curve_definition& d,
                                const refdata::domain::default_curve_configuration& r,
                                const record_reader& v) {
    if (v.has("SourceCurves") || v.has("SwitchDates"))
        throw refusal("default curve " + d.curve_id +
                      " writes SourceCurves or SwitchDates, which are not modelled yet");
    import_quotes(out, d.id, boost::uuids::uuid{}, v.child("Quotes"), r.id);
    if (const auto bootstrap = v.child("BootstrapConfig"); bootstrap)
        import_bootstrap(out, d.id, r.id, *bootstrap);
}

void import_default_curve(ores::refdata::messaging::curve_configuration_document& out,
                          const record_reader& v,
                          int position) {
    const auto d = add_definition(out,
                                  default_curves_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::default_curve_config c;
    c.id = curve_configuration_projection_new_uuid();
    c.curve_definition_id = d.id;
    c.currency = required_spelling(v, "Currency");
    curve_configuration_projection_set_audit(c);
    out.default_curves.push_back(std::move(c));

    const auto add = [&](int at) -> refdata::domain::default_curve_configuration {
        refdata::domain::default_curve_configuration r;
        r.id = curve_configuration_projection_new_uuid();
        r.curve_definition_id = d.id;
        r.position = at;
        curve_configuration_projection_set_audit(r);
        return r;
    };

    if (const auto configurations = v.child("Configurations"); configurations) {
        int at = 0;
        for (const auto& k : configurations->children("Configuration")) {
            auto r = add(at++);
            r.is_inline = false;
            r.priority = integer_text(k, "priority");
            r.default_curve_type = required_spelling(k, "Type");
            r.day_counter = required_spelling(k, "DayCounter");
            r.reinterpreted_yield_curve = k.text("ReinterpretedYieldCurve");
            import_configuration_settings(r, k);
            import_configuration_lists(out, d, r, k);
            out.default_curve_configurations.push_back(std::move(r));
        }
        return;
    }

    auto r = add(0);
    r.is_inline = true;
    r.default_curve_type = v.spelling("Type");
    r.day_counter = v.spelling("DayCounter");
    import_configuration_settings(r, v);
    import_configuration_lists(out, d, r, v);
    out.default_curve_configurations.push_back(std::move(r));
}

// A commodity quote's owner is the entry, one of its price segments, or one of
// the named lists the entry or a segment writes beside its Quotes.
void import_commodity_quotes(ores::refdata::messaging::curve_configuration_document& out,
                             const boost::uuids::uuid& definition_id,
                             const boost::uuids::uuid& price_segment_id,
                             std::optional<std::string_view> list,
                             const record_reader& v) {
    int position = 0;
    for (const auto& q : v.children("Quote")) {
        refdata::domain::curve_quote r;
        r.id = curve_configuration_projection_new_uuid();
        r.curve_definition_id = definition_id;
        r.commodity_price_segment_id = price_segment_id;
        if (list)
            r.quote_list = std::string(*list);
        r.quote_text = q.own_text().value_or("");
        r.optional_flag = q.text("optional");
        r.position = position++;
        curve_configuration_projection_set_audit(r);
        out.quotes.push_back(std::move(r));
    }
}

// The report columns are shared by an entry's Report and the document's
// per-family reports, so one pair of functions fills and reads them for both.
template <typename Row>
void fill_report(Row& r, const record_reader& v) {
    r.report_on_delta_grid = v.spelling("ReportOnDeltaGrid");
    r.report_on_moneyness_grid = v.spelling("ReportOnMoneynessGrid");
    r.report_on_strike_grid = v.spelling("ReportOnStrikeGrid");
    r.report_on_strike_spread_grid = v.spelling("ReportOnStrikeSpreadGrid");
    r.deltas = v.text("Deltas");
    r.moneyness = v.text("Moneyness");
    r.strikes = v.text("Strikes");
    r.strike_spreads = v.text("StrikeSpreads");
    r.expiries = v.text("Expiries");
    r.pillar_dates = v.text("PillarDates");
    r.underlying_tenors = v.text("UnderlyingTenors");
    r.continuation_expiry = v.text("ContinuationExpiry");
}

template <typename Row>
void export_report(record_writer v, const Row& r) {
    v.set_optional_spelling("ReportOnDeltaGrid", r.report_on_delta_grid, "ORE boolean");
    v.set_optional_spelling("ReportOnMoneynessGrid", r.report_on_moneyness_grid, "ORE boolean");
    v.set_optional_spelling("ReportOnStrikeGrid", r.report_on_strike_grid, "ORE boolean");
    v.set_optional_spelling(
        "ReportOnStrikeSpreadGrid", r.report_on_strike_spread_grid, "ORE boolean");
    v.set_optional_text("Deltas", r.deltas);
    v.set_optional_text("Moneyness", r.moneyness);
    v.set_optional_text("Strikes", r.strikes);
    v.set_optional_text("StrikeSpreads", r.strike_spreads);
    v.set_optional_text("Expiries", r.expiries);
    v.set_optional_text("PillarDates", r.pillar_dates);
    v.set_optional_text("UnderlyingTenors", r.underlying_tenors);
    v.set_optional_text("ContinuationExpiry", r.continuation_expiry);
}

void import_report(ores::refdata::messaging::curve_configuration_document& out,
                   const boost::uuids::uuid& definition_id,
                   const record_reader& v) {
    refdata::domain::curve_report_configuration r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = definition_id;
    fill_report(r, v);
    curve_configuration_projection_set_audit(r);
    out.report_configurations.push_back(std::move(r));
}

void import_parametric_smile(ores::refdata::messaging::curve_configuration_document& out,
                             const boost::uuids::uuid& definition_id,
                             const record_reader& v) {
    refdata::domain::curve_parametric_smile r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = definition_id;
    if (const auto calibration = v.child("Calibration"); calibration) {
        r.max_calibration_attempts = required_int(*calibration, "MaxCalibrationAttempts");
        r.exit_early_error_threshold = required_real(*calibration, "ExitEarlyErrorThreshold");
        r.max_acceptable_error = required_real(*calibration, "MaxAcceptableError");
    }
    if (const auto correction = v.child("ResidualCorrection"); correction)
        r.residual_correction_dimension = correction->spelling("Dimension");
    curve_configuration_projection_set_audit(r);
    out.parametric_smiles.push_back(std::move(r));

    int parameter_position = 0;
    if (const auto parameters = v.child("Parameters"); parameters)
        for (const auto& p : parameters->children("Parameter")) {
            refdata::domain::curve_parametric_smile_parameter parameter;
            parameter.id = curve_configuration_projection_new_uuid();
            parameter.curve_definition_id = definition_id;
            parameter.name = required_text(p, "Name");
            parameter.initial_value = p.text("InitialValue");
            parameter.calibration = required_spelling(p, "Calibration");
            parameter.position = parameter_position++;
            curve_configuration_projection_set_audit(parameter);
            out.parametric_smile_parameters.push_back(std::move(parameter));
        }
}

struct smile_export {
    const refdata::domain::curve_parametric_smile* row = nullptr;
    std::vector<const refdata::domain::curve_parametric_smile_parameter*> parameters;
};

void export_parametric_smile(
    record_writer v,
    const refdata::domain::curve_parametric_smile& r,
    const std::vector<const refdata::domain::curve_parametric_smile_parameter*>& parameters) {
    auto parameters_writer = v.child("Parameters");
    for (const auto* p : parameters) {
        auto parameter = parameters_writer.append("Parameter");
        parameter.set_text("Name", p->name);
        parameter.set_optional_text("InitialValue", p->initial_value);
        parameter.set_spelling("Calibration", p->calibration, "parameter calibration");
    }
    auto calibration = v.child("Calibration");
    calibration.set_integer("MaxCalibrationAttempts", r.max_calibration_attempts);
    calibration.set_real("ExitEarlyErrorThreshold", r.exit_early_error_threshold);
    calibration.set_real("MaxAcceptableError", r.max_acceptable_error);
    if (r.residual_correction_dimension) {
        auto correction = v.child("ResidualCorrection");
        correction.set_spelling(
            "Dimension", *r.residual_correction_dimension, "residual correction dimension");
    }
}

void import_commodity_curve(ores::refdata::messaging::curve_configuration_document& out,
                            const record_reader& v,
                            int position) {
    const auto d = add_definition(out,
                                  commodity_curves_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);
    const boost::uuids::uuid none{};

    refdata::domain::commodity_curve_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.currency = required_spelling(v, "Currency");
    r.base_price_curve = v.text("BasePriceCurve");
    r.base_yield_curve = v.text("BaseYieldCurve");
    r.yield_curve = v.text("YieldCurve");
    r.spot_quote = v.text("SpotQuote");
    r.has_quotes = v.has("Quotes");
    r.day_counter = v.spelling("DayCounter");
    r.interpolation_method = v.spelling("InterpolationMethod");
    r.conventions = v.text("Conventions");
    r.extrapolation = v.spelling("Extrapolation");
    r.has_basis_configuration = v.has("BasisConfiguration");
    if (const auto basis = v.child("BasisConfiguration"); basis) {
        r.basis_base_price_curve = required_text(*basis, "BasePriceCurve");
        r.basis_base_price_conventions = required_text(*basis, "BasePriceConventions");
        r.basis_conventions = required_text(*basis, "BasisConventions");
        r.basis_day_counter = basis->spelling("DayCounter");
        r.basis_interpolation_method = basis->spelling("InterpolationMethod");
        r.basis_add_basis = basis->spelling("AddBasis");
        r.basis_month_offset = optional_int(*basis, "MonthOffset");
        r.basis_average_base = basis->spelling("AverageBase");
        r.basis_price_as_historical_fixing = basis->spelling("PriceAsHistoricalFixing");
        if (const auto quotes = basis->child("BasisQuotes"); quotes)
            import_commodity_quotes(out, d.id, none, basis_quotes_list, *quotes);
    }
    r.has_price_segments = v.has("PriceSegments");
    if (const auto segments = v.child("PriceSegments"); segments) {
        int at = 0;
        for (const auto& g : segments->children("PriceSegment")) {
            refdata::domain::commodity_price_segment p;
            p.id = curve_configuration_projection_new_uuid();
            p.curve_definition_id = d.id;
            p.segment_type = required_spelling(g, "Type");
            p.priority = optional_int(g, "Priority");
            p.conventions = required_text(g, "Conventions");
            p.has_quotes = g.has("Quotes");
            p.peak_price_curve_id = g.text("PeakPriceCurveId");
            p.peak_price_calendar = g.text("PeakPriceCalendar");
            p.has_off_peak_daily = g.has("OffPeakDaily");
            p.position = at++;
            curve_configuration_projection_set_audit(p);
            if (const auto quotes = g.child("Quotes"); quotes)
                import_commodity_quotes(out, d.id, p.id, std::nullopt, *quotes);
            if (const auto daily = g.child("OffPeakDaily"); daily) {
                if (const auto quotes = daily->child("OffPeakQuotes"); quotes)
                    import_commodity_quotes(out, d.id, p.id, off_peak_quotes_list, *quotes);
                if (const auto quotes = daily->child("PeakQuotes"); quotes)
                    import_commodity_quotes(out, d.id, p.id, peak_quotes_list, *quotes);
            }
            out.commodity_price_segments.push_back(std::move(p));
        }
    }
    curve_configuration_projection_set_audit(r);
    out.commodity_curves.push_back(std::move(r));

    if (const auto quotes = v.child("Quotes"); quotes)
        import_commodity_quotes(out, d.id, none, std::nullopt, *quotes);
    if (const auto bootstrap = v.child("BootstrapConfig"); bootstrap)
        import_bootstrap(out, d.id, none, *bootstrap);
}

void import_fx_volatility(ores::refdata::messaging::curve_configuration_document& out,
                          const record_reader& v,
                          int position) {
    const auto d = add_definition(out,
                                  fx_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::fx_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.dimension = required_spelling(v, "Dimension");
    r.smile_type = v.spelling("SmileType");
    r.smile_interpolation = v.text("SmileInterpolation");
    r.deltas = v.text("Deltas");
    r.smile_delta = v.text("SmileDelta");
    r.conventions = v.text("Conventions");
    r.expiries = v.text("Expiries");
    r.fx_spot_id = v.text("FXSpotID");
    r.fx_foreign_curve_id = v.text("FXForeignCurveID");
    r.fx_domestic_curve_id = v.text("FXDomesticCurveID");
    r.calendar = v.text("Calendar");
    r.day_counter = v.spelling("DayCounter");
    r.fx_index_tag = v.text("FXIndexTag");
    r.base_volatility_1 = v.text("BaseVolatility1");
    r.base_volatility_2 = v.text("BaseVolatility2");
    r.smile_extrapolation = v.spelling("SmileExtrapolation");
    r.time_interpolation = v.text("TimeInterpolation");
    r.time_weighting = v.text("TimeWeighting");
    r.butterfly_error_tolerance = v.real("ButterflyErrorTolerance");
    curve_configuration_projection_set_audit(r);
    out.fx_volatilities.push_back(std::move(r));
    if (const auto smile = v.child("ParametricSmileConfiguration"); smile)
        import_parametric_smile(out, d.id, *smile);
    if (const auto report = v.child("Report"); report)
        import_report(out, d.id, *report);
}

void import_yield_volatility(ores::refdata::messaging::curve_configuration_document& out,
                             const record_reader& v,
                             int position) {
    const auto d = add_definition(out,
                                  yield_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::yield_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.qualifier = required_text(v, "Qualifier");
    r.dimension = v.spelling("Dimension");
    r.volatility_type = required_spelling(v, "VolatilityType");
    r.extrapolation = required_spelling(v, "Extrapolation");
    r.day_counter = required_spelling(v, "DayCounter");
    r.calendar = required_text(v, "Calendar");
    r.business_day_convention = required_spelling(v, "BusinessDayConvention");
    r.option_tenors = required_text(v, "OptionTenors");
    r.bond_tenors = required_text(v, "BondTenors");
    curve_configuration_projection_set_audit(r);
    out.yield_volatilities.push_back(std::move(r));
    if (const auto report = v.child("Report"); report)
        import_report(out, d.id, *report);
}

void import_base_correlation(ores::refdata::messaging::curve_configuration_document& out,
                             const record_reader& v,
                             int position) {
    const auto d = add_definition(out,
                                  base_correlations_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);
    if (v.has("RecoveryGrid") || v.has("RecoveryProbabilities") || v.has("QuoteTypes"))
        throw refusal("base correlation " + d.curve_id +
                      " writes RecoveryGrid, RecoveryProbabilities or QuoteTypes, which are not "
                      "modelled yet");

    refdata::domain::base_correlation_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.terms = required_text(v, "Terms");
    r.detachment_points = required_text(v, "DetachmentPoints");
    r.settlement_days = required_real(v, "SettlementDays");
    r.calendar = required_text(v, "Calendar");
    r.business_day_convention = required_spelling(v, "BusinessDayConvention");
    r.day_counter = required_spelling(v, "DayCounter");
    r.extrapolate = v.spelling("Extrapolate");
    r.quote_name = v.text("QuoteName");
    r.start_date = v.text("StartDate");
    r.rule = v.spelling("Rule");
    r.adjust_for_losses = v.spelling("AdjustForLosses");
    r.index_term = v.text("IndexTerm");
    r.index_spread = v.text("IndexSpread");
    r.currency = v.text("Currency");
    r.calibrate_constituents_to_index_spread = v.spelling("CalibrateConstituentsToIndexSpread");
    r.use_assumed_recovery = v.spelling("UseAssumedRecovery");
    curve_configuration_projection_set_audit(r);
    out.base_correlations.push_back(std::move(r));
}

void import_strike_surface(ores::refdata::messaging::curve_configuration_document& out,
                           const boost::uuids::uuid& definition_id,
                           const record_reader& v,
                           int position,
                           bool wrapped = false) {
    if (v.has("ParametricSmileConfiguration"))
        throw refusal("a strike surface writes a ParametricSmileConfiguration, which is not "
                      "modelled yet");
    refdata::domain::curve_volatility_config c;
    c.id = curve_configuration_projection_new_uuid();
    c.curve_definition_id = definition_id;
    c.kind = std::string(strike_surface_kind);
    c.is_wrapped = wrapped;
    c.priority = integer_text(v, "priority");
    c.quote_type = v.text("QuoteType");
    c.volatility_type = v.text("VolatilityType");
    c.exercise_type = v.text("ExerciseType");
    c.strikes = required_text(v, "Strikes");
    c.expiries = required_text(v, "Expiries");
    c.time_interpolation = required_text(v, "TimeInterpolation");
    c.strike_interpolation = required_text(v, "StrikeInterpolation");
    c.extrapolation = required_spelling(v, "Extrapolation");
    c.time_extrapolation = required_spelling(v, "TimeExtrapolation");
    c.time_extrapolation_variance = v.spelling("TimeExtrapolationVariance");
    c.strike_extrapolation = required_spelling(v, "StrikeExtrapolation");
    c.calendar = v.text("Calendar");
    c.position = position;
    curve_configuration_projection_set_audit(c);
    out.volatility_configs.push_back(std::move(c));
}

void export_strike_surface(record_writer v, const refdata::domain::curve_volatility_config& c) {
    set_integer_text(v, "priority", c.priority);
    v.set_optional_text("QuoteType", c.quote_type);
    v.set_optional_text("VolatilityType", c.volatility_type);
    v.set_optional_text("ExerciseType", c.exercise_type);
    v.set_text("Strikes", c.strikes.value_or(""));
    v.set_text("Expiries", c.expiries.value_or(""));
    v.set_text("TimeInterpolation", c.time_interpolation.value_or(""));
    v.set_text("StrikeInterpolation", c.strike_interpolation.value_or(""));
    v.set_spelling("Extrapolation", c.extrapolation.value_or(""), "ORE boolean");
    v.set_spelling("TimeExtrapolation", c.time_extrapolation.value_or(""), "extrapolation");
    v.set_optional_spelling(
        "TimeExtrapolationVariance", c.time_extrapolation_variance, "ORE boolean");
    v.set_spelling("StrikeExtrapolation", c.strike_extrapolation.value_or(""), "extrapolation");
    v.set_optional_text("Calendar", c.calendar);
}

refdata::domain::curve_volatility_config new_volatility_config(
    const boost::uuids::uuid& definition_id, std::string_view kind, bool wrapped, int position) {
    refdata::domain::curve_volatility_config c;
    c.id = curve_configuration_projection_new_uuid();
    c.curve_definition_id = definition_id;
    c.kind = std::string(kind);
    c.is_wrapped = wrapped;
    c.position = position;
    curve_configuration_projection_set_audit(c);
    return c;
}

void import_constant(ores::refdata::messaging::curve_configuration_document& out,
                     const boost::uuids::uuid& definition_id,
                     const record_reader& v,
                     bool wrapped,
                     int position) {
    auto c = new_volatility_config(definition_id, constant_kind, wrapped, position);
    c.priority = integer_text(v, "priority");
    c.quote_type = v.text("QuoteType");
    c.volatility_type = v.text("VolatilityType");
    c.exercise_type = v.text("ExerciseType");
    c.quote = required_text(v, "Quote");
    c.calendar = v.text("Calendar");
    out.volatility_configs.push_back(std::move(c));
}

void export_constant(record_writer v, const refdata::domain::curve_volatility_config& c) {
    set_integer_text(v, "priority", c.priority);
    v.set_optional_text("QuoteType", c.quote_type);
    v.set_optional_text("VolatilityType", c.volatility_type);
    v.set_optional_text("ExerciseType", c.exercise_type);
    v.set_text("Quote", c.quote.value_or(""));
    v.set_optional_text("Calendar", c.calendar);
}

void export_quote_list(record_writer quotes,
                       const std::vector<const refdata::domain::curve_quote*>& rows,
                       std::optional<std::string_view> list);

void import_volatility_curve(ores::refdata::messaging::curve_configuration_document& out,
                             const boost::uuids::uuid& definition_id,
                             const record_reader& v,
                             bool wrapped,
                             int position) {
    auto c = new_volatility_config(definition_id, curve_kind, wrapped, position);
    c.priority = integer_text(v, "priority");
    c.quote_type = v.text("QuoteType");
    c.volatility_type = v.text("VolatilityType");
    c.exercise_type = v.text("ExerciseType");
    c.interpolation = required_spelling(v, "Interpolation");
    c.extrapolation = required_spelling(v, "Extrapolation");
    c.enforce_monotone_variance = boolean_text(v, "EnforceMontoneVariance");
    c.calendar = v.text("Calendar");
    out.volatility_configs.push_back(std::move(c));
    if (const auto quotes = v.child("Quotes"); quotes)
        import_commodity_quotes(out,
                                definition_id,
                                boost::uuids::uuid{},
                                wrapped ? wrapped_curve_quotes_list : curve_quotes_list,
                                *quotes);
}

void export_volatility_curve(record_writer v,
                             const refdata::domain::curve_volatility_config& c,
                             const std::vector<const refdata::domain::curve_quote*>& entry_quotes) {
    set_integer_text(v, "priority", c.priority);
    v.set_optional_text("QuoteType", c.quote_type);
    v.set_optional_text("VolatilityType", c.volatility_type);
    v.set_optional_text("ExerciseType", c.exercise_type);
    export_quote_list(v.child("Quotes"),
                      entry_quotes,
                      c.is_wrapped ? wrapped_curve_quotes_list : curve_quotes_list);
    v.set_spelling("Interpolation", c.interpolation.value_or(""), "interpolation method");
    v.set_spelling("Extrapolation", c.extrapolation.value_or(""), "extrapolation");
    set_boolean_text(v, "EnforceMontoneVariance", c.enforce_monotone_variance);
    v.set_optional_text("Calendar", c.calendar);
}

void import_delta_surface(ores::refdata::messaging::curve_configuration_document& out,
                          const boost::uuids::uuid& definition_id,
                          const record_reader& v,
                          bool wrapped,
                          int position) {
    if (v.has("ParametricSmileConfiguration"))
        throw refusal("a delta surface writes a ParametricSmileConfiguration, which is not "
                      "modelled yet");
    auto c = new_volatility_config(definition_id, delta_surface_kind, wrapped, position);
    c.priority = integer_text(v, "priority");
    c.quote_type = v.text("QuoteType");
    c.volatility_type = v.text("VolatilityType");
    c.exercise_type = v.text("ExerciseType");
    c.delta_type = required_spelling(v, "DeltaType");
    c.atm_type = required_spelling(v, "AtmType");
    c.atm_delta_type = v.spelling("AtmDeltaType");
    c.put_deltas = required_text(v, "PutDeltas");
    c.call_deltas = required_text(v, "CallDeltas");
    c.expiries = required_text(v, "Expiries");
    c.time_interpolation = required_text(v, "TimeInterpolation");
    c.strike_interpolation = required_text(v, "StrikeInterpolation");
    c.extrapolation = required_spelling(v, "Extrapolation");
    c.time_extrapolation = required_spelling(v, "TimeExtrapolation");
    c.time_extrapolation_variance = v.spelling("TimeExtrapolationVariance");
    c.strike_extrapolation = required_spelling(v, "StrikeExtrapolation");
    c.future_price_correction = v.spelling("FuturePriceCorrection");
    c.calendar = v.text("Calendar");
    out.volatility_configs.push_back(std::move(c));
}

void export_delta_surface(record_writer v, const refdata::domain::curve_volatility_config& c) {
    set_integer_text(v, "priority", c.priority);
    v.set_optional_text("QuoteType", c.quote_type);
    v.set_optional_text("VolatilityType", c.volatility_type);
    v.set_optional_text("ExerciseType", c.exercise_type);
    v.set_spelling("DeltaType", c.delta_type.value_or(""), "delta type");
    v.set_spelling("AtmType", c.atm_type.value_or(""), "ATM type");
    v.set_optional_spelling("AtmDeltaType", c.atm_delta_type, "delta type");
    v.set_text("PutDeltas", c.put_deltas.value_or(""));
    v.set_text("CallDeltas", c.call_deltas.value_or(""));
    v.set_text("Expiries", c.expiries.value_or(""));
    v.set_text("TimeInterpolation", c.time_interpolation.value_or(""));
    v.set_text("StrikeInterpolation", c.strike_interpolation.value_or(""));
    v.set_spelling("Extrapolation", c.extrapolation.value_or(""), "ORE boolean");
    v.set_spelling("TimeExtrapolation", c.time_extrapolation.value_or(""), "extrapolation");
    v.set_optional_spelling(
        "TimeExtrapolationVariance", c.time_extrapolation_variance, "ORE boolean");
    v.set_spelling("StrikeExtrapolation", c.strike_extrapolation.value_or(""), "extrapolation");
    v.set_optional_spelling("FuturePriceCorrection", c.future_price_correction, "ORE boolean");
    v.set_optional_text("Calendar", c.calendar);
}

void import_proxy_surface(ores::refdata::messaging::curve_configuration_document& out,
                          const boost::uuids::uuid& definition_id,
                          const record_reader& v,
                          bool wrapped,
                          int position) {
    auto c = new_volatility_config(definition_id, proxy_surface_kind, wrapped, position);
    c.priority = integer_text(v, "priority");
    c.proxy_volatility_curve = required_text(v, "ProxyVolatilityCurve");
    c.fx_volatility_curve = v.text("FXVolatilityCurve");
    c.correlation_curve = v.text("CorrelationCurve");
    c.cds_volatility_curve = v.text("CDSVolatilityCurve");
    out.volatility_configs.push_back(std::move(c));
}

void export_proxy_surface(record_writer v, const refdata::domain::curve_volatility_config& c) {
    set_integer_text(v, "priority", c.priority);
    v.set_text("ProxyVolatilityCurve", c.proxy_volatility_curve.value_or(""));
    v.set_optional_text("FXVolatilityCurve", c.fx_volatility_curve);
    v.set_optional_text("CorrelationCurve", c.correlation_curve);
    v.set_optional_text("CDSVolatilityCurve", c.cds_volatility_curve);
}

// Reads the volatility configurations an entry or its VolatilityConfig element
// writes. Each holder has at most one of each kind, and a holder whose type has
// no slot for a kind never writes it.
void import_volatility_configs(ores::refdata::messaging::curve_configuration_document& out,
                               const boost::uuids::uuid& definition_id,
                               const std::string& curve_id,
                               const record_reader& v,
                               bool wrapped) {
    int position = 0;
    if (v.declares("Constant"))
        if (const auto c = v.child("Constant"); c)
            import_constant(out, definition_id, *c, wrapped, position++);
    if (v.declares("Curve"))
        if (const auto c = v.child("Curve"); c)
            import_volatility_curve(out, definition_id, *c, wrapped, position++);
    if (const auto c = v.child("StrikeSurface"); c)
        import_strike_surface(out, definition_id, *c, position++, wrapped);
    if (v.declares("MoneynessSurface"))
        if (v.has("MoneynessSurface"))
            throw refusal("volatility " + curve_id +
                          " writes a MoneynessSurface, which is not "
                          "modelled yet");
    if (v.declares("DeltaSurface"))
        if (const auto c = v.child("DeltaSurface"); c)
            import_delta_surface(out, definition_id, *c, wrapped, position++);
    if (v.declares("ApoFutureSurface"))
        if (v.has("ApoFutureSurface"))
            throw refusal("volatility " + curve_id +
                          " writes an ApoFutureSurface, which is not "
                          "modelled yet");
    if (v.declares("ProxySurface"))
        if (const auto c = v.child("ProxySurface"); c)
            import_proxy_surface(out, definition_id, *c, wrapped, position++);
}

// Writes one volatility configuration back into the holder it came from.
void export_volatility_config(record_writer h,
                              const refdata::domain::curve_volatility_config& c,
                              const std::vector<const refdata::domain::curve_quote*>& entry_quotes,
                              const std::string& curve_id) {
    if (c.kind == strike_surface_kind) {
        export_strike_surface(h.child("StrikeSurface"), c);
        return;
    }
    if (h.declares("Constant") && c.kind == constant_kind) {
        export_constant(h.child("Constant"), c);
        return;
    }
    if (h.declares("Curve") && c.kind == curve_kind) {
        export_volatility_curve(h.child("Curve"), c, entry_quotes);
        return;
    }
    if (h.declares("DeltaSurface") && c.kind == delta_surface_kind) {
        export_delta_surface(h.child("DeltaSurface"), c);
        return;
    }
    if (h.declares("ProxySurface") && c.kind == proxy_surface_kind) {
        export_proxy_surface(h.child("ProxySurface"), c);
        return;
    }
    throw refusal("volatility " + curve_id + " has a volatility config of kind " + c.kind +
                  ", which its section cannot hold");
}

void export_volatility_configs(
    record_writer r,
    bool has_volatility_config,
    const std::vector<const refdata::domain::curve_volatility_config*>& configs,
    const std::vector<const refdata::domain::curve_quote*>& entry_quotes,
    const std::string& curve_id) {
    std::optional<record_writer> wrapped;
    if (has_volatility_config)
        wrapped = r.child("VolatilityConfig");
    for (const auto* c : configs) {
        if (c->is_wrapped) {
            if (!wrapped)
                throw refusal("volatility " + curve_id +
                              " has a wrapped volatility config but no VolatilityConfig element");
            export_volatility_config(*wrapped, *c, entry_quotes, curve_id);
        } else {
            export_volatility_config(r, *c, entry_quotes, curve_id);
        }
    }
}

void import_equity_volatility(ores::refdata::messaging::curve_configuration_document& out,
                              const record_reader& v,
                              int position) {
    const auto d = add_definition(out,
                                  equity_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);
    if (v.has("OneDimSolverConfig"))
        throw refusal("equity volatility " + d.curve_id +
                      " writes a OneDimSolverConfig, which is not modelled yet");

    refdata::domain::equity_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.equity_id = v.text("EquityId");
    r.currency = required_text(v, "Currency");
    r.dimension = v.spelling("Dimension");
    r.expiries = v.text("Expiries");
    r.strikes = v.text("Strikes");
    r.day_counter = v.spelling("DayCounter");
    r.time_extrapolation = v.spelling("TimeExtrapolation");
    r.strike_extrapolation = v.spelling("StrikeExtrapolation");
    r.calendar = v.text("Calendar");
    r.prefer_out_of_the_money = v.spelling("PreferOutOfTheMoney");
    r.has_volatility_config = v.has("VolatilityConfig");
    curve_configuration_projection_set_audit(r);
    out.equity_volatilities.push_back(std::move(r));

    import_volatility_configs(out, d.id, d.curve_id, v, false);
    if (const auto c = v.child("VolatilityConfig"); c)
        import_volatility_configs(out, d.id, d.curve_id, *c, true);
    if (const auto report = v.child("Report"); report)
        import_report(out, d.id, *report);
}

void import_commodity_volatility(ores::refdata::messaging::curve_configuration_document& out,
                                 const record_reader& v,
                                 int position) {
    const auto d = add_definition(out,
                                  commodity_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);
    if (v.has("OneDimSolverConfig"))
        throw refusal("commodity volatility " + d.curve_id +
                      " writes a OneDimSolverConfig, which is not modelled yet");

    refdata::domain::commodity_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.currency = required_spelling(v, "Currency");
    r.instrument_type = v.text("InstrumentType");
    r.calendar_spread_offset = optional_int(v, "CalendarSpreadOffset");
    r.calendar_spread_underlying_name = v.text("CalendarSpreadUnderlyingName");
    r.day_counter = v.spelling("DayCounter");
    r.calendar = v.text("Calendar");
    r.future_conventions = v.text("FutureConventions");
    r.option_expiry_roll_days = v.text("OptionExpiryRollDays");
    r.price_curve_id = v.text("PriceCurveId");
    r.yield_curve_id = v.text("YieldCurveId");
    r.quote_suffix = v.spelling("QuoteSuffix");
    r.prefer_out_of_the_money = v.spelling("PreferOutOfTheMoney");
    r.has_volatility_config = v.has("VolatilityConfig");
    curve_configuration_projection_set_audit(r);
    out.commodity_volatilities.push_back(std::move(r));

    import_volatility_configs(out, d.id, d.curve_id, v, false);
    if (const auto c = v.child("VolatilityConfig"); c)
        import_volatility_configs(out, d.id, d.curve_id, *c, true);
    if (const auto report = v.child("Report"); report)
        import_report(out, d.id, *report);
}

void import_bond_future_volatility(ores::refdata::messaging::curve_configuration_document& out,
                                   const record_reader& v,
                                   int position) {
    const auto d = add_definition(out,
                                  bond_future_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);
    if (v.has("OneDimSolverConfig"))
        throw refusal("bond future volatility " + d.curve_id +
                      " writes a OneDimSolverConfig, which is not modelled yet");

    refdata::domain::bond_future_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.contract_name = required_text(v, "ContractName");
    r.day_counter = v.spelling("DayCounter");
    r.calendar = v.text("Calendar");
    r.yield_curve_id = v.text("YieldCurveId");
    r.strike_factor = curve_configuration_projection_decimal_as_double(v, "StrikeFactor");
    r.use_only_put_call = v.spelling("UseOnlyPutCall");
    r.prefer_out_of_the_money = v.spelling("PreferOutOfTheMoney");
    r.treat_as_european = v.spelling("TreatAsEuropean");
    r.has_volatility_config = v.has("VolatilityConfig");
    curve_configuration_projection_set_audit(r);
    out.bond_future_volatilities.push_back(std::move(r));

    import_volatility_configs(out, d.id, d.curve_id, v, false);
    if (const auto c = v.child("VolatilityConfig"); c)
        import_volatility_configs(out, d.id, d.curve_id, *c, true);
}

void import_cds_volatility(ores::refdata::messaging::curve_configuration_document& out,
                           const record_reader& v,
                           int position) {
    const auto d = add_definition(out,
                                  cds_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);
    if (v.has("Constant") || v.has("Curve") || v.has("ProxySurface"))
        throw refusal("CDS volatility " + d.curve_id +
                      " writes a Constant, Curve or ProxySurface, which is not modelled yet");
    if (v.has("PriceInfo"))
        throw refusal("CDS volatility " + d.curve_id +
                      " writes a PriceInfo, which is not modelled yet");
    if (const auto terms = v.child("Terms"); terms && terms->children("Term").empty())
        throw refusal("CDS volatility " + d.curve_id + " writes an empty Terms element");

    refdata::domain::cds_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.expiries = v.text("Expiries");
    r.day_counter = v.spelling("DayCounter");
    r.calendar = v.text("Calendar");
    r.strike_type = v.text("StrikeType");
    r.quote_name = v.text("QuoteName");
    r.strike_factor = curve_configuration_projection_decimal_as_double(v, "StrikeFactor");
    curve_configuration_projection_set_audit(r);
    out.cds_volatilities.push_back(std::move(r));

    if (const auto terms = v.child("Terms"); terms) {
        int term_position = 0;
        for (const auto& t : terms->children("Term")) {
            refdata::domain::cds_volatility_term term;
            term.id = curve_configuration_projection_new_uuid();
            term.curve_definition_id = d.id;
            term.label = required_text(t, "Label");
            term.curve = required_text(t, "Curve");
            term.maturity = t.text("Maturity");
            term.position = term_position++;
            curve_configuration_projection_set_audit(term);
            out.cds_volatility_terms.push_back(std::move(term));
        }
    }
    if (const auto surface = v.child("StrikeSurface"); surface)
        import_strike_surface(out, d.id, *surface, 0);
}

void import_inflation_cap_floor_volatility(
    ores::refdata::messaging::curve_configuration_document& out,
    const record_reader& v,
    int position) {
    const auto d = add_definition(out,
                                  inflation_cap_floor_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::inflation_cap_floor_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.inflation_type = required_spelling(v, "Type");
    r.quote_type = required_text(v, "QuoteType");
    r.volatility_type = required_spelling(v, "VolatilityType");
    r.extrapolation = required_spelling(v, "Extrapolation");
    r.tenors = required_text(v, "Tenors");
    r.settlement_days = optional_int(v, "SettlementDays");
    r.cap_strikes = v.text("CapStrikes");
    r.floor_strikes = v.text("FloorStrikes");
    r.strikes = v.text("Strikes");
    r.calendar = required_text(v, "Calendar");
    r.day_counter = required_spelling(v, "DayCounter");
    r.business_day_convention = required_spelling(v, "BusinessDayConvention");
    r.index = required_text(v, "Index");
    r.index_curve = required_text(v, "IndexCurve");
    r.index_interpolated = v.spelling("IndexInterpolated");
    r.observation_lag = required_text(v, "ObservationLag");
    r.yield_term_structure = required_text(v, "YieldTermStructure");
    r.quote_index = v.text("QuoteIndex");
    r.conventions = v.text("Conventions");
    curve_configuration_projection_set_audit(r);
    out.inflation_cap_floor_volatilities.push_back(std::move(r));
    if (const auto report = v.child("Report"); report)
        import_report(out, d.id, *report);
    if (const auto bootstrap = v.child("BootstrapConfig"); bootstrap)
        import_bootstrap(out, d.id, boost::uuids::uuid{}, *bootstrap);
}

void import_swaption_volatility(ores::refdata::messaging::curve_configuration_document& out,
                                const record_reader& v,
                                int position) {
    const auto d = add_definition(out,
                                  swaption_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::swaption_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.dimension = v.spelling("Dimension");
    r.volatility_type = v.spelling("VolatilityType");
    r.interpolation = v.text("Interpolation");
    r.extrapolation = v.text("Extrapolation");
    r.output_volatility_type = v.text("OutputVolatilityType");
    r.model_shift = v.text("ModelShift");
    r.output_shift = v.text("OutputShift");
    r.day_counter = v.spelling("DayCounter");
    r.calendar = v.text("Calendar");
    r.business_day_convention = v.spelling("BusinessDayConvention");
    r.option_tenors = v.text("OptionTenors");
    r.swap_tenors = v.text("SwapTenors");
    r.short_swap_index_base = v.text("ShortSwapIndexBase");
    r.swap_index_base = v.text("SwapIndexBase");
    r.smile_option_tenors = v.text("SmileOptionTenors");
    r.smile_swap_tenors = v.text("SmileSwapTenors");
    r.smile_spreads = v.text("SmileSpreads");
    r.quote_tag = v.text("QuoteTag");
    r.has_proxy_config = v.has("ProxyConfig");
    if (const auto proxy = v.child("ProxyConfig"); proxy) {
        if (const auto source = proxy->child("Source"); source) {
            r.proxy_source_curve_id = required_text(*source, "CurveId");
            r.proxy_source_short_swap_index_base = required_text(*source, "ShortSwapIndexBase");
            r.proxy_source_swap_index_base = required_text(*source, "SwapIndexBase");
        }
        if (const auto target = proxy->child("Target"); target) {
            r.proxy_target_short_swap_index_base = required_text(*target, "ShortSwapIndexBase");
            r.proxy_target_swap_index_base = required_text(*target, "SwapIndexBase");
        }
    }
    curve_configuration_projection_set_audit(r);
    out.swaption_volatilities.push_back(std::move(r));
    if (const auto smile = v.child("ParametricSmileConfiguration"); smile)
        import_parametric_smile(out, d.id, *smile);
    if (const auto report = v.child("Report"); report)
        import_report(out, d.id, *report);
}

void import_cap_floor_volatility(ores::refdata::messaging::curve_configuration_document& out,
                                 const record_reader& v,
                                 int position) {
    const auto d = add_definition(out,
                                  cap_floor_volatilities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::cap_floor_volatility_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.volatility_type = v.spelling("VolatilityType");
    r.output_volatility_type = v.spelling("OutputVolatilityType");
    r.model_shift = v.real("ModelShift");
    r.output_shift = v.real("OutputShift");
    r.extrapolation = v.spelling("Extrapolation");
    r.interpolation_method = v.spelling("InterpolationMethod");
    r.include_atm = v.spelling("IncludeAtm");
    r.day_counter = v.spelling("DayCounter");
    r.calendar = v.text("Calendar");
    r.business_day_convention = v.spelling("BusinessDayConvention");
    r.tenors = v.text("Tenors");
    r.strikes = v.text("Strikes");
    r.optional_quotes = v.spelling("OptionalQuotes");
    r.ibor_index = v.text("IborIndex");
    r.index = v.text("Index");
    r.rate_computation_period = v.text("RateComputationPeriod");
    r.on_cap_settlement_days = optional_int(v, "ONCapSettlementDays");
    r.discount_curve = v.text("DiscountCurve");
    r.atm_tenors = v.text("AtmTenors");
    r.settlement_days = optional_int(v, "SettlementDays");
    r.interpolate_on = v.spelling("InterpolateOn");
    r.time_interpolation = v.spelling("TimeInterpolation");
    r.strike_interpolation = v.spelling("StrikeInterpolation");
    r.input_type = v.spelling("InputType");
    r.quote_includes_index_name = v.spelling("QuoteIncludesIndexName");
    r.flat_first_period = v.spelling("FlatFirstPeriod");
    r.use_effecive_volatility = v.spelling("UseEffeciveVolatility");
    r.use_effective_volatility = v.spelling("UseEffectiveVolatility");
    if (const auto proxy = v.child("ProxyConfig"); proxy) {
        refdata::domain::cap_floor_proxy_config p;
        p.id = curve_configuration_projection_new_uuid();
        p.cap_floor_volatility_config_id = r.id;
        if (const auto source = proxy->child("Source"); source) {
            p.source_curve_id = required_text(*source, "CurveId");
            p.source_index = required_text(*source, "Index");
            p.source_rate_computation_period = source->text("RateComputationPeriod");
        }
        if (const auto target = proxy->child("Target"); target) {
            p.target_index = required_text(*target, "Index");
            p.target_rate_computation_period = target->text("RateComputationPeriod");
            p.target_on_cap_settlement_days = optional_int(*target, "ONCapSettlementDays");
        }
        p.scaling_factor = proxy->real("ScalingFactor");
        out.cap_floor_proxy_configs.push_back(std::move(p));
    }
    curve_configuration_projection_set_audit(r);
    out.cap_floor_volatilities.push_back(std::move(r));
    if (const auto smile = v.child("ParametricSmileConfiguration"); smile)
        import_parametric_smile(out, d.id, *smile);
    if (const auto bootstrap = v.child("BootstrapConfig"); bootstrap)
        import_bootstrap(out, d.id, boost::uuids::uuid{}, *bootstrap);
    if (const auto report = v.child("Report"); report)
        import_report(out, d.id, *report);
}

void import_correlation(ores::refdata::messaging::curve_configuration_document& out,
                        const record_reader& v,
                        int position) {
    const auto d = add_definition(out,
                                  correlations_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::curve_correlation_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.correlation_type = required_spelling(v, "CorrelationType");
    r.index_1 = v.text("Index1");
    r.index_2 = v.text("Index2");
    r.conventions = v.text("Conventions");
    r.swaption_volatility = v.text("SwaptionVolatility");
    r.discount_curve = v.text("DiscountCurve");
    r.currency = v.spelling("Currency");
    r.dimension = v.spelling("Dimension");
    r.quote_type = v.spelling("QuoteType");
    r.extrapolation = v.spelling("Extrapolation");
    r.day_counter = v.spelling("DayCounter");
    r.calendar = v.text("Calendar");
    r.business_day_convention = v.spelling("BusinessDayConvention");
    r.option_tenors = v.text("OptionTenors");
    curve_configuration_projection_set_audit(r);
    out.correlations.push_back(std::move(r));
}

void import_security(ores::refdata::messaging::curve_configuration_document& out,
                     const record_reader& v,
                     int position) {
    const auto d = add_definition(out,
                                  securities_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::curve_security_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.spread_quote = v.text("SpreadQuote");
    r.recovery_rate_quote = v.text("RecoveryRateQuote");
    r.cpr_quote = v.text("CPRQuote");
    r.price_quote = v.text("PriceQuote");
    r.conversion_factor = v.text("ConversionFactor");
    curve_configuration_projection_set_audit(r);
    out.securities.push_back(std::move(r));
}

void import_fx_spot(ores::refdata::messaging::curve_configuration_document& out,
                    const record_reader& v,
                    int position) {
    add_definition(out,
                   fx_spots_section,
                   required_text(v, "CurveId"),
                   required_text(v, "CurveDescription"),
                   position);
}

void import_intraday_power_curve(ores::refdata::messaging::curve_configuration_document& out,
                                 const record_reader& v,
                                 int position) {
    const auto d = add_definition(out,
                                  intraday_power_curves_section,
                                  required_text(v, "CurveId"),
                                  required_text(v, "CurveDescription"),
                                  position);

    refdata::domain::intraday_power_curve_config r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_definition_id = d.id;
    r.currency = required_spelling(v, "Currency");
    r.daily_average_price_curve = required_text(v, "DailyAveragePriceCurve");
    r.shape_quote_name = required_text(v, "ShapeQuoteName");
    r.convention = required_text(v, "Convention");
    curve_configuration_projection_set_audit(r);
    out.intraday_power_curves.push_back(std::move(r));
}
template <typename Row>
bool by_position(const Row* lhs, const Row* rhs) {
    if (lhs->position != rhs->position)
        return lhs->position < rhs->position;
    return lhs->id < rhs->id;
}

template <typename Row, typename Key>
std::map<Key, std::vector<const Row*>> group_by(const std::vector<Row>& rows,
                                                Key (*key)(const Row&)) {
    std::map<Key, std::vector<const Row*>> groups;
    for (const auto& r : rows)
        groups[key(r)].push_back(&r);
    for (auto& [k, v] : groups)
        std::sort(v.begin(), v.end(), by_position<Row>);
    return groups;
}

boost::uuids::uuid segment_of(const refdata::domain::curve_quote& q) {
    return q.curve_segment_id;
}

boost::uuids::uuid parent_segment(const refdata::domain::curve_segment_curve& c) {
    return c.curve_segment_id;
}

boost::uuids::uuid quote_definition(const refdata::domain::curve_quote& q) {
    return q.curve_definition_id;
}

boost::uuids::uuid quote_price_segment(const refdata::domain::curve_quote& q) {
    return q.commodity_price_segment_id;
}

boost::uuids::uuid price_segment_definition(const refdata::domain::commodity_price_segment& p) {
    return p.curve_definition_id;
}

boost::uuids::uuid quote_configuration(const refdata::domain::curve_quote& q) {
    return q.default_curve_configuration_id;
}

boost::uuids::uuid configuration_definition(const refdata::domain::default_curve_configuration& c) {
    return c.curve_definition_id;
}

boost::uuids::uuid factor_definition(const refdata::domain::inflation_seasonality_factor& f) {
    return f.curve_definition_id;
}

boost::uuids::uuid term_definition(const refdata::domain::cds_volatility_term& t) {
    return t.curve_definition_id;
}

boost::uuids::uuid
smile_parameter_definition(const refdata::domain::curve_parametric_smile_parameter& p) {
    return p.curve_definition_id;
}

boost::uuids::uuid volatility_config_definition(const refdata::domain::curve_volatility_config& c) {
    return c.curve_definition_id;
}

boost::uuids::uuid definition_of(const refdata::domain::curve_segment& s) {
    return s.curve_definition_id;
}

template <typename Row>
std::map<boost::uuids::uuid, const Row*> by_definition(const std::vector<Row>& rows) {
    std::map<boost::uuids::uuid, const Row*> out;
    for (const auto& r : rows)
        out.emplace(r.curve_definition_id, &r);
    return out;
}

/// The proxy an entry names, or null when it names none: one row per entry, so
/// the part's presence is the flag the flat shape spelled with a column.
const refdata::domain::cap_floor_proxy_config* proxy_of(
    const std::vector<refdata::domain::cap_floor_proxy_config>& parts,
    const boost::uuids::uuid& config_id) {
    const auto it = std::find_if(parts.begin(),
                                 parts.end(),
                                 [&](const auto& p) {
                                     return p.cap_floor_volatility_config_id == config_id;
                                 });
    return it == parts.end() ? nullptr : &*it;
}

template <typename Row>
const Row& settings_of(const std::map<boost::uuids::uuid, const Row*>& rows,
                       const refdata::domain::curve_definition& d) {
    const auto it = rows.find(d.id);
    if (it == rows.end())
        throw refusal("curve " + d.curve_id + " in section " + d.section_code +
                      " has no settings row");
    return *it->second;
}

// The rows read back for one document, grouped by the parent each attaches to,
// so building a segment finds its quotes and curves by id.
struct export_context {
    std::map<boost::uuids::uuid, std::vector<const refdata::domain::curve_quote*>> quotes;
    std::map<boost::uuids::uuid, std::vector<const refdata::domain::curve_quote*>> entry_quotes;
    std::map<boost::uuids::uuid, std::vector<const refdata::domain::curve_quote*>>
        configuration_quotes;
    std::map<boost::uuids::uuid, const refdata::domain::curve_bootstrap_config*>
        configuration_bootstraps;
    std::map<boost::uuids::uuid, std::vector<const refdata::domain::curve_quote*>>
        price_segment_quotes;
    std::map<boost::uuids::uuid, std::vector<const refdata::domain::curve_quote*>> basis_quotes;
    std::map<boost::uuids::uuid, const refdata::domain::curve_bootstrap_config*> entry_bootstraps;
    std::map<boost::uuids::uuid, std::vector<const refdata::domain::curve_segment_curve*>> curves;

    const std::vector<const refdata::domain::curve_quote*>&
    quotes_of_entry(const boost::uuids::uuid& definition_id) const {
        static const std::vector<const refdata::domain::curve_quote*> none;
        const auto it = entry_quotes.find(definition_id);
        return it == entry_quotes.end() ? none : it->second;
    }

    const std::vector<const refdata::domain::curve_quote*>&
    quotes_of(const boost::uuids::uuid& id) const {
        static const std::vector<const refdata::domain::curve_quote*> none;
        const auto it = quotes.find(id);
        return it == quotes.end() ? none : it->second;
    }

    const std::vector<const refdata::domain::curve_segment_curve*>&
    curves_of(const boost::uuids::uuid& id) const {
        static const std::vector<const refdata::domain::curve_segment_curve*> none;
        const auto it = curves.find(id);
        return it == curves.end() ? none : it->second;
    }

    void plain_quotes(record_writer out, const refdata::domain::curve_segment& s) const {
        for (const auto* q : quotes_of(s.id)) {
            if (!q->quote_text)
                throw refusal("segment " + boost::uuids::to_string(s.id) +
                              " holds a composite quote but is not an average OIS segment");
            auto item = out.append("Quote");
            item.set_own_text(*q->quote_text);
            item.set_optional_text("optional", q->optional_flag);
        }
    }

    void composite_quotes(record_writer out, const refdata::domain::curve_segment& s) const {
        for (const auto* q : quotes_of(s.id)) {
            if (!q->rate_quote || !q->spread_quote)
                throw refusal("average OIS segment " + boost::uuids::to_string(s.id) +
                              " holds a plain quote");
            auto item = out.append("CompositeQuote");
            item.set_text("RateQuote", *q->rate_quote);
            item.set_text("SpreadQuote", *q->spread_quote);
        }
    }

    std::vector<const refdata::domain::curve_segment_curve*>
    curves_in_role(const refdata::domain::curve_segment& s, std::string_view role) const {
        std::vector<const refdata::domain::curve_segment_curve*> out;
        for (const auto* c : curves_of(s.id))
            if (c->role == role)
                out.push_back(c);
        return out;
    }
};

std::string require(const std::optional<std::string>& v,
                    const refdata::domain::curve_segment& s,
                    std::string_view field) {
    if (!v)
        throw refusal("segment " + boost::uuids::to_string(s.id) + " of type '" + s.segment_type +
                      "' has no " + std::string(field));
    return *v;
}

// One segment's list of curves in one role: the container element holds one
// element per curve, named after the curve's role.
void export_index_curves(record_writer out,
                         const std::vector<const refdata::domain::curve_segment_curve*>& curves,
                         std::string_view container,
                         std::string_view item_name,
                         std::string_view index_attribute) {
    if (curves.empty())
        return;
    auto list = out.child(container);
    for (const auto* c : curves) {
        auto item = list.append(item_name);
        item.set_own_text(c->curve);
        item.set_optional_text(index_attribute, c->index_name);
    }
}

void export_segment(record_writer out,
                    const refdata::domain::curve_segment& s,
                    const export_context& ctx) {
    const auto kind = segment_kinds().find(s.segment_type);
    if (kind == segment_kinds().end())
        throw refusal("segment " + boost::uuids::to_string(s.id) + " has unknown type '" +
                      s.segment_type + "'");
    const auto& k = kind->second;

    if (k == "Direct") {
        auto r = out.append("Direct");
        r.set_spelling("Type", s.segment_type, "direct segment type");
        r.set_optional_text("Conventions", s.conventions);
        restore_common_settings(r, s);
        ctx.plain_quotes(r.child("Quotes"), s);
    } else if (k == "Simple") {
        auto r = out.append("Simple");
        r.set_spelling("Type", s.segment_type, "simple segment type");
        r.set_text("Conventions", require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        r.set_optional_text("ProjectionCurve", s.projection_curve);
        ctx.plain_quotes(r.child("Quotes"), s);
    } else if (k == "AverageOIS") {
        auto r = out.append("AverageOIS");
        r.set_text("Type", s.segment_type);
        r.set_text("Conventions", require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        r.set_optional_text("ProjectionCurve", s.projection_curve);
        ctx.composite_quotes(r.child("Quotes"), s);
    } else if (k == "TenorBasis") {
        auto r = out.append("TenorBasis");
        r.set_spelling("Type", s.segment_type, "tenor basis segment type");
        r.set_text("Conventions", require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        r.set_optional_text("ProjectionCurvePay", s.projection_curve_pay);
        r.set_optional_text("ProjectionCurveReceive", s.projection_curve_receive);
        r.set_optional_text("ProjectionCurveLong", s.projection_curve_long);
        r.set_optional_text("ProjectionCurveShort", s.projection_curve_short);
        ctx.plain_quotes(r.child("Quotes"), s);
    } else if (k == "CrossCurrency") {
        auto r = out.append("CrossCurrency");
        r.set_spelling("Type", s.segment_type, "cross currency segment type");
        r.set_text("Conventions", require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        r.set_text("DiscountCurve", require(s.discount_curve, s, "discount curve"));
        r.set_text("SpotRate", require(s.spot_rate, s, "spot rate"));
        r.set_optional_text("ProjectionCurveDomestic", s.projection_curve_domestic);
        r.set_optional_text("ProjectionCurveForeign", s.projection_curve_foreign);
        ctx.plain_quotes(r.child("Quotes"), s);
    } else if (k == "ZeroSpread") {
        auto r = out.append("ZeroSpread");
        r.set_spelling("Type", s.segment_type, "zero spread segment type");
        r.set_text("Conventions", require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        r.set_text("ReferenceCurve", require(s.reference_curve, s, "reference curve"));
        ctx.plain_quotes(r.child("Quotes"), s);
    } else if (k == "DiscountRatio") {
        auto r = out.append("DiscountRatio");
        r.set_spelling("Type", s.segment_type, "discount ratio type");
        r.set_optional_text("Conventions", s.conventions);
        restore_common_settings(r, s);
        auto base = r.child("BaseCurve");
        base.set_own_text(require(s.base_curve, s, "base curve"));
        base.set_text("currency", require(s.base_curve_currency, s, "base curve currency"));
        auto numerator = r.child("NumeratorCurve");
        numerator.set_own_text(require(s.numerator_curve, s, "numerator curve"));
        numerator.set_text("currency",
                           require(s.numerator_curve_currency, s, "numerator currency"));
        auto denominator = r.child("DenominatorCurve");
        denominator.set_own_text(require(s.denominator_curve, s, "denominator curve"));
        denominator.set_text("currency",
                             require(s.denominator_curve_currency, s, "denominator currency"));
    } else if (k == "FittedBond") {
        auto r = out.append("FittedBond");
        r.set_text("Type", s.segment_type);
        restore_common_settings(r, s);
        set_boolean_text(r, "ExtrapolateFlat", s.extrapolate_flat);
        ctx.plain_quotes(r.child("Quotes"), s);
        export_index_curves(
            r, ctx.curves_in_role(s, "IndexCurve"), "IndexCurves", "IndexCurve", "Index");
        export_index_curves(r,
                            ctx.curves_in_role(s, "IborIndexCurve"),
                            "IborIndexCurves",
                            "IborIndexCurve",
                            "iborIndex");
        export_index_curves(r,
                            ctx.curves_in_role(s, "InflationIndexCurve"),
                            "InflationIndexCurves",
                            "InflationIndexCurve",
                            "inflationIndex");
    } else if (k == "BondYieldShifted") {
        auto r = out.append("BondYieldShifted");
        r.set_text("Type", s.segment_type);
        r.set_text("Conventions", require(s.conventions, s, "conventions"));
        r.set_text("ReferenceCurve", require(s.reference_curve, s, "reference curve"));
        set_boolean_text(r, "ExtrapolateFlat", s.extrapolate_flat);
        ctx.plain_quotes(r.child("Quotes"), s);
        export_index_curves(
            r, ctx.curves_in_role(s, "IndexCurve"), "IndexCurves", "IndexCurve", "Index");
        export_index_curves(r,
                            ctx.curves_in_role(s, "IborIndexCurve"),
                            "IborIndexCurves",
                            "IborIndexCurve",
                            "iborIndex");
    } else if (k == "WeightedAverage") {
        auto r = out.append("WeightedAverage");
        r.set_text("Type", s.segment_type);
        r.set_text("ReferenceCurve1", require(s.reference_curve, s, "first reference curve"));
        r.set_text("ReferenceCurve2", require(s.reference_curve_2, s, "second reference curve"));
        r.set_real("Weight1", s.weight_1.value_or(0.0));
        r.set_real("Weight2", s.weight_2.value_or(0.0));
    } else if (k == "YieldPlusDefault") {
        auto r = out.append("YieldPlusDefault");
        r.set_text("Type", s.segment_type);
        r.set_text("ReferenceCurve", require(s.reference_curve, s, "reference curve"));
        const auto cs = ctx.curves_in_role(s, "DefaultCurve");
        if (cs.size() != 1)
            throw refusal("yield plus default segment " + boost::uuids::to_string(s.id) +
                          " needs exactly one default curve");
        r.child("DefaultCurves").set_text("DefaultCurve", cs.front()->curve);
        r.child("Weights").set_real("Weight", cs.front()->weight.value_or(0.0));
    } else {
        auto r = out.append("IborFallback");
        r.set_text("Type", s.segment_type);
        restore_common_settings(r, s);
        r.set_text("IborIndex", require(s.ibor_index, s, "Ibor index"));
        r.set_text("RfrCurve", require(s.rfr_curve, s, "risk free curve"));
        r.set_optional_text("RfrIndex", s.rfr_index);
        r.set_optional_decimal("Spread", s.spread);
    }
}

void export_bootstrap(record_writer w, const refdata::domain::curve_bootstrap_config& c) {
    w.set_optional_real("Accuracy", c.accuracy);
    w.set_optional_real("GlobalAccuracy", c.global_accuracy);
    set_boolean_text(w, "DontThrow", c.dont_throw);
    w.set_optional_integer("MaxAttempts", c.max_attempts);
    w.set_optional_real("MaxFactor", c.max_factor);
    w.set_optional_real("MinFactor", c.min_factor);
    w.set_optional_integer("DontThrowSteps", c.dont_throw_steps);
    set_boolean_text(w, "Global", c.global);
    w.set_optional_real("SmoothnessLambda", c.smoothness_lambda);
}

void export_quote_list(record_writer quotes,
                       const std::vector<const refdata::domain::curve_quote*>& rows,
                       std::optional<std::string_view> list) {
    for (const auto* q : rows) {
        const bool same = list ? (q->quote_list && *q->quote_list == *list) : !q->quote_list;
        if (!same)
            continue;
        auto item = quotes.append("Quote");
        item.set_own_text(q->quote_text.value_or(""));
        item.set_optional_text("optional", q->optional_flag);
    }
}

void export_yield_curve(record_writer r,
                        const refdata::domain::curve_definition& d,
                        const refdata::domain::yield_curve_config& y,
                        const refdata::domain::curve_bootstrap_config* bootstrap,
                        const std::vector<const refdata::domain::curve_segment*>& segments,
                        const export_context& ctx) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_spelling("Currency", y.currency, "currency code");
    r.set_text("DiscountCurve", y.discount_curve);
    r.set_optional_spelling(
        "InterpolationVariable", y.interpolation_variable, "interpolation variable");
    r.set_optional_spelling("InterpolationMethod", y.interpolation_method, "interpolation method");
    r.set_optional_integer("MixedInterpolationCutoff", y.mixed_interpolation_cutoff);
    r.set_optional_spelling("YieldCurveDayCounter", y.day_counter, "day counter");
    r.set_optional_real("Tolerance", y.tolerance);
    r.set_optional_spelling("Extrapolation", y.extrapolation, "ORE boolean");
    r.set_optional_spelling("ExtrapolationMethod", y.extrapolation_method, "extrapolation method");
    r.set_optional_spelling(
        "ExcludeT0FromInterpolation", y.exclude_t0_from_interpolation, "ORE boolean");
    if (y.has_report) {
        auto report = r.child("Report");
        report.set_optional_text("PillarDates", y.report_pillar_dates);
    }
    if (bootstrap)
        export_bootstrap(r.child("BootstrapConfig"), *bootstrap);
    if (!segments.empty()) {
        auto out = r.child("Segments");
        for (const auto* s : segments)
            export_segment(out, *s, ctx);
    }
}

void export_equity_curve(record_writer r,
                         const refdata::domain::curve_definition& d,
                         const refdata::domain::equity_curve_config& e,
                         const export_context& ctx) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_text("Currency", e.currency);
    r.set_optional_text("Calendar", e.calendar);
    r.set_text("ForecastingCurve", e.forecasting_curve);
    r.set_spelling("Type", e.equity_type, "equity curve type");
    r.set_optional_spelling("ExerciseStyle", e.exercise_style, "exercise style");
    r.set_text("SpotQuote", e.spot_quote);
    if (e.has_quotes) {
        auto quotes = r.child("Quotes");
        for (const auto* q : ctx.quotes_of_entry(d.id)) {
            auto item = quotes.append("Quote");
            item.set_own_text(q->quote_text.value_or(""));
            item.set_optional_text("optional", q->optional_flag);
        }
    }
    r.set_optional_spelling("DayCounter", e.day_counter, "day counter");
    if (e.has_dividend_interpolation) {
        auto interpolation = r.child("DividendInterpolation");
        interpolation.set_optional_spelling(
            "InterpolationVariable", e.dividend_interpolation_variable, "interpolation variable");
        interpolation.set_optional_spelling(
            "InterpolationMethod", e.dividend_interpolation_method, "interpolation method");
    }
    r.set_optional_spelling("DividendExtrapolation", e.dividend_extrapolation, "ORE boolean");
    r.set_optional_spelling("Extrapolation", e.extrapolation, "ORE boolean");
}

void export_inflation_curve(
    record_writer r,
    const refdata::domain::curve_definition& d,
    const refdata::domain::inflation_curve_config& v,
    const std::vector<const refdata::domain::inflation_seasonality_factor*>& factors,
    const export_context& ctx) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_text("NominalTermStructure", v.nominal_term_structure);
    r.set_spelling("Type", v.inflation_type, "inflation type");
    r.set_optional_text("Conventions", v.conventions);
    if (v.has_quotes) {
        auto quotes = r.child("Quotes");
        for (const auto* q : ctx.quotes_of_entry(d.id)) {
            auto item = quotes.append("Quote");
            item.set_own_text(q->quote_text.value_or(""));
            item.set_optional_text("optional", q->optional_flag);
        }
    }
    r.set_optional_spelling("Extrapolation", v.extrapolation, "ORE boolean");
    r.set_text("Calendar", v.calendar);
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_text("Lag", v.lag);
    r.set_spelling("Frequency", v.frequency, "frequency");
    r.set_optional_text("BaseRate", v.base_rate);
    r.set_optional_real("Tolerance", v.tolerance);
    if (v.has_seasonality) {
        auto season = r.child("Seasonality");
        season.set_text("BaseDate", v.seasonality_base_date.value_or(""));
        season.set_spelling("Frequency", v.seasonality_frequency.value_or(""), "frequency");
        auto list = season.child("Factors");
        for (const auto* f : factors)
            list.append_value("Factor", f->factor);
    }
    r.set_optional_spelling("UseLastFixingDate", v.use_last_fixing_date, "ORE boolean");
    r.set_optional_text("InterpolationVariable", v.interpolation_variable);
    r.set_optional_text("InterpolationMethod", v.interpolation_method);
}

void export_configuration_settings(record_writer r,
                                   const refdata::domain::default_curve_configuration& c,
                                   const export_context& ctx) {
    r.set_optional_text("DiscountCurve", c.discount_curve);
    r.set_optional_text("RecoveryRate", c.recovery_rate);
    r.set_optional_text("StartDate", c.start_date);
    if (c.has_quotes) {
        auto quotes = r.child("Quotes");
        const auto it = ctx.configuration_quotes.find(c.id);
        if (it != ctx.configuration_quotes.end())
            for (const auto* q : it->second) {
                auto item = quotes.append("Quote");
                item.set_own_text(q->quote_text.value_or(""));
                item.set_optional_text("optional", q->optional_flag);
            }
    }
    r.set_optional_text("BenchmarkCurve", c.benchmark_curve);
    r.set_optional_text("SourceCurve", c.source_curve);
    r.set_optional_text("Pillars", c.pillars);
    r.set_optional_integer("SpotLag", c.spot_lag);
    r.set_optional_text("Calendar", c.calendar);
    r.set_optional_text("Conventions", c.conventions);
    r.set_optional_spelling("Extrapolation", c.extrapolation, "ORE boolean");
    r.set_optional_decimal("RunningSpread", c.running_spread);
    r.set_optional_text("IndexTerm", c.index_term);
    r.set_optional_spelling("ImplyDefaultFromMarket", c.imply_default_from_market, "ORE boolean");
    r.set_optional_spelling("AllowNegativeRates", c.allow_negative_rates, "ORE boolean");
    r.set_optional_spelling("PriceIsUpfront", c.price_is_upfront, "ORE boolean");
    r.set_optional_text("InitialState", c.initial_state);
    r.set_optional_text("States", c.states);
    const auto b = ctx.configuration_bootstraps.find(c.id);
    if (b != ctx.configuration_bootstraps.end())
        export_bootstrap(r.child("BootstrapConfig"), *b->second);
}

void export_default_curve(
    record_writer r,
    const refdata::domain::curve_definition& d,
    const refdata::domain::default_curve_config& v,
    const std::vector<const refdata::domain::default_curve_configuration*>& configurations,
    const export_context& ctx) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_spelling("Currency", v.currency, "currency code");

    if (configurations.size() == 1 && configurations.front()->is_inline) {
        const auto& c = *configurations.front();
        r.set_optional_spelling("Type", c.default_curve_type, "default curve type");
        r.set_optional_spelling("DayCounter", c.day_counter, "day counter");
        export_configuration_settings(r, c, ctx);
        return;
    }

    auto list = r.child("Configurations");
    for (const auto* c : configurations) {
        if (c->is_inline)
            throw refusal("default curve " + d.curve_id +
                          " mixes an inline configuration with listed ones");
        auto k = list.append("Configuration");
        set_integer_text(k, "priority", c->priority);
        k.set_spelling("Type", c->default_curve_type.value_or(""), "default curve type");
        k.set_spelling("DayCounter", c->day_counter.value_or(""), "day counter");
        k.set_optional_text("ReinterpretedYieldCurve", c->reinterpreted_yield_curve);
        export_configuration_settings(k, *c, ctx);
    }
}

void export_commodity_curve(
    record_writer r,
    const refdata::domain::curve_definition& d,
    const refdata::domain::commodity_curve_config& v,
    const std::vector<const refdata::domain::commodity_price_segment*>& segments,
    const export_context& ctx) {
    static const std::vector<const refdata::domain::curve_quote*> none;
    const auto quotes_of = [&](const auto& map, const boost::uuids::uuid& id)
        -> const std::vector<const refdata::domain::curve_quote*>& {
        const auto it = map.find(id);
        return it == map.end() ? none : it->second;
    };

    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_spelling("Currency", v.currency, "currency code");
    r.set_optional_text("BasePriceCurve", v.base_price_curve);
    r.set_optional_text("BaseYieldCurve", v.base_yield_curve);
    r.set_optional_text("YieldCurve", v.yield_curve);
    r.set_optional_text("SpotQuote", v.spot_quote);
    if (v.has_quotes)
        export_quote_list(r.child("Quotes"), ctx.quotes_of_entry(d.id), std::nullopt);
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_spelling("InterpolationMethod", v.interpolation_method, "interpolation method");
    r.set_optional_text("Conventions", v.conventions);
    if (v.has_basis_configuration) {
        auto b = r.child("BasisConfiguration");
        b.set_text("BasePriceCurve", v.basis_base_price_curve.value_or(""));
        b.set_text("BasePriceConventions", v.basis_base_price_conventions.value_or(""));
        export_quote_list(
            b.child("BasisQuotes"), quotes_of(ctx.basis_quotes, d.id), basis_quotes_list);
        b.set_text("BasisConventions", v.basis_conventions.value_or(""));
        b.set_optional_spelling("DayCounter", v.basis_day_counter, "day counter");
        b.set_optional_spelling(
            "InterpolationMethod", v.basis_interpolation_method, "interpolation method");
        b.set_optional_spelling("AddBasis", v.basis_add_basis, "ORE boolean");
        b.set_optional_integer("MonthOffset", v.basis_month_offset);
        b.set_optional_spelling("AverageBase", v.basis_average_base, "ORE boolean");
        b.set_optional_spelling(
            "PriceAsHistoricalFixing", v.basis_price_as_historical_fixing, "ORE boolean");
    }
    if (v.has_price_segments) {
        auto list = r.child("PriceSegments");
        for (const auto* p : segments) {
            auto g = list.append("PriceSegment");
            g.set_spelling("Type", p->segment_type, "price segment type");
            g.set_optional_integer("Priority", p->priority);
            if (!p->conventions)
                throw refusal("price segment of " + d.curve_id + " has no conventions");
            g.set_text("Conventions", *p->conventions);
            const auto& rows = quotes_of(ctx.price_segment_quotes, p->id);
            if (p->has_quotes)
                export_quote_list(g.child("Quotes"), rows, std::nullopt);
            g.set_optional_text("PeakPriceCurveId", p->peak_price_curve_id);
            g.set_optional_text("PeakPriceCalendar", p->peak_price_calendar);
            if (p->has_off_peak_daily) {
                auto o = g.child("OffPeakDaily");
                export_quote_list(o.child("OffPeakQuotes"), rows, off_peak_quotes_list);
                export_quote_list(o.child("PeakQuotes"), rows, peak_quotes_list);
            }
        }
    }
    r.set_optional_spelling("Extrapolation", v.extrapolation, "ORE boolean");
    const auto b = ctx.entry_bootstraps.find(d.id);
    if (b != ctx.entry_bootstraps.end())
        export_bootstrap(r.child("BootstrapConfig"), *b->second);
}

void export_fx_volatility(record_writer r,
                          const refdata::domain::curve_definition& d,
                          const refdata::domain::fx_volatility_config& v,
                          const refdata::domain::curve_report_configuration* report,
                          const smile_export& smile) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_spelling("Dimension", v.dimension, "dimension");
    r.set_optional_spelling("SmileType", v.smile_type, "smile type");
    r.set_optional_text("SmileInterpolation", v.smile_interpolation);
    if (smile.row)
        export_parametric_smile(
            r.child("ParametricSmileConfiguration"), *smile.row, smile.parameters);
    r.set_optional_text("Deltas", v.deltas);
    r.set_optional_text("SmileDelta", v.smile_delta);
    r.set_optional_text("Conventions", v.conventions);
    r.set_optional_text("Expiries", v.expiries);
    r.set_optional_text("FXSpotID", v.fx_spot_id);
    r.set_optional_text("FXForeignCurveID", v.fx_foreign_curve_id);
    r.set_optional_text("FXDomesticCurveID", v.fx_domestic_curve_id);
    r.set_optional_text("Calendar", v.calendar);
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_text("FXIndexTag", v.fx_index_tag);
    r.set_optional_text("BaseVolatility1", v.base_volatility_1);
    r.set_optional_text("BaseVolatility2", v.base_volatility_2);
    if (report)
        export_report(r.child("Report"), *report);
    r.set_optional_spelling("SmileExtrapolation", v.smile_extrapolation, "extrapolation");
    r.set_optional_text("TimeInterpolation", v.time_interpolation);
    r.set_optional_text("TimeWeighting", v.time_weighting);
    r.set_optional_real("ButterflyErrorTolerance", v.butterfly_error_tolerance);
}

void export_yield_volatility(record_writer r,
                             const refdata::domain::curve_definition& d,
                             const refdata::domain::yield_volatility_config& v,
                             const refdata::domain::curve_report_configuration* report) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_text("Qualifier", v.qualifier);
    r.set_optional_spelling("Dimension", v.dimension, "dimension");
    r.set_spelling("VolatilityType", v.volatility_type, "volatility type");
    r.set_spelling("Extrapolation", v.extrapolation, "extrapolation");
    r.set_spelling("DayCounter", v.day_counter, "day counter");
    r.set_text("Calendar", v.calendar);
    r.set_spelling("BusinessDayConvention", v.business_day_convention, "business day convention");
    r.set_text("OptionTenors", v.option_tenors);
    r.set_text("BondTenors", v.bond_tenors);
    if (report)
        export_report(r.child("Report"), *report);
}

void export_base_correlation(record_writer r,
                             const refdata::domain::curve_definition& d,
                             const refdata::domain::base_correlation_config& v) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_text("Terms", v.terms);
    r.set_text("DetachmentPoints", v.detachment_points);
    r.set_real("SettlementDays", v.settlement_days);
    r.set_text("Calendar", v.calendar);
    r.set_spelling("BusinessDayConvention", v.business_day_convention, "business day convention");
    r.set_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_spelling("Extrapolate", v.extrapolate, "ORE boolean");
    r.set_optional_text("QuoteName", v.quote_name);
    r.set_optional_text("StartDate", v.start_date);
    r.set_optional_spelling("Rule", v.rule, "date rule");
    r.set_optional_spelling("AdjustForLosses", v.adjust_for_losses, "ORE boolean");
    r.set_optional_text("IndexTerm", v.index_term);
    r.set_optional_text("IndexSpread", v.index_spread);
    r.set_optional_text("Currency", v.currency);
    r.set_optional_spelling("CalibrateConstituentsToIndexSpread",
                            v.calibrate_constituents_to_index_spread,
                            "ORE boolean");
    r.set_optional_spelling("UseAssumedRecovery", v.use_assumed_recovery, "ORE boolean");
}

void export_cds_volatility(
    record_writer r,
    const refdata::domain::curve_definition& d,
    const refdata::domain::cds_volatility_config& v,
    const std::vector<const refdata::domain::cds_volatility_term*>& terms,
    const std::vector<const refdata::domain::curve_volatility_config*>& configs) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    if (!terms.empty()) {
        auto list = r.child("Terms");
        for (const auto* t : terms) {
            auto term = list.append("Term");
            term.set_text("Label", t->label);
            term.set_text("Curve", t->curve);
            term.set_optional_text("Maturity", t->maturity);
        }
    }
    r.set_optional_text("Expiries", v.expiries);
    for (const auto* c : configs) {
        if (c->kind != strike_surface_kind)
            throw refusal("CDS volatility " + d.curve_id + " has a volatility config of kind " +
                          c->kind + ", which is not modelled yet");
        export_strike_surface(r.child("StrikeSurface"), *c);
    }
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_text("Calendar", v.calendar);
    r.set_optional_text("StrikeType", v.strike_type);
    r.set_optional_text("QuoteName", v.quote_name);
    set_decimal_from_double(r, "StrikeFactor", v.strike_factor);
}

void export_inflation_cap_floor_volatility(
    record_writer r,
    const refdata::domain::curve_definition& d,
    const refdata::domain::inflation_cap_floor_volatility_config& v,
    const refdata::domain::curve_report_configuration* report,
    const refdata::domain::curve_bootstrap_config* bootstrap) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_spelling("Type", v.inflation_type, "inflation type");
    r.set_text("QuoteType", v.quote_type);
    r.set_spelling("VolatilityType", v.volatility_type, "volatility type");
    r.set_spelling("Extrapolation", v.extrapolation, "ORE boolean");
    r.set_text("Tenors", v.tenors);
    r.set_optional_integer("SettlementDays", v.settlement_days);
    r.set_optional_text("CapStrikes", v.cap_strikes);
    r.set_optional_text("FloorStrikes", v.floor_strikes);
    r.set_optional_text("Strikes", v.strikes);
    r.set_text("Calendar", v.calendar);
    r.set_spelling("DayCounter", v.day_counter, "day counter");
    r.set_spelling("BusinessDayConvention", v.business_day_convention, "business day convention");
    r.set_text("Index", v.index);
    r.set_text("IndexCurve", v.index_curve);
    r.set_optional_spelling("IndexInterpolated", v.index_interpolated, "ORE boolean");
    r.set_text("ObservationLag", v.observation_lag);
    r.set_text("YieldTermStructure", v.yield_term_structure);
    r.set_optional_text("QuoteIndex", v.quote_index);
    r.set_optional_text("Conventions", v.conventions);
    if (report)
        export_report(r.child("Report"), *report);
    if (bootstrap)
        export_bootstrap(r.child("BootstrapConfig"), *bootstrap);
}

void export_swaption_volatility(record_writer r,
                                const refdata::domain::curve_definition& d,
                                const refdata::domain::swaption_volatility_config& v,
                                const refdata::domain::curve_report_configuration* report,
                                const smile_export& smile) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    if (v.has_proxy_config) {
        auto proxy = r.child("ProxyConfig");
        auto source = proxy.child("Source");
        source.set_text("CurveId", v.proxy_source_curve_id.value_or(""));
        source.set_text("ShortSwapIndexBase", v.proxy_source_short_swap_index_base.value_or(""));
        source.set_text("SwapIndexBase", v.proxy_source_swap_index_base.value_or(""));
        auto target = proxy.child("Target");
        target.set_text("ShortSwapIndexBase", v.proxy_target_short_swap_index_base.value_or(""));
        target.set_text("SwapIndexBase", v.proxy_target_swap_index_base.value_or(""));
    }
    r.set_optional_spelling("Dimension", v.dimension, "dimension");
    r.set_optional_spelling("VolatilityType", v.volatility_type, "volatility type");
    r.set_optional_text("Interpolation", v.interpolation);
    if (smile.row)
        export_parametric_smile(
            r.child("ParametricSmileConfiguration"), *smile.row, smile.parameters);
    r.set_optional_text("Extrapolation", v.extrapolation);
    r.set_optional_text("OutputVolatilityType", v.output_volatility_type);
    r.set_optional_text("ModelShift", v.model_shift);
    r.set_optional_text("OutputShift", v.output_shift);
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_text("Calendar", v.calendar);
    r.set_optional_spelling(
        "BusinessDayConvention", v.business_day_convention, "business day convention");
    r.set_optional_text("OptionTenors", v.option_tenors);
    r.set_optional_text("SwapTenors", v.swap_tenors);
    r.set_optional_text("ShortSwapIndexBase", v.short_swap_index_base);
    r.set_optional_text("SwapIndexBase", v.swap_index_base);
    r.set_optional_text("SmileOptionTenors", v.smile_option_tenors);
    r.set_optional_text("SmileSwapTenors", v.smile_swap_tenors);
    r.set_optional_text("SmileSpreads", v.smile_spreads);
    r.set_optional_text("QuoteTag", v.quote_tag);
    if (report)
        export_report(r.child("Report"), *report);
}

void export_cap_floor_volatility(record_writer r,
                                 const refdata::domain::curve_definition& d,
                                 const refdata::domain::cap_floor_volatility_config& v,
                                 const refdata::domain::cap_floor_proxy_config* proxy_config,
                                 const refdata::domain::curve_report_configuration* report,
                                 const refdata::domain::curve_bootstrap_config* bootstrap,
                                 const smile_export& smile) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    if (proxy_config) {
        auto proxy = r.child("ProxyConfig");
        auto source = proxy.child("Source");
        source.set_text("CurveId", proxy_config->source_curve_id.value_or(""));
        source.set_text("Index", proxy_config->source_index.value_or(""));
        source.set_optional_text("RateComputationPeriod", proxy_config->source_rate_computation_period);
        auto target = proxy.child("Target");
        target.set_text("Index", proxy_config->target_index.value_or(""));
        target.set_optional_text("RateComputationPeriod",
                                 proxy_config->target_rate_computation_period);
        target.set_optional_integer("ONCapSettlementDays",
                                    proxy_config->target_on_cap_settlement_days);
        proxy.set_optional_real("ScalingFactor", proxy_config->scaling_factor);
    }
    r.set_optional_spelling("VolatilityType", v.volatility_type, "volatility type");
    r.set_optional_spelling("OutputVolatilityType", v.output_volatility_type, "volatility type");
    r.set_optional_real("ModelShift", v.model_shift);
    r.set_optional_real("OutputShift", v.output_shift);
    r.set_optional_spelling("Extrapolation", v.extrapolation, "extrapolation");
    r.set_optional_spelling("InterpolationMethod", v.interpolation_method, "interpolation method");
    r.set_optional_spelling("IncludeAtm", v.include_atm, "ORE boolean");
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_text("Calendar", v.calendar);
    r.set_optional_spelling(
        "BusinessDayConvention", v.business_day_convention, "business day convention");
    r.set_optional_text("Tenors", v.tenors);
    r.set_optional_text("Strikes", v.strikes);
    r.set_optional_spelling("OptionalQuotes", v.optional_quotes, "ORE boolean");
    r.set_optional_text("IborIndex", v.ibor_index);
    r.set_optional_text("Index", v.index);
    r.set_optional_text("RateComputationPeriod", v.rate_computation_period);
    r.set_optional_integer("ONCapSettlementDays", v.on_cap_settlement_days);
    r.set_optional_text("DiscountCurve", v.discount_curve);
    r.set_optional_text("AtmTenors", v.atm_tenors);
    r.set_optional_integer("SettlementDays", v.settlement_days);
    r.set_optional_spelling("InterpolateOn", v.interpolate_on, "interpolate on");
    r.set_optional_spelling("TimeInterpolation", v.time_interpolation, "time interpolation");
    r.set_optional_spelling("StrikeInterpolation", v.strike_interpolation, "strike interpolation");
    if (smile.row)
        export_parametric_smile(
            r.child("ParametricSmileConfiguration"), *smile.row, smile.parameters);
    r.set_optional_spelling("InputType", v.input_type, "input type");
    r.set_optional_spelling("QuoteIncludesIndexName", v.quote_includes_index_name, "ORE boolean");
    r.set_optional_spelling("FlatFirstPeriod", v.flat_first_period, "ORE boolean");
    r.set_optional_spelling("UseEffeciveVolatility", v.use_effecive_volatility, "ORE boolean");
    if (bootstrap)
        export_bootstrap(r.child("BootstrapConfig"), *bootstrap);
    if (report)
        export_report(r.child("Report"), *report);
    r.set_optional_spelling("UseEffectiveVolatility", v.use_effective_volatility, "ORE boolean");
}

void export_equity_volatility(
    record_writer r,
    const refdata::domain::curve_definition& d,
    const refdata::domain::equity_volatility_config& v,
    const std::vector<const refdata::domain::curve_volatility_config*>& configs,
    const std::vector<const refdata::domain::curve_quote*>& entry_quotes,
    const refdata::domain::curve_report_configuration* report) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_optional_text("EquityId", v.equity_id);
    r.set_text("Currency", v.currency);
    r.set_optional_spelling("Dimension", v.dimension, "dimension");
    r.set_optional_text("Expiries", v.expiries);
    r.set_optional_text("Strikes", v.strikes);
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_spelling("TimeExtrapolation", v.time_extrapolation, "extrapolation");
    r.set_optional_spelling("StrikeExtrapolation", v.strike_extrapolation, "extrapolation");
    export_volatility_configs(r, v.has_volatility_config, configs, entry_quotes, d.curve_id);
    r.set_optional_text("Calendar", v.calendar);
    r.set_optional_spelling("PreferOutOfTheMoney", v.prefer_out_of_the_money, "ORE boolean");
    if (report)
        export_report(r.child("Report"), *report);
}

void export_commodity_volatility(
    record_writer r,
    const refdata::domain::curve_definition& d,
    const refdata::domain::commodity_volatility_config& v,
    const std::vector<const refdata::domain::curve_volatility_config*>& configs,
    const std::vector<const refdata::domain::curve_quote*>& entry_quotes,
    const refdata::domain::curve_report_configuration* report) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_spelling("Currency", v.currency, "currency code");
    r.set_optional_text("InstrumentType", v.instrument_type);
    r.set_optional_integer("CalendarSpreadOffset", v.calendar_spread_offset);
    r.set_optional_text("CalendarSpreadUnderlyingName", v.calendar_spread_underlying_name);
    export_volatility_configs(r, v.has_volatility_config, configs, entry_quotes, d.curve_id);
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_text("Calendar", v.calendar);
    r.set_optional_text("FutureConventions", v.future_conventions);
    r.set_optional_text("OptionExpiryRollDays", v.option_expiry_roll_days);
    r.set_optional_text("PriceCurveId", v.price_curve_id);
    r.set_optional_text("YieldCurveId", v.yield_curve_id);
    r.set_optional_spelling("QuoteSuffix", v.quote_suffix, "quote suffix");
    r.set_optional_spelling("PreferOutOfTheMoney", v.prefer_out_of_the_money, "ORE boolean");
    if (report)
        export_report(r.child("Report"), *report);
}

void export_bond_future_volatility(
    record_writer r,
    const refdata::domain::curve_definition& d,
    const refdata::domain::bond_future_volatility_config& v,
    const std::vector<const refdata::domain::curve_volatility_config*>& configs,
    const std::vector<const refdata::domain::curve_quote*>& entry_quotes) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_text("ContractName", v.contract_name);
    export_volatility_configs(r, v.has_volatility_config, configs, entry_quotes, d.curve_id);
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_text("Calendar", v.calendar);
    r.set_optional_text("YieldCurveId", v.yield_curve_id);
    set_decimal_from_double(r, "StrikeFactor", v.strike_factor);
    r.set_optional_spelling("UseOnlyPutCall", v.use_only_put_call, "put or call");
    r.set_optional_spelling("PreferOutOfTheMoney", v.prefer_out_of_the_money, "ORE boolean");
    r.set_optional_spelling("TreatAsEuropean", v.treat_as_european, "ORE boolean");
}

void export_correlation(record_writer r,
                        const refdata::domain::curve_definition& d,
                        const refdata::domain::curve_correlation_config& v) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_spelling("CorrelationType", v.correlation_type, "correlation type");
    r.set_optional_text("Index1", v.index_1);
    r.set_optional_text("Index2", v.index_2);
    r.set_optional_text("Conventions", v.conventions);
    r.set_optional_text("SwaptionVolatility", v.swaption_volatility);
    r.set_optional_text("DiscountCurve", v.discount_curve);
    r.set_optional_spelling("Currency", v.currency, "currency code");
    r.set_optional_spelling("Dimension", v.dimension, "dimension");
    r.set_optional_spelling("QuoteType", v.quote_type, "correlation quote type");
    r.set_optional_spelling("Extrapolation", v.extrapolation, "ORE boolean");
    r.set_optional_spelling("DayCounter", v.day_counter, "day counter");
    r.set_optional_text("Calendar", v.calendar);
    r.set_optional_spelling(
        "BusinessDayConvention", v.business_day_convention, "business day convention");
    r.set_optional_text("OptionTenors", v.option_tenors);
}

void export_security(record_writer r,
                     const refdata::domain::curve_definition& d,
                     const refdata::domain::curve_security_config& v) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_optional_text("SpreadQuote", v.spread_quote);
    r.set_optional_text("RecoveryRateQuote", v.recovery_rate_quote);
    r.set_optional_text("CPRQuote", v.cpr_quote);
    r.set_optional_text("PriceQuote", v.price_quote);
    r.set_optional_text("ConversionFactor", v.conversion_factor);
}

void export_fx_spot(record_writer r, const refdata::domain::curve_definition& d) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
}

void export_intraday_power_curve(record_writer r,
                                 const refdata::domain::curve_definition& d,
                                 const refdata::domain::intraday_power_curve_config& v) {
    r.set_text("CurveId", d.curve_id);
    r.set_text("CurveDescription", d.description.value_or(""));
    r.set_spelling("Currency", v.currency, "currency code");
    r.set_text("DailyAveragePriceCurve", v.daily_average_price_curve);
    r.set_text("ShapeQuoteName", v.shape_quote_name);
    if (!v.convention)
        throw refusal("intraday power curve " + d.curve_id + " has no convention");
    r.set_text("Convention", *v.convention);
}
refdata::domain::curve_global_report new_global_report(const boost::uuids::uuid& configuration_id,
                                                       std::string_view family,
                                                       bool has_report,
                                                       int position) {
    refdata::domain::curve_global_report r;
    r.id = curve_configuration_projection_new_uuid();
    r.curve_configuration_id = configuration_id;
    r.family = std::string(family);
    r.has_report = has_report;
    r.position = position;
    curve_configuration_projection_set_audit(r);
    return r;
}

// A family's report is written in one of two shapes: the curve families state
// only their pillar dates, and the volatility families state the full set of
// columns. The report element's own table says which it is, so the projection
// asks rather than restating the schema.
void import_global_family(ores::refdata::messaging::curve_configuration_document& out,
                          const boost::uuids::uuid& configuration_id,
                          std::string_view family,
                          const record_reader& element,
                          int& position) {
    auto r = new_global_report(configuration_id, family, element.has("Report"), position++);
    if (const auto report = element.child("Report"); report) {
        if (report->declares("Deltas"))
            fill_report(r, *report);
        else
            r.pillar_dates = report->text("PillarDates");
    }
    out.global_reports.push_back(std::move(r));
}

void import_global_report(ores::refdata::messaging::curve_configuration_document& out,
                          const boost::uuids::uuid& configuration_id,
                          const record_reader& v) {
    int position = 0;
    for (const std::string_view family : {"FXVolatilities",
                                          "EquityVolatilities",
                                          "CommodityVolatilities",
                                          "IRSwaptionVolatilities",
                                          "IRCapFloorVolatilities",
                                          "YieldCurves",
                                          "InflationCapFloorVolatilities",
                                          "DefaultCurves"}) {
        if (const auto element = v.child(family); element)
            import_global_family(out, configuration_id, family, *element, position);
    }
    if (position == 0)
        throw refusal("the report configuration writes no curve family");
}

void export_global_family(record_writer element, const refdata::domain::curve_global_report& r) {
    if (!r.has_report)
        return;
    auto report = element.child("Report");
    if (report.declares("Deltas"))
        export_report(report, r);
    else
        report.set_optional_text("PillarDates", r.pillar_dates);
}

void export_global_report(record_writer v,
                          const std::vector<const refdata::domain::curve_global_report*>& rows) {
    for (const auto* r : rows) {
        if (r->family == "FXVolatilities")
            export_global_family(v.child("FXVolatilities"), *r);
        else if (r->family == "EquityVolatilities")
            export_global_family(v.child("EquityVolatilities"), *r);
        else if (r->family == "CommodityVolatilities")
            export_global_family(v.child("CommodityVolatilities"), *r);
        else if (r->family == "IRSwaptionVolatilities")
            export_global_family(v.child("IRSwaptionVolatilities"), *r);
        else if (r->family == "IRCapFloorVolatilities")
            export_global_family(v.child("IRCapFloorVolatilities"), *r);
        else if (r->family == "YieldCurves")
            export_global_family(v.child("YieldCurves"), *r);
        else if (r->family == "InflationCapFloorVolatilities")
            export_global_family(v.child("InflationCapFloorVolatilities"), *r);
        else if (r->family == "DefaultCurves")
            export_global_family(v.child("DefaultCurves"), *r);
        else
            throw refusal("the report configuration has an unknown curve family " + r->family);
    }
}

}

ores::refdata::messaging::curve_configuration_document
curve_configuration_projection::map(const document& v) {
    ores::refdata::messaging::curve_configuration_document mapped;

    auto& config = mapped.config;
    config.id = curve_configuration_projection_new_uuid();
    config.name = "CurveConfiguration";
    config.description = std::string(curve_configuration_projection_audit_commentary);
    curve_configuration_projection_set_audit(config);

    const record_reader root(
        *curve_configuration_projection_xg::curve_configuration_document().root, &v);

    if (const auto report = root.child("ReportConfiguration"); report)
        import_global_report(mapped, config.id, *report);

    for (const auto& s : sections()) {
        if (!root.has(s.code))
            continue;
        const auto section = root.child(s.code);
        const auto entries = section->children(s.entry).size();
        if (!is_modelled(s.code) && entries > 0)
            throw refusal("section " + std::string(s.code) + " holds " + std::to_string(entries) +
                          " entries, which are not modelled yet");
        refdata::domain::curve_configuration_section row;
        row.id = curve_configuration_projection_new_uuid();
        row.curve_configuration_id = config.id;
        row.section_code = std::string(s.code);
        curve_configuration_projection_set_audit(row);
        mapped.sections.push_back(std::move(row));
    }

    // One position across every section, in the order the mapper has always
    // used. That order is not the schema's element order, and it is the order
    // the stored rows were numbered in, so it must not be reordered without
    // renumbering them.
    int position = 0;
    const auto each = [&root](std::string_view section, std::string_view entry) {
        const auto found = root.child(section);
        return found ? found->children(entry) : std::vector<record_reader>{};
    };
    for (const auto& e : each("FXSpots", "FXSpot"))
        import_fx_spot(mapped, e, position++);
    for (const auto& e : each("DefaultCurves", "DefaultCurve"))
        import_default_curve(mapped, e, position++);
    for (const auto& e : each("YieldCurves", "YieldCurve"))
        import_yield_curve(mapped, e, position++);
    for (const auto& e : each("InflationCurves", "InflationCurve"))
        import_inflation_curve(mapped, e, position++);
    for (const auto& e : each("EquityCurves", "EquityCurve"))
        import_equity_curve(mapped, e, position++);
    for (const auto& e : each("Securities", "Security"))
        import_security(mapped, e, position++);
    for (const auto& e : each("CommodityCurves", "CommodityCurve"))
        import_commodity_curve(mapped, e, position++);
    for (const auto& e : each("FXVolatilities", "FXVolatility"))
        import_fx_volatility(mapped, e, position++);
    for (const auto& e : each("YieldVolatilities", "YieldVolatility"))
        import_yield_volatility(mapped, e, position++);
    for (const auto& e : each("BaseCorrelations", "BaseCorrelation"))
        import_base_correlation(mapped, e, position++);
    for (const auto& e : each("Correlations", "Correlation"))
        import_correlation(mapped, e, position++);
    for (const auto& e : each("CDSVolatilities", "CDSVolatility"))
        import_cds_volatility(mapped, e, position++);
    for (const auto& e : each("InflationCapFloorVolatilities", "InflationCapFloorVolatility"))
        import_inflation_cap_floor_volatility(mapped, e, position++);
    for (const auto& e : each("SwaptionVolatilities", "SwaptionVolatility"))
        import_swaption_volatility(mapped, e, position++);
    for (const auto& e : each("CapFloorVolatilities", "CapFloorVolatility"))
        import_cap_floor_volatility(mapped, e, position++);
    for (const auto& e : each("EquityVolatilities", "EquityVolatility"))
        import_equity_volatility(mapped, e, position++);
    for (const auto& e : each("CommodityVolatilities", "CommodityVolatility"))
        import_commodity_volatility(mapped, e, position++);
    for (const auto& e : each("BondFutureVolatilities", "BondFutureVolatility"))
        import_bond_future_volatility(mapped, e, position++);
    for (const auto& e : each("IntradayPowerCurves", "IntradayPowerCurve"))
        import_intraday_power_curve(mapped, e, position++);

    return mapped;
}

curve_configuration_projection::document curve_configuration_projection::reverse(
    const ores::refdata::messaging::curve_configuration_document& v) {
    document out{};
    auto written = record_writer(
        *curve_configuration_projection_xg::curve_configuration_document().root, &out);

    std::map<std::string, const section_access*, std::less<>> by_code;
    for (const auto& s : sections())
        by_code.emplace(std::string(s.code), &s);
    for (const auto& row : v.sections) {
        const auto it = by_code.find(row.section_code);
        if (it == by_code.end())
            throw refusal("unknown section '" + row.section_code + "'");
        written.state(it->second->code);
    }
    if (!v.global_reports.empty()) {
        std::vector<const refdata::domain::curve_global_report*> reports;
        for (const auto& r : v.global_reports)
            reports.push_back(&r);
        std::sort(
            reports.begin(), reports.end(), by_position<refdata::domain::curve_global_report>);
        export_global_report(written.child("ReportConfiguration"), reports);
    }

    std::map<boost::uuids::uuid, const refdata::domain::yield_curve_config*> yield_by_definition;
    for (const auto& y : v.yield_curves)
        yield_by_definition.emplace(y.curve_definition_id, &y);
    std::map<boost::uuids::uuid, const refdata::domain::curve_bootstrap_config*>
        bootstrap_by_definition;
    for (const auto& b : v.bootstrap_configs)
        if (b.default_curve_configuration_id == boost::uuids::uuid{})
            bootstrap_by_definition.emplace(b.curve_definition_id, &b);

    export_context ctx;
    std::vector<refdata::domain::curve_quote> segment_quotes;
    std::vector<refdata::domain::curve_quote> entry_quote_rows;
    std::vector<refdata::domain::curve_quote> configuration_quotes;
    std::vector<refdata::domain::curve_quote> price_segment_quotes;
    std::vector<refdata::domain::curve_quote> basis_quotes;
    for (const auto& q : v.quotes) {
        if (q.default_curve_configuration_id != boost::uuids::uuid{})
            configuration_quotes.push_back(q);
        else if (q.curve_segment_id != boost::uuids::uuid{})
            segment_quotes.push_back(q);
        else if (q.commodity_price_segment_id != boost::uuids::uuid{})
            price_segment_quotes.push_back(q);
        else if (q.quote_list && *q.quote_list == basis_quotes_list)
            basis_quotes.push_back(q);
        else
            entry_quote_rows.push_back(q);
    }
    ctx.price_segment_quotes = group_by(price_segment_quotes, &quote_price_segment);
    ctx.basis_quotes = group_by(basis_quotes, &quote_definition);
    ctx.quotes = group_by(segment_quotes, &segment_of);
    ctx.entry_quotes = group_by(entry_quote_rows, &quote_definition);
    ctx.configuration_quotes = group_by(configuration_quotes, &quote_configuration);
    for (const auto& b : v.bootstrap_configs) {
        if (b.default_curve_configuration_id != boost::uuids::uuid{})
            ctx.configuration_bootstraps.emplace(b.default_curve_configuration_id, &b);
        else
            ctx.entry_bootstraps.emplace(b.curve_definition_id, &b);
    }
    const auto commodity_by_definition = by_definition(v.commodity_curves);
    const auto fx_vol_by_definition = by_definition(v.fx_volatilities);
    const auto yield_vol_by_definition = by_definition(v.yield_volatilities);
    const auto base_correlation_by_definition = by_definition(v.base_correlations);
    const auto correlation_by_definition = by_definition(v.correlations);
    const auto report_by_definition = by_definition(v.report_configurations);
    const auto report_of = [&](const refdata::domain::curve_definition& d)
        -> const refdata::domain::curve_report_configuration* {
        const auto it = report_by_definition.find(d.id);
        return it == report_by_definition.end() ? nullptr : it->second;
    };
    const auto price_segments = group_by(v.commodity_price_segments, &price_segment_definition);
    const auto cds_vol_by_definition = by_definition(v.cds_volatilities);
    const auto cds_terms = group_by(v.cds_volatility_terms, &term_definition);
    const auto volatility_configs = group_by(v.volatility_configs, &volatility_config_definition);
    const auto inflation_vol_by_definition = by_definition(v.inflation_cap_floor_volatilities);
    const auto swaption_vol_by_definition = by_definition(v.swaption_volatilities);
    const auto cap_floor_vol_by_definition = by_definition(v.cap_floor_volatilities);
    const auto equity_vol_by_definition = by_definition(v.equity_volatilities);
    const auto commodity_vol_by_definition = by_definition(v.commodity_volatilities);
    const auto bond_future_vol_by_definition = by_definition(v.bond_future_volatilities);
    const auto smile_by_definition = by_definition(v.parametric_smiles);
    const auto smile_parameters =
        group_by(v.parametric_smile_parameters, &smile_parameter_definition);
    const auto smile_of = [&](const refdata::domain::curve_definition& d) -> smile_export {
        smile_export out;
        const auto it = smile_by_definition.find(d.id);
        if (it == smile_by_definition.end())
            return out;
        out.row = it->second;
        static const std::vector<const refdata::domain::curve_parametric_smile_parameter*> none;
        const auto p = smile_parameters.find(d.id);
        out.parameters = p == smile_parameters.end() ? none : p->second;
        return out;
    };
    const auto default_by_definition = by_definition(v.default_curves);
    const auto configurations = group_by(v.default_curve_configurations, &configuration_definition);
    ctx.curves = group_by(v.segment_curves, &parent_segment);
    const auto segments = group_by(v.segments, &definition_of);
    const auto equity_by_definition = by_definition(v.equity_curves);
    const auto inflation_by_definition = by_definition(v.inflation_curves);
    const auto factors = group_by(v.seasonality_factors, &factor_definition);
    const auto security_by_definition = by_definition(v.securities);
    const auto power_by_definition = by_definition(v.intraday_power_curves);

    std::vector<const refdata::domain::curve_definition*> definitions;
    for (const auto& d : v.definitions)
        definitions.push_back(&d);
    std::sort(
        definitions.begin(), definitions.end(), by_position<refdata::domain::curve_definition>);

    for (const auto* d : definitions) {
        const bool volatility_section = d->section_code == equity_volatilities_section ||
                                        d->section_code == commodity_volatilities_section ||
                                        d->section_code == bond_future_volatilities_section;
        if (volatility_section) {
            for (const auto* q : ctx.quotes_of_entry(d->id))
                if (!q->quote_list || (*q->quote_list != curve_quotes_list &&
                                       *q->quote_list != wrapped_curve_quotes_list))
                    throw refusal("volatility " + d->curve_id +
                                  " holds a quote outside a volatility curve");
        } else if (d->section_code != equity_curves_section &&
                   d->section_code != inflation_curves_section &&
                   d->section_code != commodity_curves_section && ctx.entry_quotes.contains(d->id))
            throw refusal("curve " + d->curve_id + " in section " + d->section_code +
                          " holds a quote directly on its entry");
        static const std::vector<const refdata::domain::curve_volatility_config*> no_configs_of;
        const auto configs_it = volatility_configs.find(d->id);
        const auto& configs_of =
            configs_it == volatility_configs.end() ? no_configs_of : configs_it->second;
        if (d->section_code == equity_volatilities_section) {
            export_equity_volatility(written.child("EquityVolatilities").append("EquityVolatility"),
                                     *d,
                                     settings_of(equity_vol_by_definition, *d),
                                     configs_of,
                                     ctx.quotes_of_entry(d->id),
                                     report_of(*d));
            continue;
        }
        if (d->section_code == commodity_volatilities_section) {
            export_commodity_volatility(
                written.child("CommodityVolatilities").append("CommodityVolatility"),
                *d,
                settings_of(commodity_vol_by_definition, *d),
                configs_of,
                ctx.quotes_of_entry(d->id),
                report_of(*d));
            continue;
        }
        if (d->section_code == bond_future_volatilities_section) {
            export_bond_future_volatility(
                written.child("BondFutureVolatilities").append("BondFutureVolatility"),
                *d,
                settings_of(bond_future_vol_by_definition, *d),
                configs_of,
                ctx.quotes_of_entry(d->id));
            continue;
        }
        if (d->section_code == equity_curves_section) {
            export_equity_curve(written.child("EquityCurves").append("EquityCurve"),
                                *d,
                                settings_of(equity_by_definition, *d),
                                ctx);
            continue;
        }
        if (d->section_code == fx_volatilities_section) {
            export_fx_volatility(written.child("FXVolatilities").append("FXVolatility"),
                                 *d,
                                 settings_of(fx_vol_by_definition, *d),
                                 report_of(*d),
                                 smile_of(*d));
            continue;
        }
        if (d->section_code == yield_volatilities_section) {
            export_yield_volatility(written.child("YieldVolatilities").append("YieldVolatility"),
                                    *d,
                                    settings_of(yield_vol_by_definition, *d),
                                    report_of(*d));
            continue;
        }
        if (d->section_code == base_correlations_section) {
            export_base_correlation(written.child("BaseCorrelations").append("BaseCorrelation"),
                                    *d,
                                    settings_of(base_correlation_by_definition, *d));
            continue;
        }
        if (d->section_code == cds_volatilities_section) {
            static const std::vector<const refdata::domain::cds_volatility_term*> no_terms;
            static const std::vector<const refdata::domain::curve_volatility_config*> no_configs;
            const auto t = cds_terms.find(d->id);
            const auto c = volatility_configs.find(d->id);
            export_cds_volatility(written.child("CDSVolatilities").append("CDSVolatility"),
                                  *d,
                                  settings_of(cds_vol_by_definition, *d),
                                  t == cds_terms.end() ? no_terms : t->second,
                                  c == volatility_configs.end() ? no_configs : c->second);
            continue;
        }
        if (d->section_code == swaption_volatilities_section) {
            export_swaption_volatility(
                written.child("SwaptionVolatilities").append("SwaptionVolatility"),
                *d,
                settings_of(swaption_vol_by_definition, *d),
                report_of(*d),
                smile_of(*d));
            continue;
        }
        if (d->section_code == cap_floor_volatilities_section) {
            const auto b = ctx.entry_bootstraps.find(d->id);
            const auto& settings = settings_of(cap_floor_vol_by_definition, *d);
            const auto proxy = proxy_of(v.cap_floor_proxy_configs, settings.id);
            export_cap_floor_volatility(
                written.child("CapFloorVolatilities").append("CapFloorVolatility"),
                *d,
                settings,
                proxy,
                report_of(*d),
                b == ctx.entry_bootstraps.end() ? nullptr : b->second,
                smile_of(*d));
            continue;
        }
        if (d->section_code == inflation_cap_floor_volatilities_section) {
            const auto b = ctx.entry_bootstraps.find(d->id);
            export_inflation_cap_floor_volatility(written.child("InflationCapFloorVolatilities")
                                                      .append("InflationCapFloorVolatility"),
                                                  *d,
                                                  settings_of(inflation_vol_by_definition, *d),
                                                  report_of(*d),
                                                  b == ctx.entry_bootstraps.end() ? nullptr :
                                                                                    b->second);
            continue;
        }
        if (d->section_code == correlations_section) {
            export_correlation(written.child("Correlations").append("Correlation"),
                               *d,
                               settings_of(correlation_by_definition, *d));
            continue;
        }
        if (d->section_code == commodity_curves_section) {
            static const std::vector<const refdata::domain::commodity_price_segment*> no_segments;
            const auto g = price_segments.find(d->id);
            export_commodity_curve(written.child("CommodityCurves").append("CommodityCurve"),
                                   *d,
                                   settings_of(commodity_by_definition, *d),
                                   g == price_segments.end() ? no_segments : g->second,
                                   ctx);
            continue;
        }
        if (d->section_code == default_curves_section) {
            const auto k = configurations.find(d->id);
            if (k == configurations.end())
                throw refusal("default curve " + d->curve_id + " has no configuration");
            export_default_curve(written.child("DefaultCurves").append("DefaultCurve"),
                                 *d,
                                 settings_of(default_by_definition, *d),
                                 k->second,
                                 ctx);
            continue;
        }
        if (d->section_code == inflation_curves_section) {
            static const std::vector<const refdata::domain::inflation_seasonality_factor*> none;
            const auto f = factors.find(d->id);
            export_inflation_curve(written.child("InflationCurves").append("InflationCurve"),
                                   *d,
                                   settings_of(inflation_by_definition, *d),
                                   f == factors.end() ? none : f->second,
                                   ctx);
            continue;
        }
        if (d->section_code == securities_section) {
            export_security(written.child("Securities").append("Security"),
                            *d,
                            settings_of(security_by_definition, *d));
            continue;
        }
        if (d->section_code == fx_spots_section) {
            export_fx_spot(written.child("FXSpots").append("FXSpot"), *d);
            continue;
        }
        if (d->section_code == intraday_power_curves_section) {
            export_intraday_power_curve(
                written.child("IntradayPowerCurves").append("IntradayPowerCurve"),
                *d,
                settings_of(power_by_definition, *d));
            continue;
        }
        if (d->section_code != yield_curves_section)
            throw refusal("curve " + d->curve_id + " is in section " + d->section_code +
                          ", which is not modelled yet");
        const auto y = yield_by_definition.find(d->id);
        if (y == yield_by_definition.end())
            throw refusal("yield curve " + d->curve_id + " has no settings row");
        const auto b = bootstrap_by_definition.find(d->id);
        const auto g = segments.find(d->id);
        static const std::vector<const refdata::domain::curve_segment*> none;
        export_yield_curve(written.child("YieldCurves").append("YieldCurve"),
                           *d,
                           *y->second,
                           b == bootstrap_by_definition.end() ? nullptr : b->second,
                           g == segments.end() ? none : g->second,
                           ctx);
    }

    return out;
}

curve_configuration_projection::document
curve_configuration_projection::read(std::string_view text, std::string_view origin) {
    document record;
    const auto bound = xml::binder::bind(
        curve_configuration_projection_xg::curve_configuration_document(), text, &record);
    if (!bound) {
        const auto where =
            origin.empty() ? std::string("curve configuration") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string curve_configuration_projection::write(const document& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(
        curve_configuration_projection_xg::curve_configuration_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

}
