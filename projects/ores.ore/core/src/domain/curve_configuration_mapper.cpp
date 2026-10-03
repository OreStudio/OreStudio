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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.ore.core/domain/curve_configuration_mapper.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <functional>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

constexpr std::string_view yield_curves_section = "YieldCurves";
constexpr std::string_view equity_curves_section = "EquityCurves";
constexpr std::string_view inflation_curves_section = "InflationCurves";
constexpr std::string_view default_curves_section = "DefaultCurves";
constexpr std::string_view commodity_curves_section = "CommodityCurves";

constexpr std::string_view basis_quotes_list = "BasisQuotes";
constexpr std::string_view off_peak_quotes_list = "OffPeakQuotes";
constexpr std::string_view peak_quotes_list = "PeakQuotes";
constexpr std::string_view securities_section = "Securities";
constexpr std::string_view fx_spots_section = "FXSpots";
constexpr std::string_view intraday_power_curves_section = "IntradayPowerCurves";

// The sections whose entries the mapper holds. A section outside this set is
// recorded only when it is empty.
bool is_modelled(std::string_view section) {
    return section == yield_curves_section || section == equity_curves_section ||
           section == inflation_curves_section || section == default_curves_section ||
           section == commodity_curves_section ||
           section == securities_section || section == fx_spots_section ||
           section == intraday_power_curves_section;
}

boost::uuids::uuid new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

template <typename T>
void set_audit(T& r) {
    r.modified_by = std::string(audit_modified_by);
    r.performed_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);
}

std::runtime_error refusal(const std::string& what) {
    return std::runtime_error("curve_configuration_mapper: " + what);
}

template <typename T>
std::string text(const T& v) {
    return static_cast<const std::string&>(v);
}

template <typename T>
void assign_text(T& target, const std::string& value) {
    static_cast<std::string&>(target) = value;
}

template <typename T>
std::optional<std::string> optional_text(const xsd::optional<T>& v) {
    if (!v)
        return std::nullopt;
    return text(*v);
}

template <typename T>
void assign_optional_text(xsd::optional<T>& target, const std::optional<std::string>& value) {
    if (!value)
        return;
    T t;
    assign_text(t, *value);
    target = std::move(t);
}

// The binding writes each enumeration's values as its schema spells them but
// offers no way back, so the value is found by walking the enumeration until
// its to_string runs out.
template <typename E>
E enum_from_text(const std::string& value, std::string_view what) {
    for (std::size_t i = 0;; ++i) {
        std::string candidate;
        try {
            candidate = to_string(static_cast<E>(i));
        } catch (const std::invalid_argument&) {
            break;
        }
        if (candidate == value)
            return static_cast<E>(i);
    }
    throw std::runtime_error("curve_configuration_mapper: '" + value + "' is not a valid " +
                             std::string(what));
}

template <typename E>
std::optional<std::string> optional_enum_text(const xsd::optional<E>& v) {
    if (!v)
        return std::nullopt;
    return to_string(*v);
}

template <typename E>
void assign_optional_enum(xsd::optional<E>& target,
                          const std::optional<std::string>& value,
                          std::string_view what) {
    if (value)
        target = enum_from_text<E>(*value, what);
}

std::optional<int> optional_int(const xsd::optional<uint64_t>& v) {
    if (!v)
        return std::nullopt;
    return static_cast<int>(*v);
}

void assign_optional_int(xsd::optional<uint64_t>& target, const std::optional<int>& value) {
    if (value)
        target = static_cast<uint64_t>(*value);
}

template <typename T>
std::optional<double> optional_double(const xsd::optional<T>& v) {
    if (!v)
        return std::nullopt;
    return static_cast<double>(*v);
}

template <typename T>
void assign_optional_double(xsd::optional<T>& target, const std::optional<double>& value) {
    if (value)
        target = static_cast<T>(*value);
}

std::optional<bool> optional_bool(const xsd::optional<bool>& v) {
    if (!v)
        return std::nullopt;
    return *v;
}

void assign_optional_bool(xsd::optional<bool>& target, const std::optional<bool>& value) {
    if (value)
        target = *value;
}

// One entry per section element of the document, in the order the schema
// declares them. A section is recorded as present whether or not it holds
// entries, and the entry count is what decides whether the mapper can take it.
struct section_access {
    std::string_view code;
    std::function<bool(const curveconfiguration&)> present;
    std::function<std::size_t(const curveconfiguration&)> entries;
    std::function<void(curveconfiguration&)> emplace;
};

template <typename Section, typename Entry>
section_access make_section(std::string_view code,
                            xsd::optional<Section> curveconfiguration::* member,
                            xsd::vector<Entry> Section::* list) {
    return {code,
            [member](const curveconfiguration& d) { return static_cast<bool>(d.*member); },
            [member, list](const curveconfiguration& d) -> std::size_t {
                return d.*member ? ((*(d.*member)).*list).size() : 0;
            },
            [member](curveconfiguration& d) {
                if (!(d.*member))
                    d.*member = Section{};
            }};
}

const std::vector<section_access>& sections() {
    static const std::vector<section_access> table = {
        make_section(fx_spots_section, &curveconfiguration::FXSpots, &fxSpots::FXSpot),
        make_section(
            "FXVolatilities", &curveconfiguration::FXVolatilities, &fxVolatilities::FXVolatility),
        make_section("SwaptionVolatilities",
                     &curveconfiguration::SwaptionVolatilities,
                     &swaptionVolatilities::SwaptionVolatility),
        make_section("YieldVolatilities",
                     &curveconfiguration::YieldVolatilities,
                     &yieldVolatilities::YieldVolatility),
        make_section("CapFloorVolatilities",
                     &curveconfiguration::CapFloorVolatilities,
                     &capFloorVolatilities::CapFloorVolatility),
        make_section(
            "CDSVolatilities", &curveconfiguration::CDSVolatilities, &cdsVolatilities::CDSVolatility),
        make_section(
            default_curves_section, &curveconfiguration::DefaultCurves, &defaultCurves::DefaultCurve),
        make_section(yield_curves_section, &curveconfiguration::YieldCurves, &yieldCurves::YieldCurve),
        make_section(inflation_curves_section,
                     &curveconfiguration::InflationCurves,
                     &inflationCurves::InflationCurve),
        make_section("InflationCapFloorVolatilities",
                     &curveconfiguration::InflationCapFloorVolatilities,
                     &inflationCapFloorVolatlities::InflationCapFloorVolatility),
        make_section(equity_curves_section, &curveconfiguration::EquityCurves, &equityCurves::EquityCurve),
        make_section("EquityVolatilities",
                     &curveconfiguration::EquityVolatilities,
                     &equityVolatilities::EquityVolatility),
        make_section(securities_section, &curveconfiguration::Securities, &securities::Security),
        make_section("BaseCorrelations",
                     &curveconfiguration::BaseCorrelations,
                     &baseCorrelations::BaseCorrelation),
        make_section(commodity_curves_section,
                     &curveconfiguration::CommodityCurves,
                     &simCommodityCurves::CommodityCurve),
        make_section("CommodityVolatilities",
                     &curveconfiguration::CommodityVolatilities,
                     &commodityVolatilities::CommodityVolatility),
        make_section("Correlations", &curveconfiguration::Correlations, &correlations::Correlation),
        make_section("BondFutureVolatilities",
                     &curveconfiguration::BondFutureVolatilities,
                     &bondFutureVolatilities::BondFutureVolatility),
        make_section(intraday_power_curves_section,
                     &curveconfiguration::IntradayPowerCurves,
                     &intradayPowerCurves::IntradayPowerCurve),
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

// The state one document's mapping accumulates, so each segment element's
// reader adds its rows without threading the vectors through every call.
struct import_context {
    mapped_curve_configuration& out;
    boost::uuids::uuid definition_id;

    refdata::domain::curve_segment& segment(const std::string& type, int position) {
        refdata::domain::curve_segment s;
        s.id = new_uuid();
        s.curve_definition_id = definition_id;
        s.segment_type = type;
        s.position = position;
        set_audit(s);
        out.segments.push_back(std::move(s));
        return out.segments.back();
    }

    void quotes(const boost::uuids::uuid& segment_id,
                const quoteType& v,
                const boost::uuids::uuid& configuration_id = boost::uuids::uuid{}) {
        int position = 0;
        for (const auto& q : v.Quote) {
            refdata::domain::curve_quote r;
            r.id = new_uuid();
            r.curve_definition_id = definition_id;
            r.curve_segment_id = segment_id;
            r.default_curve_configuration_id = configuration_id;
            r.quote_text = text(q);
            if (q.optional)
                r.optional_flag = std::string(*q.optional);
            r.position = position++;
            set_audit(r);
            out.quotes.push_back(std::move(r));
        }
    }

    void composite_quotes(const boost::uuids::uuid& segment_id, const compositeQuoteType& v) {
        int position = 0;
        for (const auto& q : v.CompositeQuote) {
            refdata::domain::curve_quote r;
            r.id = new_uuid();
            r.curve_definition_id = definition_id;
            r.curve_segment_id = segment_id;
            r.rate_quote = text(q.RateQuote);
            r.spread_quote = text(q.SpreadQuote);
            r.position = position++;
            set_audit(r);
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
        r.id = new_uuid();
        r.curve_segment_id = segment_id;
        r.role = std::string(role);
        r.curve = curve_id;
        r.index_name = std::move(index_name);
        r.weight = weight;
        r.position = position;
        set_audit(r);
        out.segment_curves.push_back(std::move(r));
    }
};

template <typename Segment>
void common_settings(refdata::domain::curve_segment& s, const Segment& v) {
    s.pillar_choice = optional_text(v.PillarChoice);
    s.priority = optional_int(v.Priority);
    s.min_distance = optional_int(v.MinDistance);
}

template <typename Segment>
void restore_common_settings(Segment& r, const refdata::domain::curve_segment& s) {
    assign_optional_text(r.PillarChoice, s.pillar_choice);
    assign_optional_int(r.Priority, s.priority);
    assign_optional_int(r.MinDistance, s.min_distance);
}

void import_segments(import_context& ctx, const segmentsType& v) {
    int position = 0;
    for (const auto& g : v.Direct) {
        auto& s = ctx.segment(to_string(g.Type), position++);
        s.conventions = optional_text(g.Conventions);
        common_settings(s, g);
        ctx.quotes(s.id, g.Quotes);
    }
    position = 0;
    for (const auto& g : v.Simple) {
        auto& s = ctx.segment(to_string(g.Type), position++);
        s.conventions = text(g.Conventions);
        common_settings(s, g);
        s.projection_curve = optional_text(g.ProjectionCurve);
        ctx.quotes(s.id, g.Quotes);
    }
    position = 0;
    for (const auto& g : v.AverageOIS) {
        auto& s = ctx.segment(text(g.Type), position++);
        s.conventions = text(g.Conventions);
        common_settings(s, g);
        s.projection_curve = optional_text(g.ProjectionCurve);
        ctx.composite_quotes(s.id, g.Quotes);
    }
    position = 0;
    for (const auto& g : v.TenorBasis) {
        auto& s = ctx.segment(to_string(g.Type), position++);
        s.conventions = text(g.Conventions);
        common_settings(s, g);
        s.projection_curve_pay = optional_text(g.ProjectionCurvePay);
        s.projection_curve_receive = optional_text(g.ProjectionCurveReceive);
        s.projection_curve_long = optional_text(g.ProjectionCurveLong);
        s.projection_curve_short = optional_text(g.ProjectionCurveShort);
        ctx.quotes(s.id, g.Quotes);
    }
    position = 0;
    for (const auto& g : v.CrossCurrency) {
        auto& s = ctx.segment(to_string(g.Type), position++);
        s.conventions = text(g.Conventions);
        common_settings(s, g);
        s.discount_curve = text(g.DiscountCurve);
        s.spot_rate = text(g.SpotRate);
        s.projection_curve_domestic = optional_text(g.ProjectionCurveDomestic);
        s.projection_curve_foreign = optional_text(g.ProjectionCurveForeign);
        ctx.quotes(s.id, g.Quotes);
    }
    position = 0;
    for (const auto& g : v.ZeroSpread) {
        auto& s = ctx.segment(to_string(g.Type), position++);
        s.conventions = text(g.Conventions);
        common_settings(s, g);
        s.reference_curve = text(g.ReferenceCurve);
        ctx.quotes(s.id, g.Quotes);
    }
    position = 0;
    for (const auto& g : v.DiscountRatio) {
        auto& s = ctx.segment(to_string(g.Type), position++);
        s.conventions = optional_text(g.Conventions);
        common_settings(s, g);
        s.base_curve = text(g.BaseCurve);
        s.base_curve_currency = g.BaseCurve.currency;
        s.numerator_curve = text(g.NumeratorCurve);
        s.numerator_curve_currency = g.NumeratorCurve.currency;
        s.denominator_curve = text(g.DenominatorCurve);
        s.denominator_curve_currency = g.DenominatorCurve.currency;
    }
    position = 0;
    for (const auto& g : v.FittedBond) {
        auto& s = ctx.segment(text(g.Type), position++);
        common_settings(s, g);
        s.extrapolate_flat = optional_bool(g.ExtrapolateFlat);
        ctx.quotes(s.id, g.Quotes);
        const auto id = s.id;
        int item = 0;
        if (g.IndexCurves)
            for (const auto& c : g.IndexCurves->IndexCurve)
                ctx.curve(id, "IndexCurve", text(c), optional_text(c.Index), std::nullopt, item++);
        item = 0;
        if (g.IborIndexCurves)
            for (const auto& c : g.IborIndexCurves->IborIndexCurve)
                ctx.curve(
                    id, "IborIndexCurve", text(c), optional_text(c.iborIndex), std::nullopt, item++);
        item = 0;
        if (g.InflationIndexCurves)
            for (const auto& c : g.InflationIndexCurves->InflationIndexCurve)
                ctx.curve(id,
                          "InflationIndexCurve",
                          text(c),
                          optional_text(c.inflationIndex),
                          std::nullopt,
                          item++);
    }
    position = 0;
    for (const auto& g : v.BondYieldShifted) {
        auto& s = ctx.segment(text(g.Type), position++);
        s.conventions = text(g.Conventions);
        s.reference_curve = text(g.ReferenceCurve);
        s.extrapolate_flat = optional_bool(g.ExtrapolateFlat);
        ctx.quotes(s.id, g.Quotes);
        const auto id = s.id;
        int item = 0;
        if (g.IndexCurves)
            for (const auto& c : g.IndexCurves->IndexCurve)
                ctx.curve(id, "IndexCurve", text(c), optional_text(c.Index), std::nullopt, item++);
        item = 0;
        if (g.IborIndexCurves)
            for (const auto& c : g.IborIndexCurves->IborIndexCurve)
                ctx.curve(
                    id, "IborIndexCurve", text(c), optional_text(c.iborIndex), std::nullopt, item++);
    }
    position = 0;
    for (const auto& g : v.WeightedAverage) {
        auto& s = ctx.segment(text(g.Type), position++);
        s.reference_curve = text(g.ReferenceCurve1);
        s.reference_curve_2 = text(g.ReferenceCurve2);
        s.weight_1 = static_cast<double>(g.Weight1);
        s.weight_2 = static_cast<double>(g.Weight2);
    }
    position = 0;
    for (const auto& g : v.YieldPlusDefault) {
        auto& s = ctx.segment(text(g.Type), position++);
        s.reference_curve = text(g.ReferenceCurve);
        ctx.curve(s.id,
                  "DefaultCurve",
                  text(g.DefaultCurves.DefaultCurve),
                  std::nullopt,
                  static_cast<double>(g.Weights.Weight),
                  0);
    }
    position = 0;
    for (const auto& g : v.IborFallback) {
        auto& s = ctx.segment(text(g.Type), position++);
        common_settings(s, g);
        s.ibor_index = text(g.IborIndex);
        s.rfr_curve = text(g.RfrCurve);
        s.rfr_index = optional_text(g.RfrIndex);
        s.spread = optional_double(g.Spread);
    }
}

refdata::domain::curve_definition& add_definition(mapped_curve_configuration& out,
                                                  std::string_view section,
                                                  const std::string& curve_id,
                                                  const std::string& description,
                                                  int position) {
    refdata::domain::curve_definition d;
    d.id = new_uuid();
    d.curve_configuration_id = out.config.id;
    d.section_code = std::string(section);
    d.curve_id = curve_id;
    d.description = description;
    d.position = position;
    set_audit(d);
    out.definitions.push_back(std::move(d));
    return out.definitions.back();
}

void import_bootstrap(mapped_curve_configuration& out,
                      const boost::uuids::uuid& definition_id,
                      const boost::uuids::uuid& configuration_id,
                      const bootstrapConfigType& b) {
    refdata::domain::curve_bootstrap_config c;
    c.id = new_uuid();
    c.curve_definition_id = definition_id;
    c.default_curve_configuration_id = configuration_id;
    c.accuracy = optional_double(b.Accuracy);
    c.global_accuracy = optional_double(b.GlobalAccuracy);
    c.dont_throw = optional_bool(b.DontThrow);
    c.max_attempts = optional_int(b.MaxAttempts);
    c.max_factor = optional_double(b.MaxFactor);
    c.min_factor = optional_double(b.MinFactor);
    c.dont_throw_steps = optional_int(b.DontThrowSteps);
    c.global = optional_bool(b.Global);
    c.smoothness_lambda = optional_double(b.SmoothnessLambda);
    set_audit(c);
    out.bootstrap_configs.push_back(std::move(c));
}

bootstrapConfigType export_bootstrap(const refdata::domain::curve_bootstrap_config& c) {
    bootstrapConfigType b;
    assign_optional_double(b.Accuracy, c.accuracy);
    assign_optional_double(b.GlobalAccuracy, c.global_accuracy);
    assign_optional_bool(b.DontThrow, c.dont_throw);
    assign_optional_int(b.MaxAttempts, c.max_attempts);
    assign_optional_double(b.MaxFactor, c.max_factor);
    assign_optional_double(b.MinFactor, c.min_factor);
    assign_optional_int(b.DontThrowSteps, c.dont_throw_steps);
    assign_optional_bool(b.Global, c.global);
    assign_optional_double(b.SmoothnessLambda, c.smoothness_lambda);
    return b;
}

void import_yield_curve(mapped_curve_configuration& out, const yieldCurve& v, int position) {
    const auto d = add_definition(
        out, yield_curves_section, text(v.CurveId), text(v.CurveDescription), position);

    refdata::domain::yield_curve y;
    y.id = new_uuid();
    y.curve_definition_id = d.id;
    y.currency = to_string(v.Currency);
    y.discount_curve = text(v.DiscountCurve);
    y.interpolation_variable = optional_enum_text(v.InterpolationVariable);
    y.interpolation_method = optional_enum_text(v.InterpolationMethod);
    y.mixed_interpolation_cutoff = optional_int(v.MixedInterpolationCutoff);
    y.day_counter = optional_enum_text(v.YieldCurveDayCounter);
    y.tolerance = optional_double(v.Tolerance);
    y.extrapolation = optional_enum_text(v.Extrapolation);
    y.extrapolation_method = optional_enum_text(v.ExtrapolationMethod);
    y.exclude_t0_from_interpolation = optional_enum_text(v.ExcludeT0FromInterpolation);
    y.has_report = static_cast<bool>(v.Report);
    if (v.Report)
        y.report_pillar_dates = optional_text(v.Report->PillarDates);
    set_audit(y);
    out.yield_curves.push_back(std::move(y));

    if (v.BootstrapConfig)
        import_bootstrap(out, d.id, boost::uuids::uuid{}, *v.BootstrapConfig);

    import_context ctx{out, d.id};
    import_segments(ctx, v.Segments);
}

void import_equity_curve(mapped_curve_configuration& out, const equityCurve& v, int position) {
    const auto d = add_definition(
        out, equity_curves_section, text(v.CurveId), text(v.CurveDescription), position);

    refdata::domain::equity_curve e;
    e.id = new_uuid();
    e.curve_definition_id = d.id;
    e.currency = text(v.Currency);
    e.calendar = optional_text(v.Calendar);
    e.forecasting_curve = text(v.ForecastingCurve);
    e.equity_type = to_string(v.Type);
    e.exercise_style = optional_enum_text(v.ExerciseStyle);
    e.spot_quote = text(v.SpotQuote);
    e.has_quotes = static_cast<bool>(v.Quotes);
    e.day_counter = optional_enum_text(v.DayCounter);
    e.has_dividend_interpolation = static_cast<bool>(v.DividendInterpolation);
    if (v.DividendInterpolation) {
        e.dividend_interpolation_variable =
            optional_enum_text(v.DividendInterpolation->InterpolationVariable);
        e.dividend_interpolation_method =
            optional_enum_text(v.DividendInterpolation->InterpolationMethod);
    }
    e.dividend_extrapolation = optional_enum_text(v.DividendExtrapolation);
    e.extrapolation = optional_enum_text(v.Extrapolation);
    set_audit(e);
    out.equity_curves.push_back(std::move(e));

    if (v.Quotes) {
        import_context ctx{out, d.id};
        ctx.quotes(boost::uuids::uuid{}, *v.Quotes);
    }
}

void import_inflation_curve(mapped_curve_configuration& out,
                            const inflationCurve& v,
                            int position) {
    const auto d = add_definition(
        out, inflation_curves_section, text(v.CurveId), text(v.CurveDescription), position);
    if (v.Segments)
        throw refusal("inflation curve " + d.curve_id +
                      " writes Segments, which are not modelled yet");

    refdata::domain::inflation_curve r;
    r.id = new_uuid();
    r.curve_definition_id = d.id;
    r.nominal_term_structure = text(v.NominalTermStructure);
    r.inflation_type = to_string(v.Type);
    r.conventions = optional_text(v.Conventions);
    r.has_quotes = static_cast<bool>(v.Quotes);
    r.extrapolation = optional_enum_text(v.Extrapolation);
    r.calendar = text(v.Calendar);
    r.day_counter = optional_enum_text(v.DayCounter);
    r.lag = text(v.Lag);
    r.frequency = to_string(v.Frequency);
    r.base_rate = optional_text(v.BaseRate);
    r.tolerance = optional_double(v.Tolerance);
    r.has_seasonality = static_cast<bool>(v.Seasonality);
    if (v.Seasonality) {
        r.seasonality_base_date = text(v.Seasonality->BaseDate);
        r.seasonality_frequency = to_string(v.Seasonality->Frequency);
        int factor_position = 0;
        for (const auto& f : v.Seasonality->Factors.Factor) {
            refdata::domain::inflation_seasonality_factor sf;
            sf.id = new_uuid();
            sf.curve_definition_id = d.id;
            sf.factor = text(f);
            sf.position = factor_position++;
            set_audit(sf);
            out.seasonality_factors.push_back(std::move(sf));
        }
    }
    r.use_last_fixing_date = optional_enum_text(v.UseLastFixingDate);
    r.interpolation_variable = optional_text(v.InterpolationVariable);
    r.interpolation_method = optional_text(v.InterpolationMethod);
    set_audit(r);
    out.inflation_curves.push_back(std::move(r));

    if (v.Quotes) {
        import_context ctx{out, d.id};
        ctx.quotes(boost::uuids::uuid{}, *v.Quotes);
    }
}

// The settings a default curve writes both inline and in a listed
// configuration, under the same element names.
template <typename Source>
void import_configuration_settings(refdata::domain::default_curve_configuration& r,
                                   const Source& v) {
    r.discount_curve = optional_text(v.DiscountCurve);
    r.recovery_rate = optional_text(v.RecoveryRate);
    r.start_date = optional_text(v.StartDate);
    r.has_quotes = static_cast<bool>(v.Quotes);
    r.benchmark_curve = optional_text(v.BenchmarkCurve);
    r.source_curve = optional_text(v.SourceCurve);
    r.pillars = optional_text(v.Pillars);
    if (v.SpotLag)
        r.spot_lag = static_cast<int>(*v.SpotLag);
    r.calendar = optional_text(v.Calendar);
    r.conventions = optional_text(v.Conventions);
    r.extrapolation = optional_enum_text(v.Extrapolation);
    r.running_spread = optional_double(v.RunningSpread);
    r.index_term = optional_text(v.IndexTerm);
    r.imply_default_from_market = optional_enum_text(v.ImplyDefaultFromMarket);
    r.allow_negative_rates = optional_enum_text(v.AllowNegativeRates);
    r.price_is_upfront = optional_enum_text(v.PriceIsUpfront);
    r.initial_state = optional_text(v.InitialState);
    r.states = optional_text(v.States);
}

template <typename Source>
void import_configuration_lists(mapped_curve_configuration& out,
                                const refdata::domain::curve_definition& d,
                                const refdata::domain::default_curve_configuration& r,
                                const Source& v) {
    if (v.SourceCurves || v.SwitchDates)
        throw refusal("default curve " + d.curve_id +
                      " writes SourceCurves or SwitchDates, which are not modelled yet");
    if (v.Quotes) {
        import_context ctx{out, d.id};
        ctx.quotes(boost::uuids::uuid{}, *v.Quotes, r.id);
    }
    if (v.BootstrapConfig)
        import_bootstrap(out, d.id, r.id, *v.BootstrapConfig);
}

void import_default_curve(mapped_curve_configuration& out, const defaultCurve& v, int position) {
    const auto d = add_definition(
        out, default_curves_section, text(v.CurveId), text(v.CurveDescription), position);

    refdata::domain::default_curve c;
    c.id = new_uuid();
    c.curve_definition_id = d.id;
    c.currency = to_string(v.Currency);
    set_audit(c);
    out.default_curves.push_back(std::move(c));

    const auto add = [&](int at) -> refdata::domain::default_curve_configuration {
        refdata::domain::default_curve_configuration r;
        r.id = new_uuid();
        r.curve_definition_id = d.id;
        r.position = at;
        set_audit(r);
        return r;
    };

    if (v.Configurations) {
        int at = 0;
        for (const auto& k : v.Configurations->Configuration) {
            auto r = add(at++);
            r.is_inline = false;
            r.priority = optional_int(k.priority);
            r.default_curve_type = to_string(k.Type);
            r.day_counter = to_string(k.DayCounter);
            r.reinterpreted_yield_curve = optional_text(k.ReinterpretedYieldCurve);
            import_configuration_settings(r, k);
            import_configuration_lists(out, d, r, k);
            out.default_curve_configurations.push_back(std::move(r));
        }
        return;
    }

    auto r = add(0);
    r.is_inline = true;
    r.default_curve_type = optional_enum_text(v.Type);
    r.day_counter = optional_enum_text(v.DayCounter);
    import_configuration_settings(r, v);
    import_configuration_lists(out, d, r, v);
    out.default_curve_configurations.push_back(std::move(r));
}

// A commodity quote's owner is the entry, one of its price segments, or one of
// the named lists the entry or a segment writes beside its Quotes.
void add_commodity_quotes(mapped_curve_configuration& out,
                          const boost::uuids::uuid& definition_id,
                          const boost::uuids::uuid& price_segment_id,
                          std::optional<std::string_view> list,
                          const quoteType& v) {
    int position = 0;
    for (const auto& q : v.Quote) {
        refdata::domain::curve_quote r;
        r.id = new_uuid();
        r.curve_definition_id = definition_id;
        r.commodity_price_segment_id = price_segment_id;
        if (list)
            r.quote_list = std::string(*list);
        r.quote_text = text(q);
        if (q.optional)
            r.optional_flag = std::string(*q.optional);
        r.position = position++;
        set_audit(r);
        out.quotes.push_back(std::move(r));
    }
}

void import_commodity_curve(mapped_curve_configuration& out,
                            const simCommodityCurve& v,
                            int position) {
    const auto d = add_definition(
        out, commodity_curves_section, text(v.CurveId), text(v.CurveDescription), position);
    const boost::uuids::uuid none{};

    refdata::domain::commodity_curve r;
    r.id = new_uuid();
    r.curve_definition_id = d.id;
    r.currency = to_string(v.Currency);
    r.base_price_curve = optional_text(v.BasePriceCurve);
    r.base_yield_curve = optional_text(v.BaseYieldCurve);
    r.yield_curve = optional_text(v.YieldCurve);
    r.spot_quote = optional_text(v.SpotQuote);
    r.has_quotes = static_cast<bool>(v.Quotes);
    r.day_counter = optional_enum_text(v.DayCounter);
    r.interpolation_method = optional_enum_text(v.InterpolationMethod);
    r.conventions = optional_text(v.Conventions);
    r.extrapolation = optional_enum_text(v.Extrapolation);
    r.has_basis_configuration = static_cast<bool>(v.BasisConfiguration);
    if (v.BasisConfiguration) {
        const auto& b = *v.BasisConfiguration;
        r.basis_base_price_curve = text(b.BasePriceCurve);
        r.basis_base_price_conventions = text(b.BasePriceConventions);
        r.basis_conventions = text(b.BasisConventions);
        r.basis_day_counter = optional_enum_text(b.DayCounter);
        r.basis_interpolation_method = optional_enum_text(b.InterpolationMethod);
        r.basis_add_basis = optional_enum_text(b.AddBasis);
        r.basis_month_offset = optional_int(b.MonthOffset);
        r.basis_average_base = optional_enum_text(b.AverageBase);
        r.basis_price_as_historical_fixing = optional_enum_text(b.PriceAsHistoricalFixing);
        add_commodity_quotes(out, d.id, none, basis_quotes_list, b.BasisQuotes);
    }
    r.has_price_segments = static_cast<bool>(v.PriceSegments);
    if (v.PriceSegments) {
        int at = 0;
        for (const auto& g : v.PriceSegments->PriceSegment) {
            refdata::domain::commodity_price_segment p;
            p.id = new_uuid();
            p.curve_definition_id = d.id;
            p.segment_type = to_string(g.Type);
            p.priority = optional_int(g.Priority);
            p.conventions = text(g.Conventions);
            p.has_quotes = static_cast<bool>(g.Quotes);
            p.peak_price_curve_id = optional_text(g.PeakPriceCurveId);
            p.peak_price_calendar = optional_text(g.PeakPriceCalendar);
            p.has_off_peak_daily = static_cast<bool>(g.OffPeakDaily);
            p.position = at++;
            set_audit(p);
            if (g.Quotes)
                add_commodity_quotes(out, d.id, p.id, std::nullopt, *g.Quotes);
            if (g.OffPeakDaily) {
                add_commodity_quotes(
                    out, d.id, p.id, off_peak_quotes_list, g.OffPeakDaily->OffPeakQuotes);
                add_commodity_quotes(out, d.id, p.id, peak_quotes_list, g.OffPeakDaily->PeakQuotes);
            }
            out.commodity_price_segments.push_back(std::move(p));
        }
    }
    set_audit(r);
    out.commodity_curves.push_back(std::move(r));

    if (v.Quotes)
        add_commodity_quotes(out, d.id, none, std::nullopt, *v.Quotes);
    if (v.BootstrapConfig)
        import_bootstrap(out, d.id, none, *v.BootstrapConfig);
}

void import_security(mapped_curve_configuration& out, const security& v, int position) {
    const auto d = add_definition(
        out, securities_section, text(v.CurveId), text(v.CurveDescription), position);

    refdata::domain::curve_security r;
    r.id = new_uuid();
    r.curve_definition_id = d.id;
    r.spread_quote = optional_text(v.SpreadQuote);
    r.recovery_rate_quote = optional_text(v.RecoveryRateQuote);
    r.cpr_quote = optional_text(v.CPRQuote);
    r.price_quote = optional_text(v.PriceQuote);
    r.conversion_factor = optional_text(v.ConversionFactor);
    set_audit(r);
    out.securities.push_back(std::move(r));
}

void import_fx_spot(mapped_curve_configuration& out, const fxSpot& v, int position) {
    add_definition(out, fx_spots_section, text(v.CurveId), text(v.CurveDescription), position);
}

void import_intraday_power_curve(mapped_curve_configuration& out,
                                 const intradayPowerCurve& v,
                                 int position) {
    const auto d = add_definition(
        out, intraday_power_curves_section, text(v.CurveId), text(v.CurveDescription), position);

    refdata::domain::intraday_power_curve r;
    r.id = new_uuid();
    r.curve_definition_id = d.id;
    r.currency = to_string(v.Currency);
    r.daily_average_price_curve = text(v.DailyAveragePriceCurve);
    r.shape_quote_name = text(v.ShapeQuoteName);
    r.convention = text(v.Convention);
    set_audit(r);
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

boost::uuids::uuid configuration_definition(
    const refdata::domain::default_curve_configuration& c) {
    return c.curve_definition_id;
}

boost::uuids::uuid factor_definition(const refdata::domain::inflation_seasonality_factor& f) {
    return f.curve_definition_id;
}

boost::uuids::uuid definition_of(const refdata::domain::curve_segment& s) {
    return s.curve_definition_id;
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

    const std::vector<const refdata::domain::curve_quote*>& quotes_of_entry(
        const boost::uuids::uuid& definition_id) const {
        static const std::vector<const refdata::domain::curve_quote*> none;
        const auto it = entry_quotes.find(definition_id);
        return it == entry_quotes.end() ? none : it->second;
    }
    std::map<boost::uuids::uuid, std::vector<const refdata::domain::curve_segment_curve*>> curves;

    const std::vector<const refdata::domain::curve_quote*>& quotes_of(
        const boost::uuids::uuid& id) const {
        static const std::vector<const refdata::domain::curve_quote*> none;
        const auto it = quotes.find(id);
        return it == quotes.end() ? none : it->second;
    }

    const std::vector<const refdata::domain::curve_segment_curve*>& curves_of(
        const boost::uuids::uuid& id) const {
        static const std::vector<const refdata::domain::curve_segment_curve*> none;
        const auto it = curves.find(id);
        return it == curves.end() ? none : it->second;
    }

    quoteType plain_quotes(const refdata::domain::curve_segment& s) const {
        quoteType r;
        for (const auto* q : quotes_of(s.id)) {
            if (!q->quote_text)
                throw refusal("segment " + boost::uuids::to_string(s.id) +
                              " holds a composite quote but is not an average OIS segment");
            quoteType_Quote_t item;
            assign_text(item, *q->quote_text);
            if (q->optional_flag)
                item.optional = *q->optional_flag;
            r.Quote.push_back(std::move(item));
        }
        return r;
    }

    compositeQuoteType composite_quotes(const refdata::domain::curve_segment& s) const {
        compositeQuoteType r;
        for (const auto* q : quotes_of(s.id)) {
            if (!q->rate_quote || !q->spread_quote)
                throw refusal("average OIS segment " + boost::uuids::to_string(s.id) +
                              " holds a plain quote");
            compositeQuoteType_CompositeQuote_t item;
            assign_text(item.RateQuote, *q->rate_quote);
            assign_text(item.SpreadQuote, *q->spread_quote);
            r.CompositeQuote.push_back(std::move(item));
        }
        return r;
    }

    std::vector<const refdata::domain::curve_segment_curve*> curves_in_role(
        const refdata::domain::curve_segment& s, std::string_view role) const {
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

void export_segment(segmentsType& out,
                    const refdata::domain::curve_segment& s,
                    const export_context& ctx) {
    const auto kind = segment_kinds().find(s.segment_type);
    if (kind == segment_kinds().end())
        throw refusal("segment " + boost::uuids::to_string(s.id) + " has unknown type '" +
                      s.segment_type + "'");
    const auto& k = kind->second;

    if (k == "Direct") {
        directSegmentType r;
        r.Type = enum_from_text<directSegmentTypeType>(s.segment_type, "direct segment type");
        assign_optional_text(r.Conventions, s.conventions);
        restore_common_settings(r, s);
        r.Quotes = ctx.plain_quotes(s);
        out.Direct.push_back(std::move(r));
    } else if (k == "Simple") {
        simpleSegmentType r;
        r.Type = enum_from_text<simpleSegmentTypeType>(s.segment_type, "simple segment type");
        assign_text(r.Conventions, require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        assign_optional_text(r.ProjectionCurve, s.projection_curve);
        r.Quotes = ctx.plain_quotes(s);
        out.Simple.push_back(std::move(r));
    } else if (k == "AverageOIS") {
        aoisSegmentType r;
        assign_text(r.Type, s.segment_type);
        assign_text(r.Conventions, require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        assign_optional_text(r.ProjectionCurve, s.projection_curve);
        r.Quotes = ctx.composite_quotes(s);
        out.AverageOIS.push_back(std::move(r));
    } else if (k == "TenorBasis") {
        tenorBasisSegmentType r;
        r.Type =
            enum_from_text<tenorBasisSegmentTypeType>(s.segment_type, "tenor basis segment type");
        assign_text(r.Conventions, require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        assign_optional_text(r.ProjectionCurvePay, s.projection_curve_pay);
        assign_optional_text(r.ProjectionCurveReceive, s.projection_curve_receive);
        assign_optional_text(r.ProjectionCurveLong, s.projection_curve_long);
        assign_optional_text(r.ProjectionCurveShort, s.projection_curve_short);
        r.Quotes = ctx.plain_quotes(s);
        out.TenorBasis.push_back(std::move(r));
    } else if (k == "CrossCurrency") {
        crossCurrencySegmentType r;
        r.Type = enum_from_text<crossCurrencySegmentTypeType>(s.segment_type,
                                                              "cross currency segment type");
        assign_text(r.Conventions, require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        assign_text(r.DiscountCurve, require(s.discount_curve, s, "discount curve"));
        assign_text(r.SpotRate, require(s.spot_rate, s, "spot rate"));
        assign_optional_text(r.ProjectionCurveDomestic, s.projection_curve_domestic);
        assign_optional_text(r.ProjectionCurveForeign, s.projection_curve_foreign);
        r.Quotes = ctx.plain_quotes(s);
        out.CrossCurrency.push_back(std::move(r));
    } else if (k == "ZeroSpread") {
        zeroSpreadType r;
        r.Type =
            enum_from_text<zeroSpreadSegmentTypeType>(s.segment_type, "zero spread segment type");
        assign_text(r.Conventions, require(s.conventions, s, "conventions"));
        restore_common_settings(r, s);
        assign_text(r.ReferenceCurve, require(s.reference_curve, s, "reference curve"));
        r.Quotes = ctx.plain_quotes(s);
        out.ZeroSpread.push_back(std::move(r));
    } else if (k == "DiscountRatio") {
        discountRatioType r;
        r.Type = enum_from_text<discountRatioTypeType>(s.segment_type, "discount ratio type");
        assign_optional_text(r.Conventions, s.conventions);
        restore_common_settings(r, s);
        assign_text(r.BaseCurve, require(s.base_curve, s, "base curve"));
        r.BaseCurve.currency = require(s.base_curve_currency, s, "base curve currency");
        assign_text(r.NumeratorCurve, require(s.numerator_curve, s, "numerator curve"));
        r.NumeratorCurve.currency = require(s.numerator_curve_currency, s, "numerator currency");
        assign_text(r.DenominatorCurve, require(s.denominator_curve, s, "denominator curve"));
        r.DenominatorCurve.currency =
            require(s.denominator_curve_currency, s, "denominator currency");
        out.DiscountRatio.push_back(std::move(r));
    } else if (k == "FittedBond") {
        fittedBondType r;
        assign_text(r.Type, s.segment_type);
        restore_common_settings(r, s);
        assign_optional_bool(r.ExtrapolateFlat, s.extrapolate_flat);
        r.Quotes = ctx.plain_quotes(s);
        if (const auto cs = ctx.curves_in_role(s, "IndexCurve"); !cs.empty()) {
            fittedBondType_IndexCurves_t list;
            for (const auto* c : cs) {
                fittedBondType_IndexCurves_t_IndexCurve_t item;
                assign_text(item, c->curve);
                if (c->index_name)
                    item.Index = *c->index_name;
                list.IndexCurve.push_back(std::move(item));
            }
            r.IndexCurves = std::move(list);
        }
        if (const auto cs = ctx.curves_in_role(s, "IborIndexCurve"); !cs.empty()) {
            fittedBondType_IborIndexCurves_t list;
            for (const auto* c : cs) {
                fittedBondType_IborIndexCurves_t_IborIndexCurve_t item;
                assign_text(item, c->curve);
                if (c->index_name)
                    item.iborIndex = *c->index_name;
                list.IborIndexCurve.push_back(std::move(item));
            }
            r.IborIndexCurves = std::move(list);
        }
        if (const auto cs = ctx.curves_in_role(s, "InflationIndexCurve"); !cs.empty()) {
            fittedBondType_InflationIndexCurves_t list;
            for (const auto* c : cs) {
                fittedBondType_InflationIndexCurves_t_InflationIndexCurve_t item;
                assign_text(item, c->curve);
                if (c->index_name)
                    item.inflationIndex = *c->index_name;
                list.InflationIndexCurve.push_back(std::move(item));
            }
            r.InflationIndexCurves = std::move(list);
        }
        out.FittedBond.push_back(std::move(r));
    } else if (k == "BondYieldShifted") {
        BondYieldShiftedType r;
        assign_text(r.Type, s.segment_type);
        assign_text(r.Conventions, require(s.conventions, s, "conventions"));
        assign_text(r.ReferenceCurve, require(s.reference_curve, s, "reference curve"));
        assign_optional_bool(r.ExtrapolateFlat, s.extrapolate_flat);
        r.Quotes = ctx.plain_quotes(s);
        if (const auto cs = ctx.curves_in_role(s, "IndexCurve"); !cs.empty()) {
            BondYieldShiftedType_IndexCurves_t list;
            for (const auto* c : cs) {
                BondYieldShiftedType_IndexCurves_t_IndexCurve_t item;
                assign_text(item, c->curve);
                if (c->index_name)
                    item.Index = *c->index_name;
                list.IndexCurve.push_back(std::move(item));
            }
            r.IndexCurves = std::move(list);
        }
        if (const auto cs = ctx.curves_in_role(s, "IborIndexCurve"); !cs.empty()) {
            BondYieldShiftedType_IborIndexCurves_t list;
            for (const auto* c : cs) {
                BondYieldShiftedType_IborIndexCurves_t_IborIndexCurve_t item;
                assign_text(item, c->curve);
                if (c->index_name)
                    item.iborIndex = *c->index_name;
                list.IborIndexCurve.push_back(std::move(item));
            }
            r.IborIndexCurves = std::move(list);
        }
        out.BondYieldShifted.push_back(std::move(r));
    } else if (k == "WeightedAverage") {
        weightedAverageType r;
        assign_text(r.Type, s.segment_type);
        assign_text(r.ReferenceCurve1, require(s.reference_curve, s, "first reference curve"));
        assign_text(r.ReferenceCurve2, require(s.reference_curve_2, s, "second reference curve"));
        r.Weight1 = static_cast<float>(s.weight_1.value_or(0.0));
        r.Weight2 = static_cast<float>(s.weight_2.value_or(0.0));
        out.WeightedAverage.push_back(std::move(r));
    } else if (k == "YieldPlusDefault") {
        yieldPlusDefaultType r;
        assign_text(r.Type, s.segment_type);
        assign_text(r.ReferenceCurve, require(s.reference_curve, s, "reference curve"));
        const auto cs = ctx.curves_in_role(s, "DefaultCurve");
        if (cs.size() != 1)
            throw refusal("yield plus default segment " + boost::uuids::to_string(s.id) +
                          " needs exactly one default curve");
        assign_text(r.DefaultCurves.DefaultCurve, cs.front()->curve);
        r.Weights.Weight = static_cast<float>(cs.front()->weight.value_or(0.0));
        out.YieldPlusDefault.push_back(std::move(r));
    } else {
        iborFallbackType r;
        assign_text(r.Type, s.segment_type);
        restore_common_settings(r, s);
        r.IborIndex = require(s.ibor_index, s, "Ibor index");
        assign_text(r.RfrCurve, require(s.rfr_curve, s, "risk free curve"));
        if (s.rfr_index)
            r.RfrIndex = *s.rfr_index;
        assign_optional_double(r.Spread, s.spread);
        out.IborFallback.push_back(std::move(r));
    }
}

yieldCurve export_yield_curve(const refdata::domain::curve_definition& d,
                              const refdata::domain::yield_curve& y,
                              const refdata::domain::curve_bootstrap_config* bootstrap,
                              const std::vector<const refdata::domain::curve_segment*>& segments,
                              const export_context& ctx) {
    yieldCurve r;
    assign_text(r.CurveId, d.curve_id);
    assign_text(r.CurveDescription, d.description.value_or(""));
    r.Currency = enum_from_text<currencyCode>(y.currency, "currency code");
    assign_text(r.DiscountCurve, y.discount_curve);
    assign_optional_enum(r.InterpolationVariable, y.interpolation_variable, "interpolation variable");
    assign_optional_enum(r.InterpolationMethod, y.interpolation_method, "interpolation method");
    assign_optional_int(r.MixedInterpolationCutoff, y.mixed_interpolation_cutoff);
    assign_optional_enum(r.YieldCurveDayCounter, y.day_counter, "day counter");
    assign_optional_double(r.Tolerance, y.tolerance);
    assign_optional_enum(r.Extrapolation, y.extrapolation, "ORE boolean");
    assign_optional_enum(r.ExtrapolationMethod, y.extrapolation_method, "extrapolation method");
    assign_optional_enum(
        r.ExcludeT0FromInterpolation, y.exclude_t0_from_interpolation, "ORE boolean");
    if (y.has_report) {
        yieldCurveReport report;
        assign_optional_text(report.PillarDates, y.report_pillar_dates);
        r.Report = std::move(report);
    }
    if (bootstrap)
        r.BootstrapConfig = export_bootstrap(*bootstrap);
    for (const auto* s : segments)
        export_segment(r.Segments, *s, ctx);
    return r;
}

quoteType entry_quotes(const refdata::domain::curve_definition& d, const export_context& ctx) {
    quoteType quotes;
    for (const auto* q : ctx.quotes_of_entry(d.id)) {
        quoteType_Quote_t item;
        assign_text(item, q->quote_text.value_or(""));
        if (q->optional_flag)
            item.optional = *q->optional_flag;
        quotes.Quote.push_back(std::move(item));
    }
    return quotes;
}

equityCurve export_equity_curve(const refdata::domain::curve_definition& d,
                                const refdata::domain::equity_curve& e,
                                const export_context& ctx) {
    equityCurve r;
    assign_text(r.CurveId, d.curve_id);
    assign_text(r.CurveDescription, d.description.value_or(""));
    r.Currency = e.currency;
    if (e.calendar)
        r.Calendar = *e.calendar;
    assign_text(r.ForecastingCurve, e.forecasting_curve);
    r.Type = enum_from_text<equityType>(e.equity_type, "equity curve type");
    assign_optional_enum(r.ExerciseStyle, e.exercise_style, "exercise style");
    assign_text(r.SpotQuote, e.spot_quote);
    if (e.has_quotes)
        r.Quotes = entry_quotes(d, ctx);
    assign_optional_enum(r.DayCounter, e.day_counter, "day counter");
    if (e.has_dividend_interpolation) {
        dividendInterpolation di;
        assign_optional_enum(
            di.InterpolationVariable, e.dividend_interpolation_variable, "interpolation variable");
        assign_optional_enum(
            di.InterpolationMethod, e.dividend_interpolation_method, "interpolation method");
        r.DividendInterpolation = std::move(di);
    }
    assign_optional_enum(r.DividendExtrapolation, e.dividend_extrapolation, "ORE boolean");
    assign_optional_enum(r.Extrapolation, e.extrapolation, "ORE boolean");
    return r;
}

inflationCurve export_inflation_curve(
    const refdata::domain::curve_definition& d,
    const refdata::domain::inflation_curve& v,
    const std::vector<const refdata::domain::inflation_seasonality_factor*>& factors,
    const export_context& ctx) {
    inflationCurve r;
    assign_text(r.CurveId, d.curve_id);
    assign_text(r.CurveDescription, d.description.value_or(""));
    assign_text(r.NominalTermStructure, v.nominal_term_structure);
    r.Type = enum_from_text<inflationType>(v.inflation_type, "inflation type");
    assign_optional_text(r.Conventions, v.conventions);
    if (v.has_quotes)
        r.Quotes = entry_quotes(d, ctx);
    assign_optional_enum(r.Extrapolation, v.extrapolation, "ORE boolean");
    r.Calendar = v.calendar;
    assign_optional_enum(r.DayCounter, v.day_counter, "day counter");
    assign_text(r.Lag, v.lag);
    r.Frequency = enum_from_text<frequencyType>(v.frequency, "frequency");
    assign_optional_text(r.BaseRate, v.base_rate);
    assign_optional_double(r.Tolerance, v.tolerance);
    if (v.has_seasonality) {
        seasonalityType season;
        season.BaseDate = v.seasonality_base_date.value_or("");
        season.Frequency =
            enum_from_text<frequencyType>(v.seasonality_frequency.value_or(""), "frequency");
        for (const auto* f : factors) {
            factorType_Factor_t item;
            assign_text(item, f->factor);
            season.Factors.Factor.push_back(std::move(item));
        }
        r.Seasonality = std::move(season);
    }
    assign_optional_enum(r.UseLastFixingDate, v.use_last_fixing_date, "ORE boolean");
    assign_optional_text(r.InterpolationVariable, v.interpolation_variable);
    assign_optional_text(r.InterpolationMethod, v.interpolation_method);
    return r;
}

template <typename Target>
void export_configuration_settings(Target& r,
                                   const refdata::domain::default_curve_configuration& c,
                                   const export_context& ctx) {
    assign_optional_text(r.DiscountCurve, c.discount_curve);
    assign_optional_text(r.RecoveryRate, c.recovery_rate);
    if (c.start_date)
        r.StartDate = *c.start_date;
    if (c.has_quotes) {
        quoteType quotes;
        const auto it = ctx.configuration_quotes.find(c.id);
        if (it != ctx.configuration_quotes.end()) {
            for (const auto* q : it->second) {
                quoteType_Quote_t item;
                assign_text(item, q->quote_text.value_or(""));
                if (q->optional_flag)
                    item.optional = *q->optional_flag;
                quotes.Quote.push_back(std::move(item));
            }
        }
        r.Quotes = std::move(quotes);
    }
    assign_optional_text(r.BenchmarkCurve, c.benchmark_curve);
    assign_optional_text(r.SourceCurve, c.source_curve);
    assign_optional_text(r.Pillars, c.pillars);
    if (c.spot_lag)
        r.SpotLag = static_cast<int64_t>(*c.spot_lag);
    if (c.calendar)
        r.Calendar = *c.calendar;
    assign_optional_text(r.Conventions, c.conventions);
    assign_optional_enum(r.Extrapolation, c.extrapolation, "ORE boolean");
    assign_optional_double(r.RunningSpread, c.running_spread);
    assign_optional_text(r.IndexTerm, c.index_term);
    assign_optional_enum(r.ImplyDefaultFromMarket, c.imply_default_from_market, "ORE boolean");
    assign_optional_enum(r.AllowNegativeRates, c.allow_negative_rates, "ORE boolean");
    assign_optional_enum(r.PriceIsUpfront, c.price_is_upfront, "ORE boolean");
    assign_optional_text(r.InitialState, c.initial_state);
    assign_optional_text(r.States, c.states);
    const auto b = ctx.configuration_bootstraps.find(c.id);
    if (b != ctx.configuration_bootstraps.end())
        r.BootstrapConfig = export_bootstrap(*b->second);
}

defaultCurve export_default_curve(
    const refdata::domain::curve_definition& d,
    const refdata::domain::default_curve& v,
    const std::vector<const refdata::domain::default_curve_configuration*>& configurations,
    const export_context& ctx) {
    defaultCurve r;
    assign_text(r.CurveId, d.curve_id);
    assign_text(r.CurveDescription, d.description.value_or(""));
    r.Currency = enum_from_text<currencyCode>(v.currency, "currency code");

    if (configurations.size() == 1 && configurations.front()->is_inline) {
        const auto& c = *configurations.front();
        assign_optional_enum(r.Type, c.default_curve_type, "default curve type");
        assign_optional_enum(r.DayCounter, c.day_counter, "day counter");
        export_configuration_settings(r, c, ctx);
        return r;
    }

    defaultCurve_Configurations_t list;
    for (const auto* c : configurations) {
        if (c->is_inline)
            throw refusal("default curve " + d.curve_id +
                          " mixes an inline configuration with listed ones");
        defaultCurve_Configurations_t_Configuration_t k;
        assign_optional_int(k.priority, c->priority);
        k.Type = enum_from_text<defaultCurveType>(c->default_curve_type.value_or(""),
                                                  "default curve type");
        k.DayCounter = enum_from_text<dayCounter>(c->day_counter.value_or(""), "day counter");
        assign_optional_text(k.ReinterpretedYieldCurve, c->reinterpreted_yield_curve);
        export_configuration_settings(k, *c, ctx);
        list.Configuration.push_back(std::move(k));
    }
    r.Configurations = std::move(list);
    return r;
}

quoteType quote_list(const std::vector<const refdata::domain::curve_quote*>& rows,
                     std::optional<std::string_view> list) {
    quoteType quotes;
    for (const auto* q : rows) {
        const bool same = list ? (q->quote_list && *q->quote_list == *list) : !q->quote_list;
        if (!same)
            continue;
        quoteType_Quote_t item;
        assign_text(item, q->quote_text.value_or(""));
        if (q->optional_flag)
            item.optional = *q->optional_flag;
        quotes.Quote.push_back(std::move(item));
    }
    return quotes;
}

simCommodityCurve export_commodity_curve(
    const refdata::domain::curve_definition& d,
    const refdata::domain::commodity_curve& v,
    const std::vector<const refdata::domain::commodity_price_segment*>& segments,
    const export_context& ctx) {
    static const std::vector<const refdata::domain::curve_quote*> none;
    const auto quotes_of = [&](const auto& map, const boost::uuids::uuid& id)
        -> const std::vector<const refdata::domain::curve_quote*>& {
        const auto it = map.find(id);
        return it == map.end() ? none : it->second;
    };

    simCommodityCurve r;
    assign_text(r.CurveId, d.curve_id);
    assign_text(r.CurveDescription, d.description.value_or(""));
    r.Currency = enum_from_text<currencyCode>(v.currency, "currency code");
    assign_optional_text(r.BasePriceCurve, v.base_price_curve);
    assign_optional_text(r.BaseYieldCurve, v.base_yield_curve);
    assign_optional_text(r.YieldCurve, v.yield_curve);
    assign_optional_text(r.SpotQuote, v.spot_quote);
    if (v.has_quotes)
        r.Quotes = quote_list(quotes_of(ctx.entry_quotes, d.id), std::nullopt);
    assign_optional_enum(r.DayCounter, v.day_counter, "day counter");
    assign_optional_enum(r.InterpolationMethod, v.interpolation_method, "interpolation method");
    assign_optional_text(r.Conventions, v.conventions);
    if (v.has_basis_configuration) {
        commodityBasisConfig b;
        assign_text(b.BasePriceCurve, v.basis_base_price_curve.value_or(""));
        assign_text(b.BasePriceConventions, v.basis_base_price_conventions.value_or(""));
        b.BasisQuotes = quote_list(quotes_of(ctx.basis_quotes, d.id), basis_quotes_list);
        assign_text(b.BasisConventions, v.basis_conventions.value_or(""));
        assign_optional_enum(b.DayCounter, v.basis_day_counter, "day counter");
        assign_optional_enum(b.InterpolationMethod, v.basis_interpolation_method, "interpolation method");
        assign_optional_enum(b.AddBasis, v.basis_add_basis, "ORE boolean");
        assign_optional_int(b.MonthOffset, v.basis_month_offset);
        assign_optional_enum(b.AverageBase, v.basis_average_base, "ORE boolean");
        assign_optional_enum(b.PriceAsHistoricalFixing, v.basis_price_as_historical_fixing, "ORE boolean");
        r.BasisConfiguration = std::move(b);
    }
    if (v.has_price_segments) {
        priceSegmentsType list;
        for (const auto* p : segments) {
            priceSegmentType g;
            g.Type = enum_from_text<priceSegmentTypeType>(p->segment_type, "price segment type");
            assign_optional_int(g.Priority, p->priority);
            if (!p->conventions)
                throw refusal("price segment of " + d.curve_id + " has no conventions");
            assign_text(g.Conventions, *p->conventions);
            const auto& rows = quotes_of(ctx.price_segment_quotes, p->id);
            if (p->has_quotes)
                g.Quotes = quote_list(rows, std::nullopt);
            assign_optional_text(g.PeakPriceCurveId, p->peak_price_curve_id);
            assign_optional_text(g.PeakPriceCalendar, p->peak_price_calendar);
            if (p->has_off_peak_daily) {
                offPeakDailyType o;
                o.OffPeakQuotes = quote_list(rows, off_peak_quotes_list);
                o.PeakQuotes = quote_list(rows, peak_quotes_list);
                g.OffPeakDaily = std::move(o);
            }
            list.PriceSegment.push_back(std::move(g));
        }
        r.PriceSegments = std::move(list);
    }
    assign_optional_enum(r.Extrapolation, v.extrapolation, "ORE boolean");
    const auto b = ctx.entry_bootstraps.find(d.id);
    if (b != ctx.entry_bootstraps.end())
        r.BootstrapConfig = export_bootstrap(*b->second);
    return r;
}

security export_security(const refdata::domain::curve_definition& d,
                         const refdata::domain::curve_security& v) {
    security r;
    assign_text(r.CurveId, d.curve_id);
    assign_text(r.CurveDescription, d.description.value_or(""));
    assign_optional_text(r.SpreadQuote, v.spread_quote);
    assign_optional_text(r.RecoveryRateQuote, v.recovery_rate_quote);
    assign_optional_text(r.CPRQuote, v.cpr_quote);
    assign_optional_text(r.PriceQuote, v.price_quote);
    assign_optional_text(r.ConversionFactor, v.conversion_factor);
    return r;
}

fxSpot export_fx_spot(const refdata::domain::curve_definition& d) {
    fxSpot r;
    assign_text(r.CurveId, d.curve_id);
    assign_text(r.CurveDescription, d.description.value_or(""));
    return r;
}

intradayPowerCurve export_intraday_power_curve(const refdata::domain::curve_definition& d,
                                               const refdata::domain::intraday_power_curve& v) {
    intradayPowerCurve r;
    assign_text(r.CurveId, d.curve_id);
    assign_text(r.CurveDescription, d.description.value_or(""));
    r.Currency = enum_from_text<currencyCode>(v.currency, "currency code");
    assign_text(r.DailyAveragePriceCurve, v.daily_average_price_curve);
    assign_text(r.ShapeQuoteName, v.shape_quote_name);
    if (!v.convention)
        throw refusal("intraday power curve " + d.curve_id + " has no convention");
    assign_text(r.Convention, *v.convention);
    return r;
}

template <typename Row>
std::map<boost::uuids::uuid, const Row*> by_definition(const std::vector<Row>& rows) {
    std::map<boost::uuids::uuid, const Row*> out;
    for (const auto& r : rows)
        out.emplace(r.curve_definition_id, &r);
    return out;
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

}

mapped_curve_configuration curve_configuration_mapper::map(const curveconfiguration& v) {
    mapped_curve_configuration mapped;

    auto& config = mapped.config;
    config.id = new_uuid();
    config.name = "CurveConfiguration";
    config.description = std::string(audit_commentary);
    set_audit(config);

    if (v.ReportConfiguration)
        throw refusal("the report configuration is not modelled yet");

    for (const auto& s : sections()) {
        if (!s.present(v))
            continue;
        if (!is_modelled(s.code) && s.entries(v) > 0)
            throw refusal("section " + std::string(s.code) + " holds " +
                          std::to_string(s.entries(v)) + " entries, which are not modelled yet");
        refdata::domain::curve_configuration_section row;
        row.id = new_uuid();
        row.curve_configuration_id = config.id;
        row.section_code = std::string(s.code);
        set_audit(row);
        mapped.sections.push_back(std::move(row));
    }

    int position = 0;
    if (v.FXSpots)
        for (const auto& e : v.FXSpots->FXSpot)
            import_fx_spot(mapped, e, position++);
    if (v.DefaultCurves)
        for (const auto& e : v.DefaultCurves->DefaultCurve)
            import_default_curve(mapped, e, position++);
    if (v.YieldCurves)
        for (const auto& e : v.YieldCurves->YieldCurve)
            import_yield_curve(mapped, e, position++);
    if (v.InflationCurves)
        for (const auto& e : v.InflationCurves->InflationCurve)
            import_inflation_curve(mapped, e, position++);
    if (v.EquityCurves)
        for (const auto& e : v.EquityCurves->EquityCurve)
            import_equity_curve(mapped, e, position++);
    if (v.Securities)
        for (const auto& e : v.Securities->Security)
            import_security(mapped, e, position++);
    if (v.CommodityCurves)
        for (const auto& e : v.CommodityCurves->CommodityCurve)
            import_commodity_curve(mapped, e, position++);
    if (v.IntradayPowerCurves)
        for (const auto& e : v.IntradayPowerCurves->IntradayPowerCurve)
            import_intraday_power_curve(mapped, e, position++);

    return mapped;
}

curveconfiguration curve_configuration_mapper::reverse(const mapped_curve_configuration& v) {
    curveconfiguration document;

    std::map<std::string, const section_access*, std::less<>> by_code;
    for (const auto& s : sections())
        by_code.emplace(std::string(s.code), &s);
    for (const auto& row : v.sections) {
        const auto it = by_code.find(row.section_code);
        if (it == by_code.end())
            throw refusal("unknown section '" + row.section_code + "'");
        it->second->emplace(document);
    }

    std::map<boost::uuids::uuid, const refdata::domain::yield_curve*> yield_by_definition;
    for (const auto& y : v.yield_curves)
        yield_by_definition.emplace(y.curve_definition_id, &y);
    std::map<boost::uuids::uuid, const refdata::domain::curve_bootstrap_config*>
        bootstrap_by_definition;
    for (const auto& b : v.bootstrap_configs)
        if (b.default_curve_configuration_id == boost::uuids::uuid{})
            bootstrap_by_definition.emplace(b.curve_definition_id, &b);

    export_context ctx;
    std::vector<refdata::domain::curve_quote> segment_quotes;
    std::vector<refdata::domain::curve_quote> entry_quotes;
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
            entry_quotes.push_back(q);
    }
    ctx.price_segment_quotes = group_by(price_segment_quotes, &quote_price_segment);
    ctx.basis_quotes = group_by(basis_quotes, &quote_definition);
    ctx.quotes = group_by(segment_quotes, &segment_of);
    ctx.entry_quotes = group_by(entry_quotes, &quote_definition);
    ctx.configuration_quotes = group_by(configuration_quotes, &quote_configuration);
    for (const auto& b : v.bootstrap_configs) {
        if (b.default_curve_configuration_id != boost::uuids::uuid{})
            ctx.configuration_bootstraps.emplace(b.default_curve_configuration_id, &b);
        else
            ctx.entry_bootstraps.emplace(b.curve_definition_id, &b);
    }
    const auto commodity_by_definition = by_definition(v.commodity_curves);
    const auto price_segments = group_by(v.commodity_price_segments, &price_segment_definition);
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
    std::sort(definitions.begin(), definitions.end(), by_position<refdata::domain::curve_definition>);

    for (const auto* d : definitions) {
        if (d->section_code != equity_curves_section &&
            d->section_code != inflation_curves_section &&
            d->section_code != commodity_curves_section && ctx.entry_quotes.contains(d->id))
            throw refusal("curve " + d->curve_id + " in section " + d->section_code +
                          " holds a quote directly on its entry");
        if (d->section_code == equity_curves_section) {
            if (!document.EquityCurves)
                document.EquityCurves = equityCurves{};
            document.EquityCurves->EquityCurve.push_back(
                export_equity_curve(*d, settings_of(equity_by_definition, *d), ctx));
            continue;
        }
        if (d->section_code == commodity_curves_section) {
            static const std::vector<const refdata::domain::commodity_price_segment*> no_segments;
            const auto g = price_segments.find(d->id);
            if (!document.CommodityCurves)
                document.CommodityCurves = simCommodityCurves{};
            document.CommodityCurves->CommodityCurve.push_back(
                export_commodity_curve(*d,
                                       settings_of(commodity_by_definition, *d),
                                       g == price_segments.end() ? no_segments : g->second,
                                       ctx));
            continue;
        }
        if (d->section_code == default_curves_section) {
            const auto k = configurations.find(d->id);
            if (k == configurations.end())
                throw refusal("default curve " + d->curve_id + " has no configuration");
            if (!document.DefaultCurves)
                document.DefaultCurves = defaultCurves{};
            document.DefaultCurves->DefaultCurve.push_back(
                export_default_curve(*d, settings_of(default_by_definition, *d), k->second, ctx));
            continue;
        }
        if (d->section_code == inflation_curves_section) {
            static const std::vector<const refdata::domain::inflation_seasonality_factor*> none;
            const auto f = factors.find(d->id);
            if (!document.InflationCurves)
                document.InflationCurves = inflationCurves{};
            document.InflationCurves->InflationCurve.push_back(
                export_inflation_curve(*d,
                                       settings_of(inflation_by_definition, *d),
                                       f == factors.end() ? none : f->second,
                                       ctx));
            continue;
        }
        if (d->section_code == securities_section) {
            if (!document.Securities)
                document.Securities = securities{};
            document.Securities->Security.push_back(
                export_security(*d, settings_of(security_by_definition, *d)));
            continue;
        }
        if (d->section_code == fx_spots_section) {
            if (!document.FXSpots)
                document.FXSpots = fxSpots{};
            document.FXSpots->FXSpot.push_back(export_fx_spot(*d));
            continue;
        }
        if (d->section_code == intraday_power_curves_section) {
            if (!document.IntradayPowerCurves)
                document.IntradayPowerCurves = intradayPowerCurves{};
            document.IntradayPowerCurves->IntradayPowerCurve.push_back(
                export_intraday_power_curve(*d, settings_of(power_by_definition, *d)));
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
        if (!document.YieldCurves)
            document.YieldCurves = yieldCurves{};
        document.YieldCurves->YieldCurve.push_back(
            export_yield_curve(*d,
                               *y->second,
                               b == bootstrap_by_definition.end() ? nullptr : b->second,
                               g == segments.end() ? none : g->second,
                               ctx));
    }

    return document;
}

}
