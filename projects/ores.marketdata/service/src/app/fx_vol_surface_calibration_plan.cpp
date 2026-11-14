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
#include "ores.marketdata.service/app/fx_vol_surface_calibration_plan.hpp"
#include "ores.analytics.quant/math/fx_pair_market.hpp"
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include "ores.marketdata.api/datum/value.hpp"
#include "ores.refdata.core/mapping/fx_option_convention_mapping.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <charconv>
#include <cmath>
#include <format>
#include <set>

namespace ores::marketdata::service::app {

namespace {

namespace qd = analytics::quant::domain;
namespace qm = analytics::quant::math;
namespace qs = analytics::quant::service;
namespace mapping = refdata::mapping;

/// A pillar's strike label as SSVI reads it.
struct read_label final {
    qs::ssvi_quote_kind kind = qs::ssvi_quote_kind::atm;
    double delta = 0.0;
    int delta_points = 0;
};

std::expected<read_label, std::string> read_strike_label(const std::string& point,
                                                         std::string_view label) {
    if (label == "ATM")
        return read_label{};
    if (label.size() < 2)
        return std::unexpected("pillar " + point + " has the strike label '" + std::string(label) +
                               "', which is not ATM or a delta and a suffix");
    const auto suffix_at = label.find_first_not_of("0123456789");
    if (suffix_at == 0 || suffix_at == std::string_view::npos)
        return std::unexpected("pillar " + point + " has the strike label '" + std::string(label) +
                               "', which is not ATM or a delta and a suffix");
    int points = 0;
    const auto [end, ec] = std::from_chars(label.data(), label.data() + suffix_at, points);
    if (ec != std::errc{} || points < 1 || points > 99)
        return std::unexpected("pillar " + point + " has a delta in '" + std::string(label) +
                               "' that is not from 1 to 99");
    const auto suffix = label.substr(suffix_at);
    if (suffix == "RR")
        return read_label{qs::ssvi_quote_kind::risk_reversal, points / 100.0, points};
    if (suffix == "BF")
        return read_label{qs::ssvi_quote_kind::butterfly, points / 100.0, points};
    if (suffix == "C" || suffix == "P")
        return std::unexpected("pillar " + point + " quotes a single " +
                               (suffix == "C" ? std::string("call") : std::string("put")) +
                               " at a delta, and SSVI reads ATM, risk reversals and butterflies");
    return std::unexpected("pillar " + point + " has the strike label '" + std::string(label) +
                           "', whose suffix is not RR, BF, C or P");
}


std::string pillar_key_of(const std::string& expiry, const std::string& label) {
    return expiry + "/" + label;
}

/// Whether the labels quoted at one expiry are enough for SSVI: an ATM, and a risk reversal and
/// a butterfly at one delta.
bool ssvi_has_enough(bool atm,
                     const std::set<int>& risk_reversals,
                     const std::set<int>& butterflies) {
    if (!atm)
        return false;
    return std::ranges::any_of(risk_reversals, [&](int d) { return butterflies.contains(d); });
}

std::expected<qs::ssvi_grid_strike, std::string> grid_strike_of(const std::string& text) {
    const auto parsed = datum::strike::parse(text);
    if (!parsed)
        return std::unexpected("the output strike '" + text +
                               "' is not a strike: " + parsed.error());
    if (std::holds_alternative<datum::atm_strike>(parsed->which()))
        return qs::ssvi_grid_strike{text, qs::ssvi_atm_strike{}};
    const auto* delta = std::get_if<datum::delta_strike>(&parsed->which());
    if (!delta)
        return std::unexpected("the output strike '" + text +
                               "' is neither ATM nor a delta, and the grid reads only those");
    const auto type = mapping::parse_fx_delta_type_code(delta->delta_type);
    if (!type)
        return std::unexpected("the output strike '" + text + "': " + type.error());
    if (delta->option_type != "Call" && delta->option_type != "Put")
        return std::unexpected("the output strike '" + text + "' names neither a call nor a put");
    const auto& digits = delta->delta.text();
    double size = 0.0;
    const auto [end, ec] = std::from_chars(digits.data(), digits.data() + digits.size(), size);
    if (ec != std::errc{} || end != digits.data() + digits.size() || !(size > 0.0 && size < 100.0))
        return std::unexpected("the output strike '" + text +
                               "' has a delta that is not above 0 and below 100");
    return qs::ssvi_grid_strike{text,
                                qs::ssvi_delta_strike{.type = *type,
                                                      .side = delta->option_type == "Call" ?
                                                                  qm::option_side::call :
                                                                  qm::option_side::put,
                                                      .delta = size / 100.0}};
}

int days_between(std::chrono::year_month_day a, std::chrono::year_month_day b) {
    return static_cast<int>((std::chrono::sys_days(b) - std::chrono::sys_days(a)).count());
}

/// An expiry's time, market and convention, as the engine reads them.
struct expiry_terms final {
    double years = 0.0;
    qm::fx_market market{};
    qm::fx_quote_convention convention{};
};

std::expected<expiry_terms, std::string> terms_at(const surface_calibration_recipe& recipe,
                                                  const surface_forward_set& forwards,
                                                  std::span<const qm::fx_discount_point> by_date,
                                                  const std::string& code,
                                                  qd::date_period period,
                                                  std::chrono::year_month_day as_of) {
    const auto convention = mapping::to_quote_convention(*recipe.convention, period);
    if (!convention)
        return std::unexpected("expiry " + code + ": " + convention.error());
    const auto market =
        qm::pair_market_at(forwards.spot,
                           forwards.spot_date,
                           by_date,
                           qs::calendar_arithmetic::add_period(forwards.spot_date, period),
                           "expiry " + code);
    if (!market)
        return std::unexpected(market.error());
    // An FX option expires a tenor after the trade and delivers a tenor after spot, so its time
    // runs from the as-of and its discounting from the spot date.
    return expiry_terms{
        .years = days_between(as_of, qs::calendar_arithmetic::add_period(as_of, period)) / 365.0,
        .market = *market,
        .convention = *convention};
}

}

std::expected<surface_forward_set, std::string> spot_starting(const surface_forward_set& forwards) {
    const auto at_spot = [&](const surface_forward_point& point) {
        return std::chrono::sys_days(point.value_date) == std::chrono::sys_days(forwards.spot_date);
    };
    const auto anchor = std::ranges::find_if(forwards.points, at_spot);
    if (anchor == forwards.points.end())
        return std::unexpected(
            "the forward set " + forwards.publish_id + " has no row on its spot date " +
            std::format("{}", forwards.spot_date) + ", so it cannot be read from spot");
    if (!(anchor->base_discount > 0.0) || !(anchor->quote_discount > 0.0))
        return std::unexpected("the row on the spot date " + std::format("{}", forwards.spot_date) +
                               " of the forward set " + forwards.publish_id +
                               " discounts to a factor that is not above zero");

    surface_forward_set out{.publish_id = forwards.publish_id,
                            .spot = forwards.spot,
                            .spot_date = forwards.spot_date,
                            .points = {}};
    for (const auto& point : forwards.points) {
        // ON and any other delivery before spot are not part of a spot-starting market.
        if (std::chrono::sys_days(point.value_date) < std::chrono::sys_days(forwards.spot_date))
            continue;
        if (!(point.base_discount > 0.0) || !(point.quote_discount > 0.0))
            return std::unexpected("the " + point.tenor + " row of the forward set " +
                                   forwards.publish_id +
                                   " discounts to a factor that is not above zero");
        out.points.push_back({.tenor = point.tenor,
                              .value_date = point.value_date,
                              .base_discount = point.base_discount / anchor->base_discount,
                              .quote_discount = point.quote_discount / anchor->quote_discount});
    }
    return out;
}

std::vector<std::string> split_blueprint_list(const std::string& text) {
    std::vector<std::string> out;
    std::size_t start = 0;
    while (start <= text.size()) {
        const auto end = std::min(text.find(',', start), text.size());
        auto item = text.substr(start, end - start);
        item.erase(0, item.find_first_not_of(' '));
        item.erase(item.find_last_not_of(' ') + 1);
        if (!item.empty())
            out.push_back(std::move(item));
        start = end + 1;
    }
    return out;
}

std::expected<checked_recipe, std::string> check_recipe(const surface_calibration_recipe& recipe) {
    const auto& blueprint = recipe.blueprint;
    if (blueprint.smile_model != "ssvi")
        return std::unexpected("the smile model '" + blueprint.smile_model +
                               "' is not one the calibration fits; it fits ssvi");
    if (!recipe.convention)
        return std::unexpected("the blueprint names the FX option convention '" +
                               blueprint.fx_option_convention_id + "', which is not stored");
    if (!recipe.convention->butterfly_style)
        return std::unexpected("the FX option convention '" + recipe.convention->id +
                               "' names no butterfly style, so a butterfly quote has no meaning");

    struct expiry_labels final {
        bool atm = false;
        std::set<int> risk_reversals;
        std::set<int> butterflies;
    };
    std::map<std::string, expiry_labels> labels_by_expiry;
    std::set<std::string> points;
    for (const auto& pillar : recipe.pillars) {
        const auto point = pillar_key_of(pillar.expiry_code, pillar.strike_label);
        if (!points.insert(point).second)
            return std::unexpected("pillar " + point + " appears twice in the blueprint");
        const auto label = read_strike_label(point, pillar.strike_label);
        if (!label)
            return std::unexpected(label.error());
        auto& labels = labels_by_expiry[pillar.expiry_code];
        if (label->kind == qs::ssvi_quote_kind::atm)
            labels.atm = true;
        else if (label->kind == qs::ssvi_quote_kind::risk_reversal)
            labels.risk_reversals.insert(label->delta_points);
        else
            labels.butterflies.insert(label->delta_points);
    }
    if (labels_by_expiry.empty())
        return std::unexpected("the blueprint " + blueprint.name + " has no pillars");
    for (const auto& [code, labels] : labels_by_expiry)
        if (!ssvi_has_enough(labels.atm, labels.risk_reversals, labels.butterflies))
            return std::unexpected("expiry " + code +
                                   " lacks the pillars SSVI needs: an ATM, and a risk reversal "
                                   "and a butterfly at one delta");

    checked_recipe out;
    for (const auto& [code, labels] : labels_by_expiry) {
        const auto period = mapping::parse_tenor_text(code);
        if (!period)
            return std::unexpected("expiry " + code + ": " + period.error());
        out.expiries.push_back({code, *period});
    }
    std::vector<recipe_expiry> sorted = out.expiries;
    for (std::size_t i = 0; i < sorted.size(); ++i)
        for (std::size_t j = i + 1; j < sorted.size(); ++j) {
            const auto order = qd::compare_periods(sorted[i].period, sorted[j].period);
            if (order == std::partial_ordering::unordered)
                return std::unexpected("expiries " + sorted[i].code + " and " + sorted[j].code +
                                       " cannot be ordered");
            if (order == 0)
                return std::unexpected("expiries " + sorted[i].code + " and " + sorted[j].code +
                                       " are the same length");
            if (order > 0)
                std::swap(sorted[i], sorted[j]);
        }
    out.expiries = std::move(sorted);

    const auto& first = out.expiries.front();
    const auto& last = out.expiries.back();
    for (const auto& code : split_blueprint_list(blueprint.output_expiries)) {
        const auto period = mapping::parse_tenor_text(code);
        if (!period)
            return std::unexpected("output expiry " + code + ": " + period.error());
        if (qd::compare_periods(*period, first.period) < 0 ||
            qd::compare_periods(*period, last.period) > 0 ||
            qd::compare_periods(*period, first.period) == std::partial_ordering::unordered ||
            qd::compare_periods(*period, last.period) == std::partial_ordering::unordered)
            return std::unexpected("output expiry " + code + " lies outside the quoted expiries " +
                                   first.code + " to " + last.code);
        out.grid_expiries.push_back({code, *period});
    }
    if (out.grid_expiries.empty())
        return std::unexpected("the blueprint " + blueprint.name + " names no output expiry");
    for (const auto& text : split_blueprint_list(blueprint.output_strikes)) {
        auto strike = grid_strike_of(text);
        if (!strike)
            return std::unexpected(strike.error());
        out.grid_strikes.push_back(std::move(*strike));
    }
    if (out.grid_strikes.empty())
        return std::unexpected("the blueprint " + blueprint.name + " names no output strike");
    return out;
}

std::expected<prepared_surface_calibration, std::string>
prepare_calibration(const surface_calibration_recipe& recipe,
                    const checked_recipe& checked,
                    const surface_forward_set& forwards,
                    const std::map<boost::uuids::uuid, surface_series_quotes>& quotes,
                    std::chrono::year_month_day as_of,
                    double pillar_tolerance) {
    if (!(forwards.spot > 0.0))
        return std::unexpected("the forward set " + forwards.publish_id + " has no spot");
    std::vector<qm::fx_discount_point> by_date;
    by_date.reserve(forwards.points.size());
    for (const auto& p : forwards.points)
        by_date.push_back({.date = p.value_date,
                           .base_discount = p.base_discount,
                           .quote_discount = p.quote_discount});
    std::ranges::sort(by_date, {}, [](const auto& p) { return std::chrono::sys_days(p.date); });

    prepared_surface_calibration out;
    out.request.pillar_tolerance = pillar_tolerance;
    for (const auto& expiry : checked.expiries) {
        const auto terms = terms_at(recipe, forwards, by_date, expiry.code, expiry.period, as_of);
        if (!terms)
            return std::unexpected(terms.error());
        qs::ssvi_expiry_quotes quoted{.expiry_id = expiry.code,
                                      .years = terms->years,
                                      .market = terms->market,
                                      .convention = terms->convention,
                                      .pillars = {}};
        bool atm = false;
        std::set<int> risk_reversals;
        std::set<int> butterflies;
        std::vector<std::string> unquoted;
        for (const auto& pillar : recipe.pillars) {
            if (pillar.expiry_code != expiry.code)
                continue;
            const auto point = pillar_key_of(pillar.expiry_code, pillar.strike_label);
            const auto read = read_strike_label(point, pillar.strike_label);
            if (!read)
                return std::unexpected(read.error());
            const auto& label = *read;
            qs::ssvi_pillar fit{.point_id = point,
                                .kind = label.kind,
                                .delta = label.delta,
                                .log_moneyness = 0.0,
                                .contributions = {}};
            missing_surface_pillar missing{.point = point, .series_ids = {}};
            std::vector<used_surface_source> used;
            for (const auto& source : recipe.sources) {
                if (source.pillar_id != pillar.id)
                    continue;
                const double* value = nullptr;
                if (const auto series = quotes.find(source.market_series_id);
                    series != quotes.end())
                    if (const auto found =
                            series->second.find({pillar.expiry_code, pillar.strike_label});
                        found != series->second.end())
                        value = &found->second;
                if (!value) {
                    missing.series_ids.push_back(source.market_series_id);
                    continue;
                }
                fit.contributions.push_back({*value, source.weight});
                used.push_back({source.market_series_id, source.contributor_code, source.weight});
            }
            if (fit.contributions.empty()) {
                unquoted.push_back(point);
                out.missing.push_back(std::move(missing));
                continue;
            }
            if (label.kind == qs::ssvi_quote_kind::atm)
                atm = true;
            else if (label.kind == qs::ssvi_quote_kind::risk_reversal)
                risk_reversals.insert(label.delta_points);
            else
                butterflies.insert(label.delta_points);
            out.sources_by_point.emplace(point, std::move(used));
            quoted.pillars.push_back(std::move(fit));
        }
        if (!ssvi_has_enough(atm, risk_reversals, butterflies)) {
            std::string what = "expiry " + expiry.code +
                               " lacks the pillars SSVI needs once the unquoted ones are left out";
            for (const auto& m : out.missing)
                if (std::ranges::find(unquoted, m.point) != unquoted.end()) {
                    what += "; " + m.point + " had no quote from";
                    for (const auto& id : m.series_ids)
                        what += " " + boost::uuids::to_string(id);
                }
            return std::unexpected(what);
        }
        out.request.expiries.push_back(std::move(quoted));
    }
    for (const auto& grid : checked.grid_expiries) {
        const auto terms = terms_at(recipe, forwards, by_date, grid.code, grid.period, as_of);
        if (!terms)
            return std::unexpected(terms.error());
        out.request.grid_expiries.push_back({.expiry_id = grid.code,
                                             .years = terms->years,
                                             .market = terms->market,
                                             .convention = terms->convention});
    }
    out.request.grid_strikes = checked.grid_strikes;
    return out;
}

}
