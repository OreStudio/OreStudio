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
#include <stdexcept>

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
                                                  qd::date_period period) {
    const auto convention = mapping::to_quote_convention(*recipe.convention, period);
    if (!convention)
        return std::unexpected("expiry " + code + ": " + convention.error());
    // The value date is the ladder's, not one computed here. The ladder dates a tenor on the
    // pair's calendar with its roll convention and its month-end rule, and publishes that date
    // with the row; a date added here would land elsewhere the moment the date is not a business
    // day, and the surface would discount between two rows instead of to the one it quotes. A
    // tenor the ladder does not carry has no value date, and that is refused rather than guessed.
    const auto row = std::ranges::find(forwards.points, code, &surface_forward_point::tenor);
    if (row == forwards.points.end()) {
        const auto last = std::ranges::max_element(
            forwards.points, {}, [](const auto& p) { return std::chrono::sys_days(p.value_date); });
        return std::unexpected("expiry " + code + ": the forward set " + forwards.publish_id +
                               " has no row for that tenor; the ladder ends at " +
                               (last == forwards.points.end() ?
                                    std::string("no date at all") :
                                    std::format("{}", std::chrono::sys_days(last->value_date))) +
                               ", so the expiry has no value date");
    }
    const auto market = qm::pair_market_at(
        forwards.spot, forwards.spot_date, by_date, row->value_date, "expiry " + code);
    if (!market)
        return std::unexpected(market.error());
    // A surface is quoted on a tenor grid, so its time axis is the tenor's own length: 1M is one
    // twelfth of a year whatever the calendar does to the date. The date itself is the ladder's,
    // and it is what the market discounts to; the two answer different questions and both come
    // from one definition each rather than from arithmetic done here.
    const auto years = qs::fx_vol_tenor_years(code);
    if (!years)
        return std::unexpected("expiry " + code +
                               ": the tenor is not a count and a unit, so the "
                               "surface has no time axis for it");
    return expiry_terms{.years = *years, .market = *market, .convention = *convention};
}

/// Whether a row of a forward set discounts to a factor at all: no factor is a zero or below.
bool discounts_to_a_factor(const surface_forward_point& point) {
    return point.base_discount > 0.0 && point.quote_discount > 0.0;
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
    if (!discounts_to_a_factor(*anchor))
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
        if (!discounts_to_a_factor(point))
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
                    double pillar_tolerance,
                    std::size_t minimum_expiries) {
    if (!(forwards.spot > 0.0))
        return std::unexpected("the forward set " + forwards.publish_id + " has no spot");
    // A forward ladder carries the settlement run-up as well as the discounts: an overnight or
    // tom-next row values before spot, which is what those instruments are. A pair's market is
    // anchored at its spot date -- discounting starts there, at one on both sides -- so those
    // rows are not points of it and pair_market_at refuses them. Dropping them is not a loss:
    // nothing a surface discounts to can fall before spot.
    std::vector<qm::fx_discount_point> by_date;
    by_date.reserve(forwards.points.size());
    for (const auto& p : forwards.points) {
        if (p.value_date < forwards.spot_date)
            continue;
        by_date.push_back({.date = p.value_date,
                           .base_discount = p.base_discount,
                           .quote_discount = p.quote_discount});
    }
    std::ranges::sort(by_date, {}, [](const auto& p) { return std::chrono::sys_days(p.date); });

    prepared_surface_calibration out;
    out.request.pillar_tolerance = pillar_tolerance;
    std::set<std::string> unfitted;
    double longest_fitted = 0.0;

    // The blueprint is indexed once: each expiry's pillars, and each pillar's
    // sources. A surface carries hundreds of both, so walking the whole of each
    // per expiry would cost the fit more than the fit does.
    std::map<std::string, std::vector<const refdata::domain::fx_vol_surface_pillar*>>
        pillars_of_expiry;
    for (const auto& pillar : recipe.pillars)
        pillars_of_expiry[pillar.expiry_code].push_back(&pillar);
    std::map<boost::uuids::uuid, std::vector<const refdata::domain::fx_vol_surface_pillar_source*>>
        sources_of_pillar;
    for (const auto& source : recipe.sources)
        sources_of_pillar[source.pillar_id].push_back(&source);

    for (const auto& expiry : checked.expiries) {
        const auto terms = terms_at(recipe, forwards, by_date, expiry.code, expiry.period);
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
        static const std::vector<const refdata::domain::fx_vol_surface_pillar*> no_pillars;
        const auto found_pillars = pillars_of_expiry.find(expiry.code);
        for (const auto* pillar_ptr :
             found_pillars == pillars_of_expiry.end() ? no_pillars : found_pillars->second) {
            const auto& pillar = *pillar_ptr;
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
            static const std::vector<const refdata::domain::fx_vol_surface_pillar_source*>
                no_sources;
            const auto found_sources = sources_of_pillar.find(pillar.id);
            for (const auto* source_ptr :
                 found_sources == sources_of_pillar.end() ? no_sources : found_sources->second) {
                const auto& source = *source_ptr;
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
        // An expiry the quotes cannot fit is left out and named, rather than voiding the
        // surface: the expiries that do have their pillars still make a surface.
        if (!ssvi_has_enough(atm, risk_reversals, butterflies)) {
            std::string why;
            for (const auto& point : unquoted)
                why += (why.empty() ? "" : ", ") + point;
            out.unfitted_expiries.push_back(expiry.code +
                                            (quoted.pillars.empty() ?
                                                 ": no pillar quoted" :
                                                 ": too few pillars for SSVI without " + why));
            unfitted.insert(expiry.code);
            continue;
        }
        longest_fitted = std::max(longest_fitted, quoted.years);
        out.request.expiries.push_back(std::move(quoted));
    }
    if (out.request.expiries.size() < minimum_expiries) {
        std::string what = std::to_string(out.request.expiries.size()) + " expiries fitted, " +
                           std::to_string(minimum_expiries) + " needed";
        for (const auto& u : out.unfitted_expiries)
            what += "; " + u;
        return std::unexpected(what);
    }
    for (const auto& grid : checked.grid_expiries) {
        if (unfitted.contains(grid.code))
            continue;
        const auto terms = terms_at(recipe, forwards, by_date, grid.code, grid.period);
        if (!terms)
            return std::unexpected(terms.error());
        // The grid reaches no further than the longest expiry the fit covers.
        if (terms->years > longest_fitted)
            continue;
        out.request.grid_expiries.push_back({.expiry_id = grid.code,
                                             .years = terms->years,
                                             .market = terms->market,
                                             .convention = terms->convention});
    }
    out.request.grid_strikes = checked.grid_strikes;
    return out;
}

boost::uuids::uuid quote_fit_series() {
    return boost::uuids::uuid{{0x51,
                               0x75,
                               0x6f,
                               0x74,
                               0x65,
                               0x46,
                               0x69,
                               0x74,
                               0x00,
                               0x00,
                               0x00,
                               0x00,
                               0x00,
                               0x00,
                               0x00,
                               0x01}};
}

std::expected<quote_fit_recipe, std::string>
make_quote_fit_recipe(const std::string& base_currency,
                      const std::string& quote_currency,
                      const refdata::domain::fx_option_convention& convention,
                      const std::string& source_name,
                      const std::vector<quoted_surface_point>& quotes) {
    quote_fit_recipe out;
    auto& recipe = out.recipe;
    auto& blueprint = recipe.blueprint;
    blueprint.name = base_currency + quote_currency + " " + source_name;
    blueprint.base_currency_code = base_currency;
    blueprint.quote_currency_code = quote_currency;
    blueprint.fx_option_convention_id = convention.id;
    blueprint.smile_model = "ssvi";
    blueprint.expiry_interpolation = "linear_total_variance";
    recipe.convention = convention;

    const auto series = quote_fit_series();
    auto& held = out.quotes[series];
    std::vector<recipe_expiry> expiries;
    for (const auto& q : quotes) {
        if (!std::isfinite(q.volatility) ||
            !read_strike_label(pillar_key_of(q.expiry, q.label), q.label))
            continue;
        if (!std::ranges::any_of(expiries, [&](const auto& e) { return e.code == q.expiry; })) {
            const auto period = mapping::parse_tenor_text(q.expiry);
            if (!period)
                return std::unexpected("expiry " + q.expiry + ": " + period.error());
            expiries.push_back({q.expiry, *period});
        }
        if (held.contains({q.expiry, q.label}))
            continue;
        held[{q.expiry, q.label}] = q.volatility;
        refdata::domain::fx_vol_surface_pillar pillar;
        pillar.id = boost::uuids::uuid{};
        pillar.id.data[15] = static_cast<std::uint8_t>(recipe.pillars.size() & 0xFF);
        pillar.id.data[14] = static_cast<std::uint8_t>((recipe.pillars.size() >> 8) & 0xFF);
        pillar.id.data[0] = 0x70;
        pillar.expiry_code = q.expiry;
        pillar.strike_label = q.label;
        refdata::domain::fx_vol_surface_pillar_source source;
        source.pillar_id = pillar.id;
        source.market_series_id = series;
        source.contributor_code = source_name;
        source.weight = 1.0;
        recipe.pillars.push_back(std::move(pillar));
        recipe.sources.push_back(std::move(source));
    }
    if (recipe.pillars.empty())
        return std::unexpected(source_name +
                               " quotes no ATM, risk reversal or butterfly here, so there is no "
                               "smile to fit");
    std::ranges::sort(expiries, [](const auto& a, const auto& b) {
        return qd::compare_periods(a.period, b.period) < 0;
    });
    std::string codes;
    for (const auto& e : expiries)
        codes += (codes.empty() ? "" : ",") + e.code;
    blueprint.output_expiries = codes;
    const auto& type = convention.delta_type;
    blueprint.output_strikes = "DEL/" + type + "/Put/10,DEL/" + type + "/Put/25,ATM,DEL/" + type +
                               "/Call/25,DEL/" + type + "/Call/10";
    return out;
}

std::string delta_label(const std::string& strike) {
    const auto parsed = datum::strike::parse(strike);
    if (!parsed)
        return strike;
    if (std::holds_alternative<datum::atm_strike>(parsed->which()))
        return "ATM";
    const auto* delta = std::get_if<datum::delta_strike>(&parsed->which());
    if (!delta)
        return strike;
    return delta->delta.text() + (delta->option_type == "Call" ? "C" : "P");
}

std::expected<std::vector<ores::marketdata::messaging::fx_vol_source_fit>, std::string>
fit_quote_sources(const std::string& base_currency,
                  const std::string& quote_currency,
                  const refdata::domain::fx_option_convention& convention,
                  const std::vector<ores::marketdata::messaging::fx_vol_quote_source>& sources,
                  const quote_fit_forwards& forwards_of,
                  double pillar_tolerance) {
    if (sources.size() > most_fit_sources)
        return std::unexpected(std::format(
            "A fit reads at most {} sources, not {}.", most_fit_sources, sources.size()));
    for (const auto& source : sources)
        if (source.quotes.size() > most_fit_quotes)
            return std::unexpected(std::format("A source holds at most {} quotes; {} holds {}.",
                                               most_fit_quotes,
                                               source.name,
                                               source.quotes.size()));
    std::vector<ores::marketdata::messaging::fx_vol_source_fit> out;
    std::optional<surface_forward_set> forwards;
    for (const auto& source : sources) {
        ores::marketdata::messaging::fx_vol_source_fit fit;
        fit.name = source.name;
        try {
            std::vector<quoted_surface_point> quotes;
            quotes.reserve(source.quotes.size());
            for (const auto& q : source.quotes)
                quotes.push_back({q.expiry, q.label, q.volatility});
            const auto made = make_quote_fit_recipe(
                base_currency, quote_currency, convention, source.name, quotes);
            if (!made)
                throw std::invalid_argument(made.error());
            const auto checked = check_recipe(made->recipe);
            if (!checked)
                throw std::invalid_argument(checked.error());
            if (!forwards)
                forwards = forwards_of(made->recipe);
            const auto prepared = prepare_calibration(
                made->recipe, *checked, *forwards, made->quotes, pillar_tolerance, 1);
            if (!prepared)
                throw std::invalid_argument(prepared.error());
            const auto fitted = qs::calibrate_ssvi(prepared->request);
            if (!fitted)
                throw std::invalid_argument(fitted.error());
            for (const auto& point : fitted->grid)
                fit.points.push_back({.expiry = point.expiry_id,
                                      .strike = delta_label(point.strike_id),
                                      .volatility = point.volatility});
            fit.unfitted_expiries = prepared->unfitted_expiries;
            fit.fitted = true;
        } catch (const std::exception& e) {
            fit.message = e.what();
        }
        out.push_back(std::move(fit));
    }
    return out;
}

}
