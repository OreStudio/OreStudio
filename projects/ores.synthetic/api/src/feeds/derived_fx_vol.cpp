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
#include "ores.synthetic.api/feeds/derived_fx_vol.hpp"
#include <algorithm>
#include <charconv>
#include <cmath>
#include <format>
#include <map>
#include <ranges>

namespace ores::synthetic::feed {

namespace {

namespace qm = analytics::quant::math;

/// The length of a tenor such as 2Y or 6M in years, or nothing for one that is not.
std::optional<double> years_of_tenor(std::string_view tenor) {
    if (tenor.size() < 2)
        return std::nullopt;
    int count = 0;
    const auto [end, ec] = std::from_chars(tenor.data(), tenor.data() + tenor.size() - 1, count);
    if (ec != std::errc{} || end != tenor.data() + tenor.size() - 1 || count < 0)
        return std::nullopt;
    switch (tenor.back()) {
        case 'D':
            return count / 365.0;
        case 'W':
            return count * 7.0 / 365.0;
        case 'M':
            return count / 12.0;
        case 'Y':
            return static_cast<double>(count);
        default:
            return std::nullopt;
    }
}

std::expected<qm::fx_atm_type, std::string> atm_of(std::string_view s) {
    if (s == "AtmDeltaNeutral")
        return qm::fx_atm_type::delta_neutral;
    if (s == "AtmFwd")
        return qm::fx_atm_type::forward;
    return std::unexpected("the ATM type '" + std::string(s) +
                           "' is not one the derivation quotes");
}

std::expected<qm::fx_delta_type, std::string> delta_of(std::string_view s) {
    if (s == "Spot")
        return qm::fx_delta_type::spot;
    if (s == "Fwd")
        return qm::fx_delta_type::forward;
    if (s == "PaSpot")
        return qm::fx_delta_type::premium_adjusted_spot;
    if (s == "PaFwd")
        return qm::fx_delta_type::premium_adjusted_forward;
    return std::unexpected("the delta type '" + std::string(s) +
                           "' is not one the derivation quotes");
}

/// The pair's log level on each date, keyed by date so the legs align.
std::map<std::chrono::year_month_day, double> log_levels(const dated_series& series) {
    std::map<std::chrono::year_month_day, double> out;
    const auto n = std::min(series.dates.size(), series.values.size());
    for (std::size_t i = 0; i < n; ++i)
        if (series.values[i] > 0.0 && std::isfinite(series.values[i]))
            out.insert_or_assign(series.dates[i], std::log(series.values[i]));
    return out;
}

}

std::string derived_fx_external_id(const derived_fx_quote_id& id) {
    return id.pair.base + id.pair.quote + "/" + id.expiry + "/" + id.label;
}

std::optional<derived_fx_quote_id> parse_derived_fx_external_id(std::string_view external_id) {
    const auto first = external_id.find('/');
    if (first != 6)
        return std::nullopt;
    const auto second = external_id.find('/', first + 1);
    if (second == std::string_view::npos ||
        external_id.find('/', second + 1) != std::string_view::npos)
        return std::nullopt;
    const auto expiry = external_id.substr(first + 1, second - first - 1);
    const auto label = external_id.substr(second + 1);
    const bool known_expiry = std::ranges::any_of(
        derived_fx_expiries, [&](const derived_fx_expiry& e) { return e.label == expiry; });
    const bool known_label = std::ranges::contains(derived_fx_labels, label);
    if (!known_expiry || !known_label)
        return std::nullopt;
    return derived_fx_quote_id{.pair = {.base = std::string(external_id.substr(0, 3)),
                                        .quote = std::string(external_id.substr(3, 3))},
                               .expiry = std::string(expiry),
                               .label = std::string(label)};
}

std::string fx_option_convention_id(const derived_fx_pair& pair) {
    return pair.base + "-" + pair.quote + "-FXOPTION";
}

std::string ecb_fixing_item(std::string_view currency) {
    return "oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=" + std::string(currency);
}

pair_legs ecb_legs_of(const derived_fx_pair& pair) {
    pair_legs out;
    if (pair.quote != "EUR")
        out.plus = ecb_fixing_item(pair.quote);
    if (pair.base != "EUR")
        out.minus = ecb_fixing_item(pair.base);
    return out;
}

std::expected<pair_returns, std::string>
pair_log_returns(const std::optional<dated_series>& plus,
                 const std::optional<dated_series>& minus) {
    if (!plus && !minus)
        return std::unexpected("a pair needs at least one leg");
    std::map<std::chrono::year_month_day, double> level;
    if (plus && minus) {
        const auto up = log_levels(*plus);
        const auto down = log_levels(*minus);
        for (const auto& [date, value] : up)
            if (const auto other = down.find(date); other != down.end())
                level.emplace(date, value - other->second);
    } else if (plus) {
        level = log_levels(*plus);
    } else {
        for (const auto& [date, value] : log_levels(*minus))
            level.emplace(date, -value);
    }
    pair_returns out;
    out.returns.reserve(level.empty() ? 0 : level.size() - 1);
    for (auto it = level.begin(); it != level.end(); ++it)
        if (const auto next = std::next(it); next != level.end())
            out.returns.push_back(next->second - it->second);
    if (!level.empty())
        out.last = level.rbegin()->first;
    return out;
}

std::expected<qm::fx_quote_convention, std::string>
quote_convention_at(const refdata::domain::fx_option_convention& convention, double years) {
    std::string_view atm = convention.atm_type;
    std::string_view delta = convention.delta_type;
    if (convention.switch_tenor) {
        const auto switch_years = years_of_tenor(*convention.switch_tenor);
        if (!switch_years)
            return std::unexpected("the switch tenor '" + *convention.switch_tenor +
                                   "' is not a tenor");
        if (*switch_years > 0.0 && years >= *switch_years) {
            if (convention.long_term_atm_type)
                atm = *convention.long_term_atm_type;
            if (convention.long_term_delta_type)
                delta = *convention.long_term_delta_type;
        }
    }
    const auto atm_type = atm_of(atm);
    if (!atm_type)
        return std::unexpected(atm_type.error());
    const auto delta_type = delta_of(delta);
    if (!delta_type)
        return std::unexpected(delta_type.error());
    const std::string favour = convention.risk_reversal_in_favor_of.value_or("Call");
    if (favour != "Call" && favour != "Put")
        return std::unexpected("a risk reversal is in favour of Call or Put, not '" + favour + "'");
    const std::string style = convention.butterfly_style.value_or("Smile");
    if (style != "Broker" && style != "Smile")
        return std::unexpected("a butterfly is Broker or Smile, not '" + style + "'");
    return qm::fx_quote_convention{.atm = *atm_type,
                                   .delta = *delta_type,
                                   .favour = favour == "Call" ? qm::fx_risk_reversal_favour::call :
                                                                qm::fx_risk_reversal_favour::put,
                                   .butterfly = style == "Broker" ? qm::fx_butterfly_style::broker :
                                                                    qm::fx_butterfly_style::smile};
}

std::vector<marketdata::messaging::adapter_tick>
derived_fx_ticks(const derived_fx_pair& pair,
                 const qm::fx_derivation& derivation,
                 std::chrono::year_month_day day,
                 std::uint64_t first_sequence) {
    using namespace std::chrono;
    const auto stamp = sys_days{day} + hours(17);
    std::vector<marketdata::messaging::adapter_tick> out;
    auto sequence = first_sequence;
    for (const auto& expiry : derivation.expiries) {
        if (!expiry)
            continue;
        const auto& q = expiry->quotes;
        const std::array<double, derived_fx_labels.size()> values{
            q.atm, q.rr25, q.bf25, q.rr10, q.bf10};
        for (std::size_t i = 0; i < derived_fx_labels.size(); ++i) {
            if (!std::isfinite(values[i]))
                continue;
            out.push_back({.id_scheme = std::string(derived_fx_id_scheme),
                           .external_id =
                               derived_fx_external_id({.pair = pair,
                                                       .expiry = expiry->label,
                                                       .label = std::string(derived_fx_labels[i])}),
                           .field = std::string(derived_fx_field),
                           .value = std::format("{}", values[i]),
                           .vendor_time = stamp,
                           .sequence = sequence++});
        }
    }
    return out;
}

}
