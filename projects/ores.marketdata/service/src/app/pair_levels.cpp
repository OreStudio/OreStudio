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
#include "ores.marketdata.service/app/pair_levels.hpp"
#include "ores.analytics.quant/domain/staleness_policy.hpp"
#include "ores.analytics.quant/service/fixing_replay.hpp"
#include "ores.analytics.quant/service/topology_builder.hpp"
#include <algorithm>
#include <charconv>
#include <chrono>
#include <cstddef>
#include <exception>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

namespace {

namespace quant = ores::analytics::quant;
using ores::utility::domain::outcome;
using ores::utility::domain::result;

result refusal(outcome why, std::string code, std::string message) {
    return {.outcome = why, .code = std::move(code), .message = std::move(message), .fields = {}};
}

/**
 * The most calendar days a side's last fixing may fall before the window's last date.
 *
 * A publisher fixes on its own business days, so a weekend and the longest holiday
 * closure fit inside a week. Past it the publisher has stopped fixing the currency,
 * and a level read from its last fixing would be an old rate stated as a new one.
 */
constexpr std::chrono::days fixing_gap{7};

std::optional<double> number_of(const std::string& text) {
    double value = 0.0;
    const auto [end, ec] = std::from_chars(text.data(), text.data() + text.size(), value);
    if (ec != std::errc{} || end != text.data() + text.size())
        return std::nullopt;
    return value;
}

}

::ores::marketdata::messaging::get_pair_levels_response
compose_pair_levels(const party_fixing_sources& party,
                    const ::ores::marketdata::messaging::get_pair_levels_request& request,
                    const fixing_window_reader& read) {
    ::ores::marketdata::messaging::get_pair_levels_response response;
    const auto& base = request.base_currency_code;
    const auto& quote = request.quote_currency_code;
    const auto pair_name = base + "/" + quote;

    if (base.empty() || quote.empty() || base == quote) {
        response.result = refusal(outcome::invalid,
                                  "pair_invalid",
                                  std::format("{} is not a pair of two currencies.", pair_name));
        return response;
    }
    if (!party.refusal.empty()) {
        response.result = refusal(outcome::failed, "fixing_sources_invalid", party.refusal);
        return response;
    }

    const auto chosen = quant::service::sources_for_pairs(
        party.sources, {{.base_code = base, .quote_code = quote, .is_driver = false}});
    if (!chosen) {
        response.result = refusal(outcome::failed, "fixing_sources_invalid", chosen.error());
        return response;
    }
    if (chosen->choices.empty()) {
        response.result = refusal(
            outcome::missing,
            "pair_unserved",
            std::format("No fixing source of the party quotes both sides of {}.", pair_name));
        return response;
    }
    const auto& choice = chosen->choices.front();
    response.provider_code = choice.provider_code;
    response.pivot_currency_code = choice.pivot_code;

    std::vector<std::string> legs;
    for (const auto& code : {base, quote})
        if (code != choice.pivot_code)
            legs.push_back(code);

    // Each leg's fixings by day; a day enters the replay only when every leg was fixed on
    // it, so a missing fixing is a missing level and never yesterday's rate restated.
    std::map<std::chrono::sys_days, std::vector<quant::domain::driver_quote>> by_day;
    std::map<std::chrono::sys_days, std::size_t> legs_fixed;
    for (const auto& leg : legs) {
        const auto item = party.items.find({choice.provider_code, leg});
        if (item == party.items.end()) {
            response.result =
                refusal(outcome::failed,
                        "fixing_item_missing",
                        std::format("{} is chosen for {} but names no fixing series of {}.",
                                    choice.provider_code,
                                    pair_name,
                                    leg));
            return response;
        }
        const auto window = read(choice.provider_code, item->second);
        if (window.result.outcome != outcome::ok) {
            response.result = window.result;
            return response;
        }
        const auto n = std::min(window.dates.size(), window.values.size());
        std::optional<std::chrono::sys_days> last_fixed;
        for (std::size_t i = 0; i < n; ++i)
            if (number_of(window.values[i]))
                last_fixed = std::max(last_fixed.value_or(std::chrono::sys_days{}),
                                      std::chrono::sys_days{window.dates[i]});
        if (!last_fixed || std::chrono::sys_days{request.to} - *last_fixed > fixing_gap) {
            response.result = refusal(
                outcome::missing,
                "fixing_stopped",
                last_fixed ? std::format("{} last fixed {} on {}, so {} has no level as of {}.",
                                         choice.provider_code,
                                         leg,
                                         std::chrono::year_month_day{*last_fixed},
                                         pair_name,
                                         request.to) :
                             std::format("{} has no fixing of {} in the window, so {} has no "
                                         "level as of {}.",
                                         choice.provider_code,
                                         leg,
                                         pair_name,
                                         request.to));
            return response;
        }
        std::set<std::chrono::sys_days> seen;
        for (std::size_t i = 0; i < n; ++i) {
            const auto value = number_of(window.values[i]);
            if (!value)
                continue;
            const auto day = std::chrono::sys_days{window.dates[i]};
            if (!seen.insert(day).second)
                continue;
            by_day[day].push_back({.base_code = choice.pivot_code,
                                   .quote_code = leg,
                                   .rate = *value,
                                   .observed_at = {}});
            ++legs_fixed[day];
        }
    }

    std::vector<quant::service::fixing_day> days;
    days.reserve(by_day.size());
    for (auto& [day, quotes] : by_day)
        if (legs_fixed[day] == legs.size())
            days.push_back({.day = std::chrono::year_month_day{day}, .quotes = std::move(quotes)});

    std::vector<quant::domain::ccy_pair_input> edges;
    for (const auto& leg : legs)
        edges.push_back({.base_code = choice.pivot_code, .quote_code = leg, .is_driver = true});
    const std::vector<std::pair<std::string, std::string>> pairs{{base, quote}};

    try {
        const auto topology = quant::service::topology_builder::build(edges, choice.pivot_code, {});
        // Every day replayed holds a fixing of each leg, so a level is fresh on the day it is
        // read; a day apart is the bound that keeps it so.
        const auto replayed = quant::service::replay_fixings(
            topology, days, pairs, quant::domain::staleness_policy{std::chrono::hours{24}});
        if (!replayed) {
            response.result = refusal(outcome::failed, "replay_failed", replayed.error());
            return response;
        }
        for (const auto& level : replayed->front().levels) {
            if (level.status != quant::domain::rate_status::fresh)
                continue;
            response.dates.push_back(level.day);
            response.levels.push_back(level.rate);
        }
    } catch (const std::exception& e) {
        response.result = refusal(outcome::failed,
                                  "topology_invalid",
                                  std::format("The fixings of {} do not form a topology for {}: {}",
                                              choice.provider_code,
                                              pair_name,
                                              e.what()));
    }
    return response;
}

}
