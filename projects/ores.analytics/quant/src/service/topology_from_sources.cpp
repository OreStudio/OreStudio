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
#include "ores.analytics.quant/service/topology_from_sources.hpp"
#include <algorithm>
#include <format>
#include <optional>
#include <set>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

/// The currencies a source quotes, in the order they would be read, with the base
/// itself removed: a series of the base against the base quotes nothing.
std::vector<std::string> quoted_by(const fixing_source& source) {
    std::set<std::string> distinct;
    for (const auto& currency : source.currencies)
        if (!currency.empty() && currency != source.base_code)
            distinct.insert(currency);
    return {distinct.begin(), distinct.end()};
}

/// The sources in the order resolution reads them: the rank first, then the code,
/// so an equal rank is broken by a name rather than by the order of the input.
std::vector<const fixing_source*> ranked(const std::vector<fixing_source>& sources) {
    std::vector<const fixing_source*> ordered;
    ordered.reserve(sources.size());
    for (const auto& source : sources)
        ordered.push_back(&source);
    std::ranges::sort(ordered, [](const fixing_source* lhs, const fixing_source* rhs) {
        if (lhs->precedence != rhs->precedence)
            return lhs->precedence < rhs->precedence;
        return lhs->provider_code < rhs->provider_code;
    });
    return ordered;
}

/// Why a source states too little for a star to be rooted at it, or nothing when it
/// states a provider and a base.
std::optional<std::string> unstated(const fixing_source& source) {
    if (source.provider_code.empty())
        return "A fixing source was offered with no provider code.";
    if (source.base_code.empty())
        return std::format("The {} fixing source quotes nothing against a base currency.",
                           source.provider_code);
    return std::nullopt;
}

/// The currencies whose rate a source can express: its base, and every currency it
/// quotes against that base.
std::set<std::string> expressible_by(const fixing_source& source) {
    auto held = std::set<std::string>{source.base_code};
    for (const auto& currency : quoted_by(source))
        held.insert(currency);
    return held;
}

}

std::expected<source_topology, std::string>
topology_from_sources(const std::vector<fixing_source>& sources) {
    if (sources.empty())
        return std::unexpected("No fixing source was offered to build a topology from.");

    const auto ordered = ranked(sources);
    std::string refused;
    for (const auto* source : ordered) {
        if (const auto why = unstated(*source); why)
            return std::unexpected(*why);

        const auto quotes = quoted_by(*source);
        if (quotes.empty()) {
            refused += (refused.empty() ? "" : "; ") + std::format("{} quotes nothing against {}",
                                                                   source->provider_code,
                                                                   source->base_code);
            continue;
        }

        source_topology out{
            .provider_code = source->provider_code, .pivot_code = source->base_code, .pairs = {}};
        out.pairs.reserve(quotes.size());
        for (const auto& currency : quotes)
            out.pairs.push_back(
                {.base_code = source->base_code, .quote_code = currency, .is_driver = true});
        return out;
    }

    return std::unexpected(
        std::format("No fixing source quotes enough currencies against one base to anchor a "
                    "topology: {}.",
                    refused.empty() ? "none was offered" : refused));
}

std::expected<pair_sources, std::string>
sources_for_pairs(const std::vector<fixing_source>& sources,
                  const std::vector<domain::ccy_pair_input>& pairs) {
    if (pairs.empty())
        return std::unexpected("No pair was offered to choose a fixing source for.");
    if (sources.empty())
        return std::unexpected("No fixing source was offered to choose a source from.");

    for (const auto& source : sources)
        if (const auto why = unstated(source); why)
            return std::unexpected(*why);

    for (const auto& pair : pairs) {
        if (pair.base_code.empty() || pair.quote_code.empty())
            return std::unexpected("A pair was offered with one side left empty.");
        if (pair.base_code == pair.quote_code)
            return std::unexpected(
                std::format("The pair {}/{} names one currency twice, so it has no rate.",
                            pair.base_code,
                            pair.quote_code));
    }

    // What each source can express is worked out once and not once per pair, because a
    // party asks for many pairs over the same few sources.
    std::vector<std::pair<const fixing_source*, std::set<std::string>>> candidates;
    for (const auto* source : ranked(sources))
        candidates.emplace_back(source, expressible_by(*source));

    pair_sources out;
    out.choices.reserve(pairs.size());
    for (const auto& pair : pairs) {
        const auto found = std::ranges::find_if(candidates, [&](const auto& candidate) {
            const auto& held = candidate.second;
            return held.contains(pair.base_code) && held.contains(pair.quote_code);
        });
        if (found == candidates.end()) {
            out.unserved.push_back(pair);
            continue;
        }
        out.choices.push_back({.provider_code = found->first->provider_code,
                               .pivot_code = found->first->base_code,
                               .pair = pair});
    }
    return out;
}

}
