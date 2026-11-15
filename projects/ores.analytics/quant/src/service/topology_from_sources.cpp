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
#include <set>

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

}

std::expected<source_topology, std::string>
topology_from_sources(const std::vector<fixing_source>& sources) {
    if (sources.empty())
        return std::unexpected("No fixing source was offered to build a topology from.");

    std::vector<const fixing_source*> ordered;
    ordered.reserve(sources.size());
    for (const auto& source : sources)
        ordered.push_back(&source);
    std::ranges::sort(ordered, [](const fixing_source* lhs, const fixing_source* rhs) {
        if (lhs->precedence != rhs->precedence)
            return lhs->precedence < rhs->precedence;
        return lhs->provider_code < rhs->provider_code;
    });

    std::string refused;
    for (const auto* source : ordered) {
        if (source->provider_code.empty())
            return std::unexpected("A fixing source was offered with no provider code.");
        if (source->base_code.empty())
            return std::unexpected(
                std::format("The {} fixing source quotes nothing against a base currency.",
                            source->provider_code));

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

}
