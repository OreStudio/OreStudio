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
#include "ores.marketdata.service/app/fixing_sources.hpp"
#include "ores.marketdata.core/repository/market_series_identity_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/provider_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <map>
#include <set>

namespace ores::marketdata::service::app {

namespace {

using keys = std::pair<std::string, std::string>;

/// The rank a publisher carries, and the default when the store does not rank it.
int precedence_of(const std::map<std::string, int>& ranks, const std::string& provider_code) {
    const auto it = ranks.find(provider_code);
    return it == ranks.end() ? 1000 : it->second;
}

/// The identity fields that make a fixing series a source of rates.
bool is_fx_fixing(const domain::market_series_identity& identity) {
    return identity.identity_kind == "index" && identity.asset_class == "fx";
}

}

std::expected<std::map<keys, party_fixing_sources>, std::string>
read_fixing_sources(const ores::database::context& ctx) {
    const auto series = repository::market_series_repository{}.read_latest(ctx);

    std::map<boost::uuids::uuid, const domain::market_series*> series_by_id;
    for (const auto& s : series)
        series_by_id.emplace(s.id, &s);

    std::map<std::string, int> ranks;
    for (const auto& provider : repository::provider_repository{}.read_latest(ctx))
        ranks.emplace(provider.code, provider.precedence);

    // The identity table is read a page at a time; a page boundary is not a fault, so
    // paging stops when a short page arrives.
    constexpr std::uint32_t page = 1000;
    std::vector<domain::market_series_identity> identities;
    for (std::uint32_t offset = 0;; offset += page) {
        auto buffer =
            repository::market_series_identity_repository{}.read_latest(ctx, offset, page);
        const auto found = buffer.size();
        for (auto& identity : buffer)
            identities.push_back(std::move(identity));
        if (found < page)
            break;
    }

    // (tenant, party) -> publisher -> the base that publisher quotes against, and the
    // currencies it quotes. A publisher that quotes against two bases for one party is
    // a fault, so it is recorded rather than resolved by guessing.
    struct grouped {
        std::string base;
        std::set<std::string> currencies;
        std::string second_base;
        std::map<std::string, std::string> items;
    };
    struct party_groups {
        std::string refusal;
        std::map<std::string, grouped> by_provider;
    };
    std::map<keys, party_groups> per_party;

    for (const auto& identity : identities) {
        if (!is_fx_fixing(identity))
            continue;

        const auto found = series_by_id.find(identity.series_id);
        if (found == series_by_id.end())
            continue;
        const auto& market = *found->second;
        const auto key =
            keys{market.tenant_id.to_string(), boost::uuids::to_string(market.party_id)};
        auto& party = per_party[key];

        if (identity.ccy.empty() || identity.unit_ccy.empty()) {
            if (party.refusal.empty())
                party.refusal = std::format(
                    "The fixing series {} states ccy='{}' and unit_ccy='{}', so it says nothing "
                    "about which currency it prices in.",
                    market.oresmd_uri,
                    identity.ccy,
                    identity.unit_ccy);
            continue;
        }

        auto& source = party.by_provider[market.provider_code];
        if (source.base.empty())
            source.base = identity.unit_ccy;
        else if (source.base != identity.unit_ccy && source.second_base.empty())
            source.second_base = identity.unit_ccy;
        source.currencies.insert(identity.ccy);
        source.items.emplace(identity.ccy, market.oresmd_uri);
    }

    std::map<keys, party_fixing_sources> out;
    for (const auto& [key, party] : per_party) {
        auto& written = out[key];
        written.refusal = party.refusal;
        for (const auto& [provider_code, grouped_source] : party.by_provider) {
            if (!grouped_source.second_base.empty()) {
                written.refusal = std::format(
                    "The {} fixing series of this party quote against {} and {} at once, so no "
                    "one base cancels.",
                    provider_code,
                    grouped_source.base,
                    grouped_source.second_base);
                written.sources.clear();
                written.items.clear();
                break;
            }
            for (const auto& [ccy, item] : grouped_source.items)
                written.items.emplace(std::pair{provider_code, ccy}, item);
            written.sources.push_back({.provider_code = provider_code,
                                       .base_code = grouped_source.base,
                                       .precedence = precedence_of(ranks, provider_code),
                                       .currencies = {grouped_source.currencies.begin(),
                                                      grouped_source.currencies.end()}});
        }
    }
    return out;
}

}
