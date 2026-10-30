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
#ifndef ORES_MARKETDATA_SERVICE_APP_DEMAND_CALCULATOR_HPP
#define ORES_MARKETDATA_SERVICE_APP_DEMAND_CALCULATOR_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include "ores.marketdata.api/domain/feed.hpp"
#include "ores.marketdata.api/domain/market_data_mapping.hpp"
#include "ores.marketdata.api/domain/market_data_vendor_instrument.hpp"
#include "ores.marketdata.api/domain/provider.hpp"
#include "ores.marketdata.api/domain/route.hpp"
#include "ores.marketdata.api/domain/subscription.hpp"
#include "ores.marketdata.api/messaging/adapter_operation_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <functional>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief What the demand of the adapters is computed from.
 */
struct demand_catalogue {
    /// The providers, whose terms decide whether their sources are asked for anything.
    std::vector<domain::provider> providers;
    std::vector<domain::feed> feeds;
    std::vector<domain::market_data_vendor_instrument> instruments;
    std::vector<domain::market_data_mapping> mappings;
    std::vector<domain::route> routes;
    std::vector<domain::subscription> subscriptions;
};

/**
 * @brief The items a party needs from a provider, as oresmd URIs.
 *
 * A function that returns nothing leaves the party with every item of the provider that a
 * mapping reaches. A function that returns a set restricts the party to it, and the inputs of
 * a composite are added to the set.
 */
struct demand_policy {
    std::function<std::optional<std::set<std::string>>(const boost::uuids::uuid& party_id,
                                                       const std::string& provider_code)>
        requirements;
    std::function<std::vector<std::string>(const std::string& oresmd_uri)> composite_inputs;
};

/**
 * @brief A routed party that holds no right to the provider of its route.
 */
struct refused_demand {
    std::string adapter_id;
    boost::uuids::uuid party_id;
    std::string reason;
};

/**
 * @brief What one route demands: the items of its party and the vendor ids that name them.
 *
 * A route whose party holds no right to the provider, or whose provider's terms do not let its
 * data be fetched, is not entitled, demands nothing, and says why.
 */
struct party_demand {
    ores::utility::uuid::tenant_id tenant_id = ores::utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id;
    std::string provider_code;
    boost::uuids::uuid feed_id;
    /// The name of the source, which is the id of its adapter.
    std::string adapter_id;
    bool entitled = false;
    /// Why the route is not entitled: @c not_entitled or @c terms_not_accepted. Empty when it
    /// is.
    std::string refusal;
    /// The vendor ids that name each item, by the oresmd URI of the item.
    std::map<std::string, std::vector<messaging::vendor_id>> items;
};

/**
 * @brief The demand set of each adapter, and the parties refused.
 */
struct demand_result {
    /// The vendor ids to publish, by adapter id. An adapter with routes and no demand has an
    /// empty set, so it still receives a demand that replaces its last one.
    std::map<std::string, std::vector<messaging::vendor_id>> by_adapter;
    std::vector<refused_demand> refused;
};

/**
 * @brief Computes what each route demands, one entry for each route whose source the catalogue
 * holds.
 *
 * The rules are those of @c compute_demand(), which sums this by adapter.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::vector<party_demand>
compute_party_demand(const demand_catalogue& catalogue,
                     std::chrono::system_clock::time_point now,
                     const demand_policy& policy = {});

/**
 * @brief Computes the demand set of each adapter.
 *
 * The set of an adapter is the union, over the parties routed to its source, of the vendor ids
 * of the provider whose mapping holds at @p now and whose item the party needs. A route whose
 * party holds no subscription to the provider with at least one right adds nothing and is
 * reported as refused with the reason @c not_entitled. The adapter id is the name of the source.
 * The set is sorted and holds no id twice.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT demand_result
compute_demand(const demand_catalogue& catalogue,
               std::chrono::system_clock::time_point now,
               const demand_policy& policy = {});

}

#endif
