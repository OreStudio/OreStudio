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
#ifndef ORES_HTTP_ROUTES_TRADING_STRUCTURE_ROUTES_HPP
#define ORES_HTTP_ROUTES_TRADING_STRUCTURE_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.trading.api/domain/structure.hpp"
#include "ores.trading.api/domain/structure_member.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include <boost/uuid/uuid.hpp>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::trading {

/**
 * @brief The structure screens: the kinds a grouping may choose, the trades no
 * structure holds yet, the grouping write, the list, one structure's page with
 * the tree it sits in and its legs, and the leg writes.
 *
 * The store holds a structure's parent as an id and a leg as a member row, so
 * the tree a page draws is assembled here rather than read: the tenant's
 * structures and members are read once, the requested structure's root is found
 * by walking up, and the tree is built from that root down.
 */
class structure_routes {
public:
    /**
     * @brief How a kind confirms and prices, as the screen states it beside the
     * kind.
     */
    struct verdict final {
        std::string confirms;
        std::string prices;
    };

    /// How a kind confirms and prices: one document for a package's legs.
    static verdict verdicts_of(const std::string& kind);

    /**
     * @brief The words a type code reads as when the catalogue names it nothing.
     *
     * A desk reads "FX Barrier Option", not "FxBarrierOption", and a code drawn
     * as itself is a screen that looks unfinished.
     */
    static std::string humanised(const std::string& code);

    /// The asset class's own name, as the desk says it.
    static std::string asset_class_named(const std::string& code);

    /**
     * @brief The tree a structure belongs to, as the page draws it, or nothing
     * when the structure is not one the reads hold.
     *
     * A structure states its children as the structures naming it as parent and
     * its legs as the members naming it: the two are held in different places,
     * so both are looked up rather than assumed.
     */
    static std::string
    tree_json(const boost::uuids::uuid& structure_id,
              const std::vector<ores::trading::domain::structure>& structures,
              const std::vector<ores::trading::domain::structure_member>& members,
              const std::map<std::string, std::string>& structure_references,
              const std::map<std::string, std::string>& trade_references,
              const std::map<std::string, ores::trading::domain::trade>& trades,
              const std::map<std::string, std::string>& type_names,
              const std::map<std::string, std::string>& type_asset_classes);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
