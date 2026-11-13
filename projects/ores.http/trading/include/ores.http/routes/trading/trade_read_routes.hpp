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
#ifndef ORES_HTTP_ROUTES_TRADING_TRADE_READ_ROUTES_HPP
#define ORES_HTTP_ROUTES_TRADING_TRADE_READ_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/book.hpp"
#include "ores.refdata.api/domain/counterparty.hpp"
#include "ores.trading.api/domain/structure_identifier.hpp"
#include "ores.trading.api/domain/structure_member.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.api/domain/trade_link.hpp"
#include "ores.trading.api/domain/trade_summary.hpp"
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace ores::http::routes::trading {

/**
 * @brief The trade screens' reads: a page of the trade list, one trade's page,
 * and one trade as it stood at one version, at the addresses and in the shapes
 * the BFF answered.
 *
 * The trade summary read model is the only source of a page, its filters and
 * its total. Each row is enriched with what a person reads, and each
 * enrichment read is bounded by the page: the rows name the trades, books and
 * counterparties read by key, never by walking a whole list.
 */
class trade_read_routes {
public:
    /**
     * @brief The identifier a desk quotes for each trade: the internal reference,
     * then the regulatory ones, then the ORE document's own id.
     */
    static std::map<std::string, std::string>
    best_identifiers(const std::vector<ores::trading::domain::trade_identifier>& identifiers);

    /**
     * @brief The page's rows as JSON objects: each summary with its identifier,
     * its counterparty's and book's names, and its trade's versions, or null
     * versions for a row that is not a trade.
     */
    static std::vector<std::string>
    enriched_rows(const std::vector<ores::trading::domain::trade_summary>& summaries,
                  const std::vector<ores::trading::domain::trade_identifier>& identifiers,
                  const std::vector<ores::trading::domain::structure_identifier>& structures,
                  const std::vector<ores::refdata::domain::book>& books,
                  const std::vector<ores::refdata::domain::counterparty>& counterparties,
                  const std::vector<ores::trading::domain::trade>& trades);

    /**
     * @brief The structures a trade is a leg of, each named by its reference, as
     * the trade's page reads them.
     */
    static std::string
    structures_json(const std::vector<ores::trading::domain::structure_member>& members,
                    const std::vector<ores::trading::domain::structure_identifier>& identifiers);

    /**
     * @brief The links at one end of a trade, each with the identifier of the
     * trade at its other end.
     */
    static std::string links_json(const std::vector<ores::trading::domain::trade_link>& links,
                                  bool other_is_to,
                                  const std::map<std::string, std::string>& labels);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
