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
#ifndef ORES_HTTP_ROUTES_TRADING_BOOKING_ROUTES_HPP
#define ORES_HTTP_ROUTES_TRADING_BOOKING_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/book.hpp"
#include "ores.trading.api/domain/trade_id_type.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.api/domain/trade_state.hpp"
#include <boost/uuid/uuid.hpp>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::trading {

/**
 * @brief Booking a trade: what the form offers, the links a person records
 * between trades, and the booking write itself, at the addresses and in the
 * shapes the BFF answered.
 *
 * The write is the trading service's operation, so the unit states a request
 * and reads the answer: the service decides whether the book is open, whether
 * the trade id is free and whether the instrument belongs to the trade type.
 * The browser states what only a person knows, and the gateway states the rest,
 * including the party the trade belongs to, which it reads from the caller's
 * own token.
 */
class booking_routes {
public:
    /**
     * @brief The acting party's open books, each as the form offers it: its id
     * and its name.
     *
     * A booking is stamped with the party the session acts for, and the service
     * refuses a book of any other party. A session whose reach is wider than
     * its party sees other parties' books in a plain tree read, so the filter
     * belongs here, where the acting party is known.
     */
    static std::vector<std::string>
    party_books_json(const std::vector<ores::refdata::domain::book>& books,
                     const std::string& party_id);

    /**
     * @brief The identifier schemes the form offers, sorted, each as a JSON
     * object naming its code.
     *
     * The tenant's own list decides. A tenant provisioned before that list was
     * copied has none, so the schemes its trades already use stand in: that is
     * worse than the list, and better than an empty picker that cannot book.
     */
    static std::vector<std::string>
    identifier_schemes_json(const std::vector<ores::trading::domain::trade_id_type>& declared,
                            const std::vector<ores::trading::domain::trade_identifier>& used);

    /**
     * @brief The activity of the newest state a trade holds, or nothing when
     * the trade has no state to record a link against.
     */
    static std::optional<boost::uuids::uuid>
    newest_activity(const std::vector<ores::trading::domain::trade_state>& states);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
