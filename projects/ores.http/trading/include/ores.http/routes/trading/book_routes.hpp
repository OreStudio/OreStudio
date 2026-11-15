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
#ifndef ORES_HTTP_ROUTES_TRADING_BOOK_ROUTES_HPP
#define ORES_HTTP_ROUTES_TRADING_BOOK_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/messaging/book_proposal_operations_protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace ores::http::routes::trading {

/**
 * @brief The book screens: one book and the portfolio it sits in, the rights at
 * a portfolio node, the book and portfolio writes, and the book change proposal.
 *
 * A write states the row and the version the person read: no version claims the
 * row is new, and a version claims the row is still the one read, so a write
 * made meanwhile is refused instead of lost.
 */
class book_routes {
public:
    /**
     * @brief A preview of the proposed writes, as the screen reads it: each
     * line's number, operation, subject and refusal, the columns it changes, and
     * the parts the policy names for it, with the parts for the lines together.
     */
    static std::string
    preview_json(const std::vector<ores::refdata::messaging::book_line_outcome>& lines,
                 const std::vector<std::string>& part_codes);

    /**
     * @brief A raise of the proposed writes: the preview, and the request that
     * holds them or why nothing was raised.
     */
    static std::string
    raise_json(const std::vector<ores::refdata::messaging::book_line_outcome>& lines,
               const std::vector<std::string>& part_codes,
               bool raised,
               const std::optional<boost::uuids::uuid>& request_id,
               const std::string& refusal_code,
               const std::string& refusal_message);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
