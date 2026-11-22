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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_AUTHORISATION_QUEUE_ROUTES_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_AUTHORISATION_QUEUE_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::workflow {

/**
 * @brief The Operations queue: the authorisation requests this person may
 * decide, the deal each one names, and the deal as it stands now.
 *
 * The inbox service picks which requests the person may decide. The deal is not
 * part of a request: trading keeps the pair, so the trades are joined here for
 * the page, with the books, counterparties and product types that name them.
 */
class authorisation_queue_routes final {
public:
    /// One row of the queue: the request, the deal it names, and the deal now.
    // compass:except(wide_types/over_ceiling) expires=2026-12-04 approver=marco reason=one queue row states the request and the deal it names, which #5027 joined in one struct
    struct queue_row final {
        std::string request_id;
        int request_version = 0;
        std::string kind_code;
        std::string state_code;
        std::string requested_by;
        std::string requested_at;
        std::string reason;
        std::string trade_id;
        std::string reference;
        int request_trade_version = 0;
        int trade_version = 0;
        std::string class_code;
        std::string book_id;
        std::string counterparty_id;
        std::string trade_type;
        std::string trade_type_name;
        std::string book;
        std::string counterparty;
        std::string trade_date;
        std::string status;
    };

    /// One row as the screen reads it. A deal that moved after its request was
    /// raised says so, and the row stays open rather than taking a lease on it.
    static std::string row_json(const queue_row& row);

    /// The queue's answer, which is not kept: it states what this person may decide now.
    static ores::http::domain::http_response
    queue_answer(const ores::http::domain::http_request& req, std::string body);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
