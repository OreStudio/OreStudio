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
#ifndef ORES_HTTP_ROUTES_WORKFLOW_INBOX_VIEW_ROUTES_HPP
#define ORES_HTTP_ROUTES_WORKFLOW_INBOX_VIEW_ROUTES_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include <memory>
#include <string>

namespace ores::http::routes::workflow {

/**
 * @brief The approval inbox: every open request this person may decide, with the
 * tail of those answered and still in view.
 *
 * The queue is the inbox service's own read; the names, the decisions, the roles
 * and the trades are joined beside it, one read each, and a read that cannot be
 * made leaves its name out of the row rather than failing the page.
 */
class inbox_view_routes final {
public:
    /// The inbox's answer, kept fresh for fifteen seconds.
    static ores::http::domain::http_response
    inbox_answer(const ores::http::domain::http_request& req, std::string body);

    /**
     * @brief The curve publish a request authorises, as its row carries it.
     *
     * Stated on its own so a test can read the shape without a service behind it:
     * the members are the browser's, and the instant is the wire's text.
     */
    static std::string
    published_subject_json(const ores::marketdata::messaging::published_curve_set& set);

    /// The cut a request authorises, as its row carries it.
    static std::string
    cut_subject_json(const ores::reporting::messaging::cut_authorisation_subject& subject);

    static void register_routes(std::shared_ptr<ores::http::net::router> router,
                                std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                ores::nats::service::nats_client& session);
};

}

#endif
