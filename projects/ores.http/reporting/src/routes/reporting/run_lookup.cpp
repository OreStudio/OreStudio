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
#include "ores.http/routes/reporting/run_lookup.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <vector>

namespace ores::http::routes::reporting {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;

std::optional<ores::reporting::domain::report_instance>
run_of(const ores::http::domain::http_request& req,
       ores::nats::service::nats_client& session,
       const std::string& id) {
    reporting_messaging::list_report_instances_request msg;
    msg.limit = 1;
    msg.filter.emplace();
    msg.filter->id_one_of = std::vector{boost::uuids::string_generator()(id)};
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_done(reply.result);
    if (reply.instances.empty())
        return std::nullopt;
    return reply.instances.front();
}

}
