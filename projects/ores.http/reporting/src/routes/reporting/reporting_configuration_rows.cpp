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
#include "ores.http/routes/reporting/reporting_configuration_rows.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/logger.hpp"
#include "ores.reporting.api/messaging/configuration_protocol.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <exception>
#include <string>

namespace ores::http::routes::reporting::reporting_configuration_rows {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::utility::domain::change_intent;
using ores::utility::domain::precondition_kind;
using ores::nats::service::nats_client;

auto& configuration_lg() {
    static auto instance =
        ores::logging::make_logger("ores.http.reporting.reporting_configuration");
    return instance;
}

/// The party a token states when the session chose none.
constexpr std::string_view nil_party = "00000000-0000-0000-0000-000000000000";

}

change_intent intent_or_none(const rr::stated_intent& stated) {
    return rr::intent_of(stated).value_or(change_intent{});
}

boost::uuids::uuid party_for_new(const http_request& req,
                                 const std::optional<std::string>& asked,
                                 std::string_view when_none) {
    if (asked && !asked->empty()) {
        if (!rr::is_uuid(*asked))
            rr::invalid("A configuration names its party by its id.");
        return boost::uuids::string_generator()(*asked);
    }
    if (req.authenticated_user && req.authenticated_user->party_id) {
        const auto& party = *req.authenticated_user->party_id;
        if (!party.empty() && party != nil_party)
            return boost::uuids::string_generator()(party);
    }
    rr::invalid(std::string(when_none));
}

boost::uuids::uuid register_configuration(const http_request& req,
                                          nats_client& session,
                                          const std::string& name,
                                          std::string_view type_code,
                                          std::string_view owning_component,
                                          const change_intent& intent) {
    reporting_messaging::put_configuration_request msg;
    msg.change.write.id = boost::uuids::random_generator()();
    msg.change.write.name = name;
    msg.change.write.configuration_type_code = std::string(type_code);
    msg.change.write.owning_component = std::string(owning_component);
    msg.change.precondition.kind = precondition_kind::must_not_exist;
    msg.intent = intent;
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    return msg.change.write.id;
}

void remove_configuration(const http_request& req,
                          nats_client& session,
                          const std::string& name,
                          const change_intent& intent) {
    try {
        reporting_messaging::delete_configuration_request msg;
        msg.removal.key.name = name;
        msg.removal.precondition.kind = precondition_kind::any;
        msg.intent = intent;
        forwarding::forward(req, session, msg);
    } catch (const std::exception& e) {
        // The create's own failure is the one the caller reads.
        ORES_LOG_SEV(configuration_lg(), ores::logging::warn)
            << "The configuration " << name
            << " a refused create wrote could not be removed, so it stays: " << e.what();
    }
}

}
