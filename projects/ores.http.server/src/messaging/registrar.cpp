/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.server/messaging/registrar.hpp"
#include "ores.http.server/messaging/http_info_handler.hpp"
#include "ores.http.api/messaging/http_info_protocol.hpp"
#include "ores.logging/make_logger.hpp"

namespace {
inline auto& registrar_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.http.server.messaging.registrar");
    return instance;
}
} // namespace

namespace ores::http_server::messaging {

using namespace ores::logging;

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                              ores::security::jwt::jwt_authenticator verifier,
                              const std::string& base_url) {
    auto handler = std::make_shared<http_info_handler>(
        nats, std::move(verifier), base_url);

    std::vector<ores::nats::service::subscription> subs;
    subs.push_back(nats.subscribe(
        std::string(http::messaging::get_http_info_request::nats_subject),
        [handler](ores::nats::message msg) {
            handler->get(std::move(msg));
        }));

    BOOST_LOG_SEV(registrar_lg(), info)
        << "Registered NATS handler for "
        << http::messaging::get_http_info_request::nats_subject
        << " (base_url=" << base_url << ")";

    return subs;
}

} // namespace ores::http_server::messaging
