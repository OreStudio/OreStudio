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
#ifndef ORES_HTTP_SERVER_MESSAGING_HTTP_INFO_HANDLER_HPP
#define ORES_HTTP_SERVER_MESSAGING_HTTP_INFO_HANDLER_HPP

#include <string>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.http/messaging/http_info_protocol.hpp"

namespace ores::http_server::messaging {

namespace {
inline auto& http_info_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.http.server.messaging.http_info_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using namespace ores::logging;

/**
 * @brief Handles service discovery requests for the HTTP server's base URL.
 *
 * No authentication is required — clients need this URL before they can
 * make authenticated HTTP calls.
 */
class http_info_handler {
public:
    explicit http_info_handler(ores::nats::service::client& nats,
                               std::string base_url)
        : nats_(nats), base_url_(std::move(base_url)) {}

    void get(ores::nats::message msg) {
        BOOST_LOG_SEV(http_info_handler_lg(), debug)
            << "Handling " << msg.subject;

        http::messaging::get_http_info_response resp;
        resp.base_url = base_url_;
        reply(nats_, msg, resp);

        BOOST_LOG_SEV(http_info_handler_lg(), debug)
            << "Replied with base_url: " << base_url_;
    }

private:
    ores::nats::service::client& nats_;
    std::string base_url_;
};

} // namespace ores::http_server::messaging

#endif
