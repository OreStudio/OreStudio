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
#include "ores.http.nats/forwarding.hpp"
#include "ores.logging/make_logger.hpp"
#include <array>
#include <utility>

namespace ores::http::forwarding {

using namespace ores::logging;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
namespace rr = ores::http::net::record_routes;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.http.forwarding");
    return instance;
}

/// A service refusal code, and the HTTP status and code the browser reads for it.
struct refusal_mapping final {
    std::string_view service_code;
    http_status status;
    std::string_view code;
};

constexpr std::array refusal_mappings{
    refusal_mapping{"forbidden", http_status::forbidden, "forbidden"},
    refusal_mapping{"bad_request", http_status::bad_request, "invalid-request"},
    refusal_mapping{"unauthorized", http_status::unauthorized, "not-authenticated"},
};

}

rr::refusal refusal_for(std::string_view code) {
    for (const auto& m : refusal_mappings) {
        if (m.service_code == code)
            return rr::refusal{m.status, std::string(m.code), std::string(code)};
    }
    BOOST_LOG_SEV(lg(), warn) << "Unknown service refusal: " << code;
    return rr::refusal{
        http_status::bad_gateway, "upstream-unavailable", std::string(rr::unanswered)};
}

void refuse_refused(const ores::nats::service::request_refused_error& e) {
    throw refusal_for(e.code());
}

void refuse_undecodable(std::string_view subject, const std::string& detail) {
    BOOST_LOG_SEV(lg(), error) << "Failed to decode the reply on " << subject << ": " << detail;
    rr::refuse(http_status::bad_gateway, "internal", std::string(rr::unanswered));
}

void refuse_lapsed(const ores::nats::service::session_expired_error& e) {
    rr::refuse(http_status::unauthorized, "session-expired", e.what());
}

http_response unanswered(std::string_view subject, const std::exception& e) {
    BOOST_LOG_SEV(lg(), error) << "No answer on " << subject << ": " << e.what();
    return http_response::api_error(
        http_status::bad_gateway, "internal", std::string(rr::unanswered));
}

}
