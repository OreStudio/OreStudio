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
#ifndef ORES_HTTP_SERVER_APP_AGENT_WRITE_RULES_HPP
#define ORES_HTTP_SERVER_APP_AGENT_WRITE_RULES_HPP

#include "ores.http.api/domain/http_response.hpp"
#include "ores.security/authorization/agent_withholding.hpp"
#include <boost/json/parse.hpp>
#include <algorithm>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::server::app {

/**
 * Whether a token can write at all: its permissions are the codes the exchange
 * put in its roles claim, and a token whose every code reads can write nothing.
 */
inline bool agent_token_can_write(const std::vector<std::string>& permission_codes) {
    return std::ranges::any_of(permission_codes, [](const std::string& code) {
        return !ores::security::authorization::is_read_permission(code);
    });
}

/**
 * Whether a replayed call did what it asked. A route answers its service's own
 * result in the body, so a 200 can carry a refusal: the body's success or
 * outcome decides, and its message is the reason a refusal gives.
 */
inline bool agent_write_applied(const ores::http::domain::http_response& answer, std::string& why) {
    if (static_cast<int>(answer.status) >= 400) {
        why = answer.body;
        return false;
    }
    boost::system::error_code ec;
    const auto value = boost::json::parse(answer.body, ec);
    if (ec || !value.is_object())
        return true;
    const auto& body = value.as_object();
    const auto message_of = [](const boost::json::object& o) {
        const auto* m = o.if_contains("message");
        return m && m->is_string() ? std::string(m->as_string()) : std::string();
    };
    if (const auto* ok = body.if_contains("success"); ok && ok->is_bool() && !ok->as_bool()) {
        why = message_of(body);
        return false;
    }
    if (const auto* result = body.if_contains("result"); result && result->is_object()) {
        const auto& r = result->as_object();
        if (const auto* outcome = r.if_contains("outcome");
            outcome && outcome->is_string() && outcome->as_string() != "ok") {
            why = message_of(r);
            if (why.empty())
                why = std::string(outcome->as_string());
            return false;
        }
    }
    return true;
}

}

#endif
