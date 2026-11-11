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
#ifndef ORES_HTTP_ROUTES_IAM_ACCESS_SCREEN_ANSWERS_HPP
#define ORES_HTTP_ROUTES_IAM_ACCESS_SCREEN_ANSWERS_HPP

#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include <algorithm>
#include <cstddef>
#include <exception>
#include <functional>
#include <string>
#include <string_view>

/**
 * @brief How the access routes answer, as the BFF answered the access screens.
 *
 * A refusal the route states before it forwards, a forward whose failure is
 * answered with its own status or as the service not answering, and the length
 * of a text as the BFF measured it.
 */
namespace ores::http::routes::iam::access_screen_answers {

/**
 * @brief A refusal with this status, code and message.
 */
inline ores::http::domain::http_response
refused(ores::http::domain::http_status status, std::string code, std::string message) {
    namespace rr = ores::http::net::record_routes;
    return rr::answer(rr::refusal{status, std::move(code), std::move(message)});
}

/**
 * @brief Run one forward, answering a refusal with its own status and any other
 * failure as the service not answering.
 */
template <typename Request>
ores::http::domain::http_response
guarded(const std::function<ores::http::domain::http_response()>& work) {
    namespace rr = ores::http::net::record_routes;
    try {
        return work();
    } catch (const rr::refusal& r) {
        return rr::answer(r);
    } catch (const std::exception& e) {
        return ores::http::forwarding::unanswered(Request::nats_subject, e);
    }
}

/**
 * @brief How many characters a text holds, as the BFF counted them: a byte of
 * UTF-8 that continues a character is not counted.
 */
inline std::size_t characters_of(std::string_view text) {
    return static_cast<std::size_t>(
        std::ranges::count_if(text, [](unsigned char c) { return (c & 0xC0) != 0x80; }));
}

}

#endif
