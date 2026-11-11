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
#ifndef ORES_HTTP_NATS_FORWARDING_HPP
#define ORES_HTTP_NATS_FORWARDING_HPP

#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/export.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.nats/service/request_helpers.hpp"
#include "ores.nats/service/request_refused_error.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <exception>
#include <string>
#include <string_view>
#include <utility>

/**
 * @brief The gateway's step from one HTTP request to one service request and back.
 *
 * The NATS half is ores.nats's own: a request sent on its subject and its reply
 * decoded, with a service refusal raised as request_refused_error and a lapsed
 * session as session_expired_error. This part adds only what the HTTP surface
 * owns: the caller's token delegated to the service, and each failure stated
 * as the HTTP refusal the browser reads.
 */
namespace ores::http::forwarding {

/**
 * @brief The HTTP refusal a service's refusal code names.
 *
 * One table decides it: =forbidden= is 403, =bad_request= is 400 and
 * =unauthorized= is 401. A code the table does not name is 502, because the
 * gateway cannot say what the service meant.
 * The message is the code itself: a service states a refusal in its reply
 * header as a code alone, and the client's own sentence names the subject.
 */
[[nodiscard]] ORES_HTTP_NATS_EXPORT ores::http::net::record_routes::refusal
refusal_for(std::string_view code);

/**
 * @brief Refuse with the HTTP refusal a service's refusal code names.
 */
[[noreturn]] ORES_HTTP_NATS_EXPORT void refuse_refused(const ores::nats::service::request_refused_error& e);

/**
 * @brief Refuse a reply that does not decode as the response it answers.
 */
[[noreturn]] ORES_HTTP_NATS_EXPORT void refuse_undecodable(std::string_view subject, const std::string& detail);

/**
 * @brief Refuse a session the service could not renew.
 */
[[noreturn]] ORES_HTTP_NATS_EXPORT void refuse_lapsed(const ores::nats::service::session_expired_error& e);

/**
 * @brief Send one canonical request as the caller on a subject named at run time.
 *
 * A request whose subject depends on what it asks for, such as the history of
 * an entity whose component owns its subject, states no subject of its own.
 * The caller's own token travels as a delegation, so the service checks that
 * caller's permission. A refusal, a lapsed session or a reply that does not
 * decode is thrown as a record_routes::refusal. A transport failure propagates,
 * so the handler answers it as the service not answering.
 */
template <typename Request>
typename Request::response_type forward_on(const ores::http::domain::http_request& req,
                                           ores::nats::service::nats_client& session,
                                           std::string_view subject,
                                           const Request& msg) {
    auto delegated = session.with_delegation(req.get_bearer_token().value_or(std::string{}));
    try {
        auto reply =
            ores::nats::service::authenticated_request_and_decode<typename Request::response_type>(
                delegated, subject, msg, ores::nats::service::default_request_timeout);
        if (!reply)
            refuse_undecodable(subject, reply.error().what());
        return std::move(*reply);
    } catch (const ores::nats::service::request_refused_error& e) {
        refuse_refused(e);
    } catch (const ores::nats::service::session_expired_error& e) {
        refuse_lapsed(e);
    }
}

/**
 * @brief Send one canonical request as the caller on its own subject, and answer its decoded reply.
 */
template <typename Request>
typename Request::response_type forward(const ores::http::domain::http_request& req,
                                        ores::nats::service::nats_client& session,
                                        const Request& msg) {
    return forward_on(req, session, Request::nats_subject, msg);
}

/**
 * @brief The response a failure that is not a refusal states, its detail logged.
 */
[[nodiscard]] ORES_HTTP_NATS_EXPORT ores::http::domain::http_response
unanswered(std::string_view subject, const std::exception& e);

/**
 * @brief Forward one canonical request and answer the reply as JSON.
 *
 * A refusal is answered with its code and message; any other failure as the
 * service not answering, with its detail in the log and not in the body.
 */
template <typename Request>
ores::http::domain::http_response respond(const ores::http::domain::http_request& req,
                                          ores::nats::service::nats_client& session,
                                          const Request& msg) {
    try {
        return ores::http::domain::http_response::json(
            ores::utility::codec::wire_traits<typename Request::response_type>::write_json(
                forward(req, session, msg)));
    } catch (const ores::http::net::record_routes::refusal& r) {
        return ores::http::net::record_routes::answer(r);
    } catch (const std::exception& e) {
        return unanswered(Request::nats_subject, e);
    }
}

}

#endif
