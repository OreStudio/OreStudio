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
#include "ores.nats/service/nats_connect_error.hpp"
#include "ores.nats/service/request_helpers.hpp"
#include "ores.nats/service/request_refused_error.hpp"
#include "ores.nats/service/session_expired_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstdint>
#include <exception>
#include <functional>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
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
[[noreturn]] ORES_HTTP_NATS_EXPORT void
refuse_refused(const ores::nats::service::request_refused_error& e);

/**
 * @brief Refuse a reply that does not decode as the response it answers.
 */
[[noreturn]] ORES_HTTP_NATS_EXPORT void refuse_undecodable(std::string_view subject,
                                                           const std::string& detail);

/**
 * @brief Refuse a session the service could not renew.
 */
[[noreturn]] ORES_HTTP_NATS_EXPORT void
refuse_lapsed(const ores::nats::service::session_expired_error& e);

/**
 * @brief The session a request's caller signed in to, as their token states it,
 * or nothing for a token that states none.
 */
[[nodiscard]] ORES_HTTP_NATS_EXPORT std::optional<std::string>
caller_session_id(const ores::http::domain::http_request& req);

/**
 * @brief The client a request is forwarded on: the caller's token as a
 * delegation, and the caller's session id beside it when the token states one.
 *
 * The BFF sends its session id with every call, and a service that keys on the
 * session, such as IAM's party switch, finds the caller's parties by it.
 */
[[nodiscard]] ORES_HTTP_NATS_EXPORT ores::nats::service::nats_client
caller_client(const ores::http::domain::http_request& req,
              ores::nats::service::nats_client& session);

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
                                           const Request& msg,
                                           std::chrono::milliseconds timeout) {
    auto delegated = caller_client(req, session);
    try {
        auto reply =
            ores::nats::service::authenticated_request_and_decode<typename Request::response_type>(
                delegated, subject, msg, timeout);
        if (!reply)
            refuse_undecodable(subject, reply.error().what());
        return std::move(*reply);
    } catch (const ores::nats::service::request_refused_error& e) {
        refuse_refused(e);
    } catch (const ores::nats::service::session_expired_error& e) {
        refuse_lapsed(e);
    }
}

template <typename Request>
typename Request::response_type forward_on(const ores::http::domain::http_request& req,
                                           ores::nats::service::nats_client& session,
                                           std::string_view subject,
                                           const Request& msg) {
    return forward_on(req, session, subject, msg, ores::nats::service::default_request_timeout);
}

/**
 * @brief The waiting time a composed view states for itself.
 *
 * The NATS client waits 30 seconds by default, which is longer than the HTTP
 * server's idle timeout, so a service that stays silent takes the socket down
 * instead of answering. A view that makes several reads declares the time it may
 * take instead, and a read that cannot be made inside what is left is refused as
 * the service not answering in time.
 */
inline constexpr std::chrono::milliseconds composed_view_budget =
    std::chrono::milliseconds(std::chrono::seconds(10));

/**
 * @brief The shortest wait a read is given, so a spent budget refuses rather than
 * sends a request that cannot be waited for.
 */
inline constexpr std::chrono::milliseconds shortest_read =
    std::chrono::milliseconds(std::chrono::milliseconds(1000));

/**
 * @brief Thrown when a composed view has spent the time it declared.
 */
struct ORES_HTTP_NATS_EXPORT budget_spent_error final : std::runtime_error {
    budget_spent_error()
        : std::runtime_error("the view spent its time budget") {}
};

/**
 * @brief The time one request may spend across the reads a view makes.
 *
 * A view opens one at its first read and passes =next()= to each forward, so the
 * time is the request's and not each read's.
 */
class ORES_HTTP_NATS_EXPORT budget final {
public:
    explicit budget(std::chrono::milliseconds total = composed_view_budget)
        : opened_(std::chrono::steady_clock::now())
        , total_(total) {}

    /**
     * @brief How long the next read may wait.
     *
     * @throws budget_spent_error when less than the shortest read is left.
     */
    [[nodiscard]] std::chrono::milliseconds next() const {
        const auto spent = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now() - opened_);
        const auto left = total_ - spent;
        if (left < shortest_read)
            throw budget_spent_error();
        return left;
    }

private:
    std::chrono::steady_clock::time_point opened_;
    std::chrono::milliseconds total_;
};

/**
 * @brief Send one canonical request as the caller on its own subject, and answer its decoded reply.
 */
template <typename Request>
typename Request::response_type forward(const ores::http::domain::http_request& req,
                                        ores::nats::service::nats_client& session,
                                        const Request& msg,
                                        std::chrono::milliseconds timeout) {
    return forward_on(req, session, Request::nats_subject, msg, timeout);
}

template <typename Request>
typename Request::response_type forward(const ores::http::domain::http_request& req,
                                        ores::nats::service::nats_client& session,
                                        const Request& msg) {
    return forward_on(req, session, Request::nats_subject, msg);
}

/**
 * @brief Send a one-row write as the caller. A row read at no version is new,
 * and one read at a version is written only while it still holds it.
 */
template <typename Put>
typename Put::response_type put_at_version(const ores::http::domain::http_request& req,
                                           ores::nats::service::nats_client& session,
                                           decltype(Put{}.change.write) write,
                                           std::optional<std::uint32_t> version,
                                           ores::utility::domain::change_intent intent) {
    using ores::utility::domain::precondition_kind;
    Put msg;
    msg.change.write = std::move(write);
    msg.change.precondition.kind =
        version ? precondition_kind::must_match_version : precondition_kind::must_not_exist;
    msg.change.precondition.version = version;
    msg.intent = std::move(intent);
    return forward(req, session, msg);
}

/**
 * @brief Every row of a list, read 1000 at a time until a page comes back short.
 *
 * A page that is not ok is refused with the status its outcome names. The read
 * stops with an error after 1000 pages, so a service that ignores the limit
 * cannot hold the route.
 */
template <typename Request, typename Rows>
auto forward_all(const ores::http::domain::http_request& req,
                 ores::nats::service::nats_client& session,
                 Request msg,
                 Rows member) {
    constexpr std::uint32_t page = 1000;
    std::remove_cvref_t<decltype(std::declval<typename Request::response_type>().*member)> all;
    std::uint32_t offset = 0;
    for (std::uint32_t read = 0; read < page; ++read) {
        msg.offset = offset;
        msg.limit = page;
        const auto reply = forward(req, session, msg);
        ores::http::net::record_routes::require_ok(reply.result);
        const auto& got = reply.*member;
        all.insert(all.end(), got.begin(), got.end());
        if (got.size() < page)
            return all;
        offset += static_cast<std::uint32_t>(got.size());
    }
    throw std::runtime_error("The service sent more pages than a whole read takes.");
}

/**
 * @brief Every row of a list, read 1000 at a time until a page comes back short,
 * each page read inside the time the caller's view declared.
 *
 * This is the paging a composed view uses: the view opens one budget and passes
 * it here, so a list that keeps the route waiting is refused as not answering in
 * time rather than taking the socket down.
 */
template <typename Request, typename Rows>
auto forward_all(const ores::http::domain::http_request& req,
                 ores::nats::service::nats_client& session,
                 Request msg,
                 Rows member,
                 const budget& time) {
    constexpr std::uint32_t page = 1000;
    std::remove_cvref_t<decltype(std::declval<typename Request::response_type>().*member)> all;
    std::uint32_t offset = 0;
    for (std::uint32_t read = 0; read < page; ++read) {
        msg.offset = offset;
        msg.limit = page;
        const auto reply = forward(req, session, msg, time.next());
        ores::http::net::record_routes::require_ok(reply.result);
        const auto& got = reply.*member;
        all.insert(all.end(), got.begin(), got.end());
        if (got.size() < page)
            return all;
        offset += static_cast<std::uint32_t>(got.size());
    }
    throw std::runtime_error("The service sent more pages than a whole read takes.");
}

/**
 * @brief Send one canonical request as the holder of a bearer token.
 *
 * Most services read the caller from the delegation the forwarding step adds.
 * One of them cannot: =enter_tenant= reads the caller from the =Authorization=
 * header, because it is the operation that mints a session, and the tenant reads
 * that follow run inside the session it minted. This sends the token as the
 * bearer for those, on the same connection, so the caller's own token and the
 * entered one are each the authorization the service expects.
 *
 * A refusal, a lapsed session or a reply that does not decode is thrown as a
 * record_routes::refusal.
 */
template <typename Request>
typename Request::response_type
forward_as(ores::nats::service::nats_client& session, std::string_view token, const Request& msg) {
    const std::string held(token);
    ores::nats::service::nats_client bearer(session.transport(), [held](bool) { return held; });
    try {
        auto reply =
            ores::nats::service::authenticated_request_and_decode<typename Request::response_type>(
                bearer, Request::nats_subject, msg, ores::nats::service::default_request_timeout);
        if (!reply)
            refuse_undecodable(Request::nats_subject, reply.error().what());
        return std::move(*reply);
    } catch (const ores::nats::service::request_refused_error& e) {
        refuse_refused(e);
    } catch (const ores::nats::service::session_expired_error& e) {
        refuse_lapsed(e);
    }
}

/**
 * @brief Refuse a caller who does not act on the deployment as a whole.
 *
 * A cross-tenant read reaches every tenant's rows, so the design keeps the
 * BFF's boundary: the caller must hold =installation::deployment:read=, read
 * again at each call so a grant taken away stops the next request. The codes
 * come from IAM as the caller, and =ores::security::authorization::grants=
 * decides them, which is the one rule the services, the BFF and the browser
 * already share.
 *
 * A wildcard does not cover an installation code: a tenant's administrator
 * holds =*=, and the deployment's own read is granted by its name.
 *
 * @throws refusal naming @p what and the refusal when the caller lacks it.
 */
ORES_HTTP_NATS_EXPORT void require_deployment_read(const ores::http::domain::http_request& req,
                                                   ores::nats::service::nats_client& session,
                                                   std::string_view what);

/**
 * @brief Refuse a caller who does not hold @p permission.
 *
 * A route that asks a service as the gateway rather than as the caller acts on
 * the gateway's own permissions, so the person's permission has to be checked
 * here. The codes come from IAM as the caller, read again at each call so a
 * grant taken away stops the next request, and
 * =ores::security::authorization::grants= decides them.
 *
 * @throws refusal naming @p what and @p permission when the caller lacks it.
 */
ORES_HTTP_NATS_EXPORT void require_permission(const ores::http::domain::http_request& req,
                                              ores::nats::service::nats_client& session,
                                              std::string_view permission,
                                              std::string_view what);

/**
 * @brief The response a failure that is not a refusal states, its detail logged.
 */
[[nodiscard]] ORES_HTTP_NATS_EXPORT ores::http::domain::http_response
unanswered(std::string_view subject, const std::exception& e);

/**
 * @brief The answer a request that waited out its time states.
 *
 * A read that no service answered in time is the gateway's own 504 in the error
 * contract, not a failure of the route: the browser reads the code and says the
 * service did not answer in time.
 */
[[nodiscard]] inline ores::http::domain::http_response
timed_out(const ores::nats::service::nats_connect_error&) {
    return ores::http::domain::http_response::error(
        ores::http::domain::http_status::gateway_timeout,
        "The service did not answer within the time this request allows.");
}

/**
 * @brief Run a route's forwards, answering a refusal with its own status and any
 * other failure as the service not answering.
 *
 * A route that shapes its own answer, or makes several forwards, states its work
 * as @p work; the request type names the subject the log records a failure on.
 */
template <typename Request>
ores::http::domain::http_response
guarded(const std::function<ores::http::domain::http_response()>& work) {
    try {
        return work();
    } catch (const ores::http::net::record_routes::refusal& r) {
        return ores::http::net::record_routes::answer(r);
    } catch (const budget_spent_error& e) {
        return ores::http::domain::http_response::error(
            ores::http::domain::http_status::gateway_timeout, e.what());
    } catch (const ores::nats::service::nats_connect_error& e) {
        if (e.kind() == ores::nats::service::nats_error_kind::request_timeout)
            return timed_out(e);
        return unanswered(Request::nats_subject, e);
    } catch (const std::exception& e) {
        return unanswered(Request::nats_subject, e);
    }
}

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
    } catch (const ores::nats::service::nats_connect_error& e) {
        if (e.kind() == ores::nats::service::nats_error_kind::request_timeout)
            return timed_out(e);
        return unanswered(Request::nats_subject, e);
    } catch (const std::exception& e) {
        return unanswered(Request::nats_subject, e);
    }
}

}

#endif
