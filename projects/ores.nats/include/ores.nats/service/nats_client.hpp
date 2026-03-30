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
#ifndef ORES_NATS_SERVICE_NATS_CLIENT_HPP
#define ORES_NATS_SERVICE_NATS_CLIENT_HPP

#include <chrono>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/session_expired_error.hpp"

namespace ores::nats::service { class client; }

namespace ores::nats::service {

/**
 * @brief Authenticated NATS client for both interactive and service-to-service use.
 *
 * Wraps a NATS connection with JWT-based authentication, supporting two usage paths:
 *
 * **Interactive path** (shell, Qt): the caller manages the connection lifecycle and
 * supplies the JWT externally after a successful login.
 *
 * @code
 *   nats_client nc;
 *   nc.connect(opts);
 *   // ... send login request via nc.request() ...
 *   nc.set_auth({.jwt = token, .username = user, ...});
 *   auto reply = nc.authenticated_request(subject, body);
 * @endcode
 *
 * Token refresh is **reactive**: the client throws session_expired_error on
 * token_expired or max_session_exceeded (X-Error header). The caller is
 * responsible for re-authenticating and calling set_auth() with the new token.
 *
 * **Service path** (backend services): a token_provider callback is injected
 * at construction. The provider is responsible for acquiring and proactively
 * refreshing the JWT; nats_client calls it before each authenticated request.
 *
 * @code
 *   auto provider = make_service_token_provider(raw_nats, user, pass);
 *   nats_client nc(raw_nats, std::move(provider));
 *   auto reply = nc.authenticated_request(subject, body);
 * @endcode
 *
 * The two paths are mutually exclusive on a given instance.
 */
class nats_client {
private:
    inline static std::string_view logger_name = "ores.nats.service.nats_client";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Login state for the interactive path.
     *
     * Populated via set_auth() after a successful iam.v1.auth.login call.
     */
    struct login_info {
        std::string jwt;
        std::string username;
        std::string tenant_id;
        std::string tenant_name;
    };

    /**
     * @brief Callable that returns a current Bearer token.
     *
     * Used by the service path. The provider owns all acquisition and
     * proactive refresh logic; nats_client calls it before each request.
     * When called with force=true the provider must discard any cached
     * token and re-authenticate unconditionally (used after the server
     * returns X-Error: token_expired to handle extreme clock-skew cases
     * where the client's own expiry timer has not yet fired).
     */
    using token_provider = std::function<std::string(bool force)>;

    // -- Interactive path --

    /**
     * @brief Default-construct for interactive use. Call connect() next.
     */
    nats_client() = default;

    /**
     * @brief Connect to NATS and take ownership of the connection.
     *
     * @throws std::runtime_error if the connection fails.
     */
    void connect(config::nats_options opts);

    /**
     * @brief Disconnect and clear all auth state.
     */
    void disconnect();

    [[nodiscard]] bool is_connected() const noexcept;

    /**
     * @brief Store authentication info after a successful login.
     */
    void set_auth(login_info info);

    /**
     * @brief Clear authentication info on logout.
     */
    void clear_auth();

    /**
     * @brief Return current auth info.
     * @throws std::runtime_error if not logged in.
     */
    [[nodiscard]] const login_info& auth() const;

    // -- Service path --

    /**
     * @brief Construct for service-to-service use.
     *
     * @param nats      Already-connected NATS client (not owned; must outlive this).
     * @param provider  Callable that returns a current Bearer token. The
     *                  provider owns acquisition and proactive refresh.
     */
    nats_client(client& nats, token_provider provider);

    // -- Common --

    [[nodiscard]] bool is_logged_in() const noexcept;

    /**
     * @brief Unauthenticated synchronous request.
     *
     * Used for pre-login calls (login itself, JWKS fetch, bootstrap status).
     */
    [[nodiscard]] message request(std::string_view subject,
        std::string_view json_body);

    /**
     * @brief Authenticated synchronous request — string body overload.
     *
     * Convenience for callers that already have the payload as a string.
     */
    [[nodiscard]] message authenticated_request(std::string_view subject,
        std::string_view json_body,
        std::chrono::milliseconds timeout = std::chrono::seconds(30));

    /**
     * @brief Authenticated synchronous request — byte-span overload.
     *
     * For callers that work with pre-serialised byte buffers.
     */
    [[nodiscard]] message authenticated_request(std::string_view subject,
        std::span<const std::byte> body,
        std::chrono::milliseconds timeout = std::chrono::seconds(10));

    /**
     * @brief Returns a new nats_client that shares this instance's underlying
     * connection and token provider but injects
     *   X-Delegated-Authorization: Bearer <token>
     * on every authenticated_request call.
     *
     * Used by handlers to forward the original end-user JWT to downstream
     * services without modifying message payloads.  Call unconditionally:
     * if token is empty the returned client behaves identically to *this.
     *
     * Thread-safe: the returned value is independent of *this.
     */
    [[nodiscard]] nats_client with_delegation(std::string token) const;

    /**
     * @brief Return the underlying client (interactive path only).
     *
     * Returns nullptr if constructed via the service path.
     */
    [[nodiscard]] std::shared_ptr<client> get_client() const;

private:
    [[nodiscard]] client& active_client() const;

    [[nodiscard]] message do_authenticated_request(std::string_view subject,
        std::span<const std::byte> body, std::chrono::milliseconds timeout);

    // Interactive path state
    std::shared_ptr<client> owned_client_;
    std::optional<login_info> auth_;

    // Service path state
    client* external_client_ = nullptr;
    token_provider token_provider_;

    // Optional delegation token forwarded as X-Delegated-Authorization.
    std::string delegation_token_;
};

/**
 * @brief Extracts the raw token from an "Authorization: Bearer <token>" header.
 *
 * Returns the token string (without the "Bearer " prefix).
 * Returns empty string if the header is absent or does not start with "Bearer ".
 *
 * Used by handlers to obtain the inbound user JWT for forwarding via
 * nats_client::with_delegation.
 */
[[nodiscard]] std::string extract_bearer(const ores::nats::message& msg);

} // namespace ores::nats::service

#endif
