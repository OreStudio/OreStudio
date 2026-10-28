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
#ifndef ORES_HTTP_CLIENT_NET_TRANSPORT_HPP
#define ORES_HTTP_CLIENT_NET_TRANSPORT_HPP

#include "ores.http.client/export.hpp"
#include <chrono>
#include <cstddef>
#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::client::net {

/**
 * @brief The bounds of one request: how long it may take, and how large a body it may
 * return.
 */
struct request_limits final {
    std::chrono::milliseconds timeout{std::chrono::seconds(30)};
    std::size_t max_body_bytes = 32 * 1024 * 1024;
};

/**
 * @brief What a server answered.
 */
struct response final {
    unsigned status = 0;
    std::string content_type;
    std::string body;
};

/**
 * @brief Why a request got no answer.
 */
enum class failure_kind { bad_url, network, tls, timeout, too_large };

struct failure final {
    failure_kind kind = failure_kind::network;
    /// What went wrong, with any secret in the URL masked.
    std::string message;
};

/**
 * @brief The way a caller reads an outside HTTP API.
 *
 * A caller holds this interface, so a test hands back stored responses and needs no
 * network.
 */
class transport {
public:
    virtual ~transport() = default;

    /**
     * @brief Reads @p url, an http or https address, within @p limits.
     */
    [[nodiscard]] virtual std::expected<response, failure>
    get(std::string_view url, const request_limits& limits = {}) = 0;
};

/**
 * @brief A transport over the network, on Boost.Beast and Boost.Asio, with OpenSSL for TLS.
 *
 * Over https it checks the server's certificate against the trusted authorities and its
 * name against the host asked for, and refuses either that does not match. It follows no
 * redirect: a caller that wants one reads the status and asks again.
 */
class ORES_HTTP_CLIENT_EXPORT beast_transport final : public transport {
public:
    struct options final {
        std::string user_agent = "ORE Studio";
        /// The authorities to trust, as a PEM file. Without one, the system's are trusted.
        std::optional<std::filesystem::path> trusted_authorities;
    };

    beast_transport();
    explicit beast_transport(options o);

    [[nodiscard]] std::expected<response, failure> get(std::string_view url,
                                                       const request_limits& limits = {}) override;

private:
    options options_;
};

/**
 * @brief @p url with the value of every query parameter that may hold a secret masked, so it
 * can be logged or stored: a parameter whose name holds "key", "token", "secret" or
 * "password", in any case.
 */
[[nodiscard]] ORES_HTTP_CLIENT_EXPORT std::string redacted(std::string_view url);

}

#endif
