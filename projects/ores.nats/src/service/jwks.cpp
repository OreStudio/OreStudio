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
#include "ores.nats/service/jwks.hpp"

#include <span>
#include <stdexcept>
#include <string_view>
#include <boost/json.hpp>
#include "ores.nats/service/retry.hpp"

namespace ores::nats::service {

boost::asio::awaitable<std::string>
fetch_jwks_public_key(client& nats, std::chrono::seconds per_request_timeout) {
    std::string pub_key;

    co_await retry_with_backoff(
        [&]() -> boost::asio::awaitable<void> {
            static constexpr std::string_view body = "{}";
            const auto* p = reinterpret_cast<const std::byte*>(body.data());

            auto reply = co_await nats.request(
                "iam.v1.auth.jwks",
                std::span<const std::byte>(p, body.size()),
                {},
                per_request_timeout);

            const std::string_view sv(
                reinterpret_cast<const char*>(reply.data.data()),
                reply.data.size());

            const auto json = boost::json::parse(sv);
            const auto& obj = json.as_object();

            // IAM returns {error_message: "..."} when no key is configured.
            if (obj.contains("error_message")) {
                throw std::runtime_error(
                    "IAM JWKS error: " +
                    std::string(obj.at("error_message").as_string()));
            }

            auto key = std::string(obj.at("public_key").as_string());
            if (key.empty())
                throw std::runtime_error(
                    "IAM JWKS returned empty public key");

            pub_key = std::move(key);
        },
        "JWKS public key fetch from IAM"
    );

    co_return pub_key;
}

} // namespace ores::nats::service
