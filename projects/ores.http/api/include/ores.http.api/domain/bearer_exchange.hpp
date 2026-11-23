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
#ifndef ORES_HTTP_API_DOMAIN_BEARER_EXCHANGE_HPP
#define ORES_HTTP_API_DOMAIN_BEARER_EXCHANGE_HPP

#include "ores.http.api/domain/session_store.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include <chrono>
#include <functional>
#include <optional>
#include <string>

namespace ores::http::domain {

/**
 * @brief What the gateway's exchanger made of a bearer value it recognised.
 *
 * A refused value is answered with 401 and counted as a failed credential; an
 * exchanged one is replaced by the token, which the gateway then verifies and
 * relays as it does any token. An exchange that could not be made, because IAM
 * did not answer or is shedding load, is answered with 503 and is not counted
 * against the caller, whose value may well be good. The gateway never signs:
 * the token comes from IAM.
 */
struct bearer_exchange final {
    enum class outcome { refused, exchanged, unavailable };
    outcome result = outcome::refused;
    std::string token;
};

/**
 * @brief Exchanges a bearer value that is not a token, such as an LLM agent's
 * key, for one IAM issued.
 *
 * Answers nothing for a value it does not recognise, which the gateway then
 * verifies as a token. The client address is the caller's as the gateway saw
 * it. It may block on IAM, so the gateway runs it on its worker pool.
 */
using bearer_exchanger = std::function<std::optional<bearer_exchange>(
    const std::string& bearer, const std::string& client_address)>;

/**
 * @brief A call made with an exchanged bearer value, once it is answered.
 *
 * Only the route's pattern is kept, never the address with its values, and
 * never the request or the answer.
 */
struct exchanged_call final {
    ores::security::jwt::jwt_claims claims;
    std::string method;
    std::string route;
    int status = 0;
    std::chrono::milliseconds duration{0};
};

/**
 * @brief Records a call made with an exchanged bearer value, such as an LLM
 * agent's. It may write to a store, so the gateway runs it on its worker pool.
 */
using exchanged_call_recorder = std::function<void(const exchanged_call& call)>;

/**
 * @brief The gateway's calls to IAM on the credential path: renewing a
 * browser session's token, exchanging a bearer value that is not a token, and
 * recording what a call made with such a value did.
 */
struct credential_exchange final {
    token_renewer renew;
    bearer_exchanger exchange;
    exchanged_call_recorder record;
};

}

#endif
