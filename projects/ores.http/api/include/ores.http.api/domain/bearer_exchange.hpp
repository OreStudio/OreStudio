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
 * @brief The gateway's calls to IAM on the credential path: renewing a
 * browser session's token, and exchanging a bearer value that is not a token.
 */
struct credential_exchange final {
    token_renewer renew;
    bearer_exchanger exchange;
};

}

#endif
