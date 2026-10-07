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
#ifndef ORES_NATS_SERVICE_REFUSAL_HPP
#define ORES_NATS_SERVICE_REFUSAL_HPP

#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/request_refused_error.hpp"
#include <optional>
#include <string>
#include <string_view>

namespace ores::nats::service {

/**
 * @brief The code a reply carries when the server refused the request.
 *
 * A refused request is answered with an empty body and an X-Error header
 * naming why, so a caller that reads only the body reports a malformed
 * payload for what is really a refusal. The two expiry codes are not
 * refusals: they have their own handling, so this reports nothing for them.
 *
 * A caller that handles the header itself reads the reply the request
 * returned. A caller that decodes the body cannot, so it calls
 * throw_if_refused() first.
 *
 * @return the code, such as "forbidden", or nothing for a reply that is not
 * a refusal.
 */
[[nodiscard]] inline std::optional<std::string> refusal_code(const message& reply) {
    const auto it = reply.headers.find(std::string(headers::x_error));
    if (it == reply.headers.end())
        return std::nullopt;
    if (it->second == "token_expired" || it->second == "max_session_exceeded")
        return std::nullopt;
    return it->second;
}

/**
 * @brief Throws when @p reply is a refusal, so a caller never decodes one.
 *
 * @param subject the subject the request was sent on, named in the error.
 */
inline void throw_if_refused(const message& reply, std::string_view subject) {
    if (const auto code = refusal_code(reply))
        throw request_refused_error(*code, std::string(subject));
}

}

#endif
