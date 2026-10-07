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
#ifndef ORES_NATS_SERVICE_REQUEST_REFUSED_ERROR_HPP
#define ORES_NATS_SERVICE_REQUEST_REFUSED_ERROR_HPP

#include <stdexcept>
#include <string>
#include <utility>

namespace ores::nats::service {

/**
 * @brief Thrown when the server refuses a request.
 *
 * A refusal carries no body, only the X-Error header naming why, so a client
 * that decodes the body reports a malformed payload for what is really a
 * refusal. Raised by nats_client so the caller reports the refusal the server
 * stated. The expiry codes are not refusals and keep their own handling.
 */
class request_refused_error : public std::runtime_error {
public:
    request_refused_error(std::string code, std::string subject)
        : std::runtime_error("the server refused '" + subject + "': " + code)
        , code_(std::move(code)) {}

    /// The code the server sent, such as "forbidden".
    [[nodiscard]] const std::string& code() const noexcept {
        return code_;
    }

private:
    std::string code_;
};

}

#endif
