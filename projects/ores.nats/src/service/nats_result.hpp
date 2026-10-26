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
#ifndef ORES_NATS_SERVICE_NATS_RESULT_HPP
#define ORES_NATS_SERVICE_NATS_RESULT_HPP

#include <nats/nats.h>
#include <stdexcept>
#include <string>
#include <string_view>

namespace ores::nats::service {

/**
 * @brief The error a failed cnats call raises: what failed, then the library's
 * reason for it.
 */
inline std::runtime_error nats_failure(std::string_view what, natsStatus s) {
    return std::runtime_error(std::string(what) + ": " + natsStatus_GetText(s));
}

/**
 * @brief As above, with the JetStream error code the server returned.
 */
inline std::runtime_error nats_failure(std::string_view what, natsStatus s, jsErrCode jerr) {
    return std::runtime_error(std::string(what) + ": " + natsStatus_GetText(s) + " (js err " +
                              std::to_string(static_cast<int>(jerr)) + ")");
}

/**
 * @brief Throws nats_failure unless the cnats call succeeded.
 */
inline void ensure_result(natsStatus s, std::string_view what) {
    if (s != NATS_OK)
        throw nats_failure(what, s);
}

/**
 * @brief Throws nats_failure, with the JetStream error code, unless the call
 * succeeded.
 */
inline void ensure_result(natsStatus s, jsErrCode jerr, std::string_view what) {
    if (s != NATS_OK)
        throw nats_failure(what, s, jerr);
}

}

#endif
