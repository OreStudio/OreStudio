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
#ifndef ORES_NATS_SERVICE_TIMEOUTS_HPP
#define ORES_NATS_SERVICE_TIMEOUTS_HPP

#include <chrono>

namespace ores::nats::service {

/**
 * @brief How long a request waits for its reply before giving up.
 *
 * Every request helper and client overload defaults to this value, so one
 * call does not behave differently from another merely because it bound to
 * a different overload or passed its payload as bytes instead of a string.
 * Callers that must wait longer, or fail faster, pass their own timeout.
 */
inline constexpr std::chrono::milliseconds default_request_timeout =
    std::chrono::seconds(30);

}

#endif
