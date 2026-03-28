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
#ifndef ORES_NATS_SERVICE_SESSION_EXPIRED_ERROR_HPP
#define ORES_NATS_SERVICE_SESSION_EXPIRED_ERROR_HPP

#include <stdexcept>

namespace ores::nats::service {

/**
 * @brief Thrown when a session reaches its maximum allowed duration.
 *
 * Raised by nats_client::authenticated_request() when the server returns
 * max_session_exceeded. Catching this specific type allows callers to
 * distinguish a session expiry from other transport errors.
 */
class session_expired_error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

}

#endif
