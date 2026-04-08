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
#ifndef ORES_NATS_SERVICE_NATS_CONNECT_ERROR_HPP
#define ORES_NATS_SERVICE_NATS_CONNECT_ERROR_HPP

#include <stdexcept>
#include <string>

namespace ores::nats::service {

/**
 * @brief Categorises the cause of a failed NATS connection attempt.
 *
 * Allows callers to present a meaningful error to the user rather than
 * a raw NATS library status string.
 */
enum class nats_error_kind {
    /// TCP-level failure: server not running, port closed, or host unreachable.
    server_unreachable,
    /// SSL/TLS failure: bad certificate, key mismatch, bad record MAC, or
    /// TLS version/cipher mismatch.
    tls_error,
    /// Connection attempt timed out before the server responded.
    connection_timeout,
    /// NATS connected successfully but no service is listening on the
    /// bootstrap subject (services not started or wrong namespace).
    services_unavailable,
    /// Any other NATS error not covered by the above categories.
    other
};

/**
 * @brief Thrown when a NATS connection or initial service check fails.
 *
 * Carries a nats_error_kind so callers can distinguish "NATS is down",
 * "TLS is misconfigured", "services are not started", etc. and surface
 * a readable message in the login dialog.
 */
class nats_connect_error : public std::runtime_error {
public:
    nats_connect_error(nats_error_kind kind, const std::string& msg)
        : std::runtime_error(msg), kind_(kind) {}

    [[nodiscard]] nats_error_kind kind() const noexcept { return kind_; }

private:
    nats_error_kind kind_;
};

} // namespace ores::nats::service

#endif
