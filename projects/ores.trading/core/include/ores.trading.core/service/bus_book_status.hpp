/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 *
 */
#ifndef ORES_TRADING_CORE_SERVICE_BUS_BOOK_STATUS_HPP
#define ORES_TRADING_CORE_SERVICE_BUS_BOOK_STATUS_HPP

#include "ores.nats/service/client.hpp"
#include "ores.trading.core/service/book_status_port.hpp"

namespace ores::trading::service {

/**
 * @brief A book's status, asked of the service that owns it, as the caller.
 *
 * The read is forwarded with the caller's own token in
 * `X-Delegated-Authorization`, so refdata answers for the person booking and a
 * book they cannot see comes back `unknown` rather than open. Every failure
 * answers `unknown`: a booking must not be refused for a fault in the asking.
 */
class bus_book_status final : public book_status_port {
public:
    explicit bus_book_status(ores::nats::service::client& nats)
        : nats_(nats) {}

    [[nodiscard]] state of(const boost::uuids::uuid& book_id,
                           std::string_view actor_token) const override;

private:
    ores::nats::service::client& nats_;
};

}

#endif
