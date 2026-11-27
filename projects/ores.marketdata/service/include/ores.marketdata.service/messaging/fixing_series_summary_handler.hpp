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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_FIXING_SERIES_SUMMARY_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_FIXING_SERIES_SUMMARY_HANDLER_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <optional>

namespace ores::marketdata::messaging {

/**
 * @brief NATS message handler for the fixing series a screen lists.
 *
 * The screen is system-level: it states the observations the platform holds,
 * whichever source made them, so the read runs in the system tenant and asks
 * only to read the fixings.
 */
class fixing_series_summary_handler {
public:
    fixing_series_summary_handler(ores::nats::service::client& nats,
                                  const ores::database::context& ctx,
                                  std::optional<ores::security::jwt::jwt_authenticator> verifier);

    void list(ores::nats::message msg);

private:
    ores::nats::service::client& nats_;
    const ores::database::context& ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
