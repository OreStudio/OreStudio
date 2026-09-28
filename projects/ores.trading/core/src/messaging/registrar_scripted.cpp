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
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/scripted_instrument_registrar.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_scripted_handlers(ores::nats::service::client& nats,
                           ores::database::context ctx,
                           std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    return register_scripted_instrument_handlers(nats, ctx, verifier);
}

}
