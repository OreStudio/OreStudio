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
#include "ores.trading.core/messaging/instrument_option_payment_date_history_provider_registrar.hpp"

namespace ores::trading::messaging {

void register_instrument_option_payment_date_history_provider(
    ores::history::service::dispatch_registry& registry) {
    // A compound-key row has no single-string id on the history_provider
    // interface, so nothing is registered for it yet. The direct service
    // call carries the full key; a provider shape is a composite-key
    // representation decision that stays open until a consumer exists.
    (void)registry;
}

} // namespace ores::trading::messaging
