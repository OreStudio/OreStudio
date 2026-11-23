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
#ifndef ORES_SYNTHETIC_SERVICE_FIXING_INPUTS_HPP
#define ORES_SYNTHETIC_SERVICE_FIXING_INPUTS_HPP

#include "ores.database/domain/context.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "simulator_offers.hpp"

namespace ores::synthetic::service {

/**
 * @brief What the simulated fixing publisher's offers read beyond the configs.
 *
 * Each currency pair's FX fixing is the fixing source of its default cut, taken at the cut's time
 * and zone; the pairs, the cuts and the sources are the system tenant's reference data. The days
 * the market already holds of an item are the fixings of the system party up to a date, which the
 * generators leave out of the history they state, so a value a publisher answered is the one that
 * stands.
 *
 * @param db_ctx The service's own context, which is the system tenant's.
 * @param svc_nats The service's authenticated client, which reads the fixings.
 */
[[nodiscard]] fixing_offer_inputs
read_fixing_offer_inputs(const ores::database::context& db_ctx,
                         ores::nats::service::nats_client& svc_nats);

}

#endif
