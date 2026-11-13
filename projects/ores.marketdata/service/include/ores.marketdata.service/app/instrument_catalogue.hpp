/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_MARKETDATA_SERVICE_APP_INSTRUMENT_CATALOGUE_HPP
#define ORES_MARKETDATA_SERVICE_APP_INSTRUMENT_CATALOGUE_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/export.hpp"

namespace ores::marketdata::service::app {

/**
 * @brief The instrument catalogue: one entry per instrument the party can see.
 *
 * An instrument is one series identity for one party: the instrument type, the
 * quote type and the identity fields the datum schema declares. Its axes are the
 * schema's coordinate fields, in the order the shape stores them, which is the
 * order a plot draws them. Its sources are the market series rows carrying that
 * identity, one per provider, because two providers' quotes of one ladder are
 * two series rows and not two instruments.
 *
 * Nothing here derives an identity, an axis order or a staleness verdict; the
 * schema and the shape tables state the first two, and this read states the
 * third.
 *
 * Only the caller's party is read. The series repository answers the tenant, so
 * the party filter is applied here: one series identity exists once per party,
 * and a read that omitted the party would show five of everything.
 */
ORES_MARKETDATA_SERVICE_EXPORT
ores::marketdata::messaging::list_instruments_response
list_instruments(const ores::database::context& ctx,
                 const ores::marketdata::messaging::list_instruments_request& request);

}

#endif
