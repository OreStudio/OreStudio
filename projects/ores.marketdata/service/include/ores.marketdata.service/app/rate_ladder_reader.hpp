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
#ifndef ORES_MARKETDATA_SERVICE_APP_RATE_LADDER_READER_HPP
#define ORES_MARKETDATA_SERVICE_APP_RATE_LADDER_READER_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/export.hpp"

namespace ores::marketdata::service::app {

/**
 * @brief One currency's rate ladder at an as-of, in maturity order.
 *
 * A ladder is one series read across its term axis, so the rungs are the
 * coordinates of the provider's series and the provider dimension is one series
 * row per provider. Every rung's maturity date is resolved through the tenor
 * catalog, because the calendar and the convention are the server's; the read
 * states the convention it used, since a ladder series carries none of its own.
 *
 * A provider that does not quote a rung has no entry, never a zero. The
 * consensus is the median across the quoting providers, and it states its
 * method and how many quotes it rested on.
 */
ORES_MARKETDATA_SERVICE_EXPORT
ores::marketdata::messaging::get_rate_ladder_response
get_rate_ladder(const ores::database::context& ctx,
                const ores::marketdata::messaging::get_rate_ladder_request& request);

}

#endif
