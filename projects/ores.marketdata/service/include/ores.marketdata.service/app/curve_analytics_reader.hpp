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
#ifndef ORES_MARKETDATA_SERVICE_APP_CURVE_ANALYTICS_READER_HPP
#define ORES_MARKETDATA_SERVICE_APP_CURVE_ANALYTICS_READER_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/export.hpp"

namespace ores::marketdata::service::app {

/**
 * @brief A curve's conventions, market pillars and bootstrapped points.
 *
 * The discount factor is the one the bootstrap already computes, the year
 * fraction is the day count's, and the zero rate is -ln(DF)/tau over them. The
 * implied forward is the rate the segment ending at the pillar implies, and the
 * shift is the bootstrapped zero rate less the market rate at the pillar. No
 * mathematics is added here; the read restates what the curve already is.
 */
ORES_MARKETDATA_SERVICE_EXPORT
ores::marketdata::messaging::get_curve_analytics_response
get_curve_analytics(const ores::database::context& ctx,
                    const ores::marketdata::messaging::get_curve_analytics_request& request);

}

#endif
