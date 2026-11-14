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
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_MONTAGE_READER_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_MONTAGE_READER_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/export.hpp"

namespace ores::marketdata::service::app {

/**
 * @brief One base currency's crosses at an as-of.
 *
 * A montage is for the base, so a pair whose base is on the quote side is
 * included. The change and the day's range are measured from an instant the
 * response states, because a trading day starts when the session says it does;
 * they are absent, not zero, when the window holds no price at its start.
 */
ORES_MARKETDATA_SERVICE_EXPORT
ores::marketdata::messaging::get_fx_montage_response
get_fx_montage(const ores::database::context& ctx,
               const ores::marketdata::messaging::get_fx_montage_request& request);

}

#endif
