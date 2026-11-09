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
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_FORWARD_LADDER_SERVICE_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_FORWARD_LADDER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/fx_forward_operation_protocol.hpp"
#include "ores.marketdata.service/export.hpp"

namespace ores::marketdata::service::app {

/**
 * @brief Answers a pair's FX forward ladder from its spot, its conventions and the published
 * funding curve of each currency.
 *
 * Reads only. A missing input is a reply with success false and a message that names it.
 */
class ORES_MARKETDATA_SERVICE_EXPORT fx_forward_ladder_service final {
public:
    [[nodiscard]] static ores::marketdata::messaging::get_fx_forward_ladder_response
    read(ores::database::context ctx,
         const ores::marketdata::messaging::get_fx_forward_ladder_request& request);
};

}

#endif
