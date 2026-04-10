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
#include "ores.trading.core/service/fx_digital_option_instrument_service.hpp"

#include <stdexcept>
#include "ores.service/messaging/handler_helpers.hpp"

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

fx_digital_option_instrument_service::fx_digital_option_instrument_service(
    context ctx) : ctx_(std::move(ctx)) {}

void fx_digital_option_instrument_service::save_fx_digital_option_instrument(
    const domain::fx_digital_option_instrument& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument(
            "FX digital option instrument id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving fx_digital_option_instrument: "
                               << v.instrument_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved fx_digital_option_instrument: "
                              << t.instrument_id;
}

}
