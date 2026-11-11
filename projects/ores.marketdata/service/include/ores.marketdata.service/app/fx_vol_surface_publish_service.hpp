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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_PUBLISH_SERVICE_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_PUBLISH_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include <string_view>

namespace ores::marketdata::service::app {

/// The approval kind a surface publish raises.
inline constexpr std::string_view fx_vol_surface_publish_kind_code =
    "marketdata.fx_vol_surface_publish";

/**
 * @brief The approval-gated publish of a party's calibrated FX volatility surface, and the read of
 * a publish request's subject.
 *
 * A surface belongs to its blueprint's party, and only a person acting for that party raises its
 * publish. The raise names the calibration of the blueprint at the as-of and copies its checks to
 * the request's subject, so the decider reads the fit they approve. The apply publishes that
 * calibration once, ends every live surface of the output series in the same act, and sends the
 * notice.
 */
class ORES_MARKETDATA_SERVICE_EXPORT fx_vol_surface_publish_service final {
public:
    [[nodiscard]] static ores::marketdata::messaging::publish_fx_vol_surface_response
    publish(ores::database::context ctx,
            const ores::marketdata::messaging::publish_fx_vol_surface_request& request);

    /// The published surface of a blueprint at an as-of, or a slice of it. Never a candidate.
    [[nodiscard]] static ores::marketdata::messaging::get_published_fx_vol_surface_response
    read_published(
        ores::database::context ctx,
        const ores::marketdata::messaging::get_published_fx_vol_surface_request& request);

    [[nodiscard]] static ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response
    read(ores::database::context ctx,
         const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request& request);
};

}

#endif
