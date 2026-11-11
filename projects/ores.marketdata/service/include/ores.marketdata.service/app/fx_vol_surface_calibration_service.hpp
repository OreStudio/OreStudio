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
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_CALIBRATION_SERVICE_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_CALIBRATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include "ores.marketdata.service/export.hpp"

namespace ores::marketdata::service::app {

/**
 * @brief Calibrates a party's FX volatility surface blueprint at an as-of into a candidate.
 *
 * The service reads the blueprint, its pillars and their sources, and the pair's FX option
 * convention, and checks the recipe before it reads a quote. It then reads the party's published
 * forward set of the pair and the quote of each source series at the as-of, fits the smile model,
 * and writes the candidate: the grid as observations of the blueprint's output series, each with a
 * lineage that names the source series, their weights and the blueprint version, the series'
 * expiry and strike axes, and the calibration record with the fit and its checks. A candidate is
 * not a published surface. A rerun at the same as-of writes new versions of the same rows.
 */
class ORES_MARKETDATA_SERVICE_EXPORT fx_vol_surface_calibration_service final {
public:
    /// The largest error of a pillar, in volatility, that is not reported: a tenth of a point.
    static constexpr double pillar_tolerance = 0.001;

    [[nodiscard]] static ores::marketdata::messaging::calibrate_fx_vol_surface_response
    calibrate(ores::database::context ctx,
              const ores::marketdata::messaging::calibrate_fx_vol_surface_request& request);
};

}

#endif
