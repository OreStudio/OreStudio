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
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_PUBLICATION_KIND_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_VOL_SURFACE_PUBLICATION_KIND_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.inbox.api/domain/approval_request.hpp"
#include "ores.marketdata.api/domain/fx_vol_surface_calibration.hpp"
#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Why a calibration may not be published under any rule, or nothing when it may.
 *
 * A surface with calendar or butterfly arbitrage fails a blocking check, and so does one whose
 * checks cannot be read. A pillar error beyond the tolerance is advisory: the decider weighs it.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::optional<std::string>
blocking_failure_of(const ores::marketdata::domain::fx_vol_surface_calibration& calibration);

/**
 * @brief The published surface a person reads, and the subject a request states.
 *
 * The read answers with the surface the as-of rule selects, never with a
 * candidate: the publish records which calibration became the set in use, and
 * this class finds that set and hands it to the screen. The publish itself is
 * the lifecycle's, in publication_lifecycle.cpp.
 */
class ORES_MARKETDATA_SERVICE_EXPORT fx_vol_surface_published_reader final {
public:
    /// The published surface of a blueprint at an as-of, or a slice of it. Never a candidate.
    [[nodiscard]] static ores::marketdata::messaging::get_published_fx_vol_surface_response
    read_published(
        const ores::database::context& ctx,
        const ores::marketdata::messaging::get_published_fx_vol_surface_request& request);

    [[nodiscard]] static ores::marketdata::messaging::get_fx_vol_surface_publish_subject_response
    read(const ores::database::context& ctx,
         const ores::marketdata::messaging::get_fx_vol_surface_publish_subject_request& request);
};

}

#endif
