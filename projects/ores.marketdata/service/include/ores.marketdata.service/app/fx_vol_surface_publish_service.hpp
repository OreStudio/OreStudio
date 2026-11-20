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

/// The approval kind a surface publish raises.
inline constexpr std::string_view fx_vol_surface_publish_kind_code =
    "marketdata.fx_vol_surface_publish";

/**
 * @brief Why a calibration may not be published under any rule, or nothing when it may.
 *
 * A surface with calendar or butterfly arbitrage fails a blocking check, and so does one whose
 * checks cannot be read. A pillar error beyond the tolerance is advisory: the decider weighs it.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT std::optional<std::string>
blocking_failure_of(const ores::marketdata::domain::fx_vol_surface_calibration& calibration);

/// One surface a set of publishes asks a decider to authorise.
struct surface_publish_subject {
    boost::uuids::uuid blueprint_id{};
    std::chrono::system_clock::time_point as_of{};
};

/**
 * @brief What a set raise did: the request, and the members it could not carry.
 *
 * An empty request_id means nothing was raised, because no member could be.
 * Each refusal names its blueprint and the reason, so a caller reports the
 * surface it left out rather than failing the set for it.
 */
struct surface_publish_raise {
    std::string request_id;
    std::string state_code;
    /// The blueprints whose subjects the request names, in the order asked for.
    std::vector<boost::uuids::uuid> raised;
    /// The blueprints the request could not name, each with the reason.
    std::vector<std::pair<boost::uuids::uuid, std::string>> refused;
};

/**
 * @brief Raises one request that authorises a set of a party's surfaces.
 *
 * One decision for a party's set: the request names every surface it authorises,
 * and each is judged on its own at its apply. A member with no calibration, with
 * a blocking failure, or with a raise already open is left out and named, so a
 * decider never answers for a surface the service would refuse.
 *
 * The raise judges each member before anything is raised, so a decider never
 * answers for a surface the service would refuse; what it does not judge is
 * whether the surface holds at its apply, which the apply answers per member.
 *
 * @throws std::invalid_argument when no member can be raised, the reason is
 * empty, or a surface is named twice.
 */
[[nodiscard]] ORES_MARKETDATA_SERVICE_EXPORT surface_publish_raise
raise_surface_publish_set(const ores::database::context& ctx,
                          const boost::uuids::uuid& party_id,
                          const std::vector<surface_publish_subject>& subjects,
                          const std::string& reason);

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
    publish(const ores::database::context& ctx,
            const ores::marketdata::messaging::publish_fx_vol_surface_request& request);

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
