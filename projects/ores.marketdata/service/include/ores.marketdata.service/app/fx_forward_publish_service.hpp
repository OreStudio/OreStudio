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
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_FORWARD_PUBLISH_SERVICE_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_FORWARD_PUBLISH_SERVICE_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.marketdata.api/messaging/fx_forward_operation_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include <string_view>

namespace ores::marketdata::service::app {

/// The approval kind a forward set publish raises.
inline constexpr std::string_view fx_forward_publish_kind_code = "marketdata.fx_forward_publish";

/**
 * @brief The approval-gated publish of a party's FX forward set, and its read.
 *
 * A forward set is derived data and belongs to the caller's party. The raise computes the set
 * from the party's own spot and published funding curves and stores it as the request's
 * candidate, so the decider reads the numbers they approve. The apply publishes exactly those
 * numbers. A set never changes once written, so a read returns the published numbers after the
 * market has moved.
 */
class ORES_MARKETDATA_SERVICE_EXPORT fx_forward_publish_service final {
public:
    [[nodiscard]] static ores::marketdata::messaging::publish_fx_forward_response
    publish(const ores::database::context& ctx,
            const ores::marketdata::messaging::publish_fx_forward_request& request);

    [[nodiscard]] static ores::marketdata::messaging::get_published_fx_forward_response
    read(const ores::database::context& ctx,
         const ores::marketdata::messaging::get_published_fx_forward_request& request);
};

}

#endif
