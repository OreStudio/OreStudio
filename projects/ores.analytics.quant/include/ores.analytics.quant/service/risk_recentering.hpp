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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_RISK_RECENTERING_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_RISK_RECENTERING_HPP

#include "ores.analytics.quant/domain/staleness_policy.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/rate_engine.hpp"
#include <chrono>
#include <string>

namespace ores::analytics::quant::service {

/**
 * @brief Re-roots the CRM into a star-shaped matrix around @p
 * aggregation_code, for risk: bumping one currency's edge in the returned
 * engine cannot disturb any other currency's rate, which the general-tree
 * topology cannot guarantee (see the CRM risk/recentering knowledge doc).
 *
 * A one-shot, point-in-time snapshot -- not a live transform. The
 * returned engine is seeded with @p source's *current* derived rate for
 * every currency (via @c rate_engine::rate), preserving each rate's
 * original @c as_of timestamp so the recentred engine's own staleness
 * accounting stays honest; a currency that is @c unavailable in @p
 * source is left unseeded, not fabricated. From then on the two engines
 * are independent -- updating one does not affect the other.
 *
 * Reuses @c topology_builder for the new star topology: every edge
 * shares @p aggregation_code as one side, so it can never trigger a
 * cycle conflict.
 */
ORES_ANALYTICS_QUANT_EXPORT rate_engine
recenter(const rate_engine& source,
         const std::string& aggregation_code,
         domain::staleness_policy policy,
         std::chrono::system_clock::time_point now = std::chrono::system_clock::now());

} // namespace ores::analytics::quant::service

#endif
