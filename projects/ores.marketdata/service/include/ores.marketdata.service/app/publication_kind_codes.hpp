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
#ifndef ORES_MARKETDATA_SERVICE_APP_PUBLICATION_KIND_CODES_HPP
#define ORES_MARKETDATA_SERVICE_APP_PUBLICATION_KIND_CODES_HPP

#include <string_view>

namespace ores::marketdata::service::app {

/**
 * @brief The approval kind each kind of derived market object publishes under.
 *
 * A publish is a request and not a save: the person who owns the set asks, a
 * person holding the kind's decider permission answers, and only an approved
 * request publishes. The kinds are seeded in
 * projects/ores.sql/populate/inbox/inbox_approval_kinds_populate.sql, and the
 * declaration in derived_market_object_kind.hpp pairs each with its subject.
 */
inline constexpr std::string_view curve_publish_kind_code = "marketdata.curve_publish";
inline constexpr std::string_view fx_forward_publish_kind_code = "marketdata.fx_forward_publish";
inline constexpr std::string_view fx_vol_surface_publish_kind_code =
    "marketdata.fx_vol_surface_publish";

/**
 * @brief The words a publish answers with, in the response's own status field.
 *
 * A person's publish and the sweep share them, so the two cannot report the same
 * set differently. Published moved the set. Raised waits on a decision and moved
 * nothing. Deferred did not run because an input the set declares is not there
 * yet, and the next firing tries again. Refused is the gate's no. Failed is an
 * error.
 */
inline constexpr std::string_view publish_status_published = "published";
inline constexpr std::string_view publish_status_raised = "raised";
inline constexpr std::string_view publish_status_deferred = "deferred";
inline constexpr std::string_view publish_status_refused = "refused";
inline constexpr std::string_view publish_status_failed = "failed";

}

#endif
