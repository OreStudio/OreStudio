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
#ifndef ORES_SYNTHETIC_API_DOMAIN_STEP_SUBJECTS_HPP
#define ORES_SYNTHETIC_API_DOMAIN_STEP_SUBJECTS_HPP

#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief The subjects the workflow engine dispatches this component's
 * publish-from-DQ steps to.
 *
 * The engine publishes a step command as a JetStream message, so each subject
 * must be covered by a stream before any run that dispatches it can start. The
 * string each one states is also the target_subject of the DQ artefact type
 * that names it, in projects/ores.sql/populate/dq/dq_artefact_types_populate.sql,
 * so the two are one value written twice and both move together.
 */
inline constexpr std::string_view publish_feeds_from_dq_step_subject =
    "synthetic.v1.ops.publish_feeds_from_dq";

inline constexpr std::string_view publish_market_data_providers_from_dq_step_subject =
    "synthetic.v1.ops.publish_market_data_providers_from_dq";

}

#endif
