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
#ifndef ORES_REPORTING_CORE_SERVICE_SCHEDULING_PLAN_HPP
#define ORES_REPORTING_CORE_SERVICE_SCHEDULING_PLAN_HPP

#include "ores.reporting.core/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <map>
#include <optional>
#include <string>

namespace ores::reporting::service {

/**
 * @brief The name a scheduler job carries for a report definition.
 *
 * The scheduler refuses two current jobs with one name, so this string is the
 * identity reconciliation matches on. It is a function rather than a literal
 * because getting it wrong is what made reconcile fail on every start: it
 * generated a fresh job id for a definition whose job already existed.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
scheduler_job_name(const boost::uuids::uuid& definition_id);

/**
 * @brief The job the scheduler already holds for a definition, if any.
 *
 * Reconciliation adopts what it finds rather than insisting on creating its
 * own: a job that exists under the definition's name is the job for that
 * definition, whatever id it was given, and reusing its id is what makes the
 * pass converge instead of colliding on the name index.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::optional<boost::uuids::uuid>
existing_job_for(const std::map<std::string, boost::uuids::uuid>& jobs_by_name,
                 const boost::uuids::uuid& definition_id);

}

#endif
