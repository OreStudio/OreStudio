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
#include "ores.reporting.core/service/scheduling_plan.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::reporting::service {

std::string scheduler_job_name(const boost::uuids::uuid& definition_id) {
    return "report_definition." + boost::uuids::to_string(definition_id);
}

std::optional<boost::uuids::uuid>
existing_job_for(const std::map<std::string, boost::uuids::uuid>& jobs_by_name,
                 const boost::uuids::uuid& definition_id) {
    const auto found = jobs_by_name.find(scheduler_job_name(definition_id));
    if (found == jobs_by_name.end())
        return std::nullopt;
    return found->second;
}

}
