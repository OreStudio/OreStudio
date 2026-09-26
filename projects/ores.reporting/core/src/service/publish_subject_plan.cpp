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
#include "ores.reporting.core/service/publish_subject_plan.hpp"
#include <algorithm>

namespace ores::reporting::service {

std::string publish_from_dq_function(std::string_view subject) {
    const auto v1_pos = subject.find("v1.");
    if (v1_pos == std::string_view::npos)
        return {};
    const auto start = v1_pos + 3;
    const auto end = subject.rfind(".publish-from-dq");
    if (end == std::string_view::npos || end <= start)
        return {};
    auto entity = std::string(subject.substr(start, end - start));
    std::replace(entity.begin(), entity.end(), '-', '_');
    return "ores_reporting_publish_" + entity + "_from_dq_fn";
}

}
