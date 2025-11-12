/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.accounts/repository/feature_flags_mapper.hpp"

#include <algorithm>

namespace ores::accounts::repository {

using namespace ores::utility::log;

domain::feature_flags feature_flags_mapper::map(const feature_flags_entity& v) {
    BOOST_LOG_SEV(lg(), debug) << "Mapping db entity: " << v;

    domain::feature_flags r;
    r.name = v.name.value();
    r.enabled = v.enabled != 0 ? true : false;
    r.description = v.description;
    r.modified_by = v.modified_by;

    BOOST_LOG_SEV(lg(), debug) << "Mapped db entity. Result: " << r;
    return r;
}

feature_flags_entity feature_flags_mapper::map(const domain::feature_flags& v) {
    BOOST_LOG_SEV(lg(), debug) << "Mapping domain entity: " << v;

    feature_flags_entity r;
    r.name = v.name;
    r.enabled = v.enabled;
    r.description = v.description;
    r.modified_by = v.modified_by;

    BOOST_LOG_SEV(lg(), debug) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::feature_flags>
feature_flags_mapper::map(const std::vector<feature_flags_entity>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Mapping db entities. Total: " << v.size();

    std::vector<domain::feature_flags> r;
    r.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });

    BOOST_LOG_SEV(lg(), debug) << "Mapped db entities.";
    return r;
}

std::vector<feature_flags_entity>
feature_flags_mapper::map(const std::vector<domain::feature_flags>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Mapping domain entities. Count: " << v.size();

    std::vector<feature_flags_entity> r;
    r.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });
    BOOST_LOG_SEV(lg(), debug) << "Mapped domain entities.";
    return r;
}

}
