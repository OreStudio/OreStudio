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
#include "ores.assets/repository/currency_image_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::assets::repository {

using namespace ores::telemetry::log;
using namespace ores::database::repository;

domain::currency_image currency_image_mapper::map(const currency_image_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::currency_image r;
    r.iso_code = v.iso_code.value();
    r.image_id = v.image_id;
    r.assigned_by = v.assigned_by;
    r.assigned_at = v.assigned_at.value().str();

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

currency_image_entity currency_image_mapper::map(const domain::currency_image& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    currency_image_entity r;
    r.iso_code = v.iso_code;
    r.image_id = v.image_id;
    r.assigned_by = v.assigned_by;
    // Note: assigned_at is read-only; managed by database triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::currency_image>
currency_image_mapper::map(const std::vector<currency_image_entity>& v) {
    return map_vector<currency_image_entity, domain::currency_image>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<currency_image_entity>
currency_image_mapper::map(const std::vector<domain::currency_image>& v) {
    return map_vector<domain::currency_image, currency_image_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
