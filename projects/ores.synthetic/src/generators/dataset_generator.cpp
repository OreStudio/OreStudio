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
#include "ores.synthetic/generators/dataset_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/generation/generation_keys.hpp"

namespace ores::synthetic::generators {

using ores::utility::generation::generation_keys;

dq::domain::dataset generate_synthetic_dataset(
    utility::generation::generation_context& ctx) {
    const auto modified_by = ctx.env().get_or(
        generation_keys::modified_by, "system");

    dq::domain::dataset r;
    r.version = 1;
    r.id = ctx.generate_uuid();
    if (ctx.random_bool()) {
        r.catalog_name = std::string(faker::word::noun());
    }
    r.subject_area_name = std::string(faker::word::noun());
    r.domain_name = std::string(faker::word::noun());
    if (ctx.random_bool()) {
        r.coding_scheme_code = std::string(faker::word::noun()) + "_" +
            std::string(faker::word::noun());
    }
    r.origin_code = std::string(faker::word::noun());
    r.nature_code = std::string(faker::word::noun());
    r.treatment_code = std::string(faker::word::noun());
    if (ctx.random_bool()) {
        r.methodology_id = ctx.generate_uuid();
    }
    r.name = std::string(faker::word::adjective()) + " " +
        std::string(faker::word::noun());
    r.description = std::string(faker::lorem::sentence());
    r.source_system_id = std::string(faker::word::noun()) + "_system";
    r.business_context = std::string(faker::lorem::sentence());
    r.lineage_depth = ctx.random_int(0, 5);
    r.ingestion_timestamp = ctx.past_timepoint();
    r.as_of_date = std::chrono::floor<std::chrono::days>(r.ingestion_timestamp);
    if (ctx.random_bool()) {
        r.license_info = "MIT License";
    }
    r.modified_by = modified_by;
    r.change_commentary = "Synthetic test data";
    r.recorded_at = ctx.past_timepoint();
    return r;
}

std::vector<dq::domain::dataset>
generate_synthetic_datasets(std::size_t n,
    utility::generation::generation_context& ctx) {
    std::vector<dq::domain::dataset> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_dataset(ctx));
    return r;
}

}
