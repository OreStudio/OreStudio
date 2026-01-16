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
#include "ores.dq/generators/dataset_generator.hpp"

#include <random>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/datetime.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::dq::generators {

using ores::utility::uuid::uuid_v7_generator;

domain::dataset generate_synthetic_dataset() {
    static uuid_v7_generator uuid_gen;
    static std::bernoulli_distribution bool_dist(0.5);
    static std::uniform_int_distribution<> depth_dist(0, 5);
    static std::random_device rd;
    static std::mt19937 gen(rd());

    domain::dataset r;
    r.version = 1;
    r.id = uuid_gen();
    if (bool_dist(gen)) {
        r.catalog_name = std::string(faker::word::noun());
    }
    r.subject_area_name = std::string(faker::word::noun());
    r.domain_name = std::string(faker::word::noun());
    if (bool_dist(gen)) {
        r.coding_scheme_code = std::string(faker::word::noun()) + "_" + std::string(faker::word::noun());
    }
    r.origin_code = std::string(faker::word::noun());
    r.nature_code = std::string(faker::word::noun());
    r.treatment_code = std::string(faker::word::noun());
    if (bool_dist(gen)) {
        r.methodology_id = uuid_gen();
    }
    r.name = std::string(faker::word::adjective()) + " " + std::string(faker::word::noun());
    r.description = std::string(faker::lorem::sentence());
    r.source_system_id = std::string(faker::word::noun()) + "_system";
    r.business_context = std::string(faker::lorem::sentence());
    r.lineage_depth = depth_dist(gen);
    r.ingestion_timestamp = utility::faker::datetime::past_timepoint();
    const auto dp = std::chrono::floor<std::chrono::days>(r.ingestion_timestamp);
    r.as_of_date = std::chrono::year_month_day{dp};
    if (bool_dist(gen)) {
        r.license_info = "MIT License";
    }
    r.recorded_by = std::string(faker::internet::username());
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::dataset>
generate_synthetic_datasets(std::size_t n) {
    std::vector<domain::dataset> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_dataset());
    return r;
}

}
