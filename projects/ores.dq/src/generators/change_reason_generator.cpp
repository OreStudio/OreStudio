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
#include "ores.dq/generators/change_reason_generator.hpp"

#include <random>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/faker/datetime.hpp"

namespace ores::dq::generators {

domain::change_reason generate_synthetic_change_reason() {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<> order_dist(1, 100);
    static std::bernoulli_distribution bool_dist(0.5);

    domain::change_reason r;
    r.version = 1;
    auto category = std::string{faker::word::noun()};
    auto reason = std::string{faker::word::verb()};
    r.code = category + "." + reason;
    r.description = std::string(faker::lorem::sentence());
    r.category_code = category;
    r.applies_to_amend = bool_dist(gen);
    r.applies_to_delete = bool_dist(gen);
    r.requires_commentary = bool_dist(gen);
    r.display_order = order_dist(gen);
    r.recorded_by = std::string(faker::internet::username());
    r.change_commentary = "Synthetic test data";
    r.recorded_at = utility::faker::datetime::past_timepoint();
    return r;
}

std::vector<domain::change_reason>
generate_synthetic_change_reasons(std::size_t n) {
    std::vector<domain::change_reason> r;
    r.reserve(n);
    while (r.size() < n)
        r.push_back(generate_synthetic_change_reason());
    return r;
}

}
