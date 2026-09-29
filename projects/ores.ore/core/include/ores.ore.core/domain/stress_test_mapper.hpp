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
#ifndef ORES_ORE_CORE_DOMAIN_STRESS_TEST_MAPPER_HPP
#define ORES_ORE_CORE_DOMAIN_STRESS_TEST_MAPPER_HPP

#include "ores.analytics.api/domain/stress_test_library.hpp"
#include "ores.analytics.api/domain/stress_test_scenario.hpp"
#include "ores.analytics.api/domain/stress_test_shift.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/export.hpp"
#include <map>
#include <string>
#include <vector>

namespace ores::ore::domain {

/**
 * @brief One ORE stress document, mapped to the analytics stress entities.
 *
 * The library is the document and the scenarios are its StressTest elements,
 * in order. The shift blocks a scenario carries are not modelled yet, so each
 * family a scenario uses is counted into unmodelled: a document that applies
 * shifts cannot round trip until they are, and saying which families are in
 * play is how the next step is chosen.
 */
struct mapped_stress_scenario {
    analytics::domain::stress_test_scenario scenario;
    std::vector<analytics::domain::stress_test_shift> shifts;

    friend bool operator==(const mapped_stress_scenario&, const mapped_stress_scenario&) = default;
};

struct mapped_stress_test {
    analytics::domain::stress_test_library library;
    std::vector<mapped_stress_scenario> scenarios;
    std::map<std::string, std::size_t> unmodelled;

    friend bool operator==(const mapped_stress_test&, const mapped_stress_test&) = default;
};

/**
 * @brief Maps between an ORE stress document and the analytics stress entities.
 */
class ORES_ORE_CORE_EXPORT stress_test_mapper {
public:
    static mapped_stress_test map(const stresstesting& v);
    static stresstesting reverse(const mapped_stress_test& v);
};

}

#endif
