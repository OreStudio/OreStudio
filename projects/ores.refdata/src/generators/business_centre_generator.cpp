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
#include "ores.refdata/generators/business_centre_generator.hpp"

#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include <faker-cxx/internet.h>

namespace ores::refdata::generators {

std::vector<domain::business_centre>
generate_fictional_business_centres(std::size_t n) {
    const auto now = std::chrono::system_clock::now();
    const auto user = std::string(faker::internet::username());

    std::vector<domain::business_centre> all;
    all.reserve(20);

    all.push_back({
        .code = "XYLO", .source = "FpML", .description = "Xylonia Financial Centre",
        .coding_scheme_code = "FpML", .country_alpha2_code = "AL",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "MNZB", .source = "FpML", .description = "Monzabia Trading Hub",
        .coding_scheme_code = "FpML", .country_alpha2_code = "AR",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "BLTH", .source = "FpML", .description = "Balthoria Exchange Centre",
        .coding_scheme_code = "FpML", .country_alpha2_code = "BA",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "CLDR", .source = "FpML", .description = "Calandria Capital Markets",
        .coding_scheme_code = "FpML", .country_alpha2_code = "CA",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "DLVD", .source = "FpML", .description = "Delvadia Financial District",
        .coding_scheme_code = "FpML", .country_alpha2_code = "DE",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "ERIA", .source = "FpML", .description = "Eriador Central Market",
        .coding_scheme_code = "FpML", .country_alpha2_code = "ER",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "FLOR", .source = "FpML", .description = "Feloria Securities Exchange",
        .coding_scheme_code = "FpML", .country_alpha2_code = "FE",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "GLDR", .source = "FpML", .description = "Galdoria Financial Centre",
        .coding_scheme_code = "FpML", .country_alpha2_code = "GA",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "HDRN", .source = "FpML", .description = "Hydronia Commerce Centre",
        .coding_scheme_code = "FpML", .country_alpha2_code = "HY",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "ITHC", .source = "FpML", .description = "Ithaca Trading Floor",
        .coding_scheme_code = "FpML", .country_alpha2_code = "IT",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "JRVK", .source = "FpML", .description = "Jorvik Exchange Centre",
        .coding_scheme_code = "FpML", .country_alpha2_code = "JO",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "KELR", .source = "FpML", .description = "Kaelor Capital Markets",
        .coding_scheme_code = "FpML", .country_alpha2_code = "KA",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "LMNA", .source = "FpML", .description = "Luminia Financial Hub",
        .coding_scheme_code = "FpML", .country_alpha2_code = "LU",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "MLDR", .source = "FpML", .description = "Maldoria Securities Market",
        .coding_scheme_code = "FpML", .country_alpha2_code = "MA",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "NKTN", .source = "FpML", .description = "Nektonia Exchange",
        .coding_scheme_code = "FpML", .country_alpha2_code = "NE",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "ORNC", .source = "FpML", .description = "Orinoco Trading Centre",
        .coding_scheme_code = "FpML", .country_alpha2_code = "OR",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "PYRH", .source = "FpML", .description = "Pyrrhia Financial Market",
        .coding_scheme_code = "FpML", .country_alpha2_code = "PY",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "RNDL", .source = "FpML", .description = "Rendellia Capital Exchange",
        .coding_scheme_code = "FpML", .country_alpha2_code = "RE",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "SRND", .source = "FpML", .description = "Serendia Commerce Hub",
        .coding_scheme_code = "FpML", .country_alpha2_code = "SE",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });
    all.push_back({
        .code = "VLRA", .source = "FpML", .description = "Valoria Central Exchange",
        .coding_scheme_code = "FpML", .country_alpha2_code = "VA",
        .recorded_by = user, .change_reason_code = "system.test", .change_commentary = "Synthetic test data", .recorded_at = now
    });

    if (n == 0 || n >= all.size())
        return all;

    return std::vector<domain::business_centre>(all.begin(), all.begin() + n);
}

}
