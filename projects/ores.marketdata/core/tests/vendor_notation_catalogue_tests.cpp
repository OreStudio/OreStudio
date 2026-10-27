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
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.api/domain/market_data_vendor_instrument.hpp"
#include "ores.marketdata.core/repository/market_data_vendor_instrument_repository.hpp"
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>

// The catalogue installs the vendor instruments of Bloomberg, LSEG and Demo, and the notations of
// those providers read and write ids. This test puts the two side by side: every installed row
// that has a notation must read, and must write back as itself. A row the notation refuses, or
// writes as another id, is a row that the catalogue and the notation disagree on.

namespace {

namespace vendor = ores::marketdata::vendor;

} // namespace

TEST_CASE("every_installed_vendor_instrument_roundtrips_through_its_providers_notation", "[vendor_notation][catalogue]") {
    ores::testing::scoped_database_helper h;
    const auto& tenant_ctx = h.context();

    // A tenant reads the rows the system tenant holds.
    ores::marketdata::repository::market_data_vendor_instrument_repository repo;
    const auto rows = repo.read_latest(tenant_ctx);

    std::map<std::string, std::size_t> checked;
    for (const auto& row : rows) {
        const auto* notation = vendor::notation_of(row.provider_code);
        if (!notation)
            continue;
        INFO(row.provider_code << ": " << row.external_id << " " << row.field);
        const ores::marketdata::messaging::vendor_id id{
            .id_scheme = row.id_scheme, .external_id = row.external_id, .field = row.field};

        const auto read = notation->read(id);
        REQUIRE(read.has_value());
        const auto written = notation->write(*read);
        REQUIRE(written.has_value());
        CHECK(written->id_scheme == id.id_scheme);
        CHECK(written->external_id == id.external_id);
        CHECK(written->field == id.field);
        ++checked[row.provider_code];
    }

    // The catalogue holds the 26 FX pairs and the 60 interest rate pillars of the uniform demo
    // curves for each of the three providers. Other suites share the database and may add rows of
    // their own, so the count is a floor.
    CHECK(checked["bloomberg"] >= 86);
    CHECK(checked["lseg"] >= 86);
    CHECK(checked["demo"] >= 86);
}
