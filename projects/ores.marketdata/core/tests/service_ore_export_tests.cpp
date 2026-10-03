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
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.marketdata.core/service/ore_export_service.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.ore.core/market/market_data_serializer.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <sstream>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[service][ore_export]");

}

using namespace ores::logging;
using ores::marketdata::service::import_service;
using ores::marketdata::service::ore_export_service;

namespace {

/// A tenant of this test's own.
///
/// The test tenant is provisioned per test *binary*, so every case in
/// ores.marketdata.core.tests shares one. The export reads the whole tenant, so
/// another case's rows would land in the output and the comparison below would
/// be against a file this test never imported.
struct export_tenant {
    ores::testing::database_helper base;
    ores::database::context ctx;

    export_tenant()
        : ctx(base.context()) {
        const auto code =
            ores::testing::test_database_manager::generate_test_tenant_code("marketdata.export");
        const auto tenant = ores::testing::test_database_manager::provision_test_tenant(
            ctx, code, "ores.marketdata export");
        ctx = ores::database::service::tenant_context::with_tenant(base.context(), tenant);
    }
};

/// Splits a market data file into its data lines' fields, comma or whitespace
/// separated, dropping comments and blanks.
std::vector<std::string> file_keys(const std::string& content) {
    std::vector<std::string> keys;
    std::istringstream in(content);
    std::string line;
    while (std::getline(in, line)) {
        if (line.empty() || line[0] == '#')
            continue;
        std::replace(line.begin(), line.end(), ',', ' ');
        std::istringstream fields(line);
        std::string date;
        std::string key;
        if (fields >> date >> key)
            keys.push_back(key);
    }
    std::ranges::sort(keys);
    return keys;
}

/// The keys a serialised market data body carries, in the same order.
///
/// The body is DATE<TAB>KEY<TAB>VALUE, so the key is the second field. Read
/// back rather than sorted into place, because this is the assertion that pins
/// what the export emits: a key the export invented, or one it put a point on
/// that the producer never wrote, differs here and nowhere else.
std::vector<std::string> body_keys(const std::string& text) {
    std::vector<std::string> keys;
    std::istringstream in(text);
    std::string line;
    while (std::getline(in, line)) {
        const auto first = line.find('\t');
        if (first == std::string::npos)
            continue;
        const auto second = line.find('\t', first + 1);
        keys.push_back(line.substr(first + 1, second - first - 1));
    }
    std::ranges::sort(keys);
    return keys;
}

/// The same file put through the parser and the serializer with no database in
/// the way, as the content the round trip has to return.
std::string serialize_without_the_database(const std::string& content) {
    std::istringstream in(content);
    ores::ore::market::parse_report report;
    const auto data = ores::ore::market::parse_market_data(
        in, ores::ore::market::duplicate_policy::warn, &report);
    std::ostringstream out;
    ores::ore::market::serialize_market_data(out, data);
    return out.str();
}

std::string content_of(const std::string& relative_path) {
    return ores::platform::filesystem::file::read_content(
        ores::testing::project_root::resolve(relative_path));
}

}

TEST_CASE("export_reproduces_the_market_data_file_it_imported", tags) {
    auto lg(make_logger(test_suite));

    export_tenant t;
    ores::nats::service::nats_client auth_nats;
    import_service importer(t.ctx, auth_nats);

    // Chosen for the two shapes the export has to tell apart. ZERO, SWAPTION,
    // FX_OPTION, CDS and HAZARD_RATE carry a point of their own, which the
    // export must emit; FX/RATE and RECOVERY_RATE carry none, and the rows
    // still store one -- SPOT, and the empty string -- which the export must
    // drop. A file of one shape would exercise half the rule.
    const auto content =
        content_of("external/ore/examples/MinimalSetup/Input/market_20160205_flat.txt");
    REQUIRE_FALSE(content.empty());

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = content;
    req.source = "test.ore_export";
    const auto imported = importer.import(req);
    REQUIRE(imported.success);
    REQUIRE(imported.observation_count > 0);

    const ore_export_service exporter(t.ctx);
    const auto exported = exporter.write_all();

    CHECK(exported.observation_count == imported.observation_count);

    // The file's own keys, which is the point of the exercise: the export has
    // to write back the spelling it read, not the canonical one the import
    // stored.
    CHECK(body_keys(exported.market_data) == file_keys(content));

    // And the values and dates with them, compared against the parser and
    // serializer run with no database in the way.
    std::istringstream expected_lines(serialize_without_the_database(content));
    std::istringstream actual_lines(exported.market_data);
    std::vector<std::string> expected;
    std::vector<std::string> actual;
    for (std::string line; std::getline(expected_lines, line);)
        if (!line.empty())
            expected.push_back(line);
    for (std::string line; std::getline(actual_lines, line);)
        if (!line.empty())
            actual.push_back(line);
    std::ranges::sort(expected);
    std::ranges::sort(actual);
    CHECK(actual == expected);
}

TEST_CASE("export_reproduces_the_fixings_file_it_imported", tags) {
    auto lg(make_logger(test_suite));

    export_tenant t;
    ores::nats::service::nats_client auth_nats;
    import_service importer(t.ctx, auth_nats);

    // Two index names, so a body that emitted one name for every row would
    // differ from the file rather than coincidentally match it.
    const auto content = content_of("external/ore/examples/Exposure/Input/fixings_inflation.txt");
    REQUIRE_FALSE(content.empty());

    ores::marketdata::messaging::import_market_data_request req;
    req.fixings_content = content;
    req.source = "test.ore_export";
    const auto imported = importer.import(req);
    REQUIRE(imported.success);
    REQUIRE(imported.fixing_count == 2);

    const ore_export_service exporter(t.ctx);
    const auto exported = exporter.write_all();

    CHECK(exported.fixing_count == imported.fixing_count);
    CHECK(exported.observation_count == 0);

    // A fixing line is DATE<TAB>INDEX<TAB>VALUE, so the index names are the
    // middle field and have to match the file's second field.
    std::vector<std::string> expected_names;
    std::istringstream file(content);
    for (std::string line; std::getline(file, line);) {
        std::istringstream fields(line);
        std::string date;
        std::string name;
        if (fields >> date >> name)
            expected_names.push_back(name);
    }
    std::ranges::sort(expected_names);

    std::vector<std::string> actual_names;
    std::istringstream body(exported.fixings);
    for (std::string line; std::getline(body, line);) {
        const auto first = line.find('\t');
        if (first == std::string::npos)
            continue;
        const auto second = line.find('\t', first + 1);
        actual_names.push_back(line.substr(first + 1, second - first - 1));
    }
    std::ranges::sort(actual_names);

    CHECK(actual_names == expected_names);
}

namespace {

// One FX spot series with one observation, stored with the URIs given and no key:
// the export must take each key from the datum URI the row stores.
void write_fx_row(export_tenant& t, const std::string& series_uri, const std::string& datum_uri) {
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::market_observations_repository obs_repo;
    boost::uuids::random_generator gen;

    ores::marketdata::domain::market_series s;
    s.id = gen();
    s.tenant_id = t.ctx.tenant_id();
    s.party_id = t.ctx.party_id().value_or(boost::uuids::uuid{});
    s.oresmd_uri = series_uri;
    s.series_subclass = "spot";
    s.modified_by = t.ctx.actor();
    s.performed_by = t.ctx.service_account();
    s.change_reason_code =
        std::string(ores::dq::domain::change_reason_constants::codes::external_data_import);
    s.change_commentary = "ore_export test";
    series_repo.write(t.ctx, s);

    ores::marketdata::domain::market_observation o;
    o.id = gen();
    o.tenant_id = t.ctx.tenant_id();
    o.party_id = t.ctx.party_id().value_or(boost::uuids::uuid{});
    o.series_id = s.id;
    o.observation_datetime =
        std::chrono::sys_days{std::chrono::year{2016} / std::chrono::February / 5};
    o.oresmd_uri = datum_uri;
    o.value = "1.132337";
    obs_repo.write(t.ctx, o);
}

}

TEST_CASE("export_writes_each_key_from_the_datum_uri_the_row_stores", tags) {
    auto lg(make_logger(test_suite));

    export_tenant t;
    write_fx_row(t,
                 "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD",
                 "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");

    const ore_export_service exporter(t.ctx);
    const auto result = exporter.write_all();
    CHECK(result.observation_count == 1);
    CHECK(result.market_data.contains("FX/RATE/EUR/USD"));
}

TEST_CASE("export_refuses_an_observation_whose_uri_names_no_datum", tags) {
    auto lg(make_logger(test_suite));

    export_tenant t;
    write_fx_row(t,
                 "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD",
                 "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate");

    const ore_export_service exporter(t.ctx);
    CHECK_THROWS(exporter.write_all());
}
