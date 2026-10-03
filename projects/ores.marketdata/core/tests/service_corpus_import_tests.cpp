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
#include "corpus_files.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/datum/ore_index_codec.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.ore.core/market/series_key_registry.hpp"
#include "ores.ore.core/repository/series_key_shape_repository.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/test_database_manager.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstddef>
#include <filesystem>
#include <format>
#include <functional>
#include <map>
#include <rfl/json.hpp>
#include <sstream>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

// The oresmd coverage test walks the ORE corpus and asserts that every key can
// be named and read back -- in memory. This takes the same corpus all the way
// into the database: it imports real files through the import service and then
// reads the rows out again, so that "every sample file reads into the database"
// is a measurement rather than an assumption.
//
// Both cases run by default. The sampled case is the quick signal a developer
// wants while iterating; the corpus-wide case is the acceptance the story was
// written for, and it takes long enough that a full suite run is a meal rather
// than a pause. Hiding it was the older arrangement, and it hid the acceptance
// with it: a change that broke the corpus walk merged green because no run
// looked at it.

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[corpus][import_service]");

/// The payload lines of @p content, without comments and blanks: a cheap size
/// for choosing a sample, not a reading of the file.
std::size_t payload_line_count(const std::string& content) {
    std::size_t n = 0;
    std::istringstream stream(content);
    std::string line;
    while (std::getline(stream, line)) {
        const auto start = line.find_first_not_of(" \t\r\n");
        if (start != std::string::npos && line[start] != '#')
            ++n;
    }
    return n;
}

/// The smallest payloads, until @p budget lines have been taken.
///
/// Bounded rather than capped at a file count, so the default case costs a
/// predictable number of database writes however the corpus grows.
std::vector<std::filesystem::path> sample_payloads(std::size_t budget) {
    const auto all = ores::marketdata::test::market_payloads(
        ores::testing::project_root::resolve("external/ore/examples"));
    std::vector<std::pair<std::size_t, std::filesystem::path>> sized;
    for (const auto& p : all) {
        const auto content = ores::platform::filesystem::file::read_content(p);
        sized.emplace_back(payload_line_count(content), p);
    }
    std::sort(sized.begin(), sized.end(), [](const auto& a, const auto& b) {
        return a.first != b.first ? a.first < b.first : a.second < b.second;
    });
    std::vector<std::filesystem::path> chosen;
    std::size_t taken = 0;
    for (const auto& [count, path] : sized) {
        if (count == 0)
            continue;
        if (taken + count > budget && !chosen.empty())
            break;
        chosen.push_back(path);
        taken += count;
    }
    return chosen;
}

/// The source tag this helper stamps on a file's rows.
///
/// The test database helper hands every helper the same tenant -- it is the one
/// named by the environment, not a per-instance one -- so a file's rows have to
/// be told apart from the files imported before it some other way. The source
/// column is exactly that: the import stamps it from the request, and nothing
/// else in the run carries this tag.
std::string source_tag_for(const std::filesystem::path& path) {
    auto name = path.filename().string();
    std::replace(name.begin(), name.end(), '.', '_');
    return "test.corpus." + name + "." +
           std::to_string(std::hash<std::string>{}(path.string()) % 100000);
}

/// A tenant of this test's own.
///
/// The test tenant is provisioned per test *binary*, not per case, so every
/// case in ores.marketdata.core.tests shares one. A corpus walk writes real ORE
/// keys, and the hand-written import cases in this binary read those same keys
/// back, so without a private tenant this test changes their row counts.
struct corpus_tenant {
    ores::testing::database_helper base;
    ores::database::context ctx;

    corpus_tenant()
        : ctx(base.context()) {
        const auto code =
            ores::testing::test_database_manager::generate_test_tenant_code("marketdata.corpus");
        const auto tenant = ores::testing::test_database_manager::provision_test_tenant(
            ctx, code, "ores.marketdata corpus import");
        ctx = ores::database::service::tenant_context::with_tenant(base.context(), tenant);
    }
};

/// One stored or expected value: its date, the key it is filed under, and the
/// value, so a value that reached the database under another key or date fails.
using dated_value = std::tuple<std::string, std::string, std::string>;

std::string iso(std::chrono::year_month_day d) {
    return std::format("{:%Y-%m-%d}", std::chrono::sys_days{d});
}

/// Rows of a query that returns one JSON array of three strings per row.
std::vector<dated_value> triples_of(ores::database::context ctx,
                                    const std::string& sql,
                                    const std::string& tag,
                                    const std::string& what) {
    auto lg(ores::logging::make_logger(test_suite));
    const auto rows =
        ores::database::repository::execute_parameterized_string_query(ctx, sql, {tag}, lg, what);
    std::vector<dated_value> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        const auto t = rfl::json::read<std::vector<std::string>>(row);
        if (!t || t->size() != 3)
            FAIL("unreadable row: " << row);
        result.emplace_back((*t)[0], (*t)[1], (*t)[2]);
    }
    return result;
}

/// The observations stamped with @p tag, each with the key its datum URI writes.
///
/// One query, filtered on the source tag the import stamped, so the check costs
/// the same for every file however many rows the tenant already holds.
std::vector<dated_value> stored_observations(ores::database::context ctx, const std::string& tag) {
    using namespace ores::marketdata;
    auto rows = triples_of(ctx,
                           "SELECT json_build_array(observation_datetime::date::text, oresmd_uri,"
                           " value)::text FROM ores_marketdata_market_observations_tbl"
                           " WHERE source = $1 AND valid_to = ores_utility_infinity_timestamp_fn()",
                           tag,
                           "Reading one corpus file's observations");
    for (auto& [date, uri, value] : rows) {
        const auto d = datum::oresmd_uri_codec::read(uri);
        if (!d)
            FAIL("stored URI does not read: " << uri << ": " << d.error());
        uri = datum::ore_key_codec::write(*d).value();
    }
    std::sort(rows.begin(), rows.end());
    return rows;
}

/// The fixings stamped with @p tag, each with the index name its series names.
std::vector<dated_value> stored_fixings(ores::database::context ctx, const std::string& tag) {
    using namespace ores::marketdata;
    auto rows =
        triples_of(ctx,
                   "SELECT json_build_array(f.fixing_date::text, s.oresmd_uri, f.value)::text"
                   " FROM ores_marketdata_market_fixings_tbl f"
                   " JOIN ores_marketdata_market_series_tbl s ON s.id = f.series_id"
                   " AND s.valid_to = ores_utility_infinity_timestamp_fn()"
                   " WHERE f.source = $1"
                   " AND f.valid_to = ores_utility_infinity_timestamp_fn()",
                   tag,
                   "Reading one corpus file's fixings");
    for (auto& [date, uri, value] : rows) {
        const auto index = datum::oresmd_uri_codec::read_index(uri);
        if (!index)
            FAIL("stored fixing URI does not read: " << uri << ": " << index.error());
        uri = datum::ore_index_codec::write(*index);
    }
    std::sort(rows.begin(), rows.end());
    return rows;
}

/// Imports the market payload at @p path and asserts the rows it wrote are the
/// file's values under the file's keys, in canonical spelling.
///
/// The file is read with the production tokeniser, so the expected rows are the
/// ones the import itself sees: ORE's own examples repeat a (date, key) pair, the
/// reader keeps the last one and reports each repeat, and comparing against every
/// line would report that as data loss. Returns the payload lines the file
/// carried, so a caller can total them across files.
std::size_t import_and_verify(const std::filesystem::path& path,
                              ores::marketdata::service::import_service& svc,
                              ores::database::context ctx,
                              const ores::ore::market::series_key_registry& registry) {
    using namespace ores::marketdata;
    const auto content = ores::platform::filesystem::file::read_content(path);
    const auto tag = source_tag_for(path);
    INFO("file: " << path.string());

    std::istringstream in(content);
    ores::ore::market::parse_report report;
    const auto data = ores::ore::market::parse_market_data(
        in, registry, ores::ore::market::duplicate_policy::warn, &report);

    // The import warns once per repeated (date, key) and once per key it stores
    // under another spelling.
    std::vector<dated_value> wanted;
    wanted.reserve(data.size());
    std::size_t respelled = 0;
    for (const auto& d : data) {
        const auto datum = datum::ore_key_codec::read(d.key);
        if (!datum)
            FAIL("corpus key " << d.key << " does not read: " << datum.error());
        const auto canonical = datum::ore_key_codec::write(*datum).value();
        if (canonical != d.key)
            ++respelled;
        wanted.emplace_back(iso(d.date), canonical, d.value);
    }
    std::sort(wanted.begin(), wanted.end());

    messaging::import_market_data_request req;
    req.market_data_content = content;
    req.source = tag;
    messaging::import_market_data_response resp;
    try {
        resp = svc.import(req);
    } catch (const std::exception& e) {
        FAIL(e.what());
    }

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == static_cast<int>(data.size()));
    CHECK(resp.errors.empty());
    CHECK(resp.warnings.size() == report.warnings.size() + respelled);
    CHECK(stored_observations(ctx, tag) == wanted);
    return data.size() + report.warnings.size();
}

/// Imports the fixing payload at @p path and asserts the rows it wrote are the
/// file's values under the file's index names, read with the production
/// tokeniser. A name the fixing grammar cannot name is reported and dropped by
/// the import, so it is expected to be missing and counted as a warning.
std::size_t import_fixings_and_verify(const std::filesystem::path& path,
                                      ores::marketdata::service::import_service& svc,
                                      ores::database::context ctx) {
    using namespace ores::marketdata;
    const auto content = ores::platform::filesystem::file::read_content(path);
    const auto tag = source_tag_for(path);
    INFO("file: " << path.string());

    std::istringstream in(content);
    ores::ore::market::parse_report report;
    const auto data =
        ores::ore::market::parse_fixings(in, ores::ore::market::duplicate_policy::warn, &report);

    std::vector<dated_value> wanted;
    std::size_t unnamed = 0;
    for (const auto& f : data) {
        const auto index = datum::ore_index_codec::read(f.index_name);
        if (!index) {
            ++unnamed;
            continue;
        }
        wanted.emplace_back(iso(f.date), datum::ore_index_codec::write(*index), f.value);
    }
    std::sort(wanted.begin(), wanted.end());

    messaging::import_market_data_request req;
    req.fixings_content = content;
    req.source = tag;
    messaging::import_market_data_response resp;
    try {
        resp = svc.import(req);
    } catch (const std::exception& e) {
        FAIL(e.what());
    }

    REQUIRE(resp.success);
    REQUIRE(resp.fixing_count == static_cast<int>(wanted.size()));
    CHECK(resp.errors.empty());
    CHECK(resp.warnings.size() == report.warnings.size() + unnamed);
    CHECK(stored_fixings(ctx, tag) == wanted);
    return data.size() + report.warnings.size();
}

/// The shape table the production tokeniser still asks for.
ores::ore::market::series_key_registry registry_for(ores::database::context ctx) {
    return ores::ore::market::series_key_registry{
        ores::ore::repository::series_key_shape_repository{}.read_latest(ctx)};
}

} // namespace

using namespace ores::logging;
using ores::marketdata::service::import_service;
using ores::testing::database_helper;

TEST_CASE("every_line_of_a_sampled_corpus_file_reaches_the_database", tags) {
    auto lg(make_logger(test_suite));

    corpus_tenant tenant;
    ores::nats::service::nats_client auth_nats;
    import_service svc(tenant.ctx, auth_nats);

    const auto sample = sample_payloads(50);
    REQUIRE_FALSE(sample.empty());
    const auto registry = registry_for(tenant.ctx);

    std::size_t total = 0;
    for (const auto& path : sample)
        total += import_and_verify(path, svc, tenant.ctx, registry);

    BOOST_LOG_SEV(lg, info) << "Verified " << total << " line(s) over " << sample.size()
                            << " corpus file(s).";
    CHECK(total > 0);
}

TEST_CASE("every_line_of_the_whole_corpus_reaches_the_database", "[corpus-full]") {
    auto lg(make_logger(test_suite));

    corpus_tenant tenant;
    ores::nats::service::nats_client auth_nats;
    import_service svc(tenant.ctx, auth_nats);

    const auto all = ores::marketdata::test::market_payloads(
        ores::testing::project_root::resolve("external/ore/examples"));
    REQUIRE_FALSE(all.empty());
    const auto registry = registry_for(tenant.ctx);

    std::size_t total = 0;
    for (const auto& path : all)
        total += import_and_verify(path, svc, tenant.ctx, registry);

    BOOST_LOG_SEV(lg, info) << "Verified " << total << " line(s) over " << all.size()
                            << " corpus file(s).";
    CHECK(total > 0);
}

TEST_CASE("every_line_of_every_corpus_fixing_file_reaches_the_database", tags) {
    auto lg(make_logger(test_suite));

    corpus_tenant tenant;
    ores::nats::service::nats_client auth_nats;
    import_service svc(tenant.ctx, auth_nats);

    const auto all = ores::marketdata::test::fixing_payloads(
        ores::testing::project_root::resolve("external/ore/examples"));
    REQUIRE_FALSE(all.empty());

    std::size_t total = 0;
    for (const auto& path : all)
        total += import_fixings_and_verify(path, svc, tenant.ctx);

    BOOST_LOG_SEV(lg, info) << "Verified " << total << " fixing line(s) over " << all.size()
                            << " corpus file(s).";
    CHECK(total > 0);
}
