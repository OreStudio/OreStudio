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
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.api/generators/approval_request_generator.hpp"
#include "ores.inbox.core/repository/approval_kind_repository.hpp"
#include "ores.inbox.core/repository/approval_request_repository.hpp"
#include "ores.inbox.core/repository/approval_request_state_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/tick_coords_repository.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.marketdata.core/service/ore_export_service.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.ore.core/market/market_data_parser.hpp"
#include "ores.ore.core/market/market_data_serializer.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/test_database_manager.hpp"
#include "test_suite.hpp"
#include "tick_test_support.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

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

/// The series the export tenant holds. A tenant also reads the system tenant's
/// rows, and the running fleet writes series there.
std::vector<ores::marketdata::domain::market_series> own_series(const export_tenant& t) {
    auto series = ores::marketdata::repository::market_series_repository{}.read_latest(t.ctx);
    std::erase_if(series, [&](const auto& s) { return s.tenant_id != t.ctx.tenant_id(); });
    return series;
}

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

TEST_CASE("export_reproduces_the_market_data_file_it_imported", "[service][ore_export]") {
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

TEST_CASE("export_reproduces_the_fixings_file_it_imported", "[service][ore_export]") {
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
    ores::marketdata::repository::market_observation_repository obs_repo;
    boost::uuids::random_generator gen;

    ores::marketdata::domain::market_series s;
    s.id = gen();
    s.tenant_id = t.ctx.tenant_id();
    s.party_id = t.ctx.party_id().value_or(boost::uuids::uuid{});
    s.oresmd_uri = series_uri;
    s.series_subclass = "spot";
    s.provider_code = "ore";
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
    obs_repo.insert(t.ctx, o);
}

}

TEST_CASE("export_writes_each_key_from_the_datum_uri_the_row_stores", "[service][ore_export]") {
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

TEST_CASE("export_refuses_an_observation_whose_uri_names_no_datum", "[service][ore_export]") {
    auto lg(make_logger(test_suite));

    export_tenant t;
    write_fx_row(t,
                 "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD",
                 "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate");

    const ore_export_service exporter(t.ctx);
    CHECK_THROWS(exporter.write_all());
}

TEST_CASE("export_with_a_date_keeps_the_history_and_writes_the_value_at_the_close_of_that_day",
          "[service][ore_export]") {
    export_tenant t;
    ores::nats::service::nats_client auth_nats;
    import_service importer(t.ctx, auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160204 FX/RATE/AUD/CAD 1.05\n20160205 FX/RATE/AUD/CAD 1.10\n";
    req.source = "test.ore_export";
    REQUIRE(importer.import(req).success);

    const auto series = own_series(t);
    REQUIRE(series.size() == 1);
    const auto& s = series.front();
    const auto stored =
        ores::marketdata::repository::market_observation_repository{}.read_latest_for_series(t.ctx,
                                                                                             s.id);
    REQUIRE(stored.size() == 2);

    // The tick of a live feed, later on the day of the second imported point, under the datum
    // URI the observation rows carry.
    const auto owner =
        t.ctx.with_party(t.ctx.tenant_id(), s.party_id, {s.party_id}, t.base.db_user());
    ores::marketdata::tests::subscribe(t.base, t.ctx, s.party_id, s.provider_code);
    const auto stream = ores::marketdata::tests::stream_of(s.provider_code, s.oresmd_uri);
    const auto number = ores::marketdata::repository::tick_coords_repository{}.resolve(
        owner, stream, stored.front().oresmd_uri);
    using namespace std::chrono;
    ores::marketdata::tests::ensure_tick(
        owner, stream, number, sys_days{2016y / February / 5} + 12h, 1.2);

    // Each line as "date value", sorted.
    const auto lines_of = [](const std::string& body) {
        std::vector<std::string> lines;
        std::istringstream in(body);
        for (std::string line; std::getline(in, line);) {
            const auto first = line.find('\t');
            const auto last = line.rfind('\t');
            if (first != std::string::npos && last != first)
                lines.push_back(line.substr(0, first) + " " + line.substr(last + 1));
        }
        std::ranges::sort(lines);
        return lines;
    };
    using lines = std::vector<std::string>;

    const ore_export_service exporter(owner);

    // The tick is the close of its day, and the earlier date stays.
    const auto close_of_the_day = exporter.write_all(std::string("2016-02-05"));
    CHECK(lines_of(close_of_the_day.market_data) == lines{"20160204 1.05", "20160205 1.2"});
    CHECK(close_of_the_day.observation_count == 2);

    // The tick is after this close, so the point of the day before is its value.
    CHECK(lines_of(exporter.write_all(std::string("2016-02-04")).market_data) ==
          lines{"20160204 1.05", "20160205 1.10"});

    // Before the first point the date has no line of its own, and the history stays.
    CHECK(lines_of(exporter.write_all(std::string("2016-02-03")).market_data) ==
          lines{"20160204 1.05", "20160205 1.10"});

    // A date after the tick carries its value forward as a line of its own.
    CHECK(lines_of(exporter.write_all(std::string("2016-02-06")).market_data) ==
          lines{"20160204 1.05", "20160205 1.10", "20160206 1.2"});

    CHECK_THROWS_AS(exporter.write_all(std::string("5 February")), std::invalid_argument);

    // Without a date, the export is the rows of the observation table, as before.
    CHECK(lines_of(exporter.write_all().market_data) == lines{"20160204 1.05", "20160205 1.10"});
}

TEST_CASE("the_reads_for_every_series_give_what_the_reads_for_each_series_give",
          "[service][ore_export]") {
    export_tenant t;
    ores::nats::service::nats_client auth_nats;
    import_service importer(t.ctx, auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160204 FX/RATE/SEK/NOK 1.05\n20160205 FX/RATE/SEK/NOK 1.10\n"
                              "20160204 FX/RATE/GBP/USD 1.40\n20160205 FX/RATE/GBP/USD 1.45\n"
                              "20160204 FX/RATE/CHF/USD 1.01\n";
    req.source = "test.ore_export";
    REQUIRE(importer.import(req).success);

    const auto series = own_series(t);
    REQUIRE(series.size() == 3);
    ores::marketdata::repository::market_observation_repository observations;

    // A tick after the second point of one series, so the snapshot of that series differs
    // from its observation rows.
    const auto& ticked = series.front();
    const auto stored = observations.read_latest_for_series(t.ctx, ticked.id);
    REQUIRE(!stored.empty());
    const auto owner =
        t.ctx.with_party(t.ctx.tenant_id(), ticked.party_id, {ticked.party_id}, t.base.db_user());
    ores::marketdata::tests::subscribe(t.base, t.ctx, ticked.party_id, ticked.provider_code);
    const auto stream = ores::marketdata::tests::stream_of(ticked.provider_code, ticked.oresmd_uri);
    const auto number = ores::marketdata::repository::tick_coords_repository{}.resolve(
        owner, stream, stored.front().oresmd_uri);
    using namespace std::chrono;
    ores::marketdata::tests::ensure_tick(
        owner, stream, number, sys_days{2016y / February / 5} + 12h, 1.2);

    // The datums of a read, as "uri value", sorted, so the order of the rows does not matter.
    const auto datums = [](const std::vector<ores::marketdata::domain::market_observation>& rows) {
        std::vector<std::string> out;
        for (const auto& o : rows)
            out.push_back(o.oresmd_uri + " " + o.value);
        std::ranges::sort(out);
        return out;
    };

    const auto every = observations.read_latest_all(owner);
    const auto close = sys_days{2016y / February / 5} + hours{23};
    const auto every_at_close = observations.read_as_of_all(owner, close);
    for (const auto& s : series) {
        std::vector<ores::marketdata::domain::market_observation> mine, mine_at_close;
        std::ranges::copy_if(
            every, std::back_inserter(mine), [&](const auto& o) { return o.series_id == s.id; });
        std::ranges::copy_if(every_at_close, std::back_inserter(mine_at_close), [&](const auto& o) {
            return o.series_id == s.id;
        });
        CHECK(datums(mine) == datums(observations.read_latest_for_series(owner, s.id)));
        CHECK(datums(mine_at_close) == datums(observations.read_as_of(owner, s.id, close)));
    }
    CHECK(every.size() == 5);
}

TEST_CASE("export_with_a_date_leaves_out_a_datum_whose_value_is_older_than_the_limit",
          "[service][ore_export]") {
    export_tenant t;
    ores::nats::service::nats_client auth_nats;
    import_service importer(t.ctx, auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160101 FX/RATE/EUR/USD 1.00\n20160205 FX/RATE/EUR/GBP 0.80\n";
    req.source = "test.ore_export";
    REQUIRE(importer.import(req).success);

    // Each line as "date key value", sorted.
    const auto lines_of = [](const std::string& body) {
        std::vector<std::string> lines;
        std::istringstream in(body);
        for (std::string line; std::getline(in, line);) {
            std::ranges::replace(line, '\t', ' ');
            if (!line.empty())
                lines.push_back(line);
        }
        std::ranges::sort(lines);
        return lines;
    };
    using lines = std::vector<std::string>;
    const ore_export_service exporter(t.ctx);

    // The pound quote is of the day, and the dollar quote is 35 days old, which is past the
    // default limit. Its history line stays, and the date has no line for it.
    const auto at_the_quote = exporter.write_all(std::string("2016-02-05"));
    CHECK(lines_of(at_the_quote.market_data) ==
          lines{"20160101 FX/RATE/EUR/USD 1.00", "20160205 FX/RATE/EUR/GBP 0.80"});
    CHECK(at_the_quote.carried_forward_count == 0);
    CHECK(at_the_quote.stale_count == 1);

    // Three days later the pound quote is within the limit and is carried forward.
    const auto three_days_later = exporter.write_all(std::string("2016-02-08"));
    CHECK(lines_of(three_days_later.market_data) == lines{"20160101 FX/RATE/EUR/USD 1.00",
                                                          "20160205 FX/RATE/EUR/GBP 0.80",
                                                          "20160208 FX/RATE/EUR/GBP 0.80"});
    CHECK(three_days_later.carried_forward_count == 1);
    CHECK(three_days_later.stale_count == 1);

    // A wider limit carries the dollar quote too.
    const auto wide = exporter.write_all(std::string("2016-02-05"), 60);
    CHECK(lines_of(wide.market_data) == lines{"20160101 FX/RATE/EUR/USD 1.00",
                                              "20160205 FX/RATE/EUR/GBP 0.80",
                                              "20160205 FX/RATE/EUR/USD 1.00"});
    CHECK(wide.carried_forward_count == 1);
    CHECK(wide.stale_count == 0);

    // A limit of no days carries nothing, so a quote of an earlier day is left out.
    const auto none = exporter.write_all(std::string("2016-02-08"), 0);
    CHECK(lines_of(none.market_data) ==
          lines{"20160101 FX/RATE/EUR/USD 1.00", "20160205 FX/RATE/EUR/GBP 0.80"});
    CHECK(none.stale_count == 2);

    CHECK_THROWS_AS(exporter.write_all(std::string("2016-02-05"), -1), std::invalid_argument);
}

namespace {

const std::string surface_series_uri =
    "oresmd://fx/"
    "EUR?type=series&instrument=fx_vol_surface&quote=rate_lnvol&ccy=USD&surface_id=EURUSD_SSVI";
const auto surface_as_of =
    std::chrono::sys_days{std::chrono::year{2026} / std::chrono::November / 11} +
    std::chrono::hours{2};

/// A surface series a blueprint claimed, with two points at the as-of: the 1Y ATM and 25 delta put.
boost::uuids::uuid write_surface(export_tenant& t, const boost::uuids::uuid& blueprint) {
    boost::uuids::random_generator gen;
    ores::marketdata::domain::market_series s;
    s.id = gen();
    s.tenant_id = t.ctx.tenant_id();
    s.party_id = t.ctx.party_id().value_or(boost::uuids::uuid{});
    s.oresmd_uri = surface_series_uri;
    s.series_subclass = "volatility";
    s.provider_code = "derived";
    s.derivation_kind = "FX_VOL_SURFACE_CALIBRATION";
    s.derivation_config_id = blueprint;
    s.derivation_config_version = 1;
    s.modified_by = t.ctx.actor();
    s.performed_by = t.ctx.service_account();
    s.change_reason_code = "system.derived_series";
    s.change_commentary = "ore_export test";
    ores::marketdata::repository::market_series_repository{}.write(t.ctx, s);

    ores::marketdata::repository::market_observation_repository obs_repo;
    for (const auto& [strike, value] :
         {std::pair{"ATM", "0.0551"}, std::pair{"DEL/Spot/Put/25", "0.0552"}}) {
        ores::marketdata::domain::market_observation o;
        o.id = gen();
        o.tenant_id = t.ctx.tenant_id();
        o.party_id = s.party_id;
        o.series_id = s.id;
        o.observation_datetime = surface_as_of;
        o.oresmd_uri =
            "oresmd://fx/EUR?type=quote&instrument=fx_vol_surface&quote=rate_lnvol&ccy=USD"
            "&surface_id=EURUSD_SSVI&expiry=1Y&strike=" +
            std::string(strike);
        o.value = value;
        obs_repo.insert(t.ctx, o);
    }
    return s.id;
}

/// The publish of the surface at its as-of, under an approval request the store holds.
void publish_surface(export_tenant& t,
                     const boost::uuids::uuid& blueprint,
                     const boost::uuids::uuid& series) {
    ores::testing::database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    auto party = ores::refdata::generators::generate_synthetic_party(gen_ctx);
    party.tenant_id = t.ctx.tenant_id();
    for (const auto& existing : ores::refdata::repository::party_repository().read_latest(t.ctx)) {
        party.parent_party_id = existing.id;
        break;
    }
    ores::refdata::repository::party_repository{}.write(t.ctx, party);

    auto asker = ores::iam::generators::generate_synthetic_account(gen_ctx);
    asker.tenant_id = t.ctx.tenant_id();
    asker.account_type = "service";
    asker.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(t.ctx, asker);

    const auto system_ctx =
        t.ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user());
    auto request = ores::inbox::generators::generate_synthetic_approval_request(gen_ctx);
    request.tenant_id = t.ctx.tenant_id();
    request.change_reason_code = "system.test";
    request.requested_by = asker.id;
    request.kind_code =
        ores::inbox::repository::approval_kind_repository{}.read_latest(system_ctx).front().code;
    request.state_code = ores::inbox::repository::approval_request_state_repository{}
                             .read_latest(system_ctx)
                             .front()
                             .code;
    ores::inbox::repository::approval_request_repository{}.write(t.ctx, request);

    ores::marketdata::domain::fx_vol_surface_publish p;
    p.id = boost::uuids::random_generator{}();
    p.tenant_id = t.ctx.tenant_id();
    p.request_id = request.id;
    p.party_id = party.id;
    p.blueprint_id = blueprint;
    p.as_of = surface_as_of;
    p.output_series_id = series;
    p.calibration_id = boost::uuids::random_generator{}();
    p.calibration_version = 1;
    p.converged = true;
    p.published_by = "a.trader";
    p.published_at = std::chrono::floor<std::chrono::seconds>(surface_as_of);
    p.modified_by = t.ctx.actor();
    p.performed_by = t.ctx.service_account();
    p.change_reason_code = "system.test";
    p.change_commentary = "ore_export test";
    ores::marketdata::repository::fx_vol_surface_publish_repository{}.write(t.ctx, p);
}

}

TEST_CASE("export_leaves_out_a_surface_no_one_published", "[service][ore_export]") {
    auto lg(make_logger(test_suite));

    export_tenant t;
    write_surface(t, boost::uuids::random_generator{}());

    const ore_export_service exporter(t.ctx);
    const auto result = exporter.write_all();
    CHECK_FALSE(result.market_data.contains("FX_OPTION"));
}

TEST_CASE("export_writes_a_published_surface_as_fx_option_quotes", "[service][ore_export]") {
    auto lg(make_logger(test_suite));

    export_tenant t;
    const auto blueprint = boost::uuids::random_generator{}();
    const auto series = write_surface(t, blueprint);
    publish_surface(t, blueprint, series);
    write_fx_row(
        t,
        "oresmd://fx/EUR?type=series&instrument=fx_option&quote=rate_lnvol&ccy=USD",
        "oresmd://fx/"
        "EUR?type=quote&instrument=fx_option&quote=rate_lnvol&ccy=USD&expiry=1Y&strike_label=ATM");

    const ore_export_service exporter(t.ctx);
    const auto result = exporter.write_all(std::string("2026-11-11"));
    const auto keys = body_keys(result.market_data);
    CHECK(std::ranges::count_if(keys, [](const auto& k) {
              return k.starts_with("FX_OPTION/RATE_LNVOL/EUR/USD/");
          }) == 2);
    CHECK(result.market_data.contains("FX_OPTION/RATE_LNVOL/EUR/USD/1Y/ATM"));
    CHECK(result.market_data.contains("FX_OPTION/RATE_LNVOL/EUR/USD/1Y/25P"));
    std::istringstream lines(result.market_data);
    std::string line;
    std::string atm_line;
    while (std::getline(lines, line))
        if (line.contains("FX_OPTION/RATE_LNVOL/EUR/USD/1Y/ATM"))
            atm_line = line;
    CHECK(atm_line.ends_with("0.0551"));
}

TEST_CASE("export_at_a_date_before_the_publish_leaves_the_surface_out", "[service][ore_export]") {
    auto lg(make_logger(test_suite));

    export_tenant t;
    const auto blueprint = boost::uuids::random_generator{}();
    const auto series = write_surface(t, blueprint);
    publish_surface(t, blueprint, series);

    const ore_export_service exporter(t.ctx);
    const auto result = exporter.write_all(std::string("2026-11-10"));
    CHECK_FALSE(result.market_data.contains("FX_OPTION/RATE_LNVOL/EUR/USD/1Y/25P"));
}

TEST_CASE("export_for_a_party_writes_its_own_series_and_no_other_party_s",
          "[service][ore_export]") {
    auto lg(make_logger(test_suite));

    export_tenant t;
    ores::testing::database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);
    std::vector<boost::uuids::uuid> parties;
    for (int i = 0; i < 2; ++i) {
        auto party = ores::refdata::generators::generate_synthetic_party(gen_ctx);
        party.tenant_id = t.ctx.tenant_id();
        for (const auto& existing :
             ores::refdata::repository::party_repository().read_latest(t.ctx)) {
            party.parent_party_id = existing.id;
            break;
        }
        ores::refdata::repository::party_repository{}.write(t.ctx, party);
        parties.push_back(party.id);
    }
    const auto as_party = [&](const boost::uuids::uuid& party) {
        return t.ctx.with_party(t.ctx.tenant_id(), party, parties, t.ctx.actor());
    };

    const auto base = t.ctx;
    for (const auto& party : parties) {
        t.ctx = as_party(party);
        write_fx_row(t,
                     "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD",
                     "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");
    }
    t.ctx = base;

    const auto keys =
        body_keys(ore_export_service(as_party(parties.front())).write_all().market_data);
    CHECK(std::ranges::count(keys, std::string("FX/RATE/EUR/USD")) == 1);

    const auto every_party = body_keys(ore_export_service(t.ctx).write_all().market_data);
    CHECK(std::ranges::count(every_party, std::string("FX/RATE/EUR/USD")) == 2);
}
