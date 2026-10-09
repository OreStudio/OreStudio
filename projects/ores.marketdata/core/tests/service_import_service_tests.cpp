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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_value_repository.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/messaging/currency_pair_protocol.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <utility>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[service][import_service]");

}

using namespace ores::logging;
using ores::marketdata::service::import_service;
using ores::testing::database_helper;

namespace {

// The assertions below ask whether the import filed a series under a given key.
// The series is read by its identity -- the projection of the key the import was
// given -- so the helper takes that identity rather than a decomposition.
std::vector<ores::marketdata::domain::market_series>
series_with_uri(ores::marketdata::repository::market_series_repository& repo,
                ores::database::context ctx,
                const std::string& oresmd_uri) {
    return repo.read_latest_by_uri(ctx, oresmd_uri);
}

// A curve id no earlier run used. The test tenant is shared and nothing clears it
// between runs, so a fixed id would leave the series already created on a second
// run and the import would declare no shape for it.
std::string fresh_curve_id() {
    auto tag = boost::uuids::to_string(boost::uuids::random_generator{}());
    tag.erase(std::remove(tag.begin(), tag.end(), '-'), tag.end());
    return tag;
}

// The series a ZERO key names, as the import files it.
std::string zero_series_uri(const std::string& curve_id) {
    using namespace ores::marketdata;
    const auto point = datum::ore_key_codec::read("ZERO/RATE/EUR/" + curve_id + "/A365/1Y");
    return datum::oresmd_uri_codec::write(datum::series_of(*point)).value();
}

// The values of the 'term' axis, in the order the shape stores them.
std::vector<std::string> term_values(const boost::uuids::uuid& series_id,
                                     ores::database::context ctx) {
    using namespace ores::marketdata;
    const std::vector<std::string> ids{boost::uuids::to_string(series_id)};
    std::vector<std::pair<int, std::string>> ordered;
    for (const auto& v :
         repository::series_axis_value_repository{}.read_latest_for_series(ctx, ids))
        ordered.emplace_back(v.sequence, v.value);
    std::ranges::sort(ordered);
    std::vector<std::string> values;
    for (auto& [sequence, value] : ordered)
        values.push_back(std::move(value));
    return values;
}

}

TEST_CASE("import_dedupes_duplicate_observation_and_reports_warning", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/EUR/CHF 1.0\n"
                              "20160205 FX/RATE/EUR/CHF 1.5\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    CHECK(resp.success);
    CHECK(resp.observation_count == 1);
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.errors.empty());
    CHECK(resp.warnings[0].find("FX/RATE/EUR/CHF") != std::string::npos);
}

TEST_CASE("import_with_duplicates_are_errors_skips_only_the_affected_section", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    // Market data has a duplicate; fixings does not.
    req.market_data_content = "20160205 FX/RATE/EUR/CHF 1.0\n"
                              "20160205 FX/RATE/EUR/CHF 1.5\n";
    req.fixings_content = "2016-02-05 EUR-EONIA 0.001\n";
    req.source = "test.import_service";
    req.duplicates_are_errors = true;

    const auto resp = svc.import(req);

    // Overall failure...
    CHECK_FALSE(resp.success);
    REQUIRE(resp.errors.size() == 1);
    CHECK(resp.warnings.empty());

    // ...but the clean fixings section was still persisted, and its count
    // is still surfaced rather than being silently dropped alongside the
    // errored market-data section.
    CHECK(resp.observation_count == 0);
    CHECK(resp.fixing_count == 1);
}

TEST_CASE("import_defaults_point_id_to_spot_for_fx_rate", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::market_observation_repository obs_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/EUR/USD 1.132337\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);

    const auto series =
        series_with_uri(series_repo,
                        h.context(),
                        "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD");
    REQUIRE(series.size() == 1);

    const auto observations = obs_repo.read_latest_for_series(h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(observations.front().oresmd_uri ==
          "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");
    CHECK(ores::marketdata::datum::ore_key_codec::write(
              ores::marketdata::datum::oresmd_uri_codec::read(observations.front().oresmd_uri)
                  .value()) == "FX/RATE/EUR/USD");
}

TEST_CASE("import_skips_a_short_key_oresmd_cannot_name", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    // An IR swap key needs a currency, a start, an index tenor and a term, as ORE
    // reads it, so a five-segment swap key names no datum: the row has no
    // identity to be filed under, and the import reports it and drops it.
    req.market_data_content = "20160205 IR_SWAP/RATE/EUR/2D/1D 0.01\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    CHECK(resp.success);
    CHECK(resp.observation_count == 0);
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.warnings[0].find("IR_SWAP/RATE/EUR/2D/1D") != std::string::npos);
    CHECK(resp.warnings[0].find("skipped") != std::string::npos);
}

TEST_CASE("import_skips_a_one_segment_key_and_keeps_the_rest_of_the_file", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    // The file reader does not split keys, so a key with no type and metric
    // reaches the key codec, which refuses that row alone.
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 NOTAKEY 1.0\n"
                              "20160205 FX/RATE/EUR/USD 1.09\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    CHECK(resp.success);
    CHECK(resp.errors.empty());
    CHECK(resp.observation_count == 1);
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.warnings[0].contains("NOTAKEY"));
    CHECK(resp.warnings[0].contains("skipped"));
}

TEST_CASE("import_warns_when_refdata_says_an_fx_pair_is_reversed", tags) {
    auto lg(make_logger(test_suite));

    // Reference data named here rather than fetched. This branch is the reason
    // the warning is worth reading: a key the grammar merely re-spells is a
    // tidy-up, while a key reversed against refdata is stored under the pair
    // it should have been written with, and only the file's owner can fix that.
    // The pairs are injected because the live read races whatever
    // ores.refdata.service answers on the same subject: with a fleet running,
    // the stub below is not the only responder, and the case failed on a
    // machine that had one.
    std::set<ores::ore::market::fx_quote_convention_checker::currency_pair> known{
        ores::ore::market::fx_quote_convention_checker::currency_pair{"GBP", "USD"}};

    database_helper h;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    ores::nats::service::nats_client auth_nats(nats, [](bool) { return std::string{}; });
    import_service svc(h.context(), auth_nats, [known]() { return known; });
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    // Written the wrong way round against the pair refdata reports as canonical.
    req.market_data_content = "20160205 FX/RATE/USD/GBP 1.394610179594994\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.observation_count == 1);
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.warnings[0].find("reversed relative to refdata's canonical currency pair") !=
          std::string::npos);
    REQUIRE(series_with_uri(series_repo,
                            h.context(),
                            "oresmd://fx/GBP?type=series&instrument=fx_spot&quote=rate&ccy=USD")
                .size() == 1);
}

TEST_CASE("import_keeps_the_ir_swap_settlement_segment_the_file_carried", tags) {
    auto lg(make_logger(test_suite));

    // The settlement segment is an identity field of the swap, stored as the file
    // wrote it -- a spot lag, or a start date -- so a 0D and a dated swap are two
    // series and neither is rebuilt as a 2D default.
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 IR_SWAP/RATE/USD/0D/3M/5Y 0.043120\n"
                              "20160205 IR_SWAP/RATE/GBP/20220922/3M/20270922 0.051000\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.observation_count == 2);
    CHECK(resp.errors.empty());

    REQUIRE(series_with_uri(series_repo,
                            h.context(),
                            "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate"
                            "&fwd_start=0D&tenor=3M")
                .size() == 1);
    REQUIRE(series_with_uri(series_repo,
                            h.context(),
                            "oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate"
                            "&fwd_start=20220922&tenor=3M")
                .size() == 1);
}

TEST_CASE("import_stores_an_alias_under_its_canonical_spelling_and_says_so", tags) {
    auto lg(make_logger(test_suite));

    // ORE reads FX_SPOT as FX, so the two spellings name one datum: the row is
    // stored under the canonical key and the import reports the respelling.
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::market_observation_repository obs_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX_SPOT/RATE/EUR/USD 1.09\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.observation_count == 1);
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.warnings[0].contains("-> FX/RATE/EUR/USD"));
    CHECK(resp.warnings[0].contains("not the canonical spelling"));

    const auto series =
        series_with_uri(series_repo,
                        h.context(),
                        "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD");
    REQUIRE(series.size() == 1);
    const auto observations = obs_repo.read_latest_for_series(h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(ores::marketdata::datum::ore_key_codec::write(
              ores::marketdata::datum::oresmd_uri_codec::read(observations.front().oresmd_uri)
                  .value()) == "FX/RATE/EUR/USD");
}

TEST_CASE("import_stores_a_key_in_the_case_it_was_written", tags) {
    auto lg(make_logger(test_suite));

    // The datum keeps every token as ORE reads it, and ORE does not change the case
    // of a currency, so the import stores the key as written and reports no
    // respelling: a lower-case pair is its own series.
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/gbp/jpy 188.5\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.observation_count == 1);
    CHECK(resp.warnings.empty());
    REQUIRE(series_with_uri(series_repo,
                            h.context(),
                            "oresmd://fx/gbp?type=series&instrument=fx_spot&quote=rate&ccy=jpy")
                .size() == 1);
}

TEST_CASE("import_leaves_fx_qualifier_untouched_when_currency_pairs_unreachable", tags) {
    auto lg(make_logger(test_suite));

    // auth_nats is unconnected (no live refdata to fetch currency_pair
    // reference data from) — fetch_known_currency_pairs must degrade to an
    // empty known-pairs set rather than throwing, and with no known pairs
    // fx_quote_convention_checker never corrects anything: a genuinely
    // reversed key like FX/RATE/USD/GBP is persisted exactly as given,
    // which is the safe behaviour (never guess without reference data).
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/USD/GBP 1.394610179594994\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);
    CHECK(resp.warnings.empty());

    const auto series =
        series_with_uri(series_repo,
                        h.context(),
                        "oresmd://fx/USD?type=series&instrument=fx_spot&quote=rate&ccy=GBP");
    REQUIRE(series.size() == 1);
}

TEST_CASE("import_gives_a_series_the_identity_its_key_projects_to", tags) {
    auto lg(make_logger(test_suite));

    // A market-data key and a fixing index name are different key spaces, each
    // with its own codec: ore_key_codec for the ORE key grammar, ore_index_codec
    // for the index names the fixing boundary carries. Both give the series the
    // oresmd URI of what they read, and the series is found by that URI.
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/EUR/USD 1.09\n";
    req.fixings_content = "2016-02-05 UKRPI 263.4\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.observation_count == 1);
    CHECK(resp.fixing_count == 1);

    const auto fx =
        series_with_uri(series_repo,
                        h.context(),
                        "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD");
    REQUIRE(fx.size() == 1);
    CHECK(fx.front().oresmd_uri ==
          "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD");

    const auto inflation = series_with_uri(
        series_repo, h.context(), "oresmd://inflation/UKRPI?type=fixing&index=inflation");
    REQUIRE(inflation.size() == 1);
    CHECK(inflation.front().oresmd_uri == "oresmd://inflation/UKRPI?type=fixing&index=inflation");
}

TEST_CASE("a_series_is_read_by_the_identity_its_key_projects_to", tags) {
    auto lg(make_logger(test_suite));

    // The identity is a lookup in its own right, so a reader finds a series without
    // knowing how the registry decomposed its key. The triple reader still finds the
    // same row, which is what carries a series written before the column existed.
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/EUR/USD 1.09\n";
    req.source = "test.import_service";
    REQUIRE(svc.import(req).success);

    const auto by_identity = series_repo.read_latest_by_uri(
        h.context(), "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD");
    REQUIRE(by_identity.size() == 1);
    CHECK(by_identity.front().oresmd_uri ==
          "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD");
}

TEST_CASE("a_series_carries_its_instruments_identity_not_one_of_its_points", tags) {
    auto lg(make_logger(test_suite));

    // A file's key names one datum, and the payload below carries two points of one
    // swap series. The series' identity drops the point, so the two points reach one
    // row under the instrument's identity rather than two rows under the points.
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 IR_SWAP/RATE/USD/0D/1D/5Y 0.0125\n"
                              "20160205 IR_SWAP/RATE/USD/0D/1D/10Y 0.0150\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.observation_count == 2);
    // Both points land in one series, which is what one identity buys.
    CHECK(resp.series_count == 1);

    const auto uri =
        "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D";
    const auto by_identity = series_repo.read_latest_by_uri(h.context(), uri);
    REQUIRE(by_identity.size() == 1);
    CHECK(by_identity.front().oresmd_uri == uri);
}

TEST_CASE("import_skips_a_market_data_key_oresmd_cannot_name", tags) {
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 NOSUCHTYPE/RATE/USD 1.0\n"
                              "20160205 FX/RATE/EUR/USD 1.132337\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    CHECK(resp.success);
    // The nameable key is imported; the other has no identity to be filed under.
    CHECK(resp.observation_count == 1);
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.warnings[0].find("NOSUCHTYPE/RATE/USD") != std::string::npos);
    CHECK(resp.warnings[0].find("skipped") != std::string::npos);
    REQUIRE(series_with_uri(series_repo,
                            h.context(),
                            "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD")
                .size() == 1);
}

TEST_CASE("import_skips_an_index_name_oresmd_cannot_name", tags) {
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);

    ores::marketdata::messaging::import_market_data_request req;
    // An FX index is FX-SOURCE-CCY1-CCY2; with three tokens no family reads it. A
    // hyphen-free name would read, as an inflation index a convention may define.
    req.fixings_content = "2016-02-05 FX-ECB-EUR 0.001\n"
                          "2016-02-05 EUR-EONIA 0.001\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    CHECK(resp.success);
    CHECK(resp.fixing_count == 1);
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.warnings[0].find("FX-ECB-EUR") != std::string::npos);
    CHECK(resp.warnings[0].find("skipped") != std::string::npos);
    // The warning is the unnameable name's whole record: a name no class can read
    // has no identity, so it files no series and reports itself instead.
}

TEST_CASE("import_declares_the_shape_of_a_series_it_creates", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::series_axis_repository axis_repo;

    const auto curve = fresh_curve_id();
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 ZERO/RATE/EUR/" + curve +
                              "/A365/1Y 0.01\n"
                              "20160205 ZERO/RATE/EUR/" +
                              curve + "/A365/2Y 0.02\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.series_count == 1);
    REQUIRE(resp.observation_count == 2);

    const auto series = series_with_uri(series_repo, h.context(), zero_series_uri(curve));
    REQUIRE(series.size() == 1);

    // The axis is the coordinate field the datum grammar declares for the type,
    // and it holds the terms the file named, in the order the file named them.
    const std::vector<std::string> ids{boost::uuids::to_string(series.front().id)};
    const auto axes = axis_repo.read_latest_for_series(h.context(), ids);
    REQUIRE(axes.size() == 1);
    CHECK(axes.front().axis_field == "term");
    CHECK(axes.front().sequence == 0);
    CHECK(term_values(series.front().id, h.context()) == std::vector<std::string>{"1Y", "2Y"});
}

TEST_CASE("import_re_declares_the_same_shape_without_adding_rows", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::series_axis_repository axis_repo;

    const auto curve = fresh_curve_id();
    const std::string content = "20160205 ZERO/RATE/EUR/" + curve +
                                "/A365/1Y 0.01\n"
                                "20160205 ZERO/RATE/EUR/" +
                                curve + "/A365/2Y 0.02\n";
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = content;
    req.source = "test.import_service";

    const auto first = svc.import(req);
    const auto second = svc.import(req);

    REQUIRE(first.success);
    REQUIRE(second.success);
    // The second pass creates nothing, so the shape it leaves is the one the
    // first pass declared.
    CHECK(first.series_count == 1);
    CHECK(second.series_count == 0);

    const auto series = series_with_uri(series_repo, h.context(), zero_series_uri(curve));
    REQUIRE(series.size() == 1);
    const std::vector<std::string> ids{boost::uuids::to_string(series.front().id)};
    CHECK(axis_repo.read_latest_for_series(h.context(), ids).size() == 1);
    CHECK(term_values(series.front().id, h.context()) == std::vector<std::string>{"1Y", "2Y"});
}

TEST_CASE("import_records_a_term_the_reference_tenor_table_does_not_hold", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    // 13Y is absent from the reference tenor table. A build would be refused,
    // because its pillars come from the tenant's own configuration. An import is
    // not: the term is the vendor's, and the ORE corpus carries tenors such as
    // 13Y and 1Y6M that the table does not hold.
    const auto curve = fresh_curve_id();
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 ZERO/RATE/EUR/" + curve + "/A365/13Y 0.03\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.series_count == 1);
    CHECK(resp.observation_count == 1);

    const auto series = series_with_uri(series_repo, h.context(), zero_series_uri(curve));
    REQUIRE(series.size() == 1);
    CHECK(term_values(series.front().id, h.context()) == std::vector<std::string>{"13Y"});
}

TEST_CASE("import_keeps_the_shape_of_a_series_it_does_not_name", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    // Two curves in one file, each with its own terms. Each series is given the
    // shape its own keys state, so neither borrows the other's.
    const auto first = fresh_curve_id();
    const auto second = fresh_curve_id();
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 ZERO/RATE/EUR/" + first +
                              "/A365/1Y 0.01\n"
                              "20160205 ZERO/RATE/EUR/" +
                              first +
                              "/A365/2Y 0.02\n"
                              "20160205 ZERO/RATE/EUR/" +
                              second + "/A365/3Y 0.03\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.series_count == 2);

    const auto aseries = series_with_uri(series_repo, h.context(), zero_series_uri(first));
    const auto bseries = series_with_uri(series_repo, h.context(), zero_series_uri(second));
    REQUIRE(aseries.size() == 1);
    REQUIRE(bseries.size() == 1);
    CHECK(term_values(aseries.front().id, h.context()) == std::vector<std::string>{"1Y", "2Y"});
    CHECK(term_values(bseries.front().id, h.context()) == std::vector<std::string>{"3Y"});
}

TEST_CASE("import_appends_a_term_a_later_file_states", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    // A second file for the same series brings a term the first did not state.
    // The shape accumulates, and the new term takes the place after the terms
    // already there rather than renumbering them.
    const auto curve = fresh_curve_id();
    ores::marketdata::messaging::import_market_data_request first;
    first.market_data_content = "20160205 ZERO/RATE/EUR/" + curve +
                                "/A365/1Y 0.01\n"
                                "20160205 ZERO/RATE/EUR/" +
                                curve + "/A365/2Y 0.02\n";
    first.source = "test.import_service";
    REQUIRE(svc.import(first).success);

    ores::marketdata::messaging::import_market_data_request second;
    second.market_data_content = "20160206 ZERO/RATE/EUR/" + curve + "/A365/3Y 0.03\n";
    second.source = "test.import_service";
    const auto resp = svc.import(second);

    REQUIRE(resp.success);
    CHECK(resp.observation_count == 1);
    CHECK(resp.series_count == 0);

    const auto series = series_with_uri(series_repo, h.context(), zero_series_uri(curve));
    REQUIRE(series.size() == 1);
    CHECK(term_values(series.front().id, h.context()) ==
          std::vector<std::string>{"1Y", "2Y", "3Y"});
}
