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
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/messaging/currency_pair_protocol.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[service][import_service]");

}

using namespace ores::logging;
using ores::marketdata::service::import_service;
using ores::testing::database_helper;

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
    ores::marketdata::repository::market_observations_repository obs_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 FX/RATE/EUR/USD 1.132337\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);

    const auto series = series_repo.read_latest_by_type(h.context(), "FX", "RATE", "EUR/USD");
    REQUIRE(series.size() == 1);

    const auto observations = obs_repo.read_latest(h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(observations.front().point_id == "SPOT");
}

TEST_CASE("import_leaves_point_id_empty_for_series_with_short_key", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;
    ores::marketdata::repository::market_observations_repository obs_repo;

    ores::marketdata::messaging::import_market_data_request req;
    // IR_SWAP has qualifier_depth 3 (currency/index_tenor/fixed_freq), so a
    // key with only 2 qualifier segments is short: the registry folds the
    // whole remainder into the qualifier and returns no point_id, and the
    // import must not mislabel it as "SPOT".
    req.market_data_content = "20160205 IR_SWAP/RATE/EUR/2D/1D 0.01\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    REQUIRE(resp.observation_count == 1);

    const auto series =
        series_repo.read_latest_by_type(h.context(), "IR_SWAP", "RATE", "EUR/2D/1D");
    REQUIRE(series.size() == 1);

    const auto observations = obs_repo.read_latest(h.context(), series.front().id);
    REQUIRE(observations.size() == 1);
    CHECK(observations.front().point_id.empty());
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
    import_service svc(
        h.context(), auth_nats, [known]() { return known; });
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
    REQUIRE(series_repo.read_latest_by_type(h.context(), "FX", "RATE", "GBP/USD").size() == 1);
}

TEST_CASE("import_keeps_the_ir_swap_settlement_segment_the_file_carried", tags) {
    auto lg(make_logger(test_suite));

    // The settlement segment is part of the series' own qualifier, so it is
    // stored as the file wrote it -- a spot lag, or a date. An earlier reading
    // of the corpus held that this segment was discarded and rebuilt as the
    // projection's "2D" fallback, which would store the 821 USD 0D keys and the
    // explicit-date keys as 2D. It is the fallback that is unreachable here, not
    // the segment: the identifier records settle whenever it is not "2D", and
    // the projection emits what it recorded.
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    import_service svc(h.context(), auth_nats);
    ores::marketdata::repository::market_series_repository series_repo;

    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20160205 IR_SWAP/RATE/USD/0D/3M/PAR_RATE 0.043120\n"
                              "20160205 IR_SWAP/RATE/GBP/20220922/3M/PAR_RATE 0.051000\n";
    req.source = "test.import_service";

    const auto resp = svc.import(req);

    REQUIRE(resp.success);
    CHECK(resp.observation_count == 2);
    CHECK(resp.errors.empty());

    CHECK(series_repo.read_latest_by_type(h.context(), "IR_SWAP", "RATE", "USD/2D/3M").empty());
    CHECK(series_repo.read_latest_by_type(h.context(), "IR_SWAP", "RATE", "GBP/2D/3M").empty());
    REQUIRE(series_repo.read_latest_by_type(h.context(), "IR_SWAP", "RATE", "USD/0D/3M").size() ==
            1);
    REQUIRE(
        series_repo.read_latest_by_type(h.context(), "IR_SWAP", "RATE", "GBP/20220922/3M").size() ==
        1);
}

TEST_CASE("import_stores_a_named_key_under_its_canonical_spelling", tags) {
    auto lg(make_logger(test_suite));

    // The oresmd grammar is the authority for what a key means, so a key it can
    // name becomes the series its own projection emits. Two spellings of one
    // instrument then reach one series rather than two, which is what the
    // lower-case pair below would otherwise produce: the shape registry
    // decomposes a key without touching its case, and the series table is
    // matched on the qualifier verbatim.
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
    REQUIRE(resp.warnings.size() == 1);
    CHECK(resp.warnings[0].find("FX/RATE/GBP/JPY") != std::string::npos);

    CHECK(series_repo.read_latest_by_type(h.context(), "FX", "RATE", "gbp/jpy").empty());
    REQUIRE(series_repo.read_latest_by_type(h.context(), "FX", "RATE", "GBP/JPY").size() == 1);
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

    const auto series = series_repo.read_latest_by_type(h.context(), "FX", "RATE", "USD/GBP");
    REQUIRE(series.size() == 1);
}

TEST_CASE("import_gives_a_series_the_identity_its_key_projects_to", tags) {
    auto lg(make_logger(test_suite));

    // A market-data key and a fixing index name are different key spaces, and each
    // has its own inverse: from_ore_key() for the ORE key grammar, from_index_name()
    // for the index names the fixing boundary carries. Both write what they project
    // to onto the series, so a row can be reached by the identity ORE wrote rather
    // than by the registry's (series_type, metric, qualifier) triple alone.
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

    const auto fx = series_repo.read_latest_by_type(h.context(), "FX", "RATE", "EUR/USD");
    REQUIRE(fx.size() == 1);
    CHECK(fx.front().oresmd_uri == "oresmd://fx/eurusd?type=quote&quote=spot");

    const auto inflation = series_repo.read_latest_by_type(
        h.context(), std::string(import_service::fixing_series_type), "RATE", "UKRPI");
    REQUIRE(inflation.size() == 1);
    CHECK(inflation.front().oresmd_uri == "oresmd://inflation/ukrpi?type=fixing");
}
