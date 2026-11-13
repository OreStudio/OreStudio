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
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/fx_montage_reader.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

constexpr const char* montage_tags = "[fx_montage][database]";

namespace {

using ores::marketdata::messaging::get_fx_montage_request;
using ores::marketdata::service::app::get_fx_montage;

/**
 * @brief Two crosses of AUD, each carried by two providers, with a day's prices.
 *
 * A montage is for the base, so GBP/AUD is included even though AUD is on its
 * quote side. Each series holds two prices inside the window, so the change and
 * the range have something to measure.
 */
struct montage_fixture {
    montage_fixture()
        : gctx(ores::testing::make_generation_context(h))
        , party_id(uuid_gen()) {
        write_party();
        party = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
        write_pair("AUD", "USD", "bloomberg", 0.65400, 0.65420);
        write_pair("AUD", "USD", "demo", 0.65390, 0.65410);
        write_pair("GBP", "AUD", "bloomberg", 1.95200, 1.95280);
    }

    get_fx_montage_request request() const {
        get_fx_montage_request req;
        req.party_id = boost::uuids::to_string(party_id);
        req.base_currency = "AUD";
        return req;
    }

    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gctx;
    boost::uuids::random_generator uuid_gen;
    boost::uuids::uuid party_id;
    std::optional<ores::database::context> party;

private:
    static std::string series_uri(const std::string& base, const std::string& quote) {
        return "oresmd://fx/" + base + "?type=series&instrument=fx_spot&quote=rate&ccy=" +
               quote;
    }

    static std::string datum_uri(const std::string& base, const std::string& quote) {
        return "oresmd://fx/" + base + "?instrument=fx_spot&quote=rate&ccy=" + quote;
    }

    void write_party() {
        ores::refdata::repository::party_repository repo;
        auto p = ores::refdata::generators::generate_synthetic_party(gctx);
        p.id = party_id;
        p.version = 0;
        p.is_registration_default = false;
        for (const auto& e : repo.read_latest(h.context()))
            if (e.tenant_id == h.tenant_id() && !e.parent_party_id) {
                p.parent_party_id = e.id;
                break;
            }
        p.change_reason_code = "system.test";
        p.change_commentary = "fx montage test";
        repo.write(h.context(), p);
    }

    void write_pair(const std::string& base,
                    const std::string& quote,
                    const std::string& provider,
                    double first,
                    double last) {
        ores::marketdata::domain::market_series s;
        s.id = uuid_gen();
        s.version = 0;
        s.tenant_id = h.tenant_id();
        s.party_id = party_id;
        s.oresmd_uri = series_uri(base, quote);
        s.series_subclass = "yield";
        s.provider_code = provider;
        s.derivation_kind = "OBSERVED";
        s.derivation_config_id = boost::uuids::nil_uuid();
        s.derivation_config_version = 0;
        s.modified_by = h.db_user();
        s.performed_by = h.db_user();
        s.change_reason_code = "system.test";
        s.change_commentary = "fx montage test";
        ores::marketdata::repository::market_series_repository{}.write(h.context(), s);

        const auto now = std::chrono::system_clock::now();
        const auto start = std::chrono::system_clock::time_point{
            std::chrono::floor<std::chrono::days>(now)};
        ores::marketdata::repository::market_observation_repository repo;
        int i = 0;
        for (const auto value : {first, last}) {
            ores::marketdata::domain::market_observation o;
            o.id = uuid_gen();
            o.tenant_id = h.tenant_id();
            o.party_id = party_id;
            o.series_id = s.id;
            // One price at the window's start, one at its end.
            o.observation_datetime = i++ == 0 ? start : now;
            o.oresmd_uri = datum_uri(base, quote);
            o.value = std::to_string(value);
            o.source = "fx montage test";
            repo.insert(*party, {o});
        }
    }
};

}

TEST_CASE("the_montage_answers_every_cross_of_the_base_including_the_quote_side",
          montage_tags) {
    montage_fixture f;

    const auto resp = get_fx_montage(*f.party, f.request());

    REQUIRE(resp.success);
    REQUIRE(resp.pairs.size() == 2);
    CHECK(resp.pairs[0].pair_name == "AUD/USD");
    CHECK(resp.pairs[1].pair_name == "GBP/AUD");
    CHECK(resp.pairs[1].quote_currency == "AUD");
}

TEST_CASE("each_provider_of_a_pair_is_one_spot", montage_tags) {
    montage_fixture f;

    const auto resp = get_fx_montage(*f.party, f.request());

    REQUIRE(resp.success);
    REQUIRE(resp.pairs.empty() == false);
    const auto& aud_usd = resp.pairs.front();
    CHECK(aud_usd.spot_by_provider.size() == 2);
    CHECK(aud_usd.consensus > 0.0);
    for (const auto& spot : aud_usd.spot_by_provider) {
        CHECK(spot.provider_code.empty() == false);
        CHECK(spot.series_id.empty() == false);
        CHECK(spot.age_seconds >= 0);
    }
}

TEST_CASE("the_montage_states_the_instant_its_change_is_measured_from", montage_tags) {
    montage_fixture f;

    const auto resp = get_fx_montage(*f.party, f.request());

    REQUIRE(resp.success);
    CHECK(resp.change_since <= resp.as_of);
    REQUIRE(resp.pairs.empty() == false);
    // Two prices inside the window, so the change and the range are present.
    CHECK(resp.pairs.front().change_pct.has_value());
    CHECK(resp.pairs.front().day_high.has_value());
    CHECK(resp.pairs.front().day_low.has_value());
}

TEST_CASE("a_base_nothing_quotes_is_refused_in_words", montage_tags) {
    montage_fixture f;

    auto req = f.request();
    req.base_currency = "ZZZ";
    const auto resp = get_fx_montage(*f.party, req);

    CHECK(resp.success == false);
    CHECK(resp.message.find("ZZZ") != std::string::npos);
}
