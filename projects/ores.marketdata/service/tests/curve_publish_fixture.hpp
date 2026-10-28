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
#ifndef ORES_MARKETDATA_SERVICE_TESTS_CURVE_PUBLISH_FIXTURE_HPP
#define ORES_MARKETDATA_SERVICE_TESTS_CURVE_PUBLISH_FIXTURE_HPP

/**
 * @file curve_publish_fixture.hpp
 * @brief One party, one bootstrappable curve and the market behind it, for the
 * suites that read and end published sets.
 *
 * The fixture is shared by every suite of this component that needs a curve a
 * person can publish: the gate's, the reads' and the withdrawal's. It writes
 * the whole chain a publish needs -- the party, the configuration, its pillars
 * and the market the bootstrap reads -- so a test states the act and not the
 * data behind it.
 */

#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include "ores.marketdata.core/repository/curve_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/series_snapshot_reader.hpp"
#include "ores.marketdata.service/app/curve_publish_gate.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/ir_curve_bootstrap_pillar_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <tuple>
#include <vector>

namespace ores::marketdata::service::tests {

using ores::marketdata::service::app::curve_publish_kind_code;
using ores::marketdata::service::app::publish_subject;
using ores::marketdata::service::app::resolve_publish_subject;

/// The instant the fixture's curve is bootstrapped against.
const auto as_of = std::chrono::sys_days{std::chrono::year{2026} / 3 / 2} + std::chrono::hours{12};

/**
 * @brief An account of the test tenant, and the context that acts as it.
 *
 * The database stamps every row with the account that wrote it, so a context
 * that names no account cannot raise a request or record a publication.
 */
struct person {
    ores::iam::domain::account account;
    ores::database::context ctx;
};

person seed_person(ores::testing::scoped_database_helper& h) {
    auto gctx = ores::testing::make_generation_context(h);
    auto a = ores::iam::generators::generate_synthetic_account(gctx);
    // A person's account written into the shared test tenant would end its
    // bootstrap, so the account is a service account, as the other approval
    // suites make theirs.
    a.account_type = "service";
    a.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(h.context(), a);
    return {.account = a, .ctx = h.context().with_tenant(h.tenant_id(), a.username)};
}

/**
 * @brief One curve a party can publish, with the market behind it.
 *
 * The curve is a two-pillar funding curve on a spot-starting convention whose
 * tenors the seeded convention resolves -- SPOT to 12M and 12M to 2Y -- so the
 * quote keys a feed would write are the keys the bootstrap reads, and the
 * fixture names them without knowing a single date.
 */
class curve_fixture {
public:
    curve_fixture()
        : gctx(ores::testing::make_generation_context(h))
        , party_id(uuid_gen())
        , config_id(uuid_gen())
        , output_series_id(uuid_gen()) {
        write_party();
        party = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
        write_series(output_series_id, output_series_uri());
        write_config();
        write_pillars();
        write_market();
    }

    /// @brief The subject the curve configuration states as of a given instant.
    publish_subject subject(std::chrono::system_clock::time_point at = as_of) const {
        return resolve_publish_subject(*party, boost::uuids::to_string(config_id), at);
    }

    /// @brief Every observation the published output series holds.
    std::size_t output_point_count() const {
        ores::marketdata::repository::market_observation_repository repo;
        return repo.read_latest_for_series(*party, output_series_id).size();
    }

    /// @brief Seeds a second curve of the same party, with its own output series.
    boost::uuids::uuid seed_second_curve() {
        const auto id = uuid_gen();
        const auto series_id = uuid_gen();
        write_series(series_id, output_series_uri("USD-SOFR-SECOND"));
        write_config(id, series_id);
        return id;
    }

    /// @brief The output series, as the resolver names it.
    ores::marketdata::messaging::resolve_series_identity_request identity() const {
        ores::marketdata::messaging::resolve_series_identity_request id;
        id.asset = "ir";
        id.scope = "USD";
        id.instrument_type = "discount";
        id.quote_type = "rate";
        id.fields.push_back({"curve_id", "USD-SOFR-FIXTURE"});
        return id;
    }

    /// @brief The snapshot a consumer reads, which states who published the set.
    ores::marketdata::messaging::get_curve_snapshot_response snapshot() const {
        ores::marketdata::messaging::get_curve_snapshot_request req;
        req.identity = identity();
        return ores::marketdata::service::series_snapshot_reader::read(*party, req, as_of);
    }

    /// @brief The published record of the output series, if any.
    std::optional<ores::marketdata::domain::curve_publish> published() const {
        ores::marketdata::repository::curve_publish_repository repo;
        return repo.read_latest_published_for_series(*party, output_series_id);
    }

    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gctx;
    boost::uuids::random_generator uuid_gen;

    boost::uuids::uuid party_id;
    boost::uuids::uuid config_id;
    boost::uuids::uuid output_series_id;

    /// The party's context: a curve, its quotes and its publish record are one party's.
    std::optional<ores::database::context> party;

private:
    static std::string output_series_uri(const std::string& curve_id = "USD-SOFR-FIXTURE") {
        return "oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=" + curve_id;
    }

    void write_party() {
        ores::refdata::repository::party_repository repo;
        auto p = ores::refdata::generators::generate_synthetic_party(gctx);
        p.id = party_id;
        p.version = 0;
        // The tenant already holds its root party, and the schema allows one. A
        // fixture party hangs beneath it, as every ordinary party does.
        p.is_registration_default = false;
        const auto existing = repo.read_latest(h.context());
        for (const auto& e : existing)
            if (e.tenant_id == h.tenant_id() && !e.parent_party_id) {
                p.parent_party_id = e.id;
                break;
            }
        p.change_reason_code = "system.test";
        p.change_commentary = "curve publish gate test";
        repo.write(h.context(), p);
    }

    void write_series(const boost::uuids::uuid& id, const std::string& uri) {
        ores::marketdata::domain::market_series s;
        s.id = id;
        s.version = 0;
        s.tenant_id = h.tenant_id();
        s.party_id = party_id;
        s.oresmd_uri = uri;
        s.series_subclass = "yield";
        s.provider_code = "ore";
        s.derivation_kind = "OBSERVED";
        s.derivation_config_id = boost::uuids::nil_uuid();
        s.derivation_config_version = 0;
        s.modified_by = h.db_user();
        s.performed_by = h.db_user();
        s.change_reason_code = "system.test";
        s.change_commentary = "curve publish gate test";
        ores::marketdata::repository::market_series_repository repo;
        repo.write(h.context(), s);
    }

    void write_config() {
        write_config(config_id, output_series_id);
    }

    void write_config(const boost::uuids::uuid& id, const boost::uuids::uuid& series) {
        ores::refdata::domain::ir_curve_bootstrap_config c;
        c.id = id;
        c.version = 0;
        c.tenant_id = h.tenant_id();
        c.party_id = party_id;
        // The name is unique per party among the active rows, and a fixture may
        // write more than one config, so it is taken from the id it writes.
        c.name = "USD-SOFR-" + boost::uuids::to_string(id).substr(0, 8);
        c.currency_code = "USD";
        c.index_tenor = "1D";
        c.curve_family_role = "FUNDING";
        c.discount_curve_config_id = std::nullopt;
        c.interpolation_method = "FLAT_FORWARD_THEN_LOG_LINEAR";
        c.day_count_convention = "A360";
        c.tenor_convention_code = "RATES_SPOT_FORWARD";
        c.split_tenor_code = "12M";
        c.output_series_id = series;
        c.modified_by = h.db_user();
        c.performed_by = h.db_user();
        c.change_reason_code = "system.test";
        c.change_commentary = "curve publish gate test";
        ores::refdata::repository::ir_curve_bootstrap_config_repository repo;
        repo.write(h.context(), c);
    }

    void write_pillars() {
        ores::refdata::repository::ir_curve_bootstrap_pillar_repository repo;
        int seq = 0;
        for (const auto& [start, end, role] :
             {std::tuple{"SPOT", "12M", "DEPOSIT"}, std::tuple{"12M", "2Y", "SWAP"}}) {
            ores::refdata::domain::ir_curve_bootstrap_pillar p;
            p.id = uuid_gen();
            p.version = 0;
            p.tenant_id = h.tenant_id();
            p.party_id = party_id;
            p.bootstrap_config_id = config_id;
            p.sequence_index = seq++;
            p.start_tenor_code = start;
            p.end_tenor_code = end;
            p.curve_role_code = role;
            p.modified_by = h.db_user();
            p.performed_by = h.db_user();
            p.change_reason_code = "system.test";
            p.change_commentary = "curve publish gate test";
            repo.write(h.context(), p);
        }
    }

    /// @brief The raw grid: one series per pillar, each quoted at the as-of.
    void write_market() {
        ores::marketdata::repository::market_observation_repository obs_repo;
        for (const auto& [start, end, value] :
             {std::tuple{"SPOT", "12M", "0.0432"}, std::tuple{"12M", "2Y", "0.0395"}}) {
            const auto key =
                ores::marketdata::core::make_pillar_quote_key("USD",
                                                              "1D",
                                                              start,
                                                              std::chrono::year{2026} / 1 / 1,
                                                              std::chrono::year{2026} / 3 / 2,
                                                              end);
            const auto series_id = uuid_gen();
            write_series(series_id, ores::marketdata::core::pillar_series_uri(key));

            ores::marketdata::domain::market_observation o;
            o.id = uuid_gen();
            o.tenant_id = h.tenant_id();
            o.party_id = party_id;
            o.series_id = series_id;
            o.observation_datetime = as_of;
            o.oresmd_uri = ores::marketdata::core::pillar_datum_uri(key);
            o.value = value;
            o.source = "curve publish gate test";
            obs_repo.insert(*party, {o});
        }
    }
};

}

#endif
