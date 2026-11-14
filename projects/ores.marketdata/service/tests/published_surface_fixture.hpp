/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_MARKETDATA_SERVICE_TESTS_PUBLISHED_SURFACE_FIXTURE_HPP
#define ORES_MARKETDATA_SERVICE_TESTS_PUBLISHED_SURFACE_FIXTURE_HPP

/**
 * @file published_surface_fixture.hpp
 * @brief One party's published FX volatility surface, written whole, for the
 * suite that reads it back.
 *
 * The read states what a surface means as well as its grid: the smile model,
 * the pair's delta type, the strike convention those labels are read under and
 * whether the publication has aged past the server's threshold. Proving that
 * needs a surface that is published, so the fixture writes the four records a
 * publish leaves behind -- the party, the surface series, the blueprint with its
 * convention, the publish record and the grid -- without running the calibration
 * or the approval chain, which other suites own.
 */

#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.api/datum/value.hpp"
#include "ores.marketdata.api/messaging/fx_vol_surface_operation_protocol.hpp"
#include "ores.marketdata.core/repository/fx_vol_surface_publish_repository.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.service/app/fx_vol_surface_publish_service.hpp"
#include "ores.refdata.api/domain/fx_option_convention.hpp"
#include "ores.refdata.api/domain/fx_vol_surface_blueprint.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/fx_option_convention_repository.hpp"
#include "ores.refdata.core/repository/fx_vol_surface_blueprint_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::marketdata::service::tests {

/// The convention the fixture's pair is quoted under, as refdata seeds it.
inline constexpr const char* surface_convention_id = "EUR-USD-FXOPTION";

/// The delta type the convention states, which makes a delta strike a vol a person can price.
inline constexpr const char* surface_delta_type = "Spot";

/// The smile model the fixture's blueprint fits.
inline constexpr const char* surface_model = "ssvi";

/// The expiries the fixture publishes, in the blueprint's order.
inline constexpr const char* surface_expiries = "1W,1M";

/// The strikes a pair quoted in delta publishes, in the blueprint's order.
inline constexpr const char* surface_delta_strikes = "DEL/Spot/Put/25,ATM,DEL/Spot/Call/25";

/// The strikes a surface published off absolute levels holds, in the same order.
inline constexpr const char* surface_absolute_strikes = "1.05,1.10,1.15";

/**
 * @brief One surface a party has published, and the reads that answer for it.
 *
 * Every call names the instant the publication is stamped at, so a test can
 * publish into the past and ask what the server says about its age. A blueprint
 * names its convention by a soft reference, so a fixture can state a surface
 * whose convention row is missing: its strikes are then levels, and the read
 * must say so rather than claim a delta type nobody recorded.
 */
class surface_fixture {
public:
    explicit surface_fixture(const std::chrono::system_clock::time_point published_at,
                             const bool convention_exists = true)
        : gctx(ores::testing::make_generation_context(h))
        , published_at_(published_at)
        , party_id_(uuid_gen())
        , series_id_(uuid_gen())
        , blueprint_id_(uuid_gen())
        , convention_exists_(convention_exists) {
        write_party();
        party = h.context().with_party(h.tenant_id(), party_id_, {party_id_}, actor_username());
        if (convention_exists)
            write_convention();
        write_series();
        write_blueprint();
        write_request();
        write_publish();
        write_grid();
    }

    /// @brief The read a client makes, as of an instant at or after the publication.
    ores::marketdata::messaging::get_published_fx_vol_surface_response
    read(const std::chrono::system_clock::time_point as_of) const {
        ores::marketdata::messaging::get_published_fx_vol_surface_request req;
        req.blueprint_id = boost::uuids::to_string(blueprint_id_);
        req.as_of = as_of;
        return ores::marketdata::service::app::fx_vol_surface_publish_service::read_published(
            *party, req);
    }

    /// @brief The read for the publication the fixture wrote.
    ores::marketdata::messaging::get_published_fx_vol_surface_response read() const {
        return read(published_at_);
    }

    [[nodiscard]] std::string blueprint_id() const {
        return boost::uuids::to_string(blueprint_id_);
    }

    [[nodiscard]] std::string request_id() const {
        return boost::uuids::to_string(request_id_);
    }

    [[nodiscard]] std::chrono::system_clock::time_point published_at() const {
        return published_at_;
    }

    /// The strikes the fixture's blueprint publishes on.
    [[nodiscard]] static std::string strikes_of(const bool convention_resolves) {
        return convention_resolves ? surface_delta_strikes : surface_absolute_strikes;
    }

    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gctx;
    boost::uuids::random_generator uuid_gen;
    /// The party's context: the read answers only the session party's surfaces.
    std::optional<ores::database::context> party;

private:
    /// The account the publish request is raised by, which the store validates.
    std::string actor_username() const {
        return account_username_;
    }

    void write_party() {
        ores::refdata::repository::party_repository repo;
        auto p = ores::refdata::generators::generate_synthetic_party(gctx);
        p.id = party_id_;
        p.version = 0;
        p.is_registration_default = false;
        const auto existing = repo.read_latest(h.context());
        for (const auto& e : existing)
            if (e.tenant_id == h.tenant_id() && !e.parent_party_id) {
                p.parent_party_id = e.id;
                break;
            }
        p.change_reason_code = "system.test";
        p.change_commentary = "published surface fixture";
        repo.write(h.context(), p);

        auto a = ores::iam::generators::generate_synthetic_account(gctx);
        a.account_type = "service";
        a.change_reason_code = "system.test";
        a.change_commentary = "published surface fixture";
        ores::iam::repository::account_repository{}.write(h.context(), a);
        account_username_ = a.username;
    }

    void write_convention() {
        ores::refdata::domain::fx_option_convention c;
        c.id = std::string(surface_convention_id);
        c.party_id = party_id_;
        c.atm_type = "AtmDeltaNeutral";
        c.delta_type = std::string(surface_delta_type);
        c.switch_tenor = "2Y";
        c.butterfly_style = "Broker";
        c.modified_by = h.db_user();
        c.performed_by = h.db_user();
        c.change_reason_code = "system.test";
        c.change_commentary = "published surface fixture";
        ores::refdata::repository::fx_option_convention_repository{}.write(*party, c);
    }

    void write_series() {
        ores::marketdata::domain::market_series s;
        s.id = series_id_;
        s.version = 0;
        s.tenant_id = h.tenant_id();
        s.party_id = party_id_;
        s.oresmd_uri = surface_series_uri();
        s.series_subclass = "volatility";
        s.provider_code = "ore";
        s.derivation_kind = "OBSERVED";
        s.derivation_config_id = boost::uuids::nil_uuid();
        s.derivation_config_version = 0;
        s.modified_by = h.db_user();
        s.performed_by = h.db_user();
        s.change_reason_code = "system.test";
        s.change_commentary = "published surface fixture";
        ores::marketdata::repository::market_series_repository{}.write(*party, s);
    }

    void write_blueprint() {
        ores::refdata::domain::fx_vol_surface_blueprint b;
        b.version = 0;
        b.tenant_id = h.tenant_id();
        b.id = blueprint_id_;
        b.output_series_id = series_id_;
        b.party_id = party_id_;
        b.name = "EURUSD_SSVI_FIXTURE";
        b.base_currency_code = "EUR";
        b.quote_currency_code = "USD";
        b.fx_option_convention_id = std::string(surface_convention_id);
        b.smile_model = std::string(surface_model);
        b.expiry_interpolation = "linear_total_variance";
        b.output_expiries = std::string(surface_expiries);
        b.output_strikes = strikes_of(convention_exists_);
        b.modified_by = h.db_user();
        b.performed_by = h.db_user();
        b.change_reason_code = "system.test";
        b.change_commentary = "published surface fixture";
        ores::refdata::repository::fx_vol_surface_blueprint_repository{}.write(*party, b);
    }

    /// The publish record names the request that authorised it, so one is raised.
    void write_request() {
        ores::inbox::service::approval_lifecycle lifecycle(*party);
        const auto kind = lifecycle.kind(
            std::string(ores::marketdata::service::app::fx_vol_surface_publish_kind_code));
        if (!kind)
            throw std::runtime_error("The surface publish kind is not seeded.");
        const auto me = lifecycle.actor_account_id();
        if (!me)
            throw std::runtime_error("The fixture's account was not found.");
        const auto raised = lifecycle.raise(*kind, "The fixture publishes its surface", *me);
        request_id_ = raised.id;
    }

    void write_publish() {
        ores::marketdata::domain::fx_vol_surface_publish r;
        r.id = uuid_gen();
        r.request_id = request_id_;
        r.party_id = party_id_;
        r.blueprint_id = blueprint_id_;
        r.as_of = published_at_;
        r.output_series_id = series_id_;
        r.calibration_id = uuid_gen();
        r.calibration_version = 1;
        r.converged = true;
        r.max_abs_error = 0.0;
        r.pillars_beyond_tolerance = 0;
        r.violation_count = 0;
        r.published_at = published_at_;
        r.published_by = account_username_;
        r.modified_by = h.db_user();
        r.performed_by = h.db_user();
        r.change_reason_code = "system.test";
        r.change_commentary = "published surface fixture";
        ores::marketdata::repository::fx_vol_surface_publish_repository{}.write(*party, r);
    }

    /// The grid is six observations of one series, each naming its expiry and strike.
    void write_grid() {
        const auto series = series_datum();
        std::vector<ores::marketdata::domain::market_observation> observations;
        for (const auto& [expiry, strike, volatility] : grid_values(convention_exists_)) {
            const auto point = point_datum(series, expiry, strike);
            ores::marketdata::domain::market_observation o;
            o.id = uuid_gen();
            o.tenant_id = h.tenant_id();
            o.party_id = party_id_;
            o.series_id = series_id_;
            o.observation_datetime = published_at_;
            o.oresmd_uri = point;
            o.value = volatility;
            o.source = "published surface fixture";
            observations.push_back(std::move(o));
        }
        ores::marketdata::repository::market_observation_repository{}.insert(*party, observations);
    }

    static std::vector<std::tuple<std::string, std::string, std::string>>
    grid_values(const bool convention_resolves) {
        if (!convention_resolves)
            return {{"1W", "1.05", "0.095"},
                    {"1W", "1.10", "0.085"},
                    {"1W", "1.15", "0.089"},
                    {"1M", "1.05", "0.092"},
                    {"1M", "1.10", "0.082"},
                    {"1M", "1.15", "0.086"}};
        return {{"1W", "DEL/Spot/Put/25", "0.095"},
                {"1W", "ATM", "0.085"},
                {"1W", "DEL/Spot/Call/25", "0.089"},
                {"1M", "DEL/Spot/Put/25", "0.092"},
                {"1M", "ATM", "0.082"},
                {"1M", "DEL/Spot/Call/25", "0.086"}};
    }

    static std::string surface_series_uri() {
        return "oresmd://fx/EUR?type=series&instrument=fx_vol_surface&quote=rate_lnvol"
               "&ccy=USD&surface_id=EURUSD_SSVI_FIXTURE";
    }

    static ores::marketdata::datum::market_datum series_datum() {
        const auto read = ores::marketdata::datum::oresmd_uri_codec::read(surface_series_uri());
        if (!read || !read->is_series())
            throw std::runtime_error("The fixture's surface series URI does not read.");
        return *read;
    }

    static std::string point_datum(const ores::marketdata::datum::market_datum& series,
                                   const std::string& expiry,
                                   const std::string& strike) {
        const auto term = ores::marketdata::datum::term::parse(expiry);
        if (!term)
            throw std::runtime_error("The expiry " + expiry + " is not a term: " + term.error());
        const auto level = ores::marketdata::datum::strike::parse(strike);
        if (!level)
            throw std::runtime_error("The strike " + strike + " is not a strike: " + level.error());
        const auto point = ores::marketdata::datum::datum_at(
            series,
            {{ores::marketdata::datum::field::expiry, ores::marketdata::datum::value(*term)},
             {ores::marketdata::datum::field::strike, ores::marketdata::datum::value(*level)}});
        if (!point)
            throw std::runtime_error("The grid point " + expiry + " " + strike +
                                     " does not build.");
        const auto uri = ores::marketdata::datum::oresmd_uri_codec::write(*point);
        if (!uri)
            throw std::runtime_error("The grid point " + expiry + " " + strike +
                                     " does not write.");
        return *uri;
    }

    std::chrono::system_clock::time_point published_at_;
    boost::uuids::uuid party_id_;
    boost::uuids::uuid series_id_;
    boost::uuids::uuid blueprint_id_;
    boost::uuids::uuid request_id_;
    std::string account_username_;
    bool convention_exists_;
};

}

#endif
