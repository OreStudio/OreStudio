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
#include "ores.marketdata.api/domain/i_feed.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/domain/instrument_code.hpp"
#include "ores.refdata.api/domain/tenor.hpp"
#include "ores.refdata.api/domain/tenor_convention.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/feeds/feed_factory.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>

namespace {

const std::string tags("[feed_factory]");

using ores::synthetic::domain::binding_mode;
using ores::synthetic::domain::fx_spot_generation_config;
using ores::synthetic::domain::gmm_component;
using ores::synthetic::domain::ir_curve_generation_config;
using ores::synthetic::domain::ir_curve_generation_config_process_parameter_value;
using ores::synthetic::domain::ir_curve_template_entry;
using ores::synthetic::domain::yield_curve_process_parameter_definition;
using ores::synthetic::feed::feed_build_context;
using ores::synthetic::feed::feed_build_input;
using ores::synthetic::feed::feed_factory;
using ores::synthetic::feed::fx_spot_feed_build_input;
using ores::synthetic::feed::fx_spot_feed_kind;
using ores::synthetic::feed::ir_curve_feed_build_input;
using ores::synthetic::feed::ir_curve_feed_kind;
using ores::synthetic::feed::make_default_feed_factory;

// An unconnected client pair. Feed construction stores the references only;
// nothing connects or publishes until start().
ores::nats::service::client& nats() {
    static auto instance = ores::nats::service::client(ores::nats::config::nats_options{});
    return instance;
}

ores::nats::service::nats_client& auth_nats() {
    static auto instance = ores::nats::service::nats_client();
    return instance;
}

feed_build_context make_build_context() {
    return feed_build_context{nats(), auth_nats(), {}};
}

gmm_component make_component(double mean, double stdev, double weight) {
    gmm_component c;
    c.mean = mean;
    c.stdev = stdev;
    c.weight = weight;
    return c;
}

fx_spot_generation_config make_fx_config() {
    fx_spot_generation_config cfg;
    cfg.ore_key = "FX/RATE/EUR/USD";
    cfg.source_name = "eur_usd_test";
    cfg.process_type = "geometric";
    cfg.gmm_initial_price = 1.1;
    cfg.ticks_per_hour = 60;
    return cfg;
}

// SPOT/1M catalog with a single DEPOSIT instrument, mirroring the fixture in
// ir_curve_template_resolver_tests.cpp -- enough for resolve() to derive the
// entry a tick loop needs.
ores::synthetic::feed::ir_curve_refdata_context make_refdata_context() {
    using ores::refdata::domain::instrument_code;
    using ores::refdata::domain::tenor;
    using ores::refdata::domain::tenor_convention_resolution;

    ores::synthetic::feed::ir_curve_refdata_context ctx;

    tenor spot;
    spot.code = "SPOT";
    spot.kind = "PERIOD";
    spot.unit = "DAY";
    spot.multiplier = 0;
    ctx.tenors_by_code.emplace("SPOT", spot);

    tenor m1;
    m1.code = "1M";
    m1.kind = "PERIOD";
    m1.unit = "MONTH";
    m1.multiplier = 1;
    ctx.tenors_by_code.emplace("1M", m1);

    ctx.convention.code = "RATES_SPOT_FORWARD";
    ctx.convention.measured_from = "SPOT";
    ctx.convention.resolution_algorithm = "ANCHOR_OFFSET";

    for (const auto& code : {"SPOT", "1M"}) {
        tenor_convention_resolution r;
        r.convention_code = "RATES_SPOT_FORWARD";
        r.tenor_code = code;
        ctx.resolutions_by_tenor.emplace(code, r);
    }

    instrument_code depo;
    depo.code = "DEPO";
    depo.curve_role = "DEPOSIT";
    ctx.instrument_codes_by_code.emplace("DEPO", depo);

    ctx.horizon = std::chrono::year{2026} / std::chrono::January / 1;
    ctx.spot = ctx.horizon;
    return ctx;
}

// A VASICEK config with the full {definition, value} pair set the mapping layer
// requires (kappa/theta/sigma/initial_rate), price_source "fixed" so no vintage
// lookup runs.
struct ir_fixture {
    ir_curve_generation_config config;
    std::vector<ir_curve_template_entry> entries;
    std::vector<ir_curve_generation_config_process_parameter_value> values;
    std::vector<yield_curve_process_parameter_definition> definitions;
    ores::synthetic::feed::ir_curve_refdata_context refctx;
};

ir_fixture make_ir_fixture() {
    ir_fixture f;
    f.config.currency_code = "USD";
    f.config.index_family = "SOFR";
    f.config.role = "discount";
    f.config.source_name = "usd_sofr_test";
    f.config.process_type = "VASICEK";
    f.config.ticks_per_hour = 60;

    ir_curve_template_entry e;
    e.sequence_index = 0;
    e.start_tenor_code = "SPOT";
    e.end_tenor_code = "1M";
    e.instrument_code = "DEPO";
    f.entries.push_back(e);

    for (const std::string_view name : {"kappa", "theta", "sigma", "initial_rate"}) {
        yield_curve_process_parameter_definition d;
        d.process_type_code = "VASICEK";
        d.parameter_name = name;
        d.id = boost::uuids::random_generator()();
        f.definitions.push_back(d);

        ir_curve_generation_config_process_parameter_value v;
        v.parameter_definition_id = d.id;
        v.parameter_value = name == "sigma" ? 0.01 : (name == "initial_rate" ? 0.04 : 0.05);
        f.values.push_back(v);
    }

    f.refctx = make_refdata_context();
    return f;
}

// A minimal IFeed stub proving make() dispatches to whatever builder is
// registered for the kind -- the registration point for future asset classes.
class stub_feed final : public ores::marketdata::domain::IFeed {
public:
    const std::string& source_name() const override {
        return source_name_;
    }
    const std::string& qualifier() const override {
        return qualifier_;
    }
    const std::string& role() const override {
        return role_;
    }
    std::string conflict_key() const override {
        return ores::marketdata::domain::feed_conflict_key(qualifier_, role_);
    }
    void start() override {}
    void stop() override {}
    std::uint64_t publish_count() const override {
        return 0;
    }

private:
    std::string source_name_{"stub"};
    std::string qualifier_{"STUB/KEY"};
    std::string role_;
};

}

TEST_CASE("make_default_feed_factory registers both kinds", tags) {
    const auto factory = make_default_feed_factory();
    CHECK(factory.kinds() ==
          std::vector<std::string>{std::string(fx_spot_feed_kind), std::string(ir_curve_feed_kind)});
}

TEST_CASE("factory::make constructs an fx_spot_feed from its persisted config", tags) {
    const auto factory = make_default_feed_factory();
    auto input = fx_spot_feed_build_input{make_fx_config(), {make_component(0.0, 0.1, 1.0)}};
    input.binding_mode = binding_mode::bound;

    const auto feed = factory.make(std::string(fx_spot_feed_kind), make_build_context(), input);

    const auto* fx = dynamic_cast<const ores::synthetic::feed::fx_spot_feed*>(feed.get());
    REQUIRE(fx != nullptr);
    CHECK(fx->source_name() == "eur_usd_test");
    CHECK(fx->ore_key() == "FX/RATE/EUR/USD");
    CHECK(fx->qualifier() == "EUR/USD");
    CHECK(fx->role().empty());
    CHECK(fx->conflict_key() == "EUR/USD\x1f");
    CHECK(fx->publish_count() == 0);
}

TEST_CASE("factory::make constructs an ir_curve_feed from its persisted config", tags) {
    const auto factory = make_default_feed_factory();
    const auto f = make_ir_fixture();
    const auto input = ir_curve_feed_build_input{
        f.config, f.entries, f.values, f.definitions, f.refctx};

    const auto feed = factory.make(std::string(ir_curve_feed_kind), make_build_context(), input);

    const auto* ir = dynamic_cast<const ores::synthetic::feed::ir_curve_feed*>(feed.get());
    REQUIRE(ir != nullptr);
    CHECK(ir->source_name() == "usd_sofr_test");
    CHECK(ir->qualifier() == "USD/SOFR");
    CHECK(ir->role() == "discount");
    CHECK(ir->conflict_key() == "USD/SOFR\x1f" "discount");
    CHECK(ir->publish_count() == 0);
}

TEST_CASE("factory::make rejects an unknown kind", tags) {
    const auto factory = make_default_feed_factory();
    const auto input = fx_spot_feed_build_input{make_fx_config(), {make_component(0.0, 0.1, 1.0)}};
    CHECK_THROWS_AS(factory.make("equities", make_build_context(), input), std::invalid_argument);
}

TEST_CASE("factory::make rejects a build input that is not the kind's own", tags) {
    const auto factory = make_default_feed_factory();
    const auto f = make_ir_fixture();
    const auto input = ir_curve_feed_build_input{
        f.config, f.entries, f.values, f.definitions, f.refctx};
    CHECK_THROWS_AS(
        factory.make(std::string(fx_spot_feed_kind), make_build_context(), input),
        std::invalid_argument);
}

TEST_CASE("factory::make dispatches to a replacement registration for a kind", tags) {
    feed_factory factory;
    factory.register_kind(
        std::string(fx_spot_feed_kind),
        [](const feed_build_context&, const feed_build_input&) {
            return std::shared_ptr<ores::marketdata::domain::IFeed>(new stub_feed());
        });

    const auto input = fx_spot_feed_build_input{make_fx_config(), {make_component(0.0, 0.1, 1.0)}};
    const auto feed = factory.make(std::string(fx_spot_feed_kind), make_build_context(), input);

    CHECK(dynamic_cast<const stub_feed*>(feed.get()) != nullptr);
    CHECK(feed->source_name() == "stub");
}
