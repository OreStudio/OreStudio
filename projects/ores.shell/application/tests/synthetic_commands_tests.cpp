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
#include "ores.shell/app/commands/synthetic_commands.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_process_parameter_value_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/ir_curve_template_entry_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <ostream>
#include <rfl/json.hpp>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using ores::shell::app::commands::feed_listing;
using ores::shell::app::commands::feed_state;
using ores::shell::app::commands::synthetic_commands;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.shell.application.tests");
const std::string tags("[commands][synthetic]");

namespace synthetic = ores::synthetic::domain;
namespace messaging = ores::synthetic::messaging;

// Fixed ids, so a state line names the feed the test built and not a random
// one the run happened to mint.
const std::string config_one("aaaaaaaa-0000-4000-8000-000000000001");
const std::string config_two("aaaaaaaa-0000-4000-8000-000000000002");
const std::string fx_config("bbbbbbbb-0000-4000-8000-000000000001");
const std::string ir_config("cccccccc-0000-4000-8000-000000000001");
const std::string party("3f2504e0-4f89-11d3-9a0c-0305e82c3301");

boost::uuids::uuid id_of(const std::string& text) {
    return boost::uuids::string_generator()(text);
}

synthetic::fx_spot_generation_config
fx_config_row(const std::string& id,
              const std::string& owner,
              const std::string& source_name,
              const std::string& ore_key,
              bool enabled,
              bool auto_start) {
    synthetic::fx_spot_generation_config row;
    row.id = id_of(id);
    row.party_id = id_of(party);
    row.config_id = id_of(owner);
    row.base_currency_code = "EUR";
    row.quote_currency_code = "USD";
    row.source_name = source_name;
    row.ore_key = ore_key;
    row.price_source = "fixed";
    row.gmm_initial_price = 1.0812;
    row.ticks_per_hour = 60;
    row.process_type = "geometric";
    row.enabled = enabled;
    row.auto_start = auto_start;
    return row;
}

synthetic::gmm_component gmm_row(const std::string& fx_id, int index) {
    synthetic::gmm_component row;
    row.id = id_of("dddddddd-0000-4000-8000-00000000000" + std::to_string(index));
    row.party_id = id_of(party);
    row.fx_spot_config_id = id_of(fx_id);
    row.component_index = index;
    row.description = "component " + std::to_string(index);
    row.weight = 0.5;
    row.mean = 0.0;
    row.stdev = 0.01;
    return row;
}

synthetic::ir_curve_generation_config
ir_config_row(const std::string& id, const std::string& owner, const std::string& source_name) {
    synthetic::ir_curve_generation_config row;
    row.id = id_of(id);
    row.party_id = id_of(party);
    row.config_id = id_of(owner);
    row.currency_code = "USD";
    row.index_family = "sofr";
    row.role = "self_discounting";
    row.process_type = "VASICEK";
    row.ticks_per_hour = 60;
    row.enabled = true;
    row.auto_start = true;
    row.source_name = source_name;
    return row;
}

synthetic::ir_curve_template_entry entry_row(const std::string& ir_id, int sequence) {
    synthetic::ir_curve_template_entry row;
    row.id = id_of("eeeeeeee-0000-4000-8000-00000000000" + std::to_string(sequence));
    row.party_id = id_of(party);
    row.ir_curve_config_id = id_of(ir_id);
    row.sequence_index = sequence;
    row.start_tenor_code = "1Y";
    row.end_tenor_code = "1Y";
    row.instrument_code = "ir_swap";
    return row;
}

/// One listing carrying both classes: an FX feed with its components and an IR
/// feed with its entries and values.
feed_listing two_class_listing() {
    feed_listing listing;

    synthetic::market_data_generation_config container;
    container.id = id_of(config_one);
    container.name = "Basic";
    listing.configs.push_back(container);

    listing.fx_spot.push_back(
        fx_config_row(fx_config, config_one, "synthetic.eurusd", "FX/RATE/EUR/USD", true, true));
    listing.fx_spot.push_back(
        fx_config_row("bbbbbbbb-0000-4000-8000-000000000002",
                      config_two,
                      "synthetic.eurgbp",
                      "FX/RATE/EUR/GBP",
                      true,
                      false));
    for (int i = 0; i < 3; ++i)
        listing.gmm_components.push_back(gmm_row(fx_config, i));

    listing.ir_curve.push_back(ir_config_row(ir_config, config_two, "ir_curve.usd.sofr"));
    listing.template_entries.push_back(entry_row(ir_config, 0));
    listing.template_entries.push_back(entry_row(ir_config, 1));
    synthetic::ir_curve_generation_config_process_parameter_value value;
    value.id = id_of("ffffffff-0000-4000-8000-000000000001");
    value.config_id = id_of(ir_config);
    value.parameter_definition_id = id_of("ffffffff-0000-4000-8000-000000000002");
    value.parameter_value = 0.5;
    listing.parameter_values.push_back(value);

    listing.running.push_back("synthetic.eurusd");
    return listing;
}

/// A setup context the IR flow accepts: the VASICEK process and its four
/// required parameters, as the catalogue defines them.
feed_listing ir_setup_listing() {
    feed_listing listing;

    synthetic::yield_curve_process_type vasicek;
    vasicek.code = "VASICEK";
    listing.process_types.push_back(vasicek);

    int index = 0;
    for (const std::string name : {"kappa", "theta", "sigma", "initial_rate"}) {
        synthetic::yield_curve_process_parameter_definition definition;
        definition.id = id_of("99999999-0000-4000-8000-00000000000" + std::to_string(++index));
        definition.process_type_code = "VASICEK";
        definition.parameter_name = name;
        listing.definitions.push_back(definition);
    }
    return listing;
}

/// The flags one FX setup needs beyond its kind, name and source.
std::vector<std::string> fx_setup_args() {
    return {"--kind",
            "fx",
            "--name",
            "Basic",
            "--source",
            "synthetic.eurusd",
            "--base",
            "EUR",
            "--quote",
            "USD",
            "--gmm",
            "0.7,0.0,0.01"};
}

/**
 * @brief A setup session with a canned context and no transport: it records
 * every row the flow sends, so a test reads what setup would write.
 */
class stub_setup_session final : public synthetic_commands::setup_session {
public:
    explicit stub_setup_session(feed_listing listing)
        : listing_(std::move(listing)) {}

    [[nodiscard]] bool is_logged_in() const override {
        return true;
    }

    [[nodiscard]] std::string party_id() const override {
        return party;
    }

    [[nodiscard]] std::optional<feed_listing> read_context(std::ostream&) override {
        return listing_;
    }

    [[nodiscard]] bool send(std::ostream&, const synthetic_commands::setup_row& row) override {
        rows.push_back(row);
        return true;
    }

    std::vector<synthetic_commands::setup_row> rows;

private:
    feed_listing listing_;
};

}

TEST_CASE("a_feed_line_shows_its_class_source_enabled_auto_start_and_running_state", tags) {
    auto lg(make_logger(test_suite));

    feed_state feed;
    feed.asset_class = "fx_spot";
    feed.source_name = "synthetic.eurusd";
    feed.config_id = fx_config;
    feed.enabled = true;
    feed.auto_start = false;
    feed.running = true;
    feed.child_rows = "gmm 3";

    CHECK(synthetic_commands::format_feed_state(feed) ==
          "  fx_spot  synthetic.eurusd                 [on ] [manual] [running] gmm 3");

    feed.enabled = false;
    feed.auto_start = true;
    feed.running = false;
    feed.partial = true;
    feed.child_rows = "gmm 0";

    CHECK(synthetic_commands::format_feed_state(feed) ==
          "  fx_spot  synthetic.eurusd                 [off] [auto]   [stopped] [partial] gmm 0");
}

TEST_CASE("the_listing_states_every_configured_feed_and_marks_the_partial_ones", tags) {
    auto lg(make_logger(test_suite));

    const auto feeds = synthetic_commands::feed_states(two_class_listing());

    REQUIRE(feeds.size() == 3);
    CHECK(synthetic_commands::format_feed_state(feeds[0]) ==
          "  fx_spot  synthetic.eurusd                 [on ] [auto]   [running] gmm 3");
    CHECK(synthetic_commands::format_feed_state(feeds[1]) ==
          "  fx_spot  synthetic.eurgbp                 [on ] [manual] [stopped] [partial] gmm 0");
    CHECK(synthetic_commands::format_feed_state(feeds[2]) ==
          "  ir_curve ir_curve.usd.sofr                [on ] [auto]   [stopped] template 2, "
          "params 1");
    CHECK(feeds[0].config_id == fx_config);
    CHECK(feeds[1].partial);
    CHECK_FALSE(feeds[2].partial);
}

TEST_CASE("setup_plans_one_container_one_folder_chain_and_the_fx_rows", tags) {
    auto lg(make_logger(test_suite));

    const auto rows = synthetic_commands::plan_fx(
        fx_config,
        config_one,
        std::string("11111111-0000-4000-8000-000000000004"),
        party,
        "synthetic.eurusd",
        "EUR",
        "USD",
        "geometric",
        60,
        1.0812,
        {{0.7, 0.0, 0.01}, {0.3, 0.0, 0.02}});

    REQUIRE(rows.size() == 3);
    CHECK(rows[0].subject == "synthetic.v1.fx_spot_generation_configs.put");
    const auto config = rfl::json::read<messaging::put_fx_spot_generation_config_request>(
        rows[0].body);
    REQUIRE(config);
    CHECK(config->change.write.source_name == "synthetic.eurusd");
    CHECK(config->change.write.ore_key == "FX/RATE/EUR/USD");
    CHECK(config->change.write.price_source == "fixed");
    CHECK(config->change.write.gmm_initial_price == 1.0812);
    CHECK(config->change.write.ticks_per_hour == 60);
    CHECK(config->change.write.enabled);
    CHECK_FALSE(config->change.write.auto_start);
    // config_id names the owning container, not the sub-config's own id.
    CHECK(boost::uuids::to_string(config->change.write.config_id) == config_one);
    CHECK(boost::uuids::to_string(config->change.write.id) == fx_config);
    CHECK(boost::uuids::to_string(*config->change.write.folder_id) ==
          "11111111-0000-4000-8000-000000000004");

    CHECK(rows[1].subject == "synthetic.v1.gmm_components.put");
    const auto first = rfl::json::read<messaging::put_gmm_component_request>(rows[1].body);
    REQUIRE(first);
    CHECK(first->change.write.component_index == 0);
    CHECK(first->change.write.weight == 0.7);
    CHECK(first->change.write.stdev == 0.01);

    const auto second = rfl::json::read<messaging::put_gmm_component_request>(rows[2].body);
    REQUIRE(second);
    CHECK(second->change.write.component_index == 1);
    CHECK(second->change.write.weight == 0.3);
    CHECK(second->change.write.stdev == 0.02);
}

TEST_CASE("setup_plans_the_ir_rows_a_curve_needs", tags) {
    auto lg(make_logger(test_suite));

    synthetic::yield_curve_process_parameter_definition kappa;
    kappa.id = id_of("99999999-0000-4000-8000-000000000001");
    kappa.process_type_code = "VASICEK";
    kappa.parameter_name = "kappa";

    const auto rows = synthetic_commands::plan_ir(
        ir_config,
        config_two,
        std::string("22222222-0000-4000-8000-000000000004"),
        party,
        "ir_curve.usd.sofr",
        {kappa},
        "USD",
        "sofr",
        std::string(),
        "self_discounting",
        "VASICEK",
        60,
        {{.parameter_name = "kappa", .parameter_value = 0.5}},
        {"1Y", "2Y"});

    REQUIRE(rows.size() == 4);
    CHECK(rows[0].subject == "synthetic.v1.ir_curve_generation_configs.put");
    const auto config = rfl::json::read<messaging::put_ir_curve_generation_config_request>(
        rows[0].body);
    REQUIRE(config);
    CHECK(config->change.write.process_type == "VASICEK");
    CHECK(config->change.write.currency_code == "USD");
    CHECK(config->change.write.index_family == "sofr");
    CHECK(config->change.write.source_name == "ir_curve.usd.sofr");
    // config_id names the owning container, not the sub-config's own id.
    CHECK(boost::uuids::to_string(config->change.write.config_id) == config_two);
    CHECK(boost::uuids::to_string(config->change.write.id) == ir_config);
    // The SQL check admits only 'fixed' or 'vintage'; the store copies the
    // request verbatim, so the flow must state the fixed source itself.
    CHECK(config->change.write.price_source == "fixed");
    // Required, validated, and consumed by the Swap entries the resolver builds.
    CHECK(config->change.write.fixed_leg_payment_frequency_code == "Annual");

    CHECK(rows[1].subject == "synthetic.v1.ir_curve_template_entries.put");
    const auto entry = rfl::json::read<messaging::put_ir_curve_template_entry_request>(rows[1].body);
    REQUIRE(entry);
    CHECK(entry->change.write.sequence_index == 0);
    CHECK(entry->change.write.start_tenor_code == "1Y");
    CHECK(entry->change.write.instrument_code == "ir_swap");

    CHECK(rows[2].subject == "synthetic.v1.ir_curve_template_entries.put");
    const auto second = rfl::json::read<messaging::put_ir_curve_template_entry_request>(rows[2].body);
    REQUIRE(second);
    CHECK(second->change.write.sequence_index == 1);
    CHECK(second->change.write.start_tenor_code == "2Y");

    CHECK(rows[3].subject ==
          "synthetic.v1.ir_curve_generation_config_process_parameter_values.put");
    const auto value =
        rfl::json::read<messaging::put_ir_curve_generation_config_process_parameter_value_request>(
            rows[3].body);
    REQUIRE(value);
    CHECK(value->change.write.parameter_value == 0.5);
    CHECK(boost::uuids::to_string(value->change.write.parameter_definition_id) ==
          "99999999-0000-4000-8000-000000000001");
}

TEST_CASE("a_process_parameter_with_no_definition_is_left_unwritable", tags) {
    auto lg(make_logger(test_suite));

    const auto rows = synthetic_commands::plan_ir(ir_config,
                                                  config_two,
                                                  std::string("22222222-0000-4000-8000-000000000004"),
                                                  party,
                                                  "ir_curve.usd.sofr",
                                                  {},
                                                  "USD",
                                                  "sofr",
                                                  std::string(),
                                                  "self_discounting",
                                                  "VASICEK",
                                                  60,
                                                  {{.parameter_name = "kappa",
                                                    .parameter_value = 0.5}},
                                                  {"1Y"});

    REQUIRE(rows.size() == 3);
    // The row that cannot be joined onto the catalogue carries no subject, and
    // the flow stops on it rather than writing a value nothing defines.
    CHECK(rows[2].label == "process parameter kappa");
    CHECK(rows[2].subject.empty());
}

TEST_CASE("process_setup_writes_the_container_id_as_the_fx_configs_parent", tags) {
    auto lg(make_logger(test_suite));

    stub_setup_session session(two_class_listing());
    std::ostringstream out;

    synthetic_commands::process_setup(out, session, fx_setup_args());

    // Container, three folders, the FX config and its one GMM component.
    REQUIRE(session.rows.size() == 6);
    const auto container = rfl::json::read<messaging::put_market_data_generation_config_request>(
        session.rows[0].body);
    REQUIRE(container);
    const auto container_id = container->change.write.id;

    const auto config = rfl::json::read<messaging::put_fx_spot_generation_config_request>(
        session.rows[4].body);
    REQUIRE(config);
    CHECK(config->change.write.config_id == container_id);
    CHECK(config->change.write.id != container_id);
    CHECK(config->change.write.config_id != config->change.write.id);
}

TEST_CASE("process_setup_kinds_the_asset_and_instrument_folders", tags) {
    auto lg(make_logger(test_suite));

    stub_setup_session session(two_class_listing());
    std::ostringstream out;

    synthetic_commands::process_setup(out, session, fx_setup_args());

    REQUIRE(session.rows.size() == 6);
    const auto asset = rfl::json::read<messaging::put_folder_request>(session.rows[2].body);
    REQUIRE(asset);
    CHECK(asset->change.write.kind == "asset_class");
    CHECK(asset->change.write.name == "FX");

    const auto instrument = rfl::json::read<messaging::put_folder_request>(session.rows[3].body);
    REQUIRE(instrument);
    CHECK(instrument->change.write.kind == "instrument_type");
    CHECK(instrument->change.write.name == "FX Rates");
}

TEST_CASE("process_setup_writes_the_container_id_and_required_ir_fields", tags) {
    auto lg(make_logger(test_suite));

    stub_setup_session session(ir_setup_listing());
    std::ostringstream out;

    synthetic_commands::process_setup(out,
                                      session,
                                      {"--kind",
                                       "ir",
                                       "--name",
                                       "Basic",
                                       "--source",
                                       "ir_curve.usd.sofr",
                                       "--currency",
                                       "USD",
                                       "--index-family",
                                       "sofr",
                                       "--process",
                                       "VASICEK",
                                       "--param",
                                       "kappa=0.5",
                                       "--param",
                                       "theta=0.05",
                                       "--param",
                                       "sigma=0.01",
                                       "--param",
                                       "initial_rate=0.03",
                                       "--curve-key",
                                       "1Y"});

    // Container, three folders, the IR config, one template entry and four
    // process parameter values.
    REQUIRE(session.rows.size() == 10);
    const auto container = rfl::json::read<messaging::put_market_data_generation_config_request>(
        session.rows[0].body);
    REQUIRE(container);
    const auto container_id = container->change.write.id;

    const auto config = rfl::json::read<messaging::put_ir_curve_generation_config_request>(
        session.rows[4].body);
    REQUIRE(config);
    CHECK(config->change.write.config_id == container_id);
    CHECK(config->change.write.id != container_id);
    CHECK(config->change.write.config_id != config->change.write.id);
    CHECK(config->change.write.price_source == "fixed");
    CHECK(config->change.write.fixed_leg_payment_frequency_code == "Annual");

    const auto ir_asset = rfl::json::read<messaging::put_folder_request>(session.rows[2].body);
    REQUIRE(ir_asset);
    CHECK(ir_asset->change.write.kind == "asset_class");
    CHECK(ir_asset->change.write.name == "IR");
    const auto ir_instrument = rfl::json::read<messaging::put_folder_request>(session.rows[3].body);
    REQUIRE(ir_instrument);
    CHECK(ir_instrument->change.write.kind == "instrument_type");
    CHECK(ir_instrument->change.write.name == "IR Curves");
}

TEST_CASE("process_setup_refuses_a_bad_ticks_per_hour_value", tags) {
    auto lg(make_logger(test_suite));

    stub_setup_session session(two_class_listing());
    std::ostringstream out;

    auto args = fx_setup_args();
    args.push_back("--ticks-per-hour");
    args.push_back("abc");
    synthetic_commands::process_setup(out, session, args);

    CHECK(session.rows.empty());
    CHECK(out.str().find("--ticks-per-hour") != std::string::npos);
    CHECK(out.str().find("whole number") != std::string::npos);
}
