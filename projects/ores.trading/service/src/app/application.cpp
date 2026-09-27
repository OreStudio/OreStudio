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
#include "ores.trading.service/app/application.hpp"
#include "ores.database/service/context_factory.hpp"
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.eventing.core/service/registrar.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/service/domain_service_runner.hpp"
#include "ores.service/service/heartbeat_publisher.hpp"
#include "ores.trading.core/messaging/registrar.hpp"
#include "ores.trading.service/app/application_exception.hpp"
#include "ores.trading.service/messaging/activity_type_event_registrar.hpp"
#include "ores.trading.service/messaging/ascot_event_registrar.hpp"
#include "ores.trading.service/messaging/balance_guaranteed_swap_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_forward_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_future_delivery_basket_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_future_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_issue_call_date_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_issue_conversion_target_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_issue_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_leg_amortization_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_leg_amount_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_leg_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_leg_rate_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_option_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_repo_event_registrar.hpp"
#include "ores.trading.service/messaging/bond_trs_event_registrar.hpp"
#include "ores.trading.service/messaging/callable_swap_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/cap_floor_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/commodity_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/composite_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/credit_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_accumulator_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_asian_option_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_barrier_option_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_digital_option_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_forward_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_option_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_position_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_swap_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/equity_variance_swap_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/fpml_event_type_event_registrar.hpp"
#include "ores.trading.service/messaging/fra_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/fx_accumulator_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/fx_asian_forward_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/fx_barrier_option_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/fx_digital_option_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/fx_forward_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/fx_vanilla_option_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/fx_variance_swap_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/inflation_swap_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/instrument_option_event_registrar.hpp"
#include "ores.trading.service/messaging/instrument_option_exercise_fee_event_registrar.hpp"
#include "ores.trading.service/messaging/instrument_option_payment_date_event_registrar.hpp"
#include "ores.trading.service/messaging/instrument_option_premium_event_registrar.hpp"
#include "ores.trading.service/messaging/instrument_schedule_date_event_registrar.hpp"
#include "ores.trading.service/messaging/instrument_schedule_event_registrar.hpp"
#include "ores.trading.service/messaging/instrument_strike_event_registrar.hpp"
#include "ores.trading.service/messaging/knock_out_swap_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/lifecycle_event_event_registrar.hpp"
#include "ores.trading.service/messaging/party_role_type_event_registrar.hpp"
#include "ores.trading.service/messaging/rpa_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/scripted_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/swap_leg_event_registrar.hpp"
#include "ores.trading.service/messaging/composite_leg_event_registrar.hpp"
#include "ores.trading.service/messaging/swaption_instrument_event_registrar.hpp"
#include "ores.trading.service/messaging/trade_envelope_additional_field_event_registrar.hpp"
#include "ores.trading.service/messaging/trade_envelope_event_registrar.hpp"
#include "ores.trading.service/messaging/trade_envelope_portfolio_id_event_registrar.hpp"
#include "ores.trading.service/messaging/trade_event_registrar.hpp"
#include "ores.trading.service/messaging/trade_id_type_event_registrar.hpp"
#include "ores.trading.service/messaging/trade_identifier_event_registrar.hpp"
#include "ores.trading.service/messaging/trade_party_role_event_registrar.hpp"
#include "ores.trading.service/messaging/trade_type_event_registrar.hpp"
#include "ores.trading.service/messaging/vanilla_swap_instrument_event_registrar.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/version/version.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>

namespace ores::trading::service::app {

using namespace ores::logging;
namespace ev = ores::eventing;

namespace {

constexpr std::string_view service_name = "ores.trading.service";
constexpr std::string_view service_version = ORES_VERSION;

} // namespace

ores::database::context application::make_context(const ores::database::database_options& db_opts) {
    using ores::database::context_factory;

    context_factory::configuration cfg{.database_options = db_opts,
                                       .pool_size = static_cast<std::size_t>(db_opts.pool_size),
                                       .num_attempts = 10,
                                       .wait_time_in_seconds = 1,
                                       .service_account = db_opts.user};

    return context_factory::make_context(cfg);
}

application::application() = default;

boost::asio::awaitable<void> application::run(boost::asio::io_context& io_ctx,
                                              const config::options& cfg) const {

    BOOST_LOG_SEV(lg(), info) << ores::utility::version::format_startup_message(
        "ores.trading.service", 0, 1);

    ores::nats::service::client nats(cfg.nats);
    nats.connect();

    // =========================================================================
    // Entity change event pipeline: PostgreSQL LISTEN/NOTIFY → NATS publish
    // =========================================================================
    ev::service::event_bus event_bus;
    ev::service::postgres_event_source event_source(make_context(cfg.database), event_bus);

    auto trade_sub = ores::trading::service::messaging::register_trade_event_mapping(
        event_source, event_bus, nats);

    auto equity_position_instrument_sub =
        ores::trading::service::messaging::register_equity_position_instrument_event_mapping(
            event_source, event_bus, nats);
    auto equity_variance_swap_instrument_sub =
        ores::trading::service::messaging::register_equity_variance_swap_instrument_event_mapping(
            event_source, event_bus, nats);
    auto fx_forward_instrument_sub =
        ores::trading::service::messaging::register_fx_forward_instrument_event_mapping(
            event_source, event_bus, nats);
    auto equity_forward_instrument_sub =
        ores::trading::service::messaging::register_equity_forward_instrument_event_mapping(
            event_source, event_bus, nats);
    auto fx_accumulator_instrument_sub =
        ores::trading::service::messaging::register_fx_accumulator_instrument_event_mapping(
            event_source, event_bus, nats);
    auto fx_vanilla_option_instrument_sub =
        ores::trading::service::messaging::register_fx_vanilla_option_instrument_event_mapping(
            event_source, event_bus, nats);
    auto equity_accumulator_instrument_sub =
        ores::trading::service::messaging::register_equity_accumulator_instrument_event_mapping(
            event_source, event_bus, nats);
    auto equity_asian_option_instrument_sub =
        ores::trading::service::messaging::register_equity_asian_option_instrument_event_mapping(
            event_source, event_bus, nats);
    auto equity_barrier_option_instrument_sub =
        ores::trading::service::messaging::register_equity_barrier_option_instrument_event_mapping(
            event_source, event_bus, nats);
    auto equity_digital_option_instrument_sub =
        ores::trading::service::messaging::register_equity_digital_option_instrument_event_mapping(
            event_source, event_bus, nats);
    auto equity_option_instrument_sub =
        ores::trading::service::messaging::register_equity_option_instrument_event_mapping(
            event_source, event_bus, nats);
    auto equity_swap_instrument_sub =
        ores::trading::service::messaging::register_equity_swap_instrument_event_mapping(
            event_source, event_bus, nats);
    auto fx_asian_forward_instrument_sub =
        ores::trading::service::messaging::register_fx_asian_forward_instrument_event_mapping(
            event_source, event_bus, nats);
    auto fx_barrier_option_instrument_sub =
        ores::trading::service::messaging::register_fx_barrier_option_instrument_event_mapping(
            event_source, event_bus, nats);
    auto fx_digital_option_instrument_sub =
        ores::trading::service::messaging::register_fx_digital_option_instrument_event_mapping(
            event_source, event_bus, nats);
    auto fx_variance_swap_instrument_sub =
        ores::trading::service::messaging::register_fx_variance_swap_instrument_event_mapping(
            event_source, event_bus, nats);
    auto activity_type_sub =
        ores::trading::service::messaging::register_activity_type_event_mapping(
            event_source, event_bus, nats);
    auto fpml_event_type_sub =
        ores::trading::service::messaging::register_fpml_event_type_event_mapping(
            event_source, event_bus, nats);
    auto party_role_type_sub =
        ores::trading::service::messaging::register_party_role_type_event_mapping(
            event_source, event_bus, nats);
    auto trade_id_type_sub =
        ores::trading::service::messaging::register_trade_id_type_event_mapping(
            event_source, event_bus, nats);
    auto trade_type_sub = ores::trading::service::messaging::register_trade_type_event_mapping(
        event_source, event_bus, nats);
    auto lifecycle_event_sub =
        ores::trading::service::messaging::register_lifecycle_event_event_mapping(
            event_source, event_bus, nats);
    auto trade_identifier_sub =
        ores::trading::service::messaging::register_trade_identifier_event_mapping(
            event_source, event_bus, nats);
    auto trade_party_role_sub =
        ores::trading::service::messaging::register_trade_party_role_event_mapping(
            event_source, event_bus, nats);
    auto balance_guaranteed_swap_instrument_sub = ores::trading::service::messaging::
        register_balance_guaranteed_swap_instrument_event_mapping(event_source, event_bus, nats);
    auto callable_swap_instrument_sub =
        ores::trading::service::messaging::register_callable_swap_instrument_event_mapping(
            event_source, event_bus, nats);
    auto cap_floor_instrument_sub =
        ores::trading::service::messaging::register_cap_floor_instrument_event_mapping(
            event_source, event_bus, nats);
    auto fra_instrument_sub =
        ores::trading::service::messaging::register_fra_instrument_event_mapping(
            event_source, event_bus, nats);
    auto inflation_swap_instrument_sub =
        ores::trading::service::messaging::register_inflation_swap_instrument_event_mapping(
            event_source, event_bus, nats);
    auto knock_out_swap_instrument_sub =
        ores::trading::service::messaging::register_knock_out_swap_instrument_event_mapping(
            event_source, event_bus, nats);
    auto rpa_instrument_sub =
        ores::trading::service::messaging::register_rpa_instrument_event_mapping(
            event_source, event_bus, nats);
    auto swaption_instrument_sub =
        ores::trading::service::messaging::register_swaption_instrument_event_mapping(
            event_source, event_bus, nats);
    auto vanilla_swap_instrument_sub =
        ores::trading::service::messaging::register_vanilla_swap_instrument_event_mapping(
            event_source, event_bus, nats);
    auto bond_instrument_sub =
        ores::trading::service::messaging::register_bond_instrument_event_mapping(
            event_source, event_bus, nats);
    auto commodity_instrument_sub =
        ores::trading::service::messaging::register_commodity_instrument_event_mapping(
            event_source, event_bus, nats);
    auto composite_instrument_sub =
        ores::trading::service::messaging::register_composite_instrument_event_mapping(
            event_source, event_bus, nats);
    auto credit_instrument_sub =
        ores::trading::service::messaging::register_credit_instrument_event_mapping(
            event_source, event_bus, nats);
    auto scripted_instrument_sub =
        ores::trading::service::messaging::register_scripted_instrument_event_mapping(
            event_source, event_bus, nats);
    auto swap_leg_sub = ores::trading::service::messaging::register_swap_leg_event_mapping(
        event_source, event_bus, nats);
    auto composite_leg_sub =
        ores::trading::service::messaging::register_composite_leg_event_mapping(
            event_source, event_bus, nats);

    auto ascot_sub = ores::trading::service::messaging::register_ascot_event_mapping(
        event_source, event_bus, nats);
    auto bond_forward_sub = ores::trading::service::messaging::register_bond_forward_event_mapping(
        event_source, event_bus, nats);
    auto bond_future_delivery_basket_sub =
        ores::trading::service::messaging::register_bond_future_delivery_basket_event_mapping(
            event_source, event_bus, nats);
    auto bond_future_sub = ores::trading::service::messaging::register_bond_future_event_mapping(
        event_source, event_bus, nats);
    auto bond_issue_call_date_sub =
        ores::trading::service::messaging::register_bond_issue_call_date_event_mapping(
            event_source, event_bus, nats);
    auto bond_issue_conversion_target_sub =
        ores::trading::service::messaging::register_bond_issue_conversion_target_event_mapping(
            event_source, event_bus, nats);
    auto bond_issue_sub = ores::trading::service::messaging::register_bond_issue_event_mapping(
        event_source, event_bus, nats);
    auto bond_leg_amortization_sub =
        ores::trading::service::messaging::register_bond_leg_amortization_event_mapping(
            event_source, event_bus, nats);
    auto bond_leg_amount_sub =
        ores::trading::service::messaging::register_bond_leg_amount_event_mapping(
            event_source, event_bus, nats);
    auto bond_leg_sub = ores::trading::service::messaging::register_bond_leg_event_mapping(
        event_source, event_bus, nats);
    auto bond_leg_rate_sub =
        ores::trading::service::messaging::register_bond_leg_rate_event_mapping(
            event_source, event_bus, nats);
    auto bond_option_sub = ores::trading::service::messaging::register_bond_option_event_mapping(
        event_source, event_bus, nats);
    auto bond_repo_sub = ores::trading::service::messaging::register_bond_repo_event_mapping(
        event_source, event_bus, nats);
    auto bond_trs_sub = ores::trading::service::messaging::register_bond_trs_event_mapping(
        event_source, event_bus, nats);
    auto instrument_option_sub =
        ores::trading::service::messaging::register_instrument_option_event_mapping(
            event_source, event_bus, nats);
    auto instrument_option_exercise_fee_sub =
        ores::trading::service::messaging::register_instrument_option_exercise_fee_event_mapping(
            event_source, event_bus, nats);
    auto instrument_option_payment_date_sub =
        ores::trading::service::messaging::register_instrument_option_payment_date_event_mapping(
            event_source, event_bus, nats);
    auto instrument_option_premium_sub =
        ores::trading::service::messaging::register_instrument_option_premium_event_mapping(
            event_source, event_bus, nats);
    auto instrument_schedule_date_sub =
        ores::trading::service::messaging::register_instrument_schedule_date_event_mapping(
            event_source, event_bus, nats);
    auto instrument_schedule_sub =
        ores::trading::service::messaging::register_instrument_schedule_event_mapping(
            event_source, event_bus, nats);
    auto instrument_strike_sub =
        ores::trading::service::messaging::register_instrument_strike_event_mapping(
            event_source, event_bus, nats);
    auto trade_envelope_additional_field_sub =
        ores::trading::service::messaging::register_trade_envelope_additional_field_event_mapping(
            event_source, event_bus, nats);
    auto trade_envelope_sub =
        ores::trading::service::messaging::register_trade_envelope_event_mapping(
            event_source, event_bus, nats);
    auto trade_envelope_portfolio_id_sub =
        ores::trading::service::messaging::register_trade_envelope_portfolio_id_event_mapping(
            event_source, event_bus, nats);

    event_source.start();
    BOOST_LOG_SEV(lg(), info) << "Entity change event pipeline started.";

    co_await ores::service::service::run(
        io_ctx,
        nats,
        make_context(cfg.database),
        "ores.trading.service",
        [&cfg](auto& n, auto c, auto v) {
            return ores::trading::messaging::registrar::register_handlers(
                n, std::move(c), std::move(v), cfg.http_base_url);
        },
        [&nats](boost::asio::io_context& ioc) {
            auto hb = std::make_shared<ores::service::service::heartbeat_publisher>(
                std::string(service_name), std::string(service_version), nats);
            boost::asio::co_spawn(ioc, [hb]() { return hb->run(); }, boost::asio::detached);
        });

    event_source.stop();
    co_return;
}

}
