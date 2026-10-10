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
#ifndef ORES_TRADING_CORE_REPOSITORY_TRADE_WRITE_OBSERVATION_HPP
#define ORES_TRADING_CORE_REPOSITORY_TRADE_WRITE_OBSERVATION_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/entity_write_observer.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/structure_member_entity.hpp"
#include "ores.trading.core/repository/trade_additional_field_entity.hpp"
#include "ores.trading.core/repository/trade_booking_entity.hpp"
#include "ores.trading.core/repository/trade_identifier_entity.hpp"
#include "ores.trading.core/repository/trade_party_role_entity.hpp"
#include "ores.trading.core/repository/balance_guaranteed_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/bond_instrument_entity.hpp"
#include "ores.trading.core/repository/callable_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/cap_floor_instrument_entity.hpp"
#include "ores.trading.core/repository/commodity_instrument_entity.hpp"
#include "ores.trading.core/repository/composite_instrument_entity.hpp"
#include "ores.trading.core/repository/credit_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_accumulator_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_asian_option_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_barrier_option_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_digital_option_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_forward_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_option_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_position_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_variance_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/fra_instrument_entity.hpp"
#include "ores.trading.core/repository/fx_accumulator_instrument_entity.hpp"
#include "ores.trading.core/repository/fx_asian_forward_instrument_entity.hpp"
#include "ores.trading.core/repository/fx_barrier_option_instrument_entity.hpp"
#include "ores.trading.core/repository/fx_digital_option_instrument_entity.hpp"
#include "ores.trading.core/repository/fx_forward_instrument_entity.hpp"
#include "ores.trading.core/repository/fx_vanilla_option_instrument_entity.hpp"
#include "ores.trading.core/repository/fx_variance_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/inflation_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/knock_out_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/scripted_instrument_entity.hpp"
#include "ores.trading.core/repository/swaption_instrument_entity.hpp"
#include "ores.trading.core/repository/vanilla_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/trade_portfolio_entity.hpp"
#include "ores.trading.core/repository/trade_state_entity.hpp"
#include <string>

namespace ores::trading::service {

using context = ores::database::context;

/**
 * @brief Recomputes a trade's economic digest from its own economic fields
 * and its components, and stores it when it differs.
 *
 * A trade with no stored digest yet counts as changed, so the first component
 * write states the trade's first digest. An amendment that leaves the digest
 * alone writes no trade row, so a null amend never changes the stored value.
 *
 * The external version is deliberately NOT moved here. "The digest changed" is
 * the right test for a null amend and the wrong test for an operation that is
 * not an agreement: a component write cannot tell an agreement from a step of
 * an import, so moving the version needs the customer-visible boundary as well
 * as the digest. This seam cannot ask that question, so it does not count
 * writes as agreements.
 *
 * The component set is provisional and is not yet the economic set. See the
 * fold in trade_economic_digest_writer.cpp.
 *
 * The caller must pass a context bound to the transaction of the component
 * write, so the read-back and the trade write are one unit with it.
 *
 * @param ctx The context bound to the component write's transaction.
 * @param trade_id The trade whose digest to refresh, as text.
 * @param lg The logger to use.
 */
ORES_TRADING_CORE_EXPORT void refresh_trade_economic_digest(context ctx,
                                                            const std::string& trade_id,
                                                            const std::string& change_reason_code,
                                                            ores::logging::logger_t& lg);

}

namespace ores::database::repository {

/**
 * @brief The seven components a trade is keyed by, each observed on write.
 *
 * These are the entity types whose models carry @c :parent_entity: trade, so
 * a write of any of them refreshes the trade's fold. The trade's own entity is
 * not observed: the fold writes the trade, and observing the trade would
 * recurse.
 */
/**@{*/


template <>
struct entity_write_observer<ores::trading::repository::trade_identifier_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::trade_identifier_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};



template <>
struct entity_write_observer<ores::trading::repository::trade_party_role_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::trade_party_role_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

/*
 * Every instrument family a trade type routes to. A write of any of these is a
 * change to what the customer agreed to, so it refreshes the trade fold.
 */
/**@{*/
template <>
struct entity_write_observer<ores::trading::repository::balance_guaranteed_swap_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::balance_guaranteed_swap_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::bond_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::bond_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::callable_swap_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::callable_swap_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::cap_floor_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::cap_floor_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::commodity_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::commodity_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::composite_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::composite_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::credit_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::credit_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_accumulator_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_accumulator_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_asian_option_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_asian_option_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_barrier_option_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_barrier_option_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_digital_option_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_digital_option_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_forward_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_forward_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_option_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_option_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_position_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_position_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_swap_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_swap_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::equity_variance_swap_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::equity_variance_swap_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::fra_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::fra_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::fx_accumulator_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::fx_accumulator_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::fx_asian_forward_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::fx_asian_forward_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::fx_barrier_option_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::fx_barrier_option_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::fx_digital_option_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::fx_digital_option_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::fx_forward_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::fx_forward_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::fx_vanilla_option_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::fx_vanilla_option_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::fx_variance_swap_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::fx_variance_swap_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::inflation_swap_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::inflation_swap_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::knock_out_swap_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::knock_out_swap_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::scripted_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::scripted_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::swaption_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::swaption_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

template <>
struct entity_write_observer<ores::trading::repository::vanilla_swap_instrument_entity> {
    static constexpr bool observes = true;
    static void observe(context ctx,
                        const ores::trading::repository::vanilla_swap_instrument_entity& entity,
                        ores::logging::logger_t& lg) {
        ores::trading::service::refresh_trade_economic_digest(
            ctx, entity.trade_id.value(), entity.change_reason_code, lg);
    }
};

/**@}*/


}

#endif
