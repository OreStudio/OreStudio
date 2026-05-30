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
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/IInstrumentFormPopulator.hpp"

namespace {

namespace td = ores::trading::domain;

// Phase 1: envelope — rfl silently skips the "instrument" JSON key.
struct response_envelope {
    bool success = false;
    std::string message;
    td::trade trade;
};

// Phase 2: one named wrapper per concrete leaf type.
// Named wrappers are required because C++ does not permit struct definitions
// inside template argument lists (e.g. rfl::json::read<struct{...}> is ill-formed).

// Flat types
struct bond_wrapper      { td::bond_instrument      instrument; };
struct credit_wrapper    { td::credit_instrument    instrument; };
struct commodity_wrapper { td::commodity_instrument instrument; };
struct scripted_wrapper  { td::scripted_instrument  instrument; };

// Composite (with composite legs)
struct composite_wrapper {
    td::composite_instrument             instrument;
    std::vector<td::composite_leg>       legs;
};

// Rates / swap types (with swap legs)
struct fra_wrapper {
    td::fra_instrument               instrument;
    std::vector<td::swap_leg>        legs;
};
struct vanilla_swap_wrapper {
    td::vanilla_swap_instrument      instrument;
    std::vector<td::swap_leg>        legs;
};
struct cap_floor_wrapper {
    td::cap_floor_instrument         instrument;
    std::vector<td::swap_leg>        legs;
};
struct swaption_wrapper {
    td::swaption_instrument          instrument;
    std::vector<td::swap_leg>        legs;
};
struct bgs_wrapper {
    td::balance_guaranteed_swap_instrument  instrument;
    std::vector<td::swap_leg>               legs;
};
struct callable_swap_wrapper {
    td::callable_swap_instrument     instrument;
    std::vector<td::swap_leg>        legs;
};
struct knock_out_swap_wrapper {
    td::knock_out_swap_instrument    instrument;
    std::vector<td::swap_leg>        legs;
};
struct inflation_swap_wrapper {
    td::inflation_swap_instrument    instrument;
    std::vector<td::swap_leg>        legs;
};
struct rpa_wrapper {
    td::rpa_instrument               instrument;
    std::vector<td::swap_leg>        legs;
};

// FX types
struct fx_forward_wrapper        { td::fx_forward_instrument        instrument; };
struct fx_vanilla_option_wrapper { td::fx_vanilla_option_instrument instrument; };
struct fx_barrier_option_wrapper { td::fx_barrier_option_instrument instrument; };
struct fx_digital_option_wrapper { td::fx_digital_option_instrument instrument; };
struct fx_asian_forward_wrapper  { td::fx_asian_forward_instrument  instrument; };
struct fx_accumulator_wrapper    { td::fx_accumulator_instrument    instrument; };
struct fx_variance_swap_wrapper  { td::fx_variance_swap_instrument  instrument; };

// Equity types
struct eq_option_wrapper    { td::equity_option_instrument          instrument; };
struct eq_forward_wrapper   { td::equity_forward_instrument         instrument; };
struct eq_swap_wrapper      { td::equity_swap_instrument            instrument; };
struct eq_var_swap_wrapper  { td::equity_variance_swap_instrument   instrument; };
struct eq_barrier_wrapper   { td::equity_barrier_option_instrument  instrument; };
struct eq_asian_wrapper     { td::equity_asian_option_instrument    instrument; };
struct eq_digital_wrapper   { td::equity_digital_option_instrument  instrument; };
struct eq_accum_wrapper     { td::equity_accumulator_instrument     instrument; };
struct eq_position_wrapper  { td::equity_position_instrument        instrument; };

} // namespace

namespace ores::qt {

using namespace ores::logging;

std::optional<trading::domain::trade>
ClientManager::getTradeInstrument(const std::string& trade_id,
                                  IInstrumentFormPopulator& populator) {
    try {
        trading::messaging::get_trade_instrument_request request;
        request.trade_id = trade_id;
        const auto raw = send_authenticated_request(
            trading::messaging::get_trade_instrument_request::nats_subject,
            rfl::json::write(request),
            std::chrono::seconds(30));

        // Phase 1: parse envelope — no variant, no AddTagsToVariants.
        BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: phase 1 parse";
        auto base = rfl::json::read<response_envelope>(raw);
        if (!base) {
            BOOST_LOG_SEV(lg(), error)
                << "getTradeInstrument: deserialise failed: " << base.error().what();
            return std::nullopt;
        }
        if (!base->success) {
            BOOST_LOG_SEV(lg(), error)
                << "getTradeInstrument: server error: " << base->message;
            return std::nullopt;
        }

        const auto& cls = base->trade.classification;
        const auto  pt  = cls.product_type;
        const auto& ttc = cls.trade_type;

        BOOST_LOG_SEV(lg(), debug)
            << "getTradeInstrument: product_type=" << to_string(pt)
            << " trade_type=" << ttc;

        // Phase 2: dispatch on (product_type, trade_type) — read concrete leaf
        // struct directly, no AddTagsToVariants, no trade_instrument variant.
        using ores::trading::domain::product_type;

#define READ_AND_POPULATE(WrapperType, ...)                                   \
        do {                                                                   \
            auto r = rfl::json::read<WrapperType>(raw);                       \
            if (!r) {                                                          \
                BOOST_LOG_SEV(lg(), error)                                    \
                    << "getTradeInstrument: deserialise failed: "             \
                    << r.error().what();                                       \
                return std::nullopt;                                           \
            }                                                                  \
            populator.populate(__VA_ARGS__);                                   \
        } while (0)

        switch (pt) {
        case product_type::bond:
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading bond_instrument";
            READ_AND_POPULATE(bond_wrapper, r->instrument);
            break;

        case product_type::credit:
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading credit_instrument";
            READ_AND_POPULATE(credit_wrapper, r->instrument);
            break;

        case product_type::commodity:
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading commodity_instrument";
            READ_AND_POPULATE(commodity_wrapper, r->instrument);
            break;

        case product_type::scripted:
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading scripted_instrument";
            READ_AND_POPULATE(scripted_wrapper, r->instrument);
            break;

        case product_type::composite:
            BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading composite_instrument";
            READ_AND_POPULATE(composite_wrapper, r->instrument, std::move(r->legs));
            break;

        case product_type::swap:
            if (ttc == "ForwardRateAgreement") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fra_instrument";
                READ_AND_POPULATE(fra_wrapper, r->instrument, std::move(r->legs));
            } else if (ttc == "Swap" || ttc == "CrossCurrencySwap" || ttc == "FlexiSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading vanilla_swap_instrument";
                READ_AND_POPULATE(vanilla_swap_wrapper, r->instrument, std::move(r->legs));
            } else if (ttc == "CapFloor") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading cap_floor_instrument";
                READ_AND_POPULATE(cap_floor_wrapper, r->instrument, std::move(r->legs));
            } else if (ttc == "Swaption") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading swaption_instrument";
                READ_AND_POPULATE(swaption_wrapper, r->instrument, std::move(r->legs));
            } else if (ttc == "BalanceGuaranteedSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading balance_guaranteed_swap_instrument";
                READ_AND_POPULATE(bgs_wrapper, r->instrument, std::move(r->legs));
            } else if (ttc == "CallableSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading callable_swap_instrument";
                READ_AND_POPULATE(callable_swap_wrapper, r->instrument, std::move(r->legs));
            } else if (ttc == "KnockOutSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading knock_out_swap_instrument";
                READ_AND_POPULATE(knock_out_swap_wrapper, r->instrument, std::move(r->legs));
            } else if (ttc == "InflationSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading inflation_swap_instrument";
                READ_AND_POPULATE(inflation_swap_wrapper, r->instrument, std::move(r->legs));
            } else if (ttc == "RiskParticipationAgreement") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading rpa_instrument";
                READ_AND_POPULATE(rpa_wrapper, r->instrument, std::move(r->legs));
            } else {
                BOOST_LOG_SEV(lg(), warn)
                    << "getTradeInstrument: unknown (product_type, trade_type) — ("
                    << to_string(pt) << ", " << ttc << "); returning monostate";
                return std::nullopt;
            }
            break;

        case product_type::fx:
            if (ttc == "FxForward" || ttc == "FxSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_forward_instrument";
                READ_AND_POPULATE(fx_forward_wrapper, r->instrument);
            } else if (ttc == "FxOption") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_vanilla_option_instrument";
                READ_AND_POPULATE(fx_vanilla_option_wrapper, r->instrument);
            } else if (ttc == "FxBarrierOption" || ttc == "FxGenericBarrierOption"
                       || ttc == "FxDoubleBarrierOption" || ttc == "FxEuropeanBarrierOption"
                       || ttc == "FxKIKOBarrierOption") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_barrier_option_instrument";
                READ_AND_POPULATE(fx_barrier_option_wrapper, r->instrument);
            } else if (ttc == "FxDigitalOption" || ttc == "FxDigitalBarrierOption"
                       || ttc == "FxTouchOption" || ttc == "FxDoubleTouchOption") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_digital_option_instrument";
                READ_AND_POPULATE(fx_digital_option_wrapper, r->instrument);
            } else if (ttc == "FxAverageForward" || ttc == "FxTaRF") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_asian_forward_instrument";
                READ_AND_POPULATE(fx_asian_forward_wrapper, r->instrument);
            } else if (ttc == "FxAccumulator") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_accumulator_instrument";
                READ_AND_POPULATE(fx_accumulator_wrapper, r->instrument);
            } else if (ttc == "FxVarianceSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading fx_variance_swap_instrument";
                READ_AND_POPULATE(fx_variance_swap_wrapper, r->instrument);
            } else {
                BOOST_LOG_SEV(lg(), warn)
                    << "getTradeInstrument: unknown (product_type, trade_type) — ("
                    << to_string(pt) << ", " << ttc << "); returning monostate";
                return std::nullopt;
            }
            break;

        case product_type::equity:
            if (ttc == "EquityOption" || ttc == "EquityCliquetOption"
                || ttc == "EquityOutperformanceOption") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_option_instrument";
                READ_AND_POPULATE(eq_option_wrapper, r->instrument);
            } else if (ttc == "EquityForward") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_forward_instrument";
                READ_AND_POPULATE(eq_forward_wrapper, r->instrument);
            } else if (ttc == "EquitySwap" || ttc == "EquityWorstOfBasketSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_swap_instrument";
                READ_AND_POPULATE(eq_swap_wrapper, r->instrument);
            } else if (ttc == "EquityVarianceSwap") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_variance_swap_instrument";
                READ_AND_POPULATE(eq_var_swap_wrapper, r->instrument);
            } else if (ttc == "EquityBarrierOption" || ttc == "EquityDoubleBarrierOption"
                       || ttc == "EquityEuropeanBarrierOption") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_barrier_option_instrument";
                READ_AND_POPULATE(eq_barrier_wrapper, r->instrument);
            } else if (ttc == "EquityAsianOption") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_asian_option_instrument";
                READ_AND_POPULATE(eq_asian_wrapper, r->instrument);
            } else if (ttc == "EquityDigitalOption" || ttc == "EquityTouchOption") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_digital_option_instrument";
                READ_AND_POPULATE(eq_digital_wrapper, r->instrument);
            } else if (ttc == "EquityAccumulator" || ttc == "EquityTaRF") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_accumulator_instrument";
                READ_AND_POPULATE(eq_accum_wrapper, r->instrument);
            } else if (ttc == "EquityPosition") {
                BOOST_LOG_SEV(lg(), debug) << "getTradeInstrument: reading equity_position_instrument";
                READ_AND_POPULATE(eq_position_wrapper, r->instrument);
            } else {
                BOOST_LOG_SEV(lg(), warn)
                    << "getTradeInstrument: unknown (product_type, trade_type) — ("
                    << to_string(pt) << ", " << ttc << "); returning monostate";
                return std::nullopt;
            }
            break;

        case product_type::unknown:
        default:
            BOOST_LOG_SEV(lg(), warn)
                << "getTradeInstrument: unknown (product_type, trade_type) — ("
                << to_string(pt) << ", " << ttc << "); returning monostate";
            return std::nullopt;
        }

#undef READ_AND_POPULATE

        return std::move(base->trade);

    } catch (const ores::nats::service::nats_connect_error&) {
        throw;
    } catch (const ores::nats::service::session_expired_error& e) {
        BOOST_LOG_SEV(lg(), warn) << "Session expired: " << e.what();
        QMetaObject::invokeMethod(this, [this] { emit sessionExpired(); }, Qt::QueuedConnection);
        return std::nullopt;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "getTradeInstrument failed: " << e.what();
        return std::nullopt;
    }
}

} // namespace ores::qt
