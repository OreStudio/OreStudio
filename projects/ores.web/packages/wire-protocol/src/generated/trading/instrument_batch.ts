/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_trade_type_batch.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { BondInstrumentData } from './domain/bond_instrument_data.js';
import type { CommodityInstrument } from './domain/commodity_instrument.js';
import type { CompositeInstrument } from './domain/composite_instrument.js';
import type { CreditInstrument } from './domain/credit_instrument.js';
import type { EquityAccumulatorInstrument } from './domain/equity_accumulator_instrument.js';
import type { EquityAsianOptionInstrument } from './domain/equity_asian_option_instrument.js';
import type { EquityBarrierOptionInstrument } from './domain/equity_barrier_option_instrument.js';
import type { EquityDigitalOptionInstrument } from './domain/equity_digital_option_instrument.js';
import type { EquityForwardInstrument } from './domain/equity_forward_instrument.js';
import type { EquityOptionInstrument } from './domain/equity_option_instrument.js';
import type { EquityPositionInstrument } from './domain/equity_position_instrument.js';
import type { EquitySwapInstrument } from './domain/equity_swap_instrument.js';
import type { EquityVarianceSwapInstrument } from './domain/equity_variance_swap_instrument.js';
import type { FxAccumulatorInstrument } from './domain/fx_accumulator_instrument.js';
import type { FxAsianForwardInstrument } from './domain/fx_asian_forward_instrument.js';
import type { FxBarrierOptionInstrument } from './domain/fx_barrier_option_instrument.js';
import type { FxDigitalOptionInstrument } from './domain/fx_digital_option_instrument.js';
import type { FxForwardInstrument } from './domain/fx_forward_instrument.js';
import type { FxVanillaOptionInstrument } from './domain/fx_vanilla_option_instrument.js';
import type { FxVarianceSwapInstrument } from './domain/fx_variance_swap_instrument.js';
import type { RateInstrument } from './domain/rate_instrument.js';
import type { ScriptedInstrument } from './domain/scripted_instrument.js';
import type { BalanceGuaranteedSwapInstrument } from './domain/balance_guaranteed_swap_instrument.js';
import type { CallableSwapCallDate } from './domain/callable_swap_call_date.js';
import type { CallableSwapInstrument } from './domain/callable_swap_instrument.js';
import type { CapFloorInstrument } from './domain/cap_floor_instrument.js';
import type { CommodityBasketConstituent } from './domain/commodity_basket_constituent.js';
import type { CompositeLeg } from './domain/composite_leg.js';
import type { EquityPositionOptionUnderlying } from './domain/equity_position_option_underlying.js';
import type { FraInstrument } from './domain/fra_instrument.js';
import type { InflationSwapInstrument } from './domain/inflation_swap_instrument.js';
import type { InstrumentOption } from './domain/instrument_option.js';
import type { InstrumentOptionExerciseFee } from './domain/instrument_option_exercise_fee.js';
import type { InstrumentOptionExercisePrice } from './domain/instrument_option_exercise_price.js';
import type { InstrumentOptionPaymentDate } from './domain/instrument_option_payment_date.js';
import type { InstrumentOptionPremium } from './domain/instrument_option_premium.js';
import type { InstrumentSchedule } from './domain/instrument_schedule.js';
import type { InstrumentScheduleDate } from './domain/instrument_schedule_date.js';
import type { InstrumentStrike } from './domain/instrument_strike.js';
import type { KnockOutSwapInstrument } from './domain/knock_out_swap_instrument.js';
import type { SwapLeg } from './domain/swap_leg.js';
import type { SwapLegAmount } from './domain/swap_leg_amount.js';
import type { SwapLegRate } from './domain/swap_leg_rate.js';
import type { SwaptionInstrument } from './domain/swaption_instrument.js';
import type { VanillaSwapInstrument } from './domain/vanilla_swap_instrument.js';

/**
 * Every instrument and instrument child an export carries, one typed array per
 * entity.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Each element carries the trade id it belongs to, so a client joins an
 * array to its trade by that id, and the array states the element's type.
 */
export interface InstrumentBatch {
    bond_instruments: BondInstrumentData[];
    commodity_instruments: CommodityInstrument[];
    composite_instruments: CompositeInstrument[];
    credit_instruments: CreditInstrument[];
    equity_accumulator_instruments: EquityAccumulatorInstrument[];
    equity_asian_option_instruments: EquityAsianOptionInstrument[];
    equity_barrier_option_instruments: EquityBarrierOptionInstrument[];
    equity_digital_option_instruments: EquityDigitalOptionInstrument[];
    equity_forward_instruments: EquityForwardInstrument[];
    equity_option_instruments: EquityOptionInstrument[];
    equity_position_instruments: EquityPositionInstrument[];
    equity_swap_instruments: EquitySwapInstrument[];
    equity_variance_swap_instruments: EquityVarianceSwapInstrument[];
    fx_accumulator_instruments: FxAccumulatorInstrument[];
    fx_asian_forward_instruments: FxAsianForwardInstrument[];
    fx_barrier_option_instruments: FxBarrierOptionInstrument[];
    fx_digital_option_instruments: FxDigitalOptionInstrument[];
    fx_forward_instruments: FxForwardInstrument[];
    fx_vanilla_option_instruments: FxVanillaOptionInstrument[];
    fx_variance_swap_instruments: FxVarianceSwapInstrument[];
    rate_instruments: RateInstrument[];
    scripted_instruments: ScriptedInstrument[];
    balance_guaranteed_swap_instruments: BalanceGuaranteedSwapInstrument[];
    callable_swap_call_dates: CallableSwapCallDate[];
    callable_swap_instruments: CallableSwapInstrument[];
    cap_floor_instruments: CapFloorInstrument[];
    commodity_basket_constituents: CommodityBasketConstituent[];
    composite_legs: CompositeLeg[];
    equity_position_option_underlyings: EquityPositionOptionUnderlying[];
    fra_instruments: FraInstrument[];
    inflation_swap_instruments: InflationSwapInstrument[];
    instrument_options: InstrumentOption[];
    instrument_option_exercise_fees: InstrumentOptionExerciseFee[];
    instrument_option_exercise_prices: InstrumentOptionExercisePrice[];
    instrument_option_payment_dates: InstrumentOptionPaymentDate[];
    instrument_option_premiums: InstrumentOptionPremium[];
    instrument_schedules: InstrumentSchedule[];
    instrument_schedule_dates: InstrumentScheduleDate[];
    instrument_strikes: InstrumentStrike[];
    knock_out_swap_instruments: KnockOutSwapInstrument[];
    swap_legs: SwapLeg[];
    swap_leg_amounts: SwapLegAmount[];
    swap_leg_rates: SwapLegRate[];
    swaption_instruments: SwaptionInstrument[];
    vanilla_swap_instruments: VanillaSwapInstrument[];
}
