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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: oresmd_projections.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include "ores.marketdata.core/oresmd/detail/oresmd_string_utils.hpp"
#include <algorithm>
#include <cctype>
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <sstream>
#include <variant>
#include <vector>

namespace {

using namespace ores::marketdata::domain;
using ores::marketdata::core::detail::to_lower;
using ores::marketdata::core::detail::to_upper;

/*
 * ORE spelling tables. The first segment of a quote key is the ORE series type, the
 * second is the metric; the swaption family also carries a volatility model. These
 * constants are the single source of truth for those spellings: the forward
 * ore_type()/ore_metric()/ore_vol_model() tables and the inverse dispatcher below both
 * reference them, so a rename stays in sync in both directions.
 */
namespace ore_type_spec {
constexpr std::string_view fx{"FX"};
constexpr std::string_view fxfwd{"FXFWD"};
constexpr std::string_view fx_option{"FX_OPTION"};
constexpr std::string_view ir_swap{"IR_SWAP"};
constexpr std::string_view discount{"DISCOUNT"};
constexpr std::string_view mm{"MM"};
constexpr std::string_view fra{"FRA"};
constexpr std::string_view imm_fra{"IMM_FRA"};
constexpr std::string_view basis_swap{"BASIS_SWAP"};
constexpr std::string_view bma_swap{"BMA_SWAP"};
constexpr std::string_view cc_basis_swap{"CC_BASIS_SWAP"};
constexpr std::string_view cc_fix_float_swap{"CC_FIX_FLOAT_SWAP"};
constexpr std::string_view zero{"ZERO"};
constexpr std::string_view mm_future{"MM_FUTURE"};
constexpr std::string_view oi_future{"OI_FUTURE"};
constexpr std::string_view swaption{"SWAPTION"};
constexpr std::string_view capfloor{"CAPFLOOR"};
constexpr std::string_view bond_option{"BOND_OPTION"};
constexpr std::string_view equity{"EQUITY"};
constexpr std::string_view equity_fwd{"EQUITY_FWD"};
constexpr std::string_view equity_dividend{"EQUITY_DIVIDEND"};
constexpr std::string_view equity_option{"EQUITY_OPTION"};
constexpr std::string_view commodity{"COMMODITY"};
constexpr std::string_view commodity_fwd{"COMMODITY_FWD"};
constexpr std::string_view commodity_option{"COMMODITY_OPTION"};
constexpr std::string_view cpr{"CPR"};
constexpr std::string_view cds{"CDS"};
constexpr std::string_view hazard_rate{"HAZARD_RATE"};
constexpr std::string_view recovery_rate{"RECOVERY_RATE"};
constexpr std::string_view cds_index{"CDS_INDEX"};
constexpr std::string_view index_cds_tranche{"INDEX_CDS_TRANCHE"};
constexpr std::string_view index_cds_option{"INDEX_CDS_OPTION"};
constexpr std::string_view bond{"BOND"};
constexpr std::string_view shape_profile{"SHAPE_PROFILE"};
constexpr std::string_view rating{"RATING"};
constexpr std::string_view zc_inflation_swap{"ZC_INFLATIONSWAP"};
constexpr std::string_view yy_inflation_swap{"YY_INFLATIONSWAP"};
constexpr std::string_view zc_inflation_capfloor{"ZC_INFLATIONCAPFLOOR"};
constexpr std::string_view yy_inflation_capfloor{"YY_INFLATIONCAPFLOOR"};
constexpr std::string_view seasonality{"SEASONALITY"};
constexpr std::string_view correlation{"CORRELATION"};
} // namespace ore_type_spec

namespace ore_metric_spec {
constexpr std::string_view rate{"RATE"};
constexpr std::string_view price{"PRICE"};
constexpr std::string_view basis_spread{"BASIS_SPREAD"};
constexpr std::string_view ratio{"RATIO"};
constexpr std::string_view yield_spread{"YIELD_SPREAD"};
constexpr std::string_view credit_spread{"CREDIT_SPREAD"};
constexpr std::string_view base_correlation{"BASE_CORRELATION"};
constexpr std::string_view conversion_factor{"CONVERSION_FACTOR"};
constexpr std::string_view shape_factor{"SHAPE_FACTOR"};
constexpr std::string_view transition_probability{"TRANSITION_PROBABILITY"};
} // namespace ore_metric_spec

namespace ore_vol_spec {
constexpr std::string_view rate_lnvol{"RATE_LNVOL"};
constexpr std::string_view rate_nvol{"RATE_NVOL"};
constexpr std::string_view rate_slnvol{"RATE_SLNVOL"};
constexpr std::string_view shift{"SHIFT"};
constexpr std::string_view price{"PRICE"};
} // namespace ore_vol_spec

std::vector<std::string> split_point(const std::string& point) {
    std::vector<std::string> parts;
    std::stringstream ss(point);
    std::string part;
    while (std::getline(ss, part, ','))
        parts.push_back(to_upper(part));
    return parts;
}

std::string curve_id(std::string_view ccy, std::string_view tenor) {
    return std::format("{}{}", ccy, to_upper(tenor));
}

/*
 * The index segment of an ORE key is a name, and the identifier carries it in one
 * of two ways. Producers spell one family more than one way -- the corpus writes
 * ESTER where index_family says estr, on 2,514 IR_SWAP keys and 70 MM keys -- and
 * a spelling that differs from the family's own uppercase name is carried so the
 * key reads back as it arrived. Some index tokens have no family at all: a basis
 * swap names the basis itself (SOFR_FedFunds, LIBOR_PRIME) and there is nothing
 * to resolve it to. Both cases record the token in index_spelling, and the
 * ordinary spelling records nothing, which keeps one key to one URI -- the same
 * rule the IR swap's spot lag follows.
 */
std::string index_token(const ir_market_data_identifier& id) {
    if (id.index_spelling)
        return *id.index_spelling;
    return id.index ? to_upper(std::string(magic_enum::enum_name(*id.index))) : std::string{};
}

void record_index_spelling(ir_market_data_identifier& id, std::string_view token) {
    if (!id.index)
        return;
    if (to_lower(token) == std::string(magic_enum::enum_name(*id.index)))
        return;
    id.index_spelling = std::string{token};
}

/*
 * The index-name space keeps a spelling that the ORE-key space discards.
 *
 * An ORE key segment is uppercase, so a token that differs from the family's name
 * only in case is that name written loudly, and recording it would hand one family
 * two URIs. An index name is the corpus's own text, where "USD-FedFunds" is how ORE
 * writes the family: the name has to come back exactly as it arrived, so only the
 * family's uppercase name is the ordinary spelling here.
 */
void record_index_name_spelling(ir_market_data_identifier& id, std::string_view token) {
    if (!id.index)
        return;
    if (token == to_upper(std::string(magic_enum::enum_name(*id.index))))
        return;
    id.index_spelling = std::string{token};
}

/*
 * Both index name and curve key are produced together, gated on `type=fixing` -- the
 * design doc's own worked-examples table shows a single `type=fixing` URI producing
 * BOTH an index name and a curve key (the underlying curve construct has both facets),
 * and reads "--" for both columns on every `type=quote` row. See
 * id:C3E053CA-0D4B-480B-9119-E11530160EC1, "Worked examples" > "Interest rates".
 */
std::optional<std::string> index_name_ir(const ir_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || !id.index)
        return std::nullopt;
    // The family token comes from index_token(), so a name that arrived under one
    // of the family's other spellings reads back as it came. A curve drops the
    // tenor of an overnight family into the curve key, where a fixing keeps the
    // tenor the index was published at; `role` is what separates the two.
    const auto family = index_token(id);
    const auto tenor_is_the_curves = id.role.has_value() && !requires_tenor(*id.index);
    if (!id.tenor || tenor_is_the_curves)
        return std::format("{}-{}", id.ccy, family);
    return std::format("{}-{}-{}", id.ccy, family, to_upper(*id.tenor));
}

/*
 * FX-<SOURCE>-<CCY1>-<CCY2>, the name ORE gives a fixing and nothing else. A
 * quote has no index name: its key carries no source, and the corpus writes the
 * token only in index names, a fixing file or a correlation qualifier.
 *
 * The pair is emitted as published, never in a canonical order: the two
 * directions are two published rates, and the corpus carries GBP-EUR and EUR-GBP
 * under one source as reciprocal series. The source token comes from the spelling
 * the name arrived under, when it has one.
 */
std::optional<std::string> index_name_fx(const fx_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || !id.source || id.pair.size() != 6)
        return std::nullopt;
    const auto source = id.source_spelling ? *id.source_spelling : to_upper(*id.source);
    return std::format("FX-{}-{}-{}", source, id.pair.substr(0, 3), id.pair.substr(3, 3));
}

std::optional<std::string> curve_key_ir(const ir_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || !id.tenor)
        return std::nullopt;
    return std::format("Yield/{}/{}", id.ccy, curve_id(id.ccy, *id.tenor));
}

// Default metric from ir_quote_type when metric is absent (e.g. quote=mm implies metric=rate).
metric default_metric(ir_quote_type qt) {
    switch (qt) {
        case ir_quote_type::ir_swap:
        case ir_quote_type::discount:
        case ir_quote_type::mm:
        case ir_quote_type::fra:
        case ir_quote_type::imm_fra:
        case ir_quote_type::cc_fix_float_swap:
            return metric::rate;
        case ir_quote_type::basis_swap:
        case ir_quote_type::cc_basis_swap:
            return metric::basis_spread;
        case ir_quote_type::bma_swap:
            return metric::ratio;
        case ir_quote_type::zero:
            return metric::rate;
        case ir_quote_type::mm_future:
        case ir_quote_type::oi_future:
            return metric::price;
        // A capfloor's and a bond option's ORE metric segment is their vol
        // model, which the metric enum has no member for; quote_key_ir reads
        // ore_vol_model() for it and never this. The cases exist so the switch
        // stays exhaustive.
        case ir_quote_type::capfloor:
        case ir_quote_type::bond_option:
            return metric::rate;
    }
    return metric::rate;
}

// ORE TYPE string for each ir_quote_type.
std::string_view ore_type(ir_quote_type qt) {
    switch (qt) {
        case ir_quote_type::ir_swap:
            return ore_type_spec::ir_swap;
        case ir_quote_type::discount:
            return ore_type_spec::discount;
        case ir_quote_type::mm:
            return ore_type_spec::mm;
        case ir_quote_type::fra:
            return ore_type_spec::fra;
        case ir_quote_type::imm_fra:
            return ore_type_spec::imm_fra;
        case ir_quote_type::basis_swap:
            return ore_type_spec::basis_swap;
        case ir_quote_type::bma_swap:
            return ore_type_spec::bma_swap;
        case ir_quote_type::cc_basis_swap:
            return ore_type_spec::cc_basis_swap;
        case ir_quote_type::cc_fix_float_swap:
            return ore_type_spec::cc_fix_float_swap;
        case ir_quote_type::zero:
            return ore_type_spec::zero;
        case ir_quote_type::mm_future:
            return ore_type_spec::mm_future;
        case ir_quote_type::oi_future:
            return ore_type_spec::oi_future;
        case ir_quote_type::capfloor:
            return ore_type_spec::capfloor;
        case ir_quote_type::bond_option:
            return ore_type_spec::bond_option;
    }
    return ore_type_spec::ir_swap;
}

// ORE METRIC string for each metric.
std::string_view ore_metric(metric m) {
    switch (m) {
        case metric::rate:
            return ore_metric_spec::rate;
        case metric::price:
            return ore_metric_spec::price;
        case metric::basis_spread:
            return ore_metric_spec::basis_spread;
        case metric::ratio:
            return ore_metric_spec::ratio;
        case metric::yield_spread:
            return ore_metric_spec::yield_spread;
        case metric::conversion_factor:
            return ore_metric_spec::conversion_factor;
        case metric::shape_factor:
            return ore_metric_spec::shape_factor;
        case metric::transition_probability:
            return ore_metric_spec::transition_probability;
    }
    return ore_metric_spec::rate;
}

// Whether this ir_quote_type includes the index in the qualifier (as opposed to just
// ccy/tenor for the xccy/BMA/fra families).
//
// FRA and IMM_FRA are here because ORE writes them as ccy/start/length: the
// second qualifier segment is the start tenor or the IMM date, not an index.
// See the shape table in ores_series_key_shapes_populate.sql, which states the
// same for every type.
bool qualifier_includes_index(ir_quote_type qt) {
    switch (qt) {
        case ir_quote_type::cc_basis_swap:
        case ir_quote_type::cc_fix_float_swap:
        case ir_quote_type::bma_swap:
        case ir_quote_type::fra:
        case ir_quote_type::imm_fra:
        case ir_quote_type::mm_future:
        case ir_quote_type::oi_future:
            return false;
        default:
            return true;
    }
}

std::string_view ore_vol_model(volatility_model_subtype m) {
    switch (m) {
        case volatility_model_subtype::rate_lnvol:
            return ore_vol_spec::rate_lnvol;
        case volatility_model_subtype::rate_nvol:
            return ore_vol_spec::rate_nvol;
        case volatility_model_subtype::rate_slnvol:
            return ore_vol_spec::rate_slnvol;
        case volatility_model_subtype::shift:
            return ore_vol_spec::shift;
        case volatility_model_subtype::price:
            return ore_vol_spec::price;
    }
    return ore_vol_spec::rate_lnvol;
}

std::optional<std::string> quote_key_ir(const ir_market_data_identifier& id) {
    if (id.type == instrument_type::vol) {
        // A cap/floor surface is its own family: the metric segment is the vol
        // model, and the coordinate carries the shift and strip flags the
        // market data catalogue names, not a swaption's expiry-and-tenor pair.
        if (id.quote_type == ir_quote_type::capfloor) {
            // The shift names the displaced log-normal model rather than a
            // surface coordinate of its own: CAPFLOOR/SHIFT/CCY/FLOAT_TENOR.
            if (id.vol && id.vol->model_subtype == volatility_model_subtype::shift) {
                if (!id.tenor)
                    return std::nullopt;
                return std::format("{}/{}/{}/{}",
                                   ore_type(ir_quote_type::capfloor),
                                   ore_vol_spec::shift,
                                   id.ccy,
                                   to_upper(*id.tenor));
            }
            if (!id.vol || !id.tenor || !id.shift || !id.strip)
                return std::nullopt;
            return std::format("{}/{}/{}/{}/{}/{}/{}/{}",
                               ore_type(ir_quote_type::capfloor),
                               ore_vol_model(id.vol->model_subtype),
                               id.ccy,
                               to_upper(id.vol->expiry),
                               to_upper(*id.tenor),
                               *id.shift,
                               *id.strip,
                               to_upper(id.vol->strike));
        }
        // Use typed vol struct if available, else fall back to point composite.
        if (id.vol && id.tenor) {
            const auto& v = *id.vol;
            // BOND_OPTION is the swaption shape over a bond: the same segments,
            // with the underlying where a swaption has a currency.
            const auto family = (id.quote_type == ir_quote_type::bond_option) ?
                                    ore_type_spec::bond_option :
                                    ore_type_spec::swaption;
            const auto model = ore_vol_model(v.model_subtype);
            const auto expiry = to_upper(v.expiry);
            const auto tenor = to_upper(*id.tenor);
            const auto strike = to_upper(v.strike);
            // Two qualifiers are optional and both sit ahead of the value: the
            // index the surface is quoted against, and the convention marker the
            // corpus writes as Smile, whose shift then takes the value's place.
            // So the family is six, seven or eight segments long, and the marker
            // is emitted as written rather than uppercased.
            if (id.index && v.delta_type)
                return std::format("{}/{}/{}/{}/{}/{}/{}/{}",
                                   family,
                                   model,
                                   id.ccy,
                                   index_token(id),
                                   expiry,
                                   tenor,
                                   *v.delta_type,
                                   strike);
            if (id.index)
                return std::format("{}/{}/{}/{}/{}/{}/{}",
                                   family,
                                   model,
                                   id.ccy,
                                   index_token(id),
                                   expiry,
                                   tenor,
                                   strike);
            if (v.delta_type)
                return std::format("{}/{}/{}/{}/{}/{}/{}",
                                   family,
                                   model,
                                   id.ccy,
                                   expiry,
                                   tenor,
                                   *v.delta_type,
                                   strike);
            return std::format("{}/{}/{}/{}/{}/{}", family, model, id.ccy, expiry, tenor, strike);
        }
        if (!id.point)
            return std::nullopt;
        const auto parts = split_point(*id.point);
        if (parts.size() != 3)
            return std::nullopt;
        const auto t = id.tenor ? to_upper(*id.tenor) : parts[1];
        return std::format("SWAPTION/RATE_LNVOL/{}/{}/{}/{}", id.ccy, parts[0], t, parts[2]);
    }
    // ZERO is keyed by a free-text curve id and a day count, and its coordinate
    // is the point rather than the tenor field, so it does not pass the guard
    // below. It is handled first, with the curve id emitted verbatim: the corpus
    // carries both EONIA_ESTER_SPREAD and Ester_Eonia_Spread, so the spelling is
    // data and normalising it would stop the key reading back.
    if (id.type == instrument_type::quote && id.quote_type == ir_quote_type::zero) {
        if (!id.point || !id.curve_id || !id.day_count)
            return std::nullopt;
        const auto m = id.metric ? *id.metric : default_metric(ir_quote_type::zero);
        return std::format("{}/{}/{}/{}/{}/{}",
                           ore_type(ir_quote_type::zero),
                           ore_metric(m),
                           id.ccy,
                           *id.curve_id,
                           *id.day_count,
                           to_upper(*id.point));
    }
    // A future is keyed by its delivery month and the exchange-qualified contract
    // code the producer quotes, then the underlying index tenor:
    // MM_FUTURE/PRICE/CCY/CONTRACT_MONTH/CONTRACT_CODE/TENOR. The contract code is
    // emitted verbatim -- the corpus writes XICE:FEI and XCME:SRA, and the token is
    // a name for the contract rather than a pair of fields we can decompose.
    // A future carries no point, so it does not pass the guard below.
    if (id.type == instrument_type::quote &&
        (id.quote_type == ir_quote_type::mm_future || id.quote_type == ir_quote_type::oi_future)) {
        if (!id.contract_month || !id.contract_code || !id.tenor)
            return std::nullopt;
        const auto qt = *id.quote_type;
        const auto m = id.metric ? *id.metric : default_metric(qt);
        return std::format("{}/{}/{}/{}/{}/{}",
                           ore_type(qt),
                           ore_metric(m),
                           id.ccy,
                           *id.contract_month,
                           *id.contract_code,
                           to_upper(*id.tenor));
    }
    // DISCOUNT is keyed by the name the producer gave the curve and the maturity
    // sampled off it. The corpus names curves the way curveconfig.xml declares
    // them -- USD3M beside USD-SOFR-3M, USD-FedFunds, USD-DUMMY and the bare
    // currency -- so the name is data that no derivation from the currency
    // reproduces, and it is emitted verbatim. The maturity is the identifier's
    // tenor rather than its point, so the type does not pass the guard below.
    if (id.type == instrument_type::quote && id.quote_type == ir_quote_type::discount) {
        if (!id.curve_id || !id.tenor)
            return std::nullopt;
        const auto m = id.metric ? *id.metric : default_metric(ir_quote_type::discount);
        return std::format("{}/{}/{}/{}/{}",
                           ore_type(ir_quote_type::discount),
                           ore_metric(m),
                           id.ccy,
                           *id.curve_id,
                           to_upper(*id.tenor));
    }
    if (id.type != instrument_type::quote || !id.quote_type || !id.point || !id.tenor)
        return std::nullopt;

    const auto qt = *id.quote_type;
    const auto m = id.metric ? *id.metric : default_metric(qt);
    const auto point = to_upper(*id.point);
    const auto t = to_upper(*id.tenor);

    // A single-currency basis swap quotes two index tenors against one
    // currency: the qualifier is first_tenor/second_tenor/ccy, with no index
    // and no role. Neither tenor is derivable from the other.
    if (qt == ir_quote_type::basis_swap) {
        if (!id.second_tenor)
            return std::nullopt;
        // A basis swap may name the basis itself -- SOFR_FedFunds, LIBOR_PRIME --
        // between the currency and the maturity. Those names are not index
        // families, so the token is carried as written and emitted the same way.
        const auto index = index_token(id);
        if (!index.empty())
            return std::format("{}/{}/{}/{}/{}/{}/{}",
                               ore_type(qt),
                               ore_metric(m),
                               t,
                               to_upper(*id.second_tenor),
                               id.ccy,
                               index,
                               point);
        return std::format("{}/{}/{}/{}/{}/{}",
                           ore_type(qt),
                           ore_metric(m),
                           t,
                           to_upper(*id.second_tenor),
                           id.ccy,
                           point);
    }

    // A cross-currency swap quotes two currencies against two index tenors:
    // ccy1/tenor1/ccy2/tenor2, with the maturity after them. The entity path
    // segment carries the first currency.
    if (qt == ir_quote_type::cc_basis_swap || qt == ir_quote_type::cc_fix_float_swap) {
        if (!id.second_ccy || !id.second_tenor)
            return std::nullopt;
        return std::format("{}/{}/{}/{}/{}/{}/{}",
                           ore_type(qt),
                           ore_metric(m),
                           id.ccy,
                           t,
                           *id.second_ccy,
                           to_upper(*id.second_tenor),
                           point);
    }

    if (qualifier_includes_index(qt)) {
        if (qt == ir_quote_type::ir_swap) {
            // The settlement segment is a per-currency spot lag: 2D almost
            // everywhere, 0D and 1D on some producers' lines. The corpus's own
            // default is emitted when the identifier recorded none.
            const auto settle = id.settle ? *id.settle : std::string{"2D"};
            // A swap written against a named index carries it between the
            // currency and the spot lag, which is the only difference between
            // the two forms ORE writes: ccy/settle/tenor/maturity and
            // ccy/index/settle/tenor/maturity.
            const auto index = index_token(id);
            if (!index.empty())
                return std::format("{}/{}/{}/{}/{}/{}/{}",
                                   ore_type(qt),
                                   ore_metric(m),
                                   id.ccy,
                                   index,
                                   settle,
                                   t,
                                   point);
            return std::format(
                "{}/{}/{}/{}/{}/{}", ore_type(qt), ore_metric(m), id.ccy, settle, t, point);
        }
        if (!id.index) {
            // MM is the one indexed family ORE writes both ways: ccy/settle/tenor
            // when the producer names no index (94% of the corpus), and
            // ccy/index/settle/tenor when it does. Every other indexed family
            // requires the index, so only MM reaches the short form below.
            if (qt != ir_quote_type::mm)
                return std::nullopt;
        } else {
            return std::format("{}/{}/{}/{}/{}/{}",
                               ore_type(qt),
                               ore_metric(m),
                               id.ccy,
                               index_token(id),
                               t,
                               point);
        }
    }
    // No-index types: CC_BASIS_SWAP, CC_FIX_FLOAT_SWAP, BMA_SWAP, FRA, IMM_FRA,
    // and MM when its producer names no index.
    return std::format("{}/{}/{}/{}/{}", ore_type(qt), ore_metric(m), id.ccy, t, point);
}

std::string_view ore_type(fx_quote_type qt) {
    switch (qt) {
        case fx_quote_type::spot:
            return ore_type_spec::fx;
        case fx_quote_type::fwd:
            return ore_type_spec::fxfwd;
    }
    return ore_type_spec::fx;
}

std::string_view ore_fx_metric(fx_quote_type qt) {
    switch (qt) {
        case fx_quote_type::spot:
        case fx_quote_type::fwd:
            return ore_metric_spec::rate;
    }
    return ore_metric_spec::rate;
}

std::optional<std::string> quote_key_fx(const fx_market_data_identifier& id) {
    if (id.pair.size() != 6)
        return std::nullopt;
    const auto ccy1 = id.pair.substr(0, 3);
    const auto ccy2 = id.pair.substr(3, 3);
    // A vol surface point is the FX option family, keyed by the pair, the expiry
    // and the strike or at-the-money convention:
    // FX_OPTION/MODEL/CCY1/CCY2/EXPIRY/STRIKE.
    if (id.type == instrument_type::vol) {
        if (!id.vol)
            return std::nullopt;
        return std::format("{}/{}/{}/{}/{}/{}",
                           ore_type_spec::fx_option,
                           ore_vol_model(id.vol->model_subtype),
                           ccy1,
                           ccy2,
                           to_upper(id.vol->expiry),
                           to_upper(id.vol->strike));
    }
    if (id.type != instrument_type::quote)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(fx_quote_type::spot);
    // spot: TYPE/METRIC/CCY1/CCY2 (scalar).
    if (qt == fx_quote_type::spot)
        return std::format("{}/{}/{}/{}", ore_type(qt), ore_fx_metric(qt), ccy1, ccy2);
    // fwd: TYPE/METRIC/CCY1/CCY2/TENOR — forward curve, needs point for tenor.
    if (!id.point)
        return std::nullopt;
    return std::format(
        "{}/{}/{}/{}/{}", ore_type(qt), ore_fx_metric(qt), ccy1, ccy2, to_upper(*id.point));
}

std::string_view ore_type(equity_quote_type qt) {
    switch (qt) {
        case equity_quote_type::spot:
            return ore_type_spec::equity;
        case equity_quote_type::dividend:
            return ore_type_spec::equity_dividend;
        case equity_quote_type::fwd:
            return ore_type_spec::equity_fwd;
    }
    return ore_type_spec::equity;
}

std::string_view ore_equity_metric(equity_quote_type qt) {
    switch (qt) {
        case equity_quote_type::spot:
        case equity_quote_type::fwd:
            return ore_metric_spec::price;
        case equity_quote_type::dividend:
            return ore_metric_spec::rate;
    }
    return ore_metric_spec::price;
}

std::optional<std::string> quote_key_equity(const equity_market_data_identifier& id) {
    // A vol surface point is the equity option family. Three shapes occur, and
    // which one this is follows from what the surface point carries:
    //   EQUITY_OPTION/MODEL/TICKER/CCY/EXPIRY/STRIKE
    //   EQUITY_OPTION/MODEL/TICKER/CCY/EXPIRY/STRIKE/CALL_PUT
    //   EQUITY_OPTION/MODEL/TICKER/CCY/EXPIRY/DELTA/PREMIUM/CALL_PUT/STRIKE
    // A fixing's key is an index name and carries no currency, and this emits ORE
    // quote keys only. The parser refuses a ccy-less identifier for every type
    // that reaches the branches below, so one arriving here has no key to emit.
    if (!id.ccy)
        return std::nullopt;

    if (id.type == instrument_type::vol) {
        if (!id.vol)
            return std::nullopt;
        const auto& v = *id.vol;
        const auto head = std::format(
            "{}/{}/{}", ore_type_spec::equity_option, ore_vol_model(v.model_subtype), id.ticker);
        if (v.delta_type && v.premium_type && v.call_put)
            return std::format("{}/{}/{}/{}/{}/{}/{}",
                               head,
                               *id.ccy,
                               v.expiry,
                               *v.delta_type,
                               *v.premium_type,
                               *v.call_put,
                               v.strike);
        if (v.call_put)
            return std::format("{}/{}/{}/{}/{}", head, *id.ccy, v.expiry, v.strike, *v.call_put);
        return std::format("{}/{}/{}/{}", head, *id.ccy, v.expiry, v.strike);
    }
    if (id.type != instrument_type::quote)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(equity_quote_type::spot);
    // spot: EQUITY/PRICE/TICKER/CCY (scalar, no tenor).
    if (qt == equity_quote_type::spot)
        return std::format("{}/{}/{}/{}", ore_type(qt), ore_equity_metric(qt), id.ticker, *id.ccy);
    // dividend/fwd: TYPE/METRIC/TICKER/CCY/TENOR — curves, need point for the tenor dimension.
    if (!id.point)
        return std::nullopt;
    return std::format("{}/{}/{}/{}/{}",
                       ore_type(qt),
                       ore_equity_metric(qt),
                       id.ticker,
                       *id.ccy,
                       to_upper(*id.point));
}

std::string_view ore_type(credit_quote_type qt) {
    switch (qt) {
        case credit_quote_type::cds:
            return ore_type_spec::cds;
        case credit_quote_type::hazard_rate:
            return ore_type_spec::hazard_rate;
        case credit_quote_type::recovery_rate:
            return ore_type_spec::recovery_rate;
        case credit_quote_type::cds_index:
            return ore_type_spec::cds_index;
        case credit_quote_type::index_cds_tranche:
            return ore_type_spec::index_cds_tranche;
        case credit_quote_type::index_cds_option:
            return ore_type_spec::index_cds_option;
    }
    return ore_type_spec::cds;
}

std::string_view ore_credit_metric(credit_quote_type qt) {
    switch (qt) {
        case credit_quote_type::cds:
            return ore_metric_spec::credit_spread;
        case credit_quote_type::hazard_rate:
        case credit_quote_type::recovery_rate:
        case credit_quote_type::index_cds_option:
            return ore_metric_spec::rate;
        case credit_quote_type::cds_index:
        case credit_quote_type::index_cds_tranche:
            return ore_metric_spec::base_correlation;
    }
    return ore_metric_spec::credit_spread;
}

std::optional<std::string> quote_key_credit(const credit_market_data_identifier& id) {
    const auto qt = id.quote_type.value_or(credit_quote_type::cds);
    const auto qm = ore_credit_metric(qt);

    // An index option is a volatility surface, and its metric segment is the vol
    // model rather than the type's own metric. Two forms: the term vol, which is
    //  a single number at an index tenor, and the full surface with an expiry and
    // a strike. Both carry the index in the entity.
    if (id.type == instrument_type::vol) {
        if (!id.point || !id.vol)
            return std::nullopt;
        const auto parts = split_point(*id.point);
        const auto model = ore_vol_model(id.vol->model_subtype);
        if (parts.size() == 1)
            return std::format("{}/{}/{}/{}", ore_type(qt), model, id.reference_entity, parts[0]);
        if (parts.size() == 3)
            return std::format("{}/{}/{}/{}/{}/{}",
                               ore_type(qt),
                               model,
                               id.reference_entity,
                               parts[0],
                               parts[1],
                               parts[2]);
        return std::nullopt;
    }

    if (id.type != instrument_type::quote)
        return std::nullopt;

    if (!id.point)
        return std::nullopt;

    const auto parts = split_point(*id.point);

    // RECOVERY_RATE/RATE/ENTITY/SENIORITY/CCY — point is just the seniority, and
    // the restructuring clause when the producer writes one.
    if (qt == credit_quote_type::recovery_rate) {
        if (parts.size() == 2)
            return std::format("{}/{}/{}/{}/{}/{}",
                               ore_type(qt),
                               qm,
                               id.reference_entity,
                               parts[0],
                               id.ccy,
                               parts[1]);
        if (parts.size() != 1)
            return std::nullopt;
        return std::format(
            "{}/{}/{}/{}/{}", ore_type(qt), qm, id.reference_entity, parts[0], id.ccy);
    }

    // CDS_INDEX/BASE_CORRELATION/INDEX/TENOR/DETACHMENT — no ccy dimension.
    // INDEX_CDS_TRANCHE/BASE_CORRELATION/INDEX/SERIES_TENOR/DETACHMENT — no ccy dimension.
    if (qt == credit_quote_type::cds_index || qt == credit_quote_type::index_cds_tranche) {
        if (parts.size() != 2)
            return std::nullopt;
        return std::format(
            "{}/{}/{}/{}/{}", ore_type(qt), qm, id.reference_entity, parts[0], parts[1]);
    }

    // CDS/CREDIT_SPREAD/ENTITY/SENIORITY/CCY/TENOR — point=seniority,tenor (2 parts).
    // HAZARD_RATE/RATE/ENTITY/SENIORITY/CCY/TENOR — same 6-segment shape.
    // A third part is the credit derivatives' restructuring clause, which ORE
    // writes between the currency and the tenor:
    // CDS/CREDIT_SPREAD/ENTITY/SENIORITY/CCY/XR14/10Y.
    if (parts.size() == 3)
        return std::format("{}/{}/{}/{}/{}/{}/{}",
                           ore_type(qt),
                           qm,
                           id.reference_entity,
                           parts[0],
                           id.ccy,
                           parts[1],
                           parts[2]);
    if (parts.size() != 2)
        return std::nullopt;
    return std::format(
        "{}/{}/{}/{}/{}/{}", ore_type(qt), qm, id.reference_entity, parts[0], id.ccy, parts[1]);
}

std::string_view ore_type(commodity_quote_type qt) {
    switch (qt) {
        case commodity_quote_type::spot:
            return ore_type_spec::commodity;
        case commodity_quote_type::fwd:
            return ore_type_spec::commodity_fwd;
        case commodity_quote_type::option:
            return ore_type_spec::commodity_option;
    }
    return ore_type_spec::commodity;
}

std::string_view ore_commodity_metric(commodity_quote_type qt) {
    switch (qt) {
        case commodity_quote_type::spot:
        case commodity_quote_type::fwd:
            return ore_metric_spec::price;
        case commodity_quote_type::option:
            return ore_metric_spec::rate;
    }
    return ore_metric_spec::price;
}

std::string_view ore_type(inflation_quote_type qt) {
    switch (qt) {
        case inflation_quote_type::zc_swap:
            return ore_type_spec::zc_inflation_swap;
        case inflation_quote_type::yy_swap:
            return ore_type_spec::yy_inflation_swap;
        case inflation_quote_type::seasonality:
            return ore_type_spec::seasonality;
        case inflation_quote_type::zc_capfloor:
            return ore_type_spec::zc_inflation_capfloor;
        case inflation_quote_type::yy_capfloor:
            return ore_type_spec::yy_inflation_capfloor;
        case inflation_quote_type::cf_price:
            return ore_type_spec::capfloor;
    }
    return ore_type_spec::zc_inflation_swap;
}

std::string_view ore_inflation_metric(inflation_quote_type qt) {
    switch (qt) {
        case inflation_quote_type::zc_swap:
        case inflation_quote_type::yy_swap:
        case inflation_quote_type::seasonality:
            return ore_metric_spec::rate;
        case inflation_quote_type::zc_capfloor:
        case inflation_quote_type::yy_capfloor:
        case inflation_quote_type::cf_price:
            return ore_metric_spec::price;
    }
    return ore_metric_spec::rate;
}

std::optional<std::string> quote_key_inflation(const inflation_market_data_identifier& id) {
    // An inflation cap/floor is a vol surface, not a quote: its metric segment is
    // the surface (price or normal vol), and the coordinate is the maturity, the
    // cap-or-floor flag and the strike.
    if (id.type == instrument_type::vol) {
        if (!id.vol || !id.quote_type || !id.vol->call_put)
            return std::nullopt;
        const auto qt = *id.quote_type;
        if (qt != inflation_quote_type::zc_capfloor && qt != inflation_quote_type::yy_capfloor &&
            qt != inflation_quote_type::cf_price)
            return std::nullopt;
        return std::format("{}/{}/{}/{}/{}/{}",
                           ore_type(qt),
                           ore_vol_model(id.vol->model_subtype),
                           id.index_code,
                           to_upper(id.vol->expiry),
                           to_upper(*id.vol->call_put),
                           to_upper(id.vol->strike));
    }
    if (id.type != instrument_type::quote || !id.quote_type || !id.point)
        return std::nullopt;
    const auto qt = *id.quote_type;
    // SEASONALITY/RATE/MULT/<INDEX>/<POINT> — 5-segment key with literal MULT.
    if (qt == inflation_quote_type::seasonality)
        return std::format("{}/{}/MULT/{}/{}",
                           ore_type(qt),
                           ore_inflation_metric(qt),
                           id.index_code,
                           to_upper(*id.point));
    return std::format(
        "{}/{}/{}/{}", ore_type(qt), ore_inflation_metric(qt), id.index_code, to_upper(*id.point));
}

std::optional<std::string> quote_key_correlation(const correlation_market_data_identifier& id) {
    if (id.type != instrument_type::quote)
        return std::nullopt;
    // A single factor pair is a scalar. A pairwise surface names two operands
    // and then the expiry and strike, which is where the corpus puts them.
    if (!id.second_factor)
        return std::format("CORRELATION/RATE/{}", id.factor_pair);
    if (!id.point)
        return std::nullopt;
    // The point is stored comma-separated, as every compound point here is; the
    // correlation key spells it with a slash.
    const auto parts = split_point(*id.point);
    if (parts.size() != 2)
        return std::nullopt;
    return std::format(
        "CORRELATION/RATE/{}/{}/{}/{}", id.factor_pair, *id.second_factor, parts[0], parts[1]);
}

std::string_view ore_type(security_quote_type qt) {
    switch (qt) {
        case security_quote_type::bond_price:
            return ore_type_spec::bond;
        case security_quote_type::bond_yield_spread:
            return ore_type_spec::bond;
        case security_quote_type::bond_conversion_factor:
            return ore_type_spec::bond;
        case security_quote_type::recovery_rate:
            return ore_type_spec::recovery_rate;
        case security_quote_type::cpr:
            return ore_type_spec::cpr;
    }
    return ore_type_spec::bond;
}

std::string_view ore_security_metric(security_quote_type qt) {
    switch (qt) {
        case security_quote_type::bond_price:
            return ore_metric_spec::price;
        case security_quote_type::bond_yield_spread:
            return ore_metric_spec::yield_spread;
        case security_quote_type::bond_conversion_factor:
            return ore_metric_spec::conversion_factor;
        case security_quote_type::recovery_rate:
        case security_quote_type::cpr:
            return ore_metric_spec::rate;
    }
    return ore_metric_spec::price;
}

std::optional<std::string> quote_key_security(const security_market_data_identifier& id) {
    // The class carries no currency, no tenor and no point: the entity is the
    // security and the metric is the whole qualifier, so the key is
    // TYPE/METRIC/SECURITY. ORE writes one metric per type, which is why each
    // metric is its own quote type here.
    if (id.type != instrument_type::quote)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(security_quote_type::bond_price);
    return std::format("{}/{}/{}", ore_type(qt), ore_security_metric(qt), id.security_id);
}

std::string_view ore_type(shape_profile_quote_type qt) {
    switch (qt) {
        case shape_profile_quote_type::shape_factor:
            return ore_type_spec::shape_profile;
    }
    return ore_type_spec::shape_profile;
}

std::string_view ore_shape_profile_metric(shape_profile_quote_type qt) {
    switch (qt) {
        case shape_profile_quote_type::shape_factor:
            return ore_metric_spec::shape_factor;
    }
    return ore_metric_spec::shape_factor;
}

std::optional<std::string> quote_key_shape_profile(const shape_profile_market_data_identifier& id) {
    // SHAPE_PROFILE/SHAPE_FACTOR/PROFILE/DATE/SECOND/PERIOD, with the DST flag the
    // corpus writes as a seventh segment. No currency, no tenor and no metric: the
    // point carries the whole coordinate.
    if (id.type != instrument_type::quote || !id.point)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(shape_profile_quote_type::shape_factor);
    const auto parts = split_point(*id.point);
    if (parts.size() != 3 && parts.size() != 4)
        return std::nullopt;
    const auto head =
        std::format("{}/{}/{}", ore_type(qt), ore_shape_profile_metric(qt), id.profile_id);
    if (parts.size() == 4)
        return std::format("{}/{}/{}/{}/{}", head, parts[0], parts[1], parts[2], parts[3]);
    return std::format("{}/{}/{}/{}", head, parts[0], parts[1], parts[2]);
}

std::string_view ore_type(rating_quote_type qt) {
    switch (qt) {
        case rating_quote_type::transition_probability:
            return ore_type_spec::rating;
    }
    return ore_type_spec::rating;
}

std::string_view ore_rating_metric(rating_quote_type qt) {
    switch (qt) {
        case rating_quote_type::transition_probability:
            return ore_metric_spec::transition_probability;
    }
    return ore_metric_spec::transition_probability;
}

std::optional<std::string> quote_key_rating(const rating_market_data_identifier& id) {
    // RATING/TRANSITION_PROBABILITY/PROVIDER/FROM/TO, and the four-segment form
    // the corpus also writes, which names the provider and no grades. No currency,
    // no tenor and no metric: the point carries the grades.
    if (id.type != instrument_type::quote || !id.point)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(rating_quote_type::transition_probability);
    const auto parts = split_point(*id.point);
    const auto head = std::format("{}/{}/{}", ore_type(qt), ore_rating_metric(qt), id.provider_id);
    // The degenerate form's point is present but empty, and an empty point splits
    // to no parts at all; the trailing segment is what says so.
    if (parts.empty())
        return head + "/";
    if (parts.size() == 1)
        return head + "/" + parts[0];
    if (parts.size() != 2)
        return std::nullopt;
    return std::format("{}/{}/{}", head, parts[0], parts[1]);
}

std::optional<std::string> quote_key_commodity(const commodity_market_data_identifier& id) {
    // A vol surface point is the commodity option family, which writes the same
    // three shapes the equity option does with a commodity code where the equity
    // has a ticker:
    //   COMMODITY_OPTION/MODEL/CODE/CCY/EXPIRY/STRIKE
    //   COMMODITY_OPTION/MODEL/CODE/CCY/EXPIRY/STRIKE/CALL_PUT
    //   COMMODITY_OPTION/MODEL/CODE/CCY/EXPIRY/DELTA/PREMIUM/CALL_PUT/STRIKE
    // A fixing's key is a contract name and carries no currency, and this emits ORE
    // quote keys only. The parser refuses a ccy-less identifier for every type
    // that reaches the branches below, so one arriving here has no key to emit.
    if (!id.ccy)
        return std::nullopt;

    if (id.type == instrument_type::vol) {
        if (!id.vol)
            return std::nullopt;
        const auto& v = *id.vol;
        const auto head = std::format("{}/{}/{}",
                                      ore_type_spec::commodity_option,
                                      ore_vol_model(v.model_subtype),
                                      id.commodity_code);
        if (v.delta_type && v.premium_type && v.call_put)
            return std::format("{}/{}/{}/{}/{}/{}/{}",
                               head,
                               *id.ccy,
                               v.expiry,
                               *v.delta_type,
                               *v.premium_type,
                               *v.call_put,
                               v.strike);
        if (v.call_put)
            return std::format("{}/{}/{}/{}/{}", head, *id.ccy, v.expiry, v.strike, *v.call_put);
        return std::format("{}/{}/{}/{}", head, *id.ccy, v.expiry, v.strike);
    }
    if (id.type != instrument_type::quote)
        return std::nullopt;
    const auto qt = id.quote_type.value_or(commodity_quote_type::spot);
    // spot: COMMODITY/PRICE/CODE/CCY (scalar).
    if (qt == commodity_quote_type::spot)
        return std::format(
            "{}/{}/{}/{}", ore_type(qt), ore_commodity_metric(qt), id.commodity_code, *id.ccy);
    // fwd: TYPE/METRIC/CODE/CCY/TENOR — a curve, needs point for the tenor.
    if (!id.point)
        return std::nullopt;
    return std::format("{}/{}/{}/{}/{}",
                       ore_type(qt),
                       ore_commodity_metric(qt),
                       id.commodity_code,
                       *id.ccy,
                       to_upper(*id.point));
}


/*
 * ─── Inverse projection: ORE quote key string → oresmd identifier ─────────────
 *
 * Follows the quote-key forward projections above in reverse. Seeded by the
 * series_key_registry's decomposition table: the segment boundaries mirror that
 * table's per-type rows (qualifier vs point), and every type the table knows is
 * either mapped here or has no oresmd identifier at all (BOND, the option and
 * capfloor families) — those, plus unknown types and malformed keys, yield
 * nullopt. Segment spelling normalisation mirrors the parser: codes/ccy upper,
 * tenor/point/index lower.
 */

std::optional<std::vector<std::string>> split_key(const std::string& key) {
    std::vector<std::string> parts;
    std::stringstream ss(key);
    std::string tok;
    while (std::getline(ss, tok, '/'))
        parts.push_back(tok);
    if (parts.size() < 3)
        return std::nullopt;
    if (std::ranges::any_of(parts, [](const std::string& p) { return p.empty(); }))
        return std::nullopt;
    return parts;
}

// Every dash-separated segment of a name, empty segments included; the callers
// decide which shapes they admit.
std::vector<std::string> split_on_dash(const std::string& name) {
    std::vector<std::string> parts;
    std::stringstream ss(name);
    std::string tok;
    while (std::getline(ss, tok, '-'))
        parts.push_back(tok);
    return parts;
}

// The index-name key space splits on '-' (e.g. "USD-LIBOR-3M"): exactly two
// or three segments, all non-empty. It is not the ORE key grammar and shares
// none of its structure -- a fixing's key names an index, not a series and a
// point -- so it gets its own splitter rather than a mode of split_key's.
std::optional<std::vector<std::string>> split_index_name(const std::string& name) {
    auto parts = split_on_dash(name);
    if (parts.size() < 2 || parts.size() > 3)
        return std::nullopt;
    if (std::ranges::any_of(parts, [](const std::string& p) { return p.empty(); }))
        return std::nullopt;
    return parts;
}

bool is_digits(std::string_view x) {
    return !x.empty() &&
           std::ranges::all_of(x, [](unsigned char c) { return std::isdigit(c) != 0; });
}

// A segment of a delivery coordinate that is a fixed-width number: four digits
// for a year, two for a month or a day, and any width for a count of seconds.
bool is_number(const std::string& x, std::size_t digits) {
    return x.size() == digits && is_digits(x);
}

/*
 * A delivery coordinate, the part of a commodity fixing's, a security fixing's and
 * an intraday power index's name that says which contract the series is. The
 * classes read it differently because ORE writes it differently: a commodity
 * future's and a bond future's period is a contract month, and an intraday power
 * index's is a date, then optionally a half-open window in seconds from midnight,
 * then optionally the DST flag, each level available only under the one before it.
 *
 * Both checks are shape checks. Whether the date is a real calendar date, and
 * whether the window lies inside one day, are left to the migration that gives
 * this library a calendar, the way the FX pair's currencies are left to the one
 * that gives it a currency list.
 */
bool power_delivery_is_the_shape(const std::string& delivery) {
    const auto parts = split_on_dash(delivery);
    if (parts.size() < 3 || parts.size() > 6)
        return false;
    if (!is_number(parts[0], 4) || !is_number(parts[1], 2) || !is_number(parts[2], 2))
        return false;
    if (parts.size() > 3 && (parts.size() < 5 || !is_digits(parts[3]) || !is_digits(parts[4])))
        return false;
    if (parts.size() == 5)
        return true;
    return parts.size() == 3 || parts[5] == "dst";
}

// A contract month, the coordinate a commodity future and a bond future carry:
// four digits for the year and two for the month.
bool contract_month_is_the_shape(const std::string& delivery) {
    const auto parts = split_on_dash(delivery);
    return parts.size() == 2 && is_number(parts[0], 4) && is_number(parts[1], 2);
}

/*
 * POWER-<CODE>[-<DELIVERY>], the name ORE gives an intraday power index and
 * nothing else: the class has no quote key, so this is its only projection.
 *
 * The delivery is carried as the name's whole tail because ORE's own grammar
 * nests it that way. The DST flag is the one segment ORE spells upper-case, and
 * the field is lower-cased on the way in like every other query key, so the name
 * restores it.
 */
std::optional<std::string> index_name_power(const power_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || id.commodity_code.empty())
        return std::nullopt;
    if (!id.delivery)
        return std::format("POWER-{}", to_upper(id.commodity_code));
    if (!power_delivery_is_the_shape(*id.delivery))
        return std::nullopt;
    auto delivery = *id.delivery;
    if (delivery.ends_with("-dst"))
        delivery.replace(delivery.size() - 3, 3, "DST");
    return std::format("POWER-{}-{}", to_upper(id.commodity_code), delivery);
}

/*
 * The class has no quote key. ORE quotes an intraday power price's two factors
 * under other classes -- the shape as SHAPE_PROFILE/SHAPE_FACTOR/... and the
 * daily average as COMMODITY_FWD/PRICE/... -- and the corpus writes no POWER/...
 * key at all. The function exists because the variant dispatch calls one per
 * class, and it is pinned by the model's own projection case.
 */
std::optional<std::string> quote_key_power(const power_market_data_identifier&) {
    return std::nullopt;
}

/*
 * COMM-<CODE>[-<MONTH>], the name ORE gives a commodity future index. A
 * commodity *quote* carries its delivery period as the last segment of
 * COMMODITY_FWD/PRICE/<CODE>/<CCY>/<PERIOD>, which is `point`; a fixing carries
 * it in the index name, which is `delivery`. They are two fields because a
 * quote's observations each name their own period and a fixing's do not.
 *
 * Only the contract month is read, which is the only period the corpus carries.
 * ORE's commodity parser also reads a full delivery date, and a commodity code
 * that itself contains a dash; neither is in the corpus, so neither is admitted
 * here rather than guessed at.
 */
std::optional<std::string> index_name_commodity(const commodity_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || id.commodity_code.empty())
        return std::nullopt;
    if (!id.delivery)
        return std::format("COMM-{}", to_upper(id.commodity_code));
    if (!contract_month_is_the_shape(*id.delivery))
        return std::nullopt;
    return std::format("COMM-{}-{}", to_upper(id.commodity_code), *id.delivery);
}

/*
 * GENERIC-<NAME>, the name ORE gives a generic index and nothing else. ORE's own
 * parser resolves the prefix, and the name is all the identity there is: the
 * corpus carries no other key for either series, and there is no quote key to
 * project.
 *
 * The name is written from the spelling the corpus used when it has one, and from
 * the upper-cased name otherwise, exactly as an FX source is: two spellings of
 * one name are one series here, and the name it arrived under is what the fixing
 * boundary has to write back.
 */
std::optional<std::string> index_name_generic(const generic_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || id.name.empty())
        return std::nullopt;
    const auto name = id.name_spelling ? *id.name_spelling : to_upper(id.name);
    return std::format("GENERIC-{}", name);
}

/*
 * The class has no quote key: ORE's generic index is its fixing-only fallback,
 * and the corpus writes GENERIC-<name> on fixing rows alone. The function exists
 * because the variant dispatch calls one per class, and it is pinned by the
 * model's own projection case.
 */
std::optional<std::string> quote_key_generic(const generic_market_data_identifier&) {
    return std::nullopt;
}

/*
 * The inflation index codes the model carries. A dash-less token is an inflation
 * index name only when it is one of them, spelled as the corpus spells it: the name
 * has no prefix, so nothing else in the string says which class it belongs to, and a
 * header word or a token from another vocabulary would otherwise be given an
 * inflation identity, while a code written in another case would be reshaped on the
 * way back instead of reading as it arrived.
 */
bool inflation_index_code_is_known(std::string_view name) {
    static const std::string_view codes[] = {
        "AUCPI",
        "EUHICP",
        "EUHICPXT",
        "FRHICP",
        "UKRPI",
        "USCPI",
        "ZACPI",
    };
    return std::ranges::any_of(codes, [name](std::string_view known) { return known == name; });
}

/*
 * The code alone, the one index-name prefix pair that is a bare token. An inflation
 * quote's key is ZC_INFLATIONSWAP/RATE/<CODE>/<POINT> and a fixing's name is the
 * code, so this is the class whose index name is not a prefix and a tail.
 */
std::optional<std::string> index_name_inflation(const inflation_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || id.index_code.empty())
        return std::nullopt;
    return to_upper(id.index_code);
}

/*
 * EQ-<TICKER>, the name ORE gives an equity index fixing. The ticker is the whole
 * tail, identifier scheme included, because that is how ORE spells the scheme in
 * both directions: the corpus carries EQ-RIC:.SPX as a fixing and
 * EQUITY_OPTION/RATE_LNVOL/RIC:.SPX/USD/... as the quote key for the same
 * instrument. A fixing carries no currency, and the parser requires none for one.
 */
std::optional<std::string> index_name_equity(const equity_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || id.ticker.empty())
        return std::nullopt;
    return std::format("EQ-{}", to_upper(id.ticker));
}

/*
 * BOND-<SECURITY>[-<MONTH>], the name ORE gives a security fixing. The security is
 * written under the scheme its quote keys use -- ISIN:IE00BH3SQ895, as
 * BOND/PRICE/ISIN:DE000A3H2WP2 is -- and the trailing month, where there is one, is
 * the bond future's expiry: the delivery field carries it the way commodity carries
 * a contract month.
 */
std::optional<std::string> index_name_security(const security_market_data_identifier& id) {
    if (id.type != instrument_type::fixing || id.security_id.empty())
        return std::nullopt;
    if (!id.delivery)
        return std::format("BOND-{}", to_upper(id.security_id));
    if (!contract_month_is_the_shape(*id.delivery))
        return std::nullopt;
    return std::format("BOND-{}-{}", to_upper(id.security_id), *id.delivery);
}

template <typename Enum>
std::optional<Enum> parse_enum_lower(std::string_view value) {
    return magic_enum::enum_cast<Enum>(to_lower(value));
}

std::optional<metric> parse_metric(std::string_view value) {
    return parse_enum_lower<metric>(value);
}

std::optional<index_family> parse_index(std::string_view value) {
    if (const auto exact = parse_enum_lower<index_family>(value))
        return exact;
    // ORE spells some families differently from the enum name -- it writes ESTER
    // for estr, and TONAR for tona. The family is what an alias resolves to, and
    // the spelling that arrived is carried beside it so the name reads back as it
    // came. Generated from the Index family table's aliases column.
    static const std::pair<std::string_view, index_family> aliases[] = {
        {"ester", index_family::estr},
        {"tonar", index_family::tona},
    };
    const auto spelling = to_lower(value);
    for (const auto& [alias, family] : aliases) {
        if (spelling == alias)
            return family;
    }
    return std::nullopt;
}

std::optional<volatility_model_subtype> parse_vol_model(std::string_view value) {
    return parse_enum_lower<volatility_model_subtype>(value);
}

// The forward projections emit one fixed METRIC segment per type for every
// non-IR asset class; a key whose metric differs could not have come from a
// forward projection and is rejected.
bool metric_is(std::string_view metric_segment, std::string_view expected) {
    return to_upper(metric_segment) == expected;
}

// The forward projections emit exactly three alphabetic characters per
// currency segment; a key with anything else could not have come from a
// forward projection.
bool is_currency_code(const std::string& x) {
    return x.size() == 3 && std::ranges::all_of(x, [](unsigned char c) { return std::isalpha(c); });
}

std::optional<market_data_identifier>
from_fx_spot(const std::vector<std::string>& parts,
             const ores::ore::market::fx_quote_convention_checker* checker) {
    // FX/RATE/CCY1/CCY2 — scalar, no point.
    if (parts.size() != 4 || !metric_is(parts[1], ore_metric_spec::rate))
        return std::nullopt;
    if (!is_currency_code(parts[2]) || !is_currency_code(parts[3]))
        return std::nullopt;
    auto base = to_upper(parts[2]);
    auto quote = to_upper(parts[3]);
    if (checker) {
        const auto result = checker->check(base, quote);
        base = result.base_currency;
        quote = result.quote_currency;
    }
    fx_market_data_identifier id;
    id.pair = base + quote;
    id.type = instrument_type::quote;
    id.quote_type = fx_quote_type::spot;
    return id;
}

std::optional<market_data_identifier> from_fx_option(const std::vector<std::string>& parts) {
    // FX_OPTION/MODEL/CCY1/CCY2/EXPIRY/STRIKE -- the pair names the currencies,
    // so the surface coordinate is the expiry and the strike after them.
    if (parts.size() != 6)
        return std::nullopt;
    const auto model = parse_vol_model(parts[1]);
    if (!model)
        return std::nullopt;
    if (!is_currency_code(parts[2]) || !is_currency_code(parts[3]))
        return std::nullopt;
    fx_market_data_identifier id;
    id.pair = to_upper(parts[2]) + to_upper(parts[3]);
    id.type = instrument_type::vol;
    // The parser keeps the point composite lowercased and the surface's own
    // coordinates uppercased, so the inverse builds both the same way.
    id.point = to_lower(parts[4]) + "," + to_lower(parts[5]);
    volatility_surface_point v;
    v.model_subtype = *model;
    v.expiry = to_upper(parts[4]);
    v.strike = to_upper(parts[5]);
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier> from_fx_fwd(const std::vector<std::string>& parts) {
    // FXFWD/RATE/CCY1/CCY2/TENOR.
    if (parts.size() != 5 || !metric_is(parts[1], ore_metric_spec::rate))
        return std::nullopt;
    if (!is_currency_code(parts[2]) || !is_currency_code(parts[3]))
        return std::nullopt;
    fx_market_data_identifier id;
    id.pair = to_upper(parts[2]) + to_upper(parts[3]);
    id.type = instrument_type::quote;
    id.quote_type = fx_quote_type::fwd;
    id.point = to_lower(parts[4]);
    return id;
}

std::optional<market_data_identifier> from_ir_swap(const std::vector<std::string>& parts) {
    // IR_SWAP/METRIC/CCY/SETTLE/TENOR/POINT, and the seven-segment form
    // IR_SWAP/METRIC/CCY/INDEX/SETTLE/TENOR/POINT that names the index the swap
    // is written against. The settlement segment is a per-currency spot lag,
    // recorded only when it is not the 2D the forward emits by default -- so the
    // ordinary key keeps one URI rather than two.
    if (parts.size() != 6 && parts.size() != 7)
        return std::nullopt;
    const auto m = parse_metric(parts[1]);
    if (!m)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::quote;
    id.quote_type = ir_quote_type::ir_swap;
    id.metric = *m;
    const auto named_index = parts.size() == 7;
    if (named_index) {
        if (const auto idx = parse_index(parts[3])) {
            id.index = *idx;
            record_index_spelling(id, parts[3]);
        } else {
            // A token that names no benchmark family: the corpus writes
            // NoDiscount in the index slot, which marks a curve rather than a
            // rate. There is nothing to classify it with, so the token is
            // carried whole and emitted whole, the way a basis swap's named
            // basis is.
            id.index_spelling = parts[3];
        }
    }
    const auto settle = named_index ? parts[4] : parts[3];
    if (settle != "2D")
        id.settle = settle;
    id.tenor = to_lower(parts[named_index ? 5 : 4]);
    id.point = to_lower(parts[named_index ? 6 : 5]);
    return id;
}

std::optional<market_data_identifier> from_ir_discount(const std::vector<std::string>& parts) {
    // DISCOUNT/METRIC/CCY/CURVE/TENOR, where CURVE is the name curveconfig.xml
    // gives the curve and TENOR the maturity sampled off it. The name is kept
    // whole and recorded always: the corpus carries USD-SOFR-3M, USD-FedFunds
    // and the bare currency, none of which a currency-plus-tenor rule rebuilds.
    if (parts.size() != 5)
        return std::nullopt;
    const auto m = parse_metric(parts[1]);
    if (!m)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::quote;
    id.quote_type = ir_quote_type::discount;
    id.metric = *m;
    id.curve_id = parts[3];
    id.tenor = to_lower(parts[4]);
    return id;
}

std::optional<market_data_identifier> from_ir_zero(const std::vector<std::string>& parts) {
    // ZERO/METRIC/CCY/CURVE_ID/DAY_COUNT/TENOR -- the curve id is whatever name
    // the producer gave the zero curve, and the day count the token beside it.
    if (parts.size() != 6)
        return std::nullopt;
    const auto m = parse_metric(parts[1]);
    if (!m)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::quote;
    id.quote_type = ir_quote_type::zero;
    id.metric = *m;
    id.curve_id = parts[3];
    id.day_count = parts[4];
    id.point = to_lower(parts[5]);
    return id;
}

std::optional<market_data_identifier> from_ir_indexed(ir_quote_type qt,
                                                      const std::vector<std::string>& parts) {
    // TYPE/METRIC/CCY/INDEX/TENOR/POINT — the families whose qualifier includes
    // the index (MM, FRA, IMM_FRA, BASIS_SWAP, ZERO).
    if (parts.size() != 6)
        return std::nullopt;
    const auto m = parse_metric(parts[1]);
    const auto idx = parse_index(parts[3]);
    if (!m || !idx)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::quote;
    id.quote_type = qt;
    id.metric = *m;
    id.index = *idx;
    record_index_spelling(id, parts[3]);
    id.tenor = to_lower(parts[4]);
    id.point = to_lower(parts[5]);
    return id;
}

std::optional<market_data_identifier> from_ir_future(ir_quote_type qt,
                                                     const std::vector<std::string>& parts) {
    // TYPE/METRIC/CCY/CONTRACT_MONTH/CONTRACT_CODE/TENOR -- the delivery month and
    // the exchange-qualified contract code sit between the currency and the
    // underlying index tenor (MM_FUTURE, OI_FUTURE).
    if (parts.size() != 6)
        return std::nullopt;
    const auto m = parse_metric(parts[1]);
    if (!m)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::quote;
    id.quote_type = qt;
    id.metric = *m;
    id.contract_month = parts[3];
    id.contract_code = parts[4];
    id.tenor = to_lower(parts[5]);
    return id;
}

std::optional<market_data_identifier> from_ir_basis_swap(const std::vector<std::string>& parts) {
    // BASIS_SWAP/BASIS_SPREAD/FIRST_TENOR/SECOND_TENOR/CCY/MATURITY -- the two
    // index tenors come first, then the single currency, then the maturity. A
    // seventh segment names the basis itself, which sits between the currency and
    // the maturity.
    if (parts.size() != 6 && parts.size() != 7)
        return std::nullopt;
    const auto m = parse_metric(parts[1]);
    if (!m)
        return std::nullopt;
    ir_market_data_identifier id;
    id.type = instrument_type::quote;
    id.quote_type = ir_quote_type::basis_swap;
    id.metric = *m;
    id.tenor = to_lower(parts[2]);
    id.second_tenor = to_lower(parts[3]);
    id.ccy = to_upper(parts[4]);
    if (parts.size() == 7) {
        // A named basis has no index family to resolve to, so the token is kept
        // whole -- the same field that carries a producer's spelling when there
        // is a family to spell.
        id.index_spelling = parts[5];
        id.point = to_lower(parts[6]);
    } else {
        id.point = to_lower(parts[5]);
    }
    return id;
}

std::optional<market_data_identifier>
from_ir_cross_currency(ir_quote_type qt, const std::vector<std::string>& parts) {
    // TYPE/METRIC/CCY1/TENOR1/CCY2/TENOR2/MATURITY -- two currencies and two
    // index tenors, then the maturity (CC_BASIS_SWAP, CC_FIX_FLOAT_SWAP).
    if (parts.size() != 7)
        return std::nullopt;
    const auto m = parse_metric(parts[1]);
    if (!m)
        return std::nullopt;
    ir_market_data_identifier id;
    id.type = instrument_type::quote;
    id.quote_type = qt;
    id.metric = *m;
    id.ccy = to_upper(parts[2]);
    id.tenor = to_lower(parts[3]);
    id.second_ccy = to_upper(parts[4]);
    id.second_tenor = to_lower(parts[5]);
    id.point = to_lower(parts[6]);
    return id;
}

std::optional<market_data_identifier> from_ir_no_index(ir_quote_type qt,
                                                       const std::vector<std::string>& parts) {
    // TYPE/METRIC/CCY/TENOR/POINT — the xccy/BMA families whose qualifier is just
    // ccy/tenor (CC_BASIS_SWAP, CC_FIX_FLOAT_SWAP, BMA_SWAP).
    if (parts.size() != 5)
        return std::nullopt;
    const auto m = parse_metric(parts[1]);
    if (!m)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::quote;
    id.quote_type = qt;
    id.metric = *m;
    id.tenor = to_lower(parts[3]);
    id.point = to_lower(parts[4]);
    return id;
}

std::optional<market_data_identifier>
from_ir_capfloor_shift(const std::vector<std::string>& parts) {
    // CAPFLOOR/SHIFT/CCY/FLOAT_TENOR -- the displaced log-normal shift, keyed
    // by currency and tenor with no surface coordinate.
    if (parts.size() != 4 || !metric_is(parts[1], ore_vol_spec::shift))
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::vol;
    id.quote_type = ir_quote_type::capfloor;
    id.tenor = to_lower(parts[3]);
    volatility_surface_point v;
    v.model_subtype = volatility_model_subtype::shift;
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier> from_ir_capfloor(const std::vector<std::string>& parts) {
    // CAPFLOOR/MODEL/CCY/MATURITY/FLOAT_TENOR/SHIFT/STRIP/STRIKE. The metric
    // segment is the vol model, so RATE_NVOL and RATE_LNVOL are the same
    // family and differ only in which surface they quote.
    if (parts.size() != 8)
        return std::nullopt;
    const auto model = parse_vol_model(parts[1]);
    if (!model)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::vol;
    id.quote_type = ir_quote_type::capfloor;
    id.tenor = to_lower(parts[4]);
    id.shift = to_lower(parts[5]);
    id.strip = to_lower(parts[6]);
    id.point = to_lower(parts[3]) + "," + to_lower(parts[4]) + "," + to_lower(parts[5]) + "," +
               to_lower(parts[6]) + "," + to_lower(parts[7]);
    volatility_surface_point v;
    v.model_subtype = *model;
    v.expiry = to_upper(parts[3]);
    v.strike = to_upper(parts[7]);
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier> from_ir_bond_option(const std::vector<std::string>& parts) {
    // BOND_OPTION/MODEL/UNDERLYING/EXPIRY/BOND_TENOR/STRIKE -- the swaption
    // shape over a bond. The third segment is a generic underlying name, not a
    // currency, so it is taken verbatim.
    if (parts.size() != 6)
        return std::nullopt;
    const auto model = parse_vol_model(parts[1]);
    if (!model)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::vol;
    id.quote_type = ir_quote_type::bond_option;
    id.tenor = to_lower(parts[4]);
    id.point = to_lower(parts[3]) + "," + to_lower(parts[4]) + "," + to_lower(parts[5]);
    volatility_surface_point v;
    v.model_subtype = *model;
    v.expiry = to_upper(parts[3]);
    v.strike = to_upper(parts[5]);
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier> from_ir_swaption(const std::vector<std::string>& parts) {
    // SWAPTION/MODEL/CCY/[INDEX/]EXPIRY/TENOR/[MARKER/]VALUE -- six, seven or
    // eight segments, because the index and the convention marker that carries
    // the smile shift are each optional. A seven-segment key is read by asking
    // whether its fourth segment names an index: the corpus's other seven-segment
    // form writes the marker there, and no marker is an index family.
    //
    // The forward emits either branch (typed vol struct or point composite) in
    // the same shape; both collapse into the vol struct plus the serialised
    // point, matching what the parser builds for a type=vol URI.
    if (parts.size() < 6 || parts.size() > 8)
        return std::nullopt;
    const auto model = parse_vol_model(parts[1]);
    if (!model)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper(parts[2]);
    id.type = instrument_type::vol;
    auto at = std::size_t{3};
    if (parts.size() >= 7) {
        if (const auto idx = parse_index(parts[3])) {
            id.index = *idx;
            record_index_spelling(id, parts[3]);
            at = 4;
        }
    }
    const auto remaining = parts.size() - at;
    if (remaining != 3 && remaining != 4)
        return std::nullopt;
    volatility_surface_point v;
    v.expiry = to_upper(parts[at]);
    id.tenor = to_lower(parts[at + 1]);
    const auto expiry = to_lower(parts[at]);
    const auto tenor = to_lower(parts[at + 1]);
    if (remaining == 4) {
        // The marker keeps the case it arrives with: the corpus writes Smile,
        // and the key has to read back as it came.
        v.delta_type = parts[at + 2];
        v.strike = to_upper(parts[at + 3]);
        id.point = expiry + "," + tenor + "," + *v.delta_type + "," + to_lower(parts[at + 3]);
    } else {
        v.strike = to_upper(parts[at + 2]);
        id.point = expiry + "," + tenor + "," + to_lower(parts[at + 2]);
    }
    v.model_subtype = *model;
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier> from_equity_option(const std::vector<std::string>& parts) {
    // The three shapes the forward emits. Which one this is follows from the
    // segment count alone, because the call/put moves: the seven-segment form
    // puts it after the strike, the nine-segment delta form puts it before the
    // strike, and the six-segment form has none.
    if (parts.size() != 6 && parts.size() != 7 && parts.size() != 9)
        return std::nullopt;
    const auto model = parse_vol_model(parts[1]);
    if (!model)
        return std::nullopt;
    equity_market_data_identifier id;
    id.ticker = parts[2];
    id.ccy = to_upper(parts[3]);
    id.type = instrument_type::vol;
    // The parser keeps the point composite lowercased and the surface's own
    // coordinates uppercased, so the inverse builds both the same way.
    std::string point;
    for (auto i = std::size_t{4}; i < parts.size(); ++i) {
        if (i > 4)
            point += ",";
        point += to_lower(parts[i]);
    }
    id.point = std::move(point);
    volatility_surface_point v;
    v.model_subtype = *model;
    // Verbatim, not uppercased: the corpus writes these in mixed case
    // (AtmDeltaNeutral, Spot, Call) and the spelling is data.
    v.expiry = parts[4];
    if (parts.size() == 6) {
        v.strike = parts[5];
    } else if (parts.size() == 7) {
        v.strike = parts[5];
        v.call_put = parts[6];
    } else {
        v.delta_type = parts[5];
        v.premium_type = parts[6];
        v.call_put = parts[7];
        v.strike = parts[8];
    }
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier> from_equity_spot(const std::vector<std::string>& parts) {
    // EQUITY/PRICE/TICKER/CCY — scalar, no point.
    if (parts.size() != 4 || !metric_is(parts[1], ore_metric_spec::price))
        return std::nullopt;
    equity_market_data_identifier id;
    id.ticker = parts[2];
    id.ccy = to_upper(parts[3]);
    id.type = instrument_type::quote;
    id.quote_type = equity_quote_type::spot;
    return id;
}

std::optional<market_data_identifier> from_equity_curve(equity_quote_type qt,
                                                        const std::vector<std::string>& parts) {
    // EQUITY_FWD/PRICE/TICKER/CCY/TENOR, EQUITY_DIVIDEND/RATE/TICKER/CCY/TENOR.
    const auto expected =
        (qt == equity_quote_type::dividend) ? ore_metric_spec::rate : ore_metric_spec::price;
    if (parts.size() != 5 || !metric_is(parts[1], expected))
        return std::nullopt;
    equity_market_data_identifier id;
    id.ticker = parts[2];
    id.ccy = to_upper(parts[3]);
    id.type = instrument_type::quote;
    id.quote_type = qt;
    id.point = to_lower(parts[4]);
    return id;
}

std::optional<market_data_identifier> from_rating(rating_quote_type qt,
                                                  const std::vector<std::string>& parts) {
    // RATING/TRANSITION_PROBABILITY/PROVIDER/FROM/TO, and the four-segment form
    // that names only the provider. No currency and no tenor: the point is the
    // pair of grades, or the single empty part of the degenerate form.
    if (parts.size() != 4 && parts.size() != 5)
        return std::nullopt;
    if (!metric_is(parts[1], ore_rating_metric(qt)))
        return std::nullopt;
    rating_market_data_identifier id;
    id.provider_id = parts[2];
    id.type = instrument_type::quote;
    id.quote_type = qt;
    std::string point;
    for (auto i = std::size_t{3}; i < parts.size(); ++i) {
        if (i > 3)
            point += ",";
        point += to_lower(parts[i]);
    }
    id.point = std::move(point);
    return id;
}

std::optional<market_data_identifier> from_shape_profile(shape_profile_quote_type qt,
                                                         const std::vector<std::string>& parts) {
    // SHAPE_PROFILE/SHAPE_FACTOR/PROFILE/DATE/SECOND/PERIOD, and a seventh segment
    // holding the DST flag. No currency and no tenor: the coordinate is the date,
    // the second and the period, and the identifier carries it as its point.
    if (parts.size() != 6 && parts.size() != 7)
        return std::nullopt;
    if (!metric_is(parts[1], ore_shape_profile_metric(qt)))
        return std::nullopt;
    shape_profile_market_data_identifier id;
    id.profile_id = parts[2];
    id.type = instrument_type::quote;
    id.quote_type = qt;
    std::string point;
    for (auto i = std::size_t{3}; i < parts.size(); ++i) {
        if (i > 3)
            point += ",";
        point += to_lower(parts[i]);
    }
    id.point = std::move(point);
    return id;
}

std::optional<market_data_identifier> from_security(security_quote_type qt,
                                                    const std::vector<std::string>& parts) {
    // BOND/METRIC/SECURITY and RECOVERY_RATE/RATE/SECURITY -- three segments, with
    // no currency, no tenor and no point. The identifier is the whole key.
    if (parts.size() != 3 || !metric_is(parts[1], ore_security_metric(qt)))
        return std::nullopt;
    security_market_data_identifier id;
    id.security_id = parts[2];
    id.type = instrument_type::quote;
    id.quote_type = qt;
    return id;
}

std::optional<market_data_identifier> from_commodity_option(const std::vector<std::string>& parts) {
    // The same three shapes the equity option writes, with a commodity code where
    // the equity has a ticker. Which one this is follows from the segment count,
    // because the call/put moves: the seven-segment form puts it after the strike,
    // the nine-segment delta form puts it before the strike, and the six-segment
    // form has none.
    if (parts.size() != 6 && parts.size() != 7 && parts.size() != 9)
        return std::nullopt;
    const auto model = parse_vol_model(parts[1]);
    if (!model)
        return std::nullopt;
    commodity_market_data_identifier id;
    id.commodity_code = parts[2];
    id.ccy = to_upper(parts[3]);
    id.type = instrument_type::vol;
    id.quote_type = commodity_quote_type::option;
    std::string point;
    for (auto i = std::size_t{4}; i < parts.size(); ++i) {
        if (i > 4)
            point += ",";
        point += to_lower(parts[i]);
    }
    id.point = std::move(point);
    volatility_surface_point v;
    v.model_subtype = *model;
    // Verbatim, not uppercased: the corpus writes these in mixed case
    // (AtmDeltaNeutral, Spot, Fwd) and the spelling is data.
    v.expiry = parts[4];
    if (parts.size() == 6) {
        v.strike = parts[5];
    } else if (parts.size() == 7) {
        v.strike = parts[5];
        v.call_put = parts[6];
    } else {
        v.delta_type = parts[5];
        v.premium_type = parts[6];
        v.call_put = parts[7];
        v.strike = parts[8];
    }
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier> from_commodity_spot(const std::vector<std::string>& parts) {
    // COMMODITY/PRICE/CODE/CCY — scalar, no point.
    if (parts.size() != 4 || !metric_is(parts[1], ore_metric_spec::price))
        return std::nullopt;
    commodity_market_data_identifier id;
    id.commodity_code = parts[2];
    id.ccy = to_upper(parts[3]);
    id.type = instrument_type::quote;
    id.quote_type = commodity_quote_type::spot;
    return id;
}

std::optional<market_data_identifier> from_commodity_curve(commodity_quote_type qt,
                                                           const std::vector<std::string>& parts) {
    // COMMODITY_FWD/PRICE/CODE/CCY/TENOR.
    if (parts.size() != 5 || !metric_is(parts[1], ore_metric_spec::price))
        return std::nullopt;
    commodity_market_data_identifier id;
    id.commodity_code = parts[2];
    id.ccy = to_upper(parts[3]);
    id.type = instrument_type::quote;
    id.quote_type = qt;
    id.point = to_lower(parts[4]);
    return id;
}

std::optional<market_data_identifier> from_credit_curve(credit_quote_type qt,
                                                        const std::vector<std::string>& parts) {
    // CDS/CREDIT_SPREAD/ENTITY/SENIORITY/CCY/TENOR,
    // HAZARD_RATE/RATE/ENTITY/SENIORITY/CCY/TENOR — point = seniority,tenor.
    // ORE also writes a seven-segment form with the credit derivatives'
    // restructuring clause between the currency and the tenor -- the corpus
    // carries XR14 and MR14, and 1,717 of its 2,324 CDS keys -- so the point
    // carries the clause when it is there.
    const auto expected =
        (qt == credit_quote_type::cds) ? ore_metric_spec::credit_spread : ore_metric_spec::rate;
    if ((parts.size() != 6 && parts.size() != 7) || !metric_is(parts[1], expected))
        return std::nullopt;
    credit_market_data_identifier id;
    id.reference_entity = parts[2];
    id.ccy = to_upper(parts[4]);
    id.type = instrument_type::quote;
    id.quote_type = qt;
    if (parts.size() == 7)
        id.point = to_lower(parts[3]) + "," + to_lower(parts[5]) + "," + to_lower(parts[6]);
    else
        id.point = to_lower(parts[3]) + "," + to_lower(parts[5]);
    return id;
}

std::optional<market_data_identifier> from_credit_recovery(const std::vector<std::string>& parts) {
    // RECOVERY_RATE/RATE/ENTITY/SENIORITY/CCY — scalar; point is just the
    // seniority, plus the restructuring clause on the keys that name one.
    if ((parts.size() != 5 && parts.size() != 6) || !metric_is(parts[1], ore_metric_spec::rate))
        return std::nullopt;
    credit_market_data_identifier id;
    id.reference_entity = parts[2];
    id.ccy = to_upper(parts[4]);
    id.type = instrument_type::quote;
    id.quote_type = credit_quote_type::recovery_rate;
    id.point =
        parts.size() == 6 ? to_lower(parts[3]) + "," + to_lower(parts[5]) : to_lower(parts[3]);
    return id;
}

std::optional<market_data_identifier> from_credit_index(credit_quote_type qt,
                                                        const std::vector<std::string>& parts) {
    // CDS_INDEX/BASE_CORRELATION/INDEX/TENOR/DETACHMENT,
    // INDEX_CDS_TRANCHE/BASE_CORRELATION/INDEX/SERIES_TENOR/DETACHMENT — no ccy
    // dimension; point = tenor,detachment.
    if (parts.size() != 5 || !metric_is(parts[1], ore_metric_spec::base_correlation))
        return std::nullopt;
    credit_market_data_identifier id;
    id.reference_entity = parts[2];
    id.type = instrument_type::quote;
    id.quote_type = qt;
    id.point = to_lower(parts[3]) + "," + to_lower(parts[4]);
    return id;
}

std::optional<market_data_identifier>
from_credit_index_option(credit_quote_type qt, const std::vector<std::string>& parts) {
    // INDEX_CDS_OPTION/MODEL/INDEX/TENOR/EXPIRY/STRIKE -- no ccy dimension, and
    // the point carries the index tenor ahead of the surface's own coordinates.
    // The corpus also writes a four-segment term vol, with the tenor alone and no
    // expiry or strike: INDEX_CDS_OPTION/MODEL/INDEX/TENOR.
    if (parts.size() != 4 && parts.size() != 6)
        return std::nullopt;
    const auto model = parse_vol_model(parts[1]);
    if (!model)
        return std::nullopt;
    credit_market_data_identifier id;
    id.reference_entity = parts[2];
    id.type = instrument_type::vol;
    id.quote_type = qt;
    volatility_surface_point v;
    v.model_subtype = *model;
    if (parts.size() == 6) {
        v.expiry = to_upper(parts[4]);
        v.strike = to_upper(parts[5]);
        id.point = to_lower(parts[3]) + "," + to_lower(parts[4]) + "," + to_lower(parts[5]);
    } else {
        id.point = to_lower(parts[3]);
    }
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier>
from_inflation_capfloor(inflation_quote_type qt, const std::vector<std::string>& parts) {
    // ZC_INFLATIONCAPFLOOR/METRIC/INDEX/MATURITY/CAP_OR_FLOOR/STRIKE, and the
    // same shape for the year-on-year family. The metric is the surface: PRICE
    // or RATE_NVOL.
    if (parts.size() != 6)
        return std::nullopt;
    const auto model = parse_vol_model(parts[1]);
    if (!model)
        return std::nullopt;
    inflation_market_data_identifier id;
    id.index_code = to_upper(parts[2]);
    id.type = instrument_type::vol;
    id.quote_type = qt;
    id.point = to_lower(parts[3]) + "," + to_lower(parts[4]) + "," + to_lower(parts[5]);
    volatility_surface_point v;
    v.model_subtype = *model;
    v.expiry = to_upper(parts[3]);
    v.call_put = to_upper(parts[4]);
    v.strike = to_upper(parts[5]);
    id.vol = std::move(v);
    return id;
}

std::optional<market_data_identifier> from_inflation_swap(inflation_quote_type qt,
                                                          const std::vector<std::string>& parts) {
    // ZC_INFLATIONSWAP/RATE/INDEX/POINT, YY_INFLATIONSWAP/RATE/INDEX/POINT.
    if (parts.size() != 4 || !metric_is(parts[1], ore_metric_spec::rate))
        return std::nullopt;
    inflation_market_data_identifier id;
    id.index_code = to_upper(parts[2]);
    id.type = instrument_type::quote;
    id.quote_type = qt;
    id.point = to_lower(parts[3]);
    return id;
}

std::optional<market_data_identifier>
from_inflation_seasonality(const std::vector<std::string>& parts) {
    // SEASONALITY/RATE/MULT/INDEX/POINT — the forward emits the literal MULT in the
    // third segment; it is accepted verbatim and dropped.
    if (parts.size() != 5 || !metric_is(parts[1], ore_metric_spec::rate))
        return std::nullopt;
    inflation_market_data_identifier id;
    id.index_code = to_upper(parts[3]);
    id.type = instrument_type::quote;
    id.quote_type = inflation_quote_type::seasonality;
    id.point = to_lower(parts[4]);
    return id;
}

std::optional<market_data_identifier> from_correlation(const std::vector<std::string>& parts) {
    // CORRELATION/RATE/FACTOR_PAIR -- a scalar, and
    // CORRELATION/RATE/OPERAND1/OPERAND2/EXPIRY/STRIKE -- a surface. The two
    // operands take fixed positions, so the coordinate after them is the point.
    if (parts.size() < 3 || !metric_is(parts[1], ore_metric_spec::rate))
        return std::nullopt;
    correlation_market_data_identifier id;
    id.factor_pair = to_upper(parts[2]);
    id.type = instrument_type::quote;
    id.quote_type = correlation_quote_type::pairwise;
    if (parts.size() == 3)
        return id;
    if (parts.size() != 6)
        return std::nullopt;
    id.second_factor = to_upper(parts[3]);
    // Every compound point in this library is comma-separated in the
    // identifier, whatever separator the key spells it with. The forward joins
    // this one with a slash; storing it slash-joined here would make the two
    // disagree and the key would not read back.
    id.point = to_lower(parts[4]) + "," + to_lower(parts[5]);
    return id;
}

std::optional<market_data_identifier>
inverse_projection(const std::vector<std::string>& parts,
                   const ores::ore::market::fx_quote_convention_checker* checker) {
    const auto type = to_upper(parts[0]);
    if (type == ore_type_spec::fx)
        return from_fx_spot(parts, checker);
    if (type == ore_type_spec::fx_option)
        return from_fx_option(parts);
    if (type == ore_type_spec::fxfwd)
        return from_fx_fwd(parts);
    if (type == ore_type_spec::ir_swap)
        return from_ir_swap(parts);
    if (type == ore_type_spec::discount)
        return from_ir_discount(parts);
    if (type == ore_type_spec::bond_option)
        return from_ir_bond_option(parts);
    if (type == ore_type_spec::swaption)
        return from_ir_swaption(parts);
    if (type == ore_type_spec::capfloor) {
        // CAPFLOOR spells two instruments. At eight segments it is the rate
        // cap/floor vol surface; at six it is an inflation cap/floor price, the
        // shape the catalogue gives ZC_INFLATIONCAPFLOOR, under the type name
        // the corpus carries beside it for the same instrument. Both spellings
        // get their own quote type so a key round-trips to the name it arrived
        // under rather than to a normalised one.
        if (parts.size() == 8)
            return from_ir_capfloor(parts);
        if (parts.size() == 4)
            return from_ir_capfloor_shift(parts);
        if (parts.size() == 6)
            return from_inflation_capfloor(inflation_quote_type::cf_price, parts);
        return std::nullopt;
    }
    if (type == ore_type_spec::mm) {
        // MM is written both ways in the corpus, so the segment count decides
        // which shape this key is: ccy/index/settle/tenor or ccy/settle/tenor.
        const auto qt = parse_enum_lower<ir_quote_type>(type);
        if (!qt)
            return std::nullopt;
        return parts.size() == 6 ? from_ir_indexed(*qt, parts) : from_ir_no_index(*qt, parts);
    }
    if (type == ore_type_spec::basis_swap)
        return from_ir_basis_swap(parts);
    if (type == ore_type_spec::cc_basis_swap || type == ore_type_spec::cc_fix_float_swap) {
        const auto qt = parse_enum_lower<ir_quote_type>(type);
        return qt ? from_ir_cross_currency(*qt, parts) : std::nullopt;
    }
    if (type == ore_type_spec::zero)
        return from_ir_zero(parts);
    if (type == ore_type_spec::mm_future || type == ore_type_spec::oi_future) {
        const auto qt = parse_enum_lower<ir_quote_type>(type);
        return qt ? from_ir_future(*qt, parts) : std::nullopt;
    }
    // FRA and IMM_FRA carry no index: ORE writes ccy/start/length, where the
    // start is a tenor or an IMM date. The registry's shape table states the
    // same, and the corpus agrees with it.
    if (type == ore_type_spec::bma_swap || type == ore_type_spec::fra ||
        type == ore_type_spec::imm_fra) {
        const auto qt = parse_enum_lower<ir_quote_type>(type);
        return qt ? from_ir_no_index(*qt, parts) : std::nullopt;
    }
    if (type == ore_type_spec::equity_option)
        return from_equity_option(parts);
    if (type == ore_type_spec::equity)
        return from_equity_spot(parts);
    // The ORE spellings differ from the enum names for these types ("EQUITY_FWD" vs
    // the enumerator "fwd"), so they are dispatched explicitly rather than by
    // magic_enum name lookup -- mirroring the forward ore_type(quote_type) tables.
    if (type == ore_type_spec::equity_fwd)
        return from_equity_curve(equity_quote_type::fwd, parts);
    if (type == ore_type_spec::equity_dividend)
        return from_equity_curve(equity_quote_type::dividend, parts);
    if (type == ore_type_spec::commodity_option)
        return from_commodity_option(parts);
    if (type == ore_type_spec::commodity)
        return from_commodity_spot(parts);
    if (type == ore_type_spec::commodity_fwd)
        return from_commodity_curve(commodity_quote_type::fwd, parts);
    if (type == ore_type_spec::cpr)
        return from_security(security_quote_type::cpr, parts);
    if (type == ore_type_spec::cds || type == ore_type_spec::hazard_rate) {
        const auto qt = parse_enum_lower<credit_quote_type>(type);
        return qt ? from_credit_curve(*qt, parts) : std::nullopt;
    }
    if (type == ore_type_spec::recovery_rate) {
        // Three segments is a security-level recovery rate, whose entity is the
        // security; the credit forms name a seniority and a currency as well.
        if (parts.size() == 3)
            return from_security(security_quote_type::recovery_rate, parts);
        return from_credit_recovery(parts);
    }
    if (type == ore_type_spec::rating)
        return from_rating(rating_quote_type::transition_probability, parts);
    if (type == ore_type_spec::shape_profile)
        return from_shape_profile(shape_profile_quote_type::shape_factor, parts);
    if (type == ore_type_spec::bond) {
        if (parts.size() != 3)
            return std::nullopt;
        // BOND carries the metric in its second segment and nothing else, so the
        // metric selects the quote type.
        for (const auto qt : {security_quote_type::bond_price,
                              security_quote_type::bond_yield_spread,
                              security_quote_type::bond_conversion_factor}) {
            if (metric_is(parts[1], ore_security_metric(qt)))
                return from_security(qt, parts);
        }
        return std::nullopt;
    }
    if (type == ore_type_spec::cds_index || type == ore_type_spec::index_cds_tranche) {
        const auto qt = parse_enum_lower<credit_quote_type>(type);
        return qt ? from_credit_index(*qt, parts) : std::nullopt;
    }
    if (type == ore_type_spec::index_cds_option) {
        const auto qt = parse_enum_lower<credit_quote_type>(type);
        return qt ? from_credit_index_option(*qt, parts) : std::nullopt;
    }
    if (type == ore_type_spec::zc_inflation_swap)
        return from_inflation_swap(inflation_quote_type::zc_swap, parts);
    if (type == ore_type_spec::yy_inflation_swap)
        return from_inflation_swap(inflation_quote_type::yy_swap, parts);
    if (type == ore_type_spec::seasonality)
        return from_inflation_seasonality(parts);
    if (type == ore_type_spec::zc_inflation_capfloor)
        return from_inflation_capfloor(inflation_quote_type::zc_capfloor, parts);
    if (type == ore_type_spec::yy_inflation_capfloor)
        return from_inflation_capfloor(inflation_quote_type::yy_capfloor, parts);
    if (type == ore_type_spec::correlation)
        return from_correlation(parts);
    // No oresmd mapping: BOND, FX_OPTION, CAPFLOOR, EQUITY_OPTION,
    // COMMODITY_OPTION, ZC_INFLATIONCAPFLOOR, YY_INFLATIONCAPFLOOR, and any type
    // the registry does not know.
    return std::nullopt;
}

}

namespace ores::marketdata::core {

std::optional<std::string>
oresmd_projections::to_index_name(const domain::market_data_identifier& identifier) {
    if (const auto* ir = std::get_if<ir_market_data_identifier>(&identifier))
        return index_name_ir(*ir);
    if (const auto* fx = std::get_if<fx_market_data_identifier>(&identifier))
        return index_name_fx(*fx);
    if (const auto* commodity = std::get_if<commodity_market_data_identifier>(&identifier))
        return index_name_commodity(*commodity);
    if (const auto* power = std::get_if<power_market_data_identifier>(&identifier))
        return index_name_power(*power);
    if (const auto* generic = std::get_if<generic_market_data_identifier>(&identifier))
        return index_name_generic(*generic);
    if (const auto* inflation = std::get_if<inflation_market_data_identifier>(&identifier))
        return index_name_inflation(*inflation);
    if (const auto* equity = std::get_if<equity_market_data_identifier>(&identifier))
        return index_name_equity(*equity);
    if (const auto* security = std::get_if<security_market_data_identifier>(&identifier))
        return index_name_security(*security);
    return std::nullopt;
}

std::optional<std::string>
oresmd_projections::to_curve_key(const domain::market_data_identifier& identifier) {
    if (const auto* ir = std::get_if<ir_market_data_identifier>(&identifier))
        return curve_key_ir(*ir);
    return std::nullopt;
}

std::optional<std::string>
oresmd_projections::to_quote_key(const domain::market_data_identifier& identifier) {
    return std::visit(
        [](const auto& id) -> std::optional<std::string> {
            using T = std::decay_t<decltype(id)>;
            if constexpr (std::is_same_v<T, fx_market_data_identifier>)
                return quote_key_fx(id);
            else if constexpr (std::is_same_v<T, ir_market_data_identifier>)
                return quote_key_ir(id);
            else if constexpr (std::is_same_v<T, equity_market_data_identifier>)
                return quote_key_equity(id);
            else if constexpr (std::is_same_v<T, credit_market_data_identifier>)
                return quote_key_credit(id);
            else if constexpr (std::is_same_v<T, commodity_market_data_identifier>)
                return quote_key_commodity(id);
            else if constexpr (std::is_same_v<T, inflation_market_data_identifier>)
                return quote_key_inflation(id);
            else if constexpr (std::is_same_v<T, correlation_market_data_identifier>)
                return quote_key_correlation(id);
            else if constexpr (std::is_same_v<T, security_market_data_identifier>)
                return quote_key_security(id);
            else if constexpr (std::is_same_v<T, shape_profile_market_data_identifier>)
                return quote_key_shape_profile(id);
            else if constexpr (std::is_same_v<T, rating_market_data_identifier>)
                return quote_key_rating(id);
            else if constexpr (std::is_same_v<T, power_market_data_identifier>)
                return quote_key_power(id);
            else if constexpr (std::is_same_v<T, generic_market_data_identifier>)
                return quote_key_generic(id);
        },
        identifier);
}

std::optional<market_series_key>
oresmd_projections::split_market_series_key(const std::string& key) {
    std::vector<std::string> parts;
    std::stringstream ss(key);
    std::string tok;
    while (std::getline(ss, tok, '/'))
        parts.push_back(tok);
    if (parts.size() < 3)
        return std::nullopt;

    market_series_key result;
    result.series_type = parts[0];
    result.metric = parts[1];
    result.qualifier = parts[2];
    for (std::size_t i = 3; i < parts.size(); ++i)
        result.qualifier += "/" + parts[i];
    return result;
}

std::optional<domain::market_data_identifier>
oresmd_projections::from_ore_key(const std::string& key) {
    const auto parts = split_key(key);
    if (!parts)
        return std::nullopt;
    return inverse_projection(*parts, nullptr);
}

std::optional<domain::market_data_identifier>
oresmd_projections::from_ore_key(const std::string& key,
                                 const ores::ore::market::fx_quote_convention_checker& checker) {
    const auto parts = split_key(key);
    if (!parts)
        return std::nullopt;
    return inverse_projection(*parts, &checker);
}

std::optional<domain::market_data_identifier>
oresmd_projections::from_index_name(const std::string& index_name) {
    // The reverse of index_name_fx(): FX-SOURCE-CCY1-CCY2. The four segments
    // cannot be read as an interest-rate name, whose split accepts two or three,
    // so the two never compete for the same string.
    if (index_name.starts_with("FX-")) {
        const auto fx_parts = split_on_dash(index_name);
        if (fx_parts.size() == 4 && !fx_parts[1].empty() && is_currency_code(fx_parts[2]) &&
            is_currency_code(fx_parts[3])) {
            fx_market_data_identifier id;
            id.type = instrument_type::fixing;
            id.pair = to_upper(fx_parts[2]) + to_upper(fx_parts[3]);
            id.source = to_lower(fx_parts[1]);
            if (fx_parts[1] != to_upper(fx_parts[1]))
                id.source_spelling = fx_parts[1];
            return id;
        }
        return std::nullopt;
    }

    // The reverse of index_name_power(): POWER-COMMNAME[-DATE[-START-END[-DST]]].
    // ORE's own parser reads the first dash-delimited token as the commodity name
    // and admits three token counts, so the name's boundary is the first dash and
    // a colon inside that token is free. The DST flag is required as ORE spells
    // it: a flag written any other way could not be read back as it arrived.
    if (index_name.starts_with("POWER-")) {
        const auto parts = split_on_dash(index_name.substr(6));
        if (parts.empty() || parts[0].empty())
            return std::nullopt;
        power_market_data_identifier id;
        id.type = instrument_type::fixing;
        id.commodity_code = to_upper(parts[0]);
        if (parts.size() == 1)
            return id;
        // The shape check runs on the lower-cased tail, so it cannot tell the flag
        // ORE spells from one a producer invented. This refuses the latter, and is
        // what makes the spelling a rule rather than an accident of lower-casing.
        if (parts.size() == 7 && parts[6] != "DST")
            return std::nullopt;
        std::string tail = parts[1];
        for (std::size_t i = 2; i < parts.size(); ++i)
            tail += "-" + parts[i];
        tail = to_lower(tail);
        if (!power_delivery_is_the_shape(tail))
            return std::nullopt;
        id.delivery = tail;
        return id;
    }

    // The reverse of index_name_commodity(): COMM-COMMNAME[-YYYY-MM]. The period
    // is the last two tokens and the code is the one token before it, so a name
    // whose code carries a dash is refused rather than split at a guess.
    if (index_name.starts_with("COMM-")) {
        const auto parts = split_on_dash(index_name.substr(5));
        if (parts.empty() || parts[0].empty())
            return std::nullopt;
        commodity_market_data_identifier id;
        id.type = instrument_type::fixing;
        id.commodity_code = to_upper(parts[0]);
        if (parts.size() == 1)
            return id;
        if (parts.size() != 3)
            return std::nullopt;
        const auto tail = to_lower(parts[1] + "-" + parts[2]);
        if (!contract_month_is_the_shape(tail))
            return std::nullopt;
        id.delivery = tail;
        return id;
    }

    // The reverse of index_name_generic(): GENERIC-<NAME>. ORE's generic index is
    // whatever its producer called it, so the tail is taken whole, and its case is
    // kept as a spelling: the identifier holds the upper-cased name while the name
    // reads back as it arrived. A tail carrying a slash is not an index name but a
    // GENERIC-MD/... market-data key, which Products/Input/fixings.csv misfiles as
    // a fixing.
    constexpr std::string_view generic_prefix{"GENERIC-"};
    if (index_name.starts_with(generic_prefix)) {
        const auto name = index_name.substr(generic_prefix.size());
        if (name.empty() || name.contains('/'))
            return std::nullopt;
        generic_market_data_identifier id;
        id.type = instrument_type::fixing;
        id.name = to_upper(name);
        if (name != id.name)
            id.name_spelling = name;
        return id;
    }

    // The reverse of index_name_equity(): EQ-<TICKER>. The tail is the ticker as
    // ORE writes it, identifier scheme included, and a fixing's currency is not part
    // of it: the parser requires no ccy for a fixing, and equality here must produce
    // an identifier the parser accepts. A tail carrying a slash is not an index name.
    constexpr std::string_view equity_prefix{"EQ-"};
    if (index_name.starts_with(equity_prefix)) {
        const auto ticker = index_name.substr(equity_prefix.size());
        if (ticker.empty() || ticker.contains('/'))
            return std::nullopt;
        equity_market_data_identifier id;
        id.type = instrument_type::fixing;
        id.ticker = to_upper(ticker);
        return id;
    }

    // The reverse of index_name_security(): BOND-<SECURITY>[-<MONTH>]. The security
    // carries the scheme its quote keys spell, whose separators are colons and dots,
    // and an ISIN carries no dash, so the last two tokens are the contract month and
    // everything before them is the security. Any other token count is refused
    // rather than split at a guess.
    constexpr std::string_view security_prefix{"BOND-"};
    if (index_name.starts_with(security_prefix)) {
        const auto rest = index_name.substr(security_prefix.size());
        const auto parts = split_on_dash(rest);
        if (parts.empty() || parts[0].empty())
            return std::nullopt;
        security_market_data_identifier id;
        id.type = instrument_type::fixing;
        if (parts.size() == 1) {
            id.security_id = to_upper(rest);
            return id;
        }
        if (parts.size() != 3)
            return std::nullopt;
        const auto month = to_lower(parts[1] + "-" + parts[2]);
        if (!contract_month_is_the_shape(month))
            return std::nullopt;
        id.security_id = to_upper(parts[0]);
        id.delivery = month;
        return id;
    }

    // The reverse of index_name_inflation(): the code alone, and only a code the
    // model carries spelled as it carries it. A dash-less token that is not one
    // keeps its name rather than being given an inflation identity, because nothing
    // in the string says which class it belongs to.
    if (!index_name.contains('-') && inflation_index_code_is_known(index_name)) {
        inflation_market_data_identifier id;
        id.type = instrument_type::fixing;
        id.index_code = index_name;
        return id;
    }

    // The reverse of index_name_ir(): CCY-FAMILY (overnight, no tenor) or
    // CCY-FAMILY-TENOR. Segment spelling normalisation mirrors the parser:
    // ccy upper, family and tenor lower.
    const auto parts = split_index_name(index_name);
    if (!parts)
        return std::nullopt;
    const auto idx = parse_index((*parts)[1]);
    if (!idx)
        return std::nullopt;
    ir_market_data_identifier id;
    id.ccy = to_upper((*parts)[0]);
    id.type = instrument_type::fixing;
    id.index = *idx;
    // A family ORE spells differently keeps the spelling it arrived under, so the
    // identifier projects back to the name it came from rather than to the enum's
    // own spelling of it.
    record_index_name_spelling(id, (*parts)[1]);
    if (parts->size() == 3) {
        id.tenor = to_lower((*parts)[2]);
    } else if (requires_tenor(*idx)) {
        // A term family without a tenor could not have come from the forward
        // projection, which emits the two-segment form for overnight families
        // alone, and the parser rejects it. Mirror both rather than accept a
        // name the URI it produces could not be read back from.
        return std::nullopt;
    }
    return id;
}

bool oresmd_projections::is_scalar(const domain::market_data_identifier& identifier) {
    return std::visit(
        [](const auto& id) {
            if (id.type != instrument_type::quote && id.type != instrument_type::fixing)
                return false;
            if constexpr (requires { id.point; }) {
                if (id.point)
                    return false;
            }
            if constexpr (requires { id.vol; }) {
                if (id.vol)
                    return false;
            }
            return true;
        },
        identifier);
}

}
