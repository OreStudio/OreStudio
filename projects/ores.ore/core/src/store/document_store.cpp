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

#include "ores.ore.core/store/document_store.hpp"
#include "ores.analytics.core/repository/pricing_model_config_repository.hpp"
#include "ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"
#include "ores.analytics.core/repository/pricing_model_product_repository.hpp"
#include "ores.analytics.core/repository/todays_market_collection_repository.hpp"
#include "ores.analytics.core/repository/todays_market_config_repository.hpp"
#include "ores.analytics.core/repository/todays_market_configuration_binding_repository.hpp"
#include "ores.analytics.core/repository/todays_market_configuration_repository.hpp"
#include "ores.analytics.core/repository/todays_market_entry_repository.hpp"
#include "ores.ore.core/store/detail/store_helpers.hpp"
#include "ores.refdata.core/repository/average_ois_convention_repository.hpp"
#include "ores.refdata.core/repository/base_correlation_config_repository.hpp"
#include "ores.refdata.core/repository/bma_basis_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/bond_future_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/bond_yield_convention_repository.hpp"
#include "ores.refdata.core/repository/cap_floor_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cds_convention_repository.hpp"
#include "ores.refdata.core/repository/cds_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cds_volatility_term_repository.hpp"
#include "ores.refdata.core/repository/cms_spread_option_convention_repository.hpp"
#include "ores.refdata.core/repository/commodity_forward_convention_repository.hpp"
#include "ores.refdata.core/repository/commodity_future_convention_repository.hpp"
#include "ores.refdata.core/repository/commodity_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/cross_currency_basis_convention_repository.hpp"
#include "ores.refdata.core/repository/cross_currency_fix_float_convention_repository.hpp"
#include "ores.refdata.core/repository/curve_bootstrap_config_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_configuration_section_repository.hpp"
#include "ores.refdata.core/repository/curve_correlation_config_repository.hpp"
#include "ores.refdata.core/repository/curve_definition_repository.hpp"
#include "ores.refdata.core/repository/curve_global_report_repository.hpp"
#include "ores.refdata.core/repository/curve_parametric_smile_parameter_repository.hpp"
#include "ores.refdata.core/repository/curve_parametric_smile_repository.hpp"
#include "ores.refdata.core/repository/curve_quote_repository.hpp"
#include "ores.refdata.core/repository/curve_report_configuration_repository.hpp"
#include "ores.refdata.core/repository/curve_security_config_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_curve_repository.hpp"
#include "ores.refdata.core/repository/curve_segment_repository.hpp"
#include "ores.refdata.core/repository/curve_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/default_curve_config_repository.hpp"
#include "ores.refdata.core/repository/default_curve_configuration_repository.hpp"
#include "ores.refdata.core/repository/deposit_convention_repository.hpp"
#include "ores.refdata.core/repository/equity_curve_config_repository.hpp"
#include "ores.refdata.core/repository/equity_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/fra_convention_repository.hpp"
#include "ores.refdata.core/repository/future_convention_repository.hpp"
#include "ores.refdata.core/repository/fx_option_convention_repository.hpp"
#include "ores.refdata.core/repository/fx_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/ibor_index_convention_repository.hpp"
#include "ores.refdata.core/repository/inflation_cap_floor_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/inflation_curve_config_repository.hpp"
#include "ores.refdata.core/repository/inflation_seasonality_factor_repository.hpp"
#include "ores.refdata.core/repository/inflation_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/intraday_power_curve_config_repository.hpp"
#include "ores.refdata.core/repository/intraday_power_load_convention_repository.hpp"
#include "ores.refdata.core/repository/ois_convention_repository.hpp"
#include "ores.refdata.core/repository/overnight_index_convention_repository.hpp"
#include "ores.refdata.core/repository/swap_convention_repository.hpp"
#include "ores.refdata.core/repository/swap_index_convention_repository.hpp"
#include "ores.refdata.core/repository/swaption_volatility_config_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/tenor_basis_two_swap_convention_repository.hpp"
#include "ores.refdata.core/repository/yield_curve_config_repository.hpp"
#include "ores.refdata.core/repository/zero_convention_repository.hpp"
#include "ores.refdata.core/repository/zero_inflation_index_convention_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <map>
#include <set>
#include <utility>

namespace ores::ore::store {

using namespace ores::analytics::repository;
using namespace ores::refdata::repository;
using database::context;
using detail::read_one;
using detail::read_where;
using detail::stamp_party;

namespace {

// A party's convention is keyed by its ORE id and the party, so a second import
// of the same id replaces the row the party holds rather than colliding with it.
template <typename Repository, typename Row>
void replace_party_rows(const context& ctx, Repository repo, std::vector<Row> rows) {
    if (rows.empty())
        return;
    std::map<std::pair<std::string, boost::uuids::uuid>, int> held;
    for (const auto& r : repo.read_latest(ctx))
        held[{r.id, r.party_id}] = r.version;
    for (auto& r : rows) {
        const auto it = held.find({r.id, r.party_id});
        r.version = it == held.end() ? 0 : it->second;
    }
    repo.write(ctx, rows);
}

// A world convention is the tenant's, whoever imports a document naming it, so
// one the tenant holds is never replaced by an import.
template <typename Repository, typename Row>
void add_missing_world_rows(const context& ctx,
                            Repository repo,
                            const std::vector<Row>& rows,
                            std::vector<std::string>& kept) {
    std::set<std::string> held;
    for (const auto& r : repo.read_latest(ctx))
        held.insert(r.id);
    std::vector<Row> missing;
    for (const auto& r : rows) {
        if (held.contains(r.id))
            kept.push_back(r.id);
        else
            missing.push_back(r);
    }
    if (!missing.empty())
        repo.write(ctx, missing);
}

}

void write(const context& ctx, domain::mapped_pricing_engines v) {
    stamp_party(ctx, v);
    pricing_model_config_repository().write(ctx, v.config);
    pricing_model_product_repository().write(ctx, v.products);
    pricing_model_product_parameter_repository().write(ctx, v.parameters);
}

domain::mapped_pricing_engines read_pricing_engines(const context& ctx,
                                                    const boost::uuids::uuid& config_id) {
    domain::mapped_pricing_engines r;
    r.config =
        read_one(ctx, pricing_model_config_repository(), "pricing engines document", config_id);
    const auto of_config = [&](const auto& row) {
        return row.pricing_model_config_id == config_id;
    };
    r.products = read_where(ctx, pricing_model_product_repository(), of_config);
    r.parameters = read_where(ctx, pricing_model_product_parameter_repository(), of_config);
    return r;
}

void write(const context& ctx, domain::mapped_todays_market v) {
    stamp_party(ctx, v);
    todays_market_config_repository().write(ctx, v.config);
    todays_market_collection_repository().write(ctx, v.collections);
    todays_market_entry_repository().write(ctx, v.entries);
    todays_market_configuration_repository().write(ctx, v.configurations);
    todays_market_configuration_binding_repository().write(ctx, v.bindings);
}

domain::mapped_todays_market read_todays_market(const context& ctx,
                                                const boost::uuids::uuid& config_id) {
    domain::mapped_todays_market r;
    r.config =
        read_one(ctx, todays_market_config_repository(), "today's market document", config_id);
    const auto of_config = [&](const auto& row) {
        return row.todays_market_config_id == config_id;
    };
    r.collections = read_where(ctx, todays_market_collection_repository(), of_config);
    r.entries = read_where(ctx, todays_market_entry_repository(), of_config);
    r.configurations = read_where(ctx, todays_market_configuration_repository(), of_config);
    std::set<boost::uuids::uuid> configurations;
    for (const auto& c : r.configurations)
        configurations.insert(c.id);
    r.bindings =
        read_where(ctx, todays_market_configuration_binding_repository(), [&](const auto& row) {
            return configurations.contains(row.todays_market_configuration_id);
        });
    return r;
}

void write(const context& ctx, domain::mapped_curve_configuration v) {
    stamp_party(ctx, v);
    curve_configuration_repository().write(ctx, v.config);

    curve_configuration_section_repository().write(ctx, v.sections);
    curve_global_report_repository().write(ctx, v.global_reports);
    curve_definition_repository().write(ctx, v.definitions);
    yield_curve_config_repository().write(ctx, v.yield_curves);
    equity_curve_config_repository().write(ctx, v.equity_curves);
    default_curve_config_repository().write(ctx, v.default_curves);
    default_curve_configuration_repository().write(ctx, v.default_curve_configurations);
    inflation_curve_config_repository().write(ctx, v.inflation_curves);
    inflation_seasonality_factor_repository().write(ctx, v.seasonality_factors);
    curve_security_config_repository().write(ctx, v.securities);
    intraday_power_curve_config_repository().write(ctx, v.intraday_power_curves);
    fx_volatility_config_repository().write(ctx, v.fx_volatilities);
    base_correlation_config_repository().write(ctx, v.base_correlations);
    curve_correlation_config_repository().write(ctx, v.correlations);
    curve_report_configuration_repository().write(ctx, v.report_configurations);
    cds_volatility_config_repository().write(ctx, v.cds_volatilities);
    cds_volatility_term_repository().write(ctx, v.cds_volatility_terms);
    curve_volatility_config_repository().write(ctx, v.volatility_configs);
    inflation_cap_floor_volatility_config_repository().write(ctx,
                                                             v.inflation_cap_floor_volatilities);
    swaption_volatility_config_repository().write(ctx, v.swaption_volatilities);
    cap_floor_volatility_config_repository().write(ctx, v.cap_floor_volatilities);
    curve_parametric_smile_repository().write(ctx, v.parametric_smiles);
    curve_parametric_smile_parameter_repository().write(ctx, v.parametric_smile_parameters);
    equity_volatility_config_repository().write(ctx, v.equity_volatilities);
    commodity_volatility_config_repository().write(ctx, v.commodity_volatilities);
    bond_future_volatility_config_repository().write(ctx, v.bond_future_volatilities);
    curve_bootstrap_config_repository().write(ctx, v.bootstrap_configs);
    curve_segment_repository().write(ctx, v.segments);
    curve_segment_curve_repository().write(ctx, v.segment_curves);
    curve_quote_repository().write(ctx, v.quotes);
}

domain::mapped_curve_configuration read_curve_configuration(const context& ctx,
                                                            const boost::uuids::uuid& config_id) {
    domain::mapped_curve_configuration r;
    r.config = read_one(ctx, curve_configuration_repository(), "curve configuration", config_id);
    const auto of_config = [&](const auto& row) {
        return row.curve_configuration_id == config_id;
    };
    r.sections = read_where(ctx, curve_configuration_section_repository(), of_config);
    r.global_reports = read_where(ctx, curve_global_report_repository(), of_config);
    r.definitions = read_where(ctx, curve_definition_repository(), of_config);
    std::set<boost::uuids::uuid> definitions;
    for (const auto& d : r.definitions)
        definitions.insert(d.id);
    const auto of_definition = [&](const auto& row) {
        return definitions.contains(row.curve_definition_id);
    };
    r.yield_curves = read_where(ctx, yield_curve_config_repository(), of_definition);
    r.equity_curves = read_where(ctx, equity_curve_config_repository(), of_definition);
    r.default_curves = read_where(ctx, default_curve_config_repository(), of_definition);
    r.default_curve_configurations =
        read_where(ctx, default_curve_configuration_repository(), of_definition);
    r.inflation_curves = read_where(ctx, inflation_curve_config_repository(), of_definition);
    r.seasonality_factors =
        read_where(ctx, inflation_seasonality_factor_repository(), of_definition);
    r.securities = read_where(ctx, curve_security_config_repository(), of_definition);
    r.intraday_power_curves =
        read_where(ctx, intraday_power_curve_config_repository(), of_definition);
    r.fx_volatilities = read_where(ctx, fx_volatility_config_repository(), of_definition);
    r.base_correlations = read_where(ctx, base_correlation_config_repository(), of_definition);
    r.correlations = read_where(ctx, curve_correlation_config_repository(), of_definition);
    r.report_configurations =
        read_where(ctx, curve_report_configuration_repository(), of_definition);
    r.cds_volatilities = read_where(ctx, cds_volatility_config_repository(), of_definition);
    r.cds_volatility_terms = read_where(ctx, cds_volatility_term_repository(), of_definition);
    r.volatility_configs = read_where(ctx, curve_volatility_config_repository(), of_definition);
    r.inflation_cap_floor_volatilities =
        read_where(ctx, inflation_cap_floor_volatility_config_repository(), of_definition);
    r.swaption_volatilities =
        read_where(ctx, swaption_volatility_config_repository(), of_definition);
    r.cap_floor_volatilities =
        read_where(ctx, cap_floor_volatility_config_repository(), of_definition);
    r.parametric_smiles = read_where(ctx, curve_parametric_smile_repository(), of_definition);
    r.parametric_smile_parameters =
        read_where(ctx, curve_parametric_smile_parameter_repository(), of_definition);
    r.equity_volatilities = read_where(ctx, equity_volatility_config_repository(), of_definition);
    r.commodity_volatilities =
        read_where(ctx, commodity_volatility_config_repository(), of_definition);
    r.bond_future_volatilities =
        read_where(ctx, bond_future_volatility_config_repository(), of_definition);
    r.bootstrap_configs = read_where(ctx, curve_bootstrap_config_repository(), of_definition);
    r.segments = read_where(ctx, curve_segment_repository(), of_definition);
    r.quotes = read_where(ctx, curve_quote_repository(), of_definition);
    std::set<boost::uuids::uuid> segments;
    for (const auto& s : r.segments)
        segments.insert(s.id);
    r.segment_curves = read_where(ctx, curve_segment_curve_repository(), [&](const auto& row) {
        return segments.contains(row.curve_segment_id);
    });
    return r;
}

conventions_write_result write(const context& ctx, domain::mapped_conventions v) {
    stamp_party(ctx, v);
    conventions_write_result r;
    replace_party_rows(ctx, zero_convention_repository(), std::move(v.zero));
    replace_party_rows(ctx, average_ois_convention_repository(), std::move(v.average_ois));
    replace_party_rows(ctx, bma_basis_swap_convention_repository(), std::move(v.bma_basis_swap));
    replace_party_rows(
        ctx, cross_currency_basis_convention_repository(), std::move(v.cross_currency_basis));
    replace_party_rows(ctx,
                       cross_currency_fix_float_convention_repository(),
                       std::move(v.cross_currency_fix_float));
    replace_party_rows(
        ctx, tenor_basis_swap_convention_repository(), std::move(v.tenor_basis_swap));
    replace_party_rows(
        ctx, tenor_basis_two_swap_convention_repository(), std::move(v.tenor_basis_two_swap));
    replace_party_rows(ctx, deposit_convention_repository(), std::move(v.deposit));
    replace_party_rows(ctx, swap_convention_repository(), std::move(v.swap));
    replace_party_rows(ctx, swap_index_convention_repository(), std::move(v.swap_index));
    replace_party_rows(ctx, future_convention_repository(), std::move(v.future));
    replace_party_rows(ctx, fx_option_convention_repository(), std::move(v.fx_option));
    replace_party_rows(ctx, inflation_swap_convention_repository(), std::move(v.inflation_swap));
    replace_party_rows(
        ctx, intraday_power_load_convention_repository(), std::move(v.intraday_power_load));
    replace_party_rows(ctx, ois_convention_repository(), std::move(v.ois));
    replace_party_rows(ctx, fra_convention_repository(), std::move(v.fra));
    replace_party_rows(
        ctx, zero_inflation_index_convention_repository(), std::move(v.zero_inflation_index));
    replace_party_rows(ctx, cds_convention_repository(), std::move(v.cds));
    replace_party_rows(
        ctx, cms_spread_option_convention_repository(), std::move(v.cms_spread_option));
    replace_party_rows(
        ctx, commodity_future_convention_repository(), std::move(v.commodity_future));
    replace_party_rows(
        ctx, commodity_forward_convention_repository(), std::move(v.commodity_forward));
    replace_party_rows(ctx, bond_yield_convention_repository(), std::move(v.bond_yield));
    add_missing_world_rows(ctx, ibor_index_convention_repository(), v.ibor_index, r.world_kept);
    add_missing_world_rows(
        ctx, overnight_index_convention_repository(), v.overnight_index, r.world_kept);
    for (const auto& fx : v.fx)
        r.fx_skipped.push_back(fx.pair.base_currency + "-" + fx.pair.quote_currency +
                               "-FX-CONVENTIONS");
    return r;
}

domain::mapped_conventions read_conventions(const context& ctx) {
    domain::mapped_conventions r;
    r.zero = zero_convention_repository().read_latest(ctx);
    r.average_ois = average_ois_convention_repository().read_latest(ctx);
    r.bma_basis_swap = bma_basis_swap_convention_repository().read_latest(ctx);
    r.cross_currency_basis = cross_currency_basis_convention_repository().read_latest(ctx);
    r.cross_currency_fix_float = cross_currency_fix_float_convention_repository().read_latest(ctx);
    r.tenor_basis_swap = tenor_basis_swap_convention_repository().read_latest(ctx);
    r.tenor_basis_two_swap = tenor_basis_two_swap_convention_repository().read_latest(ctx);
    r.deposit = deposit_convention_repository().read_latest(ctx);
    r.swap = swap_convention_repository().read_latest(ctx);
    r.swap_index = swap_index_convention_repository().read_latest(ctx);
    r.future = future_convention_repository().read_latest(ctx);
    r.fx_option = fx_option_convention_repository().read_latest(ctx);
    r.inflation_swap = inflation_swap_convention_repository().read_latest(ctx);
    r.intraday_power_load = intraday_power_load_convention_repository().read_latest(ctx);
    r.ois = ois_convention_repository().read_latest(ctx);
    r.fra = fra_convention_repository().read_latest(ctx);
    r.zero_inflation_index = zero_inflation_index_convention_repository().read_latest(ctx);
    r.cds = cds_convention_repository().read_latest(ctx);
    r.cms_spread_option = cms_spread_option_convention_repository().read_latest(ctx);
    r.commodity_future = commodity_future_convention_repository().read_latest(ctx);
    r.commodity_forward = commodity_forward_convention_repository().read_latest(ctx);
    r.bond_yield = bond_yield_convention_repository().read_latest(ctx);
    r.ibor_index = ibor_index_convention_repository().read_latest(ctx);
    r.overnight_index = overnight_index_convention_repository().read_latest(ctx);
    return r;
}

}
